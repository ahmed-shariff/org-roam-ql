;;; org-roam-ql.el --- Intgrating org-roam and org-ql -*- lexical-binding: t -*-

;; Copyright (c) 2023 Shariff AM Faleel (shariff.mfa@outlook.com)

;; Author: Shariff AM Faleel
;; Version: 0.1
;; Package-Requires: ((emacs "28") (org-ql "0.7-pre") (org "9.0") (org-roam "2.2.2") (s "1.13.1"))
;; Homepage: https://github.com/ahmed-shariff/org-roam-ql
;; Keywords: org-roam, query
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; TBD

;;; Code:

(require 'org-ql)
(require 'org-ql-view)
(require 'org-roam-utils)
(require 'org-roam-node)
(require 'dash)
(require 's)

(defvar org-roam-ql--current-nodes nil)
;; FIXME: What is this docstring!
(defvar org-roam-ql--query-comparison-functions (make-hash-table) "Holds the function to check different elements of the roam-query.")
(defvar org-roam-ql--cache (make-hash-table))

;;;###autoload
(defun org-roam-ql-nodes (source-or-query)
  "Convert SOURCE-OR-QUERY to org-roam-nodes.  SOURCE-OR-QUERY can be
one of the following:
- A org-roam-ql query.
- A buffer-name of a org-roam-mode buffer.
- A list of params that can be passed to `org-roam-db-query'. Expected
  to have the form (QUERY ARG1 ARG2 ARG3...). `org-roam-db-query' will
  called with the list or parameters as: (org-roam-db-query QUERY ARG1
  ARG2 ARG3...). The first element in each row in the result from the
  query is expected to have the ID of a corresponding node, which will
  be conerted to a org-roam-node. QUERY can be a complete query. If
  the query is going to be of the form [:select [id] :from nodes
  :where (= todo \"TODO\")], you can omit the part till after
  :where. i.e., pass only [(= todo \"TODO\")] and the rest will get
  appended in the front.
- A list of org-roam-nodes
- A function that returns a list of org-roam-nodes.

If called programmatically, some values may get cached, make sure to
call `org-roam-ql-clear-cache'."
  (cond
   ;; TODO: think of a better way to display the nodes in the query
   ;; without showing it all. Perhaps use only ids?
   ((-all-p #'org-roam-node-p source-or-query) source-or-query)
   ;; get-buffer returns a buffer if source-or-query is a buffer obj
   ;; or the name of a buffer
   ((-when-let (buffer (and (or (stringp source-or-query) (bufferp source-or-query)) (get-buffer source-or-query)))
      (with-current-buffer buffer (derived-mode-p 'org-roam-mode)))
    (org-roam-ql--nodes-from-roam-buffer (get-buffer source-or-query)))
   ((and (listp source-or-query) (vectorp (car source-or-query)))
    (let ((query (car source-or-query))
          (args (cdr source-or-query)))
      (--map (org-roam-node-from-id (car it))
             (apply #'org-roam-db-query
                    (if (equal :select (aref query 0))
                        query
                      (vconcat [:select id :from nodes :where] query))
                    args))))
   ((and (listp source-or-query) (org-roam-ql--check-if-valid-query source-or-query))
    (-filter (lambda (it)
               (org-roam-ql--expand-query source-or-query it)) (org-roam-node-list)))
   ((functionp source-or-query) (funcall source-or-query))))

;; Can we make org-roam-ql aware of any changes that can happen?
(defun org-roam-ql--nodes-cached (source-or-query)
  "Cache results of org-roam-ql-nodes.
  Not caching or invalidating in the top level function as the
database/buffers can change. currently this is only used by the
internal functions"
  (let ((cached-value (gethash source-or-query org-roam-ql--cache)))
    (if cached-value
        cached-value
      (puthash source-or-query
               (org-roam-ql-nodes source-or-query)
               org-roam-ql--cache))))

(defun org-roam-ql-clear-cache ()
  "Clear the org-roam-ql cache."
  (setq org-roam-ql--cache (make-hash-table)))

(defun org-roam-ql--nodes-files (nodes)
  "Returns the list of files from the list of NODES."
  (-uniq (mapcar #'org-roam-node-file nodes)))

(defun org-roam-ql--check-if-valid-query (s-exp)
  "Check if S-EXP can be expanded to a roam-query."
  (if (listp s-exp)
      (or (and (member (car s-exp) '(or and))
               (--map (org-roam-ql--check-if-valid-query it) (cdr s-exp)))
          (member (car s-exp) (hash-table-keys org-roam-ql--query-comparison-functions)))
    t))

;;;###autoload
(defmacro org-roam-ql-defpred (name extraction-function comparison-function)
  "Creates a roam-predicate with the NAME.  The COMPARISON-FUNCTION is
a function that returns non-nil if this predicate doesn't fail for a
given org-roam-node. The first value passed to this function would be
the value from calling the EXTRACT-FUNCTION with the respective node,
and the remainder of the arguments from the predicate itself."
  `(puthash ,name (cons ,extraction-function ,comparison-function) org-roam-ql--query-comparison-functions))

(defun org-roam-ql--predicate-s-match (value regexp &optional exact)
  (when (and value regexp)
    (if exact
        (s-equals-p value regexp)
      (s-match regexp value))))

(defun org-roam-ql--predicate-s-equals-p (value other)
  (when (and value other)
    (s-equals-p value other)))

;; TODO: Multivalue properties
(defun org-roam-ql--predicate-property-match (value prop prop-val)
  (-when-let (val (assoc prop value))
    (s-match prop-val (cdr val))))

(defun org-roam-ql--predicate-tags-match (values &rest tags)
  (--all-p (member it values) (-list tags)))

;; TODO: option of or/and
(defun org-roam-ql--predicate-backlinked-to (value source-or-query)
  "VALUE is the list if IDs of nodes backlinked from a given node. If
any of the nodes from source-or-query are in that list, return
non-nil."
  (let ((target-node-ids (--map (org-roam-node-id it) (org-roam-ql--nodes-cached source-or-query))))
    (-intersection value target-node-ids)))

(defun org-roam-ql--extract-forwardlink-ids (node)
  (-map #'car (org-roam-db-query
                [:select :distinct links:dest
                         :from links
                         :where (in links:source $v1)]
                (vector (org-roam-node-id node)))))

(defun org-roam-ql--predicate-backlinked-from (value source-or-query)
  "VALUE is the list of backlink destinations."
  (let ((destination-nodes (org-roam-ql--nodes-cached source-or-query)))
    (-intersection value destination-nodes)))

(defun org-roam-ql--extract-backlink-source (node)
  (--map (org-roam-backlink-source-node it) (org-roam-backlinks-get node)))

(defun org-roam-ql--predicate-in-query (value source-or-query)
  (let* ((nodes (org-roam-ql--nodes-cached source-or-query))
         ;; FIXME: equalp directly on the nodes is not working?
         (node-ids (-map #'org-roam-node-id nodes)))
    (member (org-roam-node-id value) node-ids)))

(defun org-roam-ql--expand-query (query it)
  (if (and (listp query) (member (car query) '(or and)))
      (funcall
       (pcase (car query)
         ('or #'-any-p)
         ('and #'-all-p))
       'identity
       (-map (lambda (sub-query) (org-roam-ql--expand-query sub-query it)) (cdr query)))
    (-if-let* ((query-key (and (listp query) (car query)))
               (query-comparison-function-info (gethash query-key org-roam-ql--query-comparison-functions)))
        (let ((val (funcall (car query-comparison-function-info) it)))
          (and val
               (apply (cdr query-comparison-function-info) (append (list val) (cdr query)))))
      (-if-let (nodes (org-roam-ql--nodes-cached query))
          (member (org-roam-node-id it) (-map #'org-roam-node-id nodes))
        (user-error (format "Invalid query %s. %s not in org-roam-ql predicate list (See `org-roam-ql-defpred') or recognized by `org-roam-ql-nodes'."
                            query (car query)))))))

;;;###autoload
(defun org-roam-ql-view (source-or-query &optional title query super-groups)
  "Basically what `org-ql-search does', but for org-roam-nodes.  See
`org-roam-ql-nodes' for what SOURCE-OR-QUERY can be. TITLE is a title
to associate with the view.  See `org-roam-search' for details on
SUPER-GROUPS."
  (interactive (list (let ((query (read-minibuffer "Query: ")))
                       (if (vectorp query)
                           (list query)
                         query))
                     (read-string "Title: ")))

  (with-temp-buffer
    (let* ((nodes (org-roam-ql-nodes source-or-query))
           (strings '())
           ;; TODO: Think of a better way to get a default title
           (title (format "org-roam - %s" (or title (substring source-or-query 0 10))))
           (buffer (format "%s %s*" org-ql-view-buffer-name-prefix title))
           (header (org-ql-view--header-line-format
                    :title title))
           (org-ql-view-buffers-files (org-roam-ql--nodes-files nodes))
           ;; TODO: When the query also has a org-roam-query
           (org-ql-view-query (append
                               `(and (org-roam-query ,source-or-query))
                               query))
           (org-ql-view-sort nil)
           (org-ql-view-narrow nil)
           (org-ql-view-super-groups super-groups)
           (org-ql-view-title title))
      ;; Invalidating cache to allow detecting changes.
      (org-roam-ql-clear-cache)
      (dolist-with-progress-reporter (node nodes)
          (format "Processing %s nodes" (length nodes))
        (push (org-roam-ql-view--format-node node) strings))
      (when super-groups
        (let ((org-super-agenda-groups (cl-etypecase super-groups
                                         (symbol (symbol-value super-groups))
                                         (list super-groups))))
          (setf strings (org-super-agenda--group-items strings))))
      (org-ql-view--display :buffer buffer :header header
        :string (s-join "\n" strings))
      (with-current-buffer buffer
        ;; HACK - to make the buffer get rendered properly.
        (org-ql-view-refresh)))))

;; FIXME: To be performant this can be done by constructing the
;; results instead of going through org-ql?
;;;###autoload
(defun org-roam-ql-select (source-or-query &optional ql-query action narrow sort)
  "Process SOURCE-OR-QUERY with org-roam-db and pass it to org-ql to
be filtered with QL-QUERY.  ACTION NARROW and SORT are passed to
`org-ql-select' as is.

See `org-roam-ql-nodes' for the values that can be passed to
SOURCE-OR-QUERY."
  (let* ((nodes (org-roam-ql-nodes source-or-query))
         (buffers (org-roam-ql--nodes-files nodes))
         (query (append `(and (org-roam-query ,source-or-query)) ql-query)))
    (org-roam-ql-clear-cache)
    (when buffers
      (org-ql-select buffers query :action action :narrow narrow :sort sort))))

(org-ql-defpred org-roam-query (query)
  "To be used with the org-roam-ql. Checks if a node is a result of a passed query."
  :preambles ((`(,predicate-names . ,query)
               (list :regexp (rx-to-string
                              `(seq bol (0+ space) ":ID:" (0+ space)
                                    (or ,@(-map
                                           #'org-roam-node-id
                                           org-roam-ql--current-nodes))
                                    eol))
                     :query t))))

(defun org-roam-ql--get-queries (query)
  "Recursively traverse and get the org-roam-query's from a org-ql query."
  (if (listp query)
      (if (equal (car query) 'org-roam-query)
          (list query)
        (apply #'append (-non-nil (--map (org-roam-ql--get-queries it) query))))
    nil))

(defun org-roam-ql--refresh (other-func &rest rest)
  "When `org-ql-view' is refreshed, if this is created from a
`org-roam-ql' function, update the variables accordingly."
  (unless org-ql-view-buffers-files
    (user-error "Not an Org QL View buffer"))
  (-when-let (queries (org-roam-ql--get-queries org-ql-view-query))
    (let* ((nodes (apply #'append (--map (apply #'org-roam-ql--nodes-cached (cdr it)) queries))))
      (org-roam-ql-clear-cache)
      (setq org-roam-ql--current-nodes nodes
            org-ql-view-buffers-files (org-roam-ql--nodes-files nodes))))
  (apply other-func rest))

(defmacro with-plain-file (file keep-buf-p &rest body)
  "Same as `org-roam-with-file', but doesn't start `org-roam'."
  (declare (indent 2) (debug t))
  `(let* (new-buf
          (delay-mode-hooks t)
          (auto-mode-alist nil)
          (find-file-hook nil)
          (change-major-mode-after-body-hook nil)
          (after-change-major-mode-hook nil)
          (buf (or
                (and (not ,file)
                     (current-buffer)) ;If FILE is nil, use current buffer
                (find-buffer-visiting ,file) ; If FILE is already visited, find buffer
                (progn
                  (setq new-buf t)
                  (find-file-noselect ,file)))) ; Else, visit FILE and return buffer
          res)
     (with-current-buffer buf
       (setq res (progn ,@body))
       (unless (and new-buf (not ,keep-buf-p))
         (save-buffer)))
     (if (and new-buf (not ,keep-buf-p))
         (when (find-buffer-visiting ,file)
           (kill-buffer (find-buffer-visiting ,file))))
     res))

;; modified org-ql-view--format-element to work with org-roam nodes
(defun org-roam-ql-view--format-node (node)
  "Return NODE as a string with text-properties set by its property
list.  If NODE is nil, return an empty string."
  (if (not node)
      ""
    (let* ((marker
            (org-roam-ql--get-file-marker node))
           (properties (list
                        'org-marker marker
                        'org-hd-marker marker))
           ;; (properties '())
           (string ;;(org-roam-node-title node))
            (s-join " "
                    (-non-nil
                     (list
                      (when-let (todo (org-roam-node-todo node))
                        (org-ql-view--add-todo-face todo))
                      (when-let (priority (org-roam-node-priority node))
                        (org-ql-view--add-priority-face (byte-to-string priority)))
                      (org-roam-node-title node)
                      nil;;due-string
                      (when-let (tags (org-roam-node-tags node))
                         (--> tags
                           (s-join ":" it)
                           (s-wrap it ":")
                           (org-add-props it nil 'face 'org-tag))))))))
      (remove-list-of-text-properties 0 (length string) '(line-prefix) string)
      ;; Add all the necessary properties and faces to the whole string
      (--> string
        ;; FIXME: Use proper prefix
        (concat "  " it)
        (org-add-props it properties
          'org-agenda-type 'search
          'todo-state (org-roam-node-todo node)
          'tags (org-roam-node-tags node)
          ;;'org-habit-p (org)
          )))))

(defun org-roam-ql--get-file-marker (node)
  (org-roam-with-file (org-roam-node-file node) t
  ;; (with-current-buffer (find-file-noselect (org-roam-node-file node))
  ;; (with-plain-file (org-roam-node-file node) t
    (goto-char (org-roam-node-point node))
    (point-marker)))

(defun org-roam-ql--nodes-from-roam-buffer (org-roam-buffer)
  "Collect the org-roam-nodes from a ORG-ROAM-BUFFER."
  (with-current-buffer org-roam-buffer
    (when (derived-mode-p 'org-roam-mode)
      (let (nodes)
        (goto-char 0)
        (while (condition-case err
                   (progn
                     (magit-section-forward)
                     t ;; keep the while loop going
                     )
                 (user-error
                  (if (equal (error-message-string err) "No next section")
                      nil ;; end while loop
                    (signal (car err) (cdr err))))) ;; somthing else happened, re-throw
          (let ((magit-section (plist-get (text-properties-at (point)) 'magit-section)))
            (when (org-roam-node-section-p magit-section)
              (push (slot-value magit-section 'node) nodes))))
        nodes))))

;;;###autoload
(defun org-roam-ql-ql-buffer-from-roam-buffer ()
  "Convert a roam buffer to org-ql buffer."
  (interactive)
  (when (derived-mode-p 'org-roam-mode)
    ;; TODO: if the buffer is the *org-roam* buffer, this will change
    ;; if the current-org-roam-node itself changes.  Because of the
    ;; `org-roam-node-sections', I am not sure how to compute the
    ;; nodes without re-rendering it a new buffer
    (org-roam-ql-view (buffer-name (current-buffer)) (--if-let header-line-format it ""))))

(defun org-roam-ql--refresh-buffer (fn &rest args)
  (when (equal (buffer-name) org-roam-buffer)
    (apply fn args)))

(define-derived-mode org-roam-ql-mode org-roam-mode "Org-roam-ql"
  "A major mode to display a list of nodes. Similar to org-roam-mode, but doesn't default to the org-roam-current-node."
  :group 'org-roam-ql
  (advice-add 'org-roam-buffer-refresh :around #'org-roam-ql--refresh-buffer))

;;;###autoload
(defun org-roam-ql-roam-buffer-from-ql-buffer ()
  "Convert a org-ql reusult to a roam-buffer."
  (interactive)
  (unless org-ql-view-buffers-files
    (user-error "Not an Org QL View buffer"))
  ;; Copied from `org-agenda-finalize'
  (let (mrk nodes (line-output 0))
    (save-excursion
      (goto-char (point-min))
      (while (equal line-output 0)
	(when (setq mrk (get-text-property (point) 'org-hd-marker))
          (org-with-point-at mrk
            ;; pick only nodes
            (-if-let (id (org-id-get))
                (push (org-roam-node-from-id id) nodes)
              (user-error "Non roam-node headings in query."))))
        (setq line-output (forward-line))))
    (org-roam-ql--buffer-for-nodes nodes
                                   org-ql-view-title
                                   (format "*From ql: %s*" org-ql-view-title))))

(defun org-roam-ql--render-buffer (sections title buffer-name)
  "Render SECTIONS (list of functions) in an org-roam-ql buffer."
  ;; copied  from `org-roam-buffer-render-contents'
  (with-current-buffer (get-buffer-create buffer-name)
    (let ((inhibit-read-only t))
      (erase-buffer)
      (org-roam-ql-mode)
      (org-roam-buffer-set-header-line-format title)
      (insert ?\n)
      (dolist (section sections)
        (funcall section))
      (goto-char 0))
    (display-buffer (current-buffer))))

(defmacro org-roam-ql--nodes-section (nodes &optional heading)
  "Returns a function that can be passed as a section for
`okm-render-org-roam-buffer' with the NODES.  Nodes should be a list
of org-roam nodes."
  `(lambda ()
     (magit-insert-section (org-roam)
       (magit-insert-heading)
       (dolist (entry
                ;;(seq-uniq  ;; removing duplicates as the whole subtree will be getting displayed
                ,nodes)
         (let ((pos (org-roam-node-point entry))
               (properties (org-roam-node-properties entry)))
           (org-roam-node-insert-section :source-node entry :point pos :properties properties))
         (insert ?\n))
       (run-hooks 'org-roam-buffer-postrender-functions))))

(defun org-roam-ql--buffer-for-nodes (nodes title buffer-name)
  "View nodes in org-roam-ql buffer"
  (org-roam-ql--render-buffer
   (list
    (org-roam-ql--nodes-section nodes))
   title buffer-name))

(advice-add 'org-ql-view-refresh :around #'org-roam-ql--refresh)

(dolist (predicate '((file org-roam-node-file . org-roam-ql--predicate-s-match)
                     (file-title org-roam-node-file-title . org-roam-ql--predicate-s-match)
                     (file-atime org-roam-node-file-atime . time-equal-p)
                     (file-mtime org-roam-node-file-mtime . time-equal-p)
                     (id org-roam-node-id . org-roam-ql--predicate-s-equals-p)
                     (level org-roam-node-level . equal)
                     (point org-roam-node-point . equal)
                     (todo org-roam-node-todo . org-roam-ql--predicate-s-match)
                     (priority org-roam-node-priority . org-roam-ql--predicate-s-match)
                     (scheduled org-roam-node-scheduled . time-less-p)
                     (deadline org-roam-node-deadline . time-less-p)
                     (title org-roam-node-title . org-roam-ql--predicate-s-match)
                     (properties org-roam-node-properties . org-roam-ql--predicate-property-match)
                     (tags org-roam-node-tags . org-roam-ql--predicate-tags-match)
                     (refs org-roam-node-refs . org-roam-ql--predicate-s-match)
                     (backlink-to org-roam-ql--extract-forwardlink-ids . org-roam-ql--predicate-backlinked-to)
                     (backlink-from org-roam-ql--extract-backlink-source . org-roam-ql--predicate-backlinked-from)
                     (in-buffer identity . org-roam-ql--predicate-in-query)
                     (nodes-list identity . org-roam-ql--predicate-in-query)
                     (function identity . org-roam-ql--predicate-in-query)))
  (org-roam-ql-defpred (car predicate) (cadr predicate) (cddr predicate)))

(provide 'org-roam-ql)

;;; org-roam-ql.el ends here
