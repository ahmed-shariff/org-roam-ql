;;; org-roam-ql.el --- Intgrating org-roam and org-ql -*- lexical-binding: t -*-

;; Copyright (c) 2023 Shariff AM Faleel (shariff.mfa@outlook.com)

;; Author: Shariff AM Faleel
;; Version: 0.1
;; Package-Requires: ((emacs "28") (org-ql "0.7-pre") (org "9.0") (org-roam "2.2.2") (s "1.13.1") (magit-section "3.3.0"))
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
- A list of org-roam-nodes or an org-roam-node.
- A function that returns a list of org-roam-nodes.

If called programmatically, some values may get cached, make sure to
call `org-roam-ql-clear-cache'."
  (cond
   ;; TODO: think of a better way to display the nodes in the query
   ;; without showing it all. Perhaps use only ids?
   ((or (org-roam-node-p source-or-query)
        (-all-p #'org-roam-node-p source-or-query))
    (-list source-or-query))
   ;; get-buffer returns a buffer if source-or-query is a buffer obj
   ;; or the name of a buffer
   ((-when-let (buffer (and (or (stringp source-or-query)
                                (bufferp source-or-query))
                            (get-buffer source-or-query)))
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
  `(puthash ,name
            (cons ,extraction-function ,comparison-function)
            org-roam-ql--query-comparison-functions))

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

(defun org-roam-ql--predicate-funcall (value f)
  (funcall f value))

(defun org-roam-ql--expand-query (query it)
  "Used to expand a QUERY."
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
        (user-error
         (format "Invalid query %s. %s not in org-roam-ql predicate list (See `org-roam-ql-defpred') or recognized by `org-roam-ql-nodes'."
                            query (car query)))))))

;;;###autoload
(defun org-roam-ql-search (source-or-query display-in &optional title super-groups)
  "Basically what `org-ql-search does', but for org-roam-nodes.  See
`org-roam-ql-nodes' for what SOURCE-OR-QUERY can be. TITLE is a title
to associate with the view. DISPLAY-IN is expected to be a symbol,
either `'org-ql' or `'org-roam'. If its `org-ql', the results from the
SOURCE-OR-QUERY will be displayed in `org-ql's agenda buffer. If its
`org-roam', will be displayed in a org-roam-ql buffer. See
`org-roam-search' for details on SUPER-GROUPS."
  (interactive (list (let ((query (read-minibuffer "Query: ")))
                       (if (vectorp query)
                           (list query)
                         query))
                     (intern-soft (completing-read "Display in: " '(org-roam org-ql) nil t))
                     (read-string "Title: ")))
    (let* ((nodes (org-roam-ql-nodes source-or-query))
           (title (org-roam-ql--get-formatted-title title source-or-query)))
      (pcase display-in
       ('org-ql
        (with-temp-buffer
          (let* ((strings '())
                 (buffer (org-roam-ql--get-formatted-buffer-name title source-or-query))
                 (header (org-ql-view--header-line-format
                          :title title))
                 (org-ql-view-buffers-files (org-roam-ql--nodes-files nodes))
                 ;; TODO: When the query also has a org-roam-query
                 (org-ql-view-query `(org-roam-query ,source-or-query))
                 (org-ql-view-sort nil)
                 (org-ql-view-narrow nil)
                 (org-ql-view-super-groups super-groups)
                 (org-ql-view-title title))
            ;; Invalidating cache to allow detecting changes.
            (org-roam-ql-clear-cache)
            (if (not nodes)
                (user-error "Empty result for query.")
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
              ;; (with-current-buffer buffer
              ;;   ;; HACK - to make the buffer get rendered properly.
              ;;   (org-ql-view-refresh))))))
              ))))
       ('org-roam
        (org-roam-ql--buffer-for-nodes
         nodes
         title
         (org-roam-ql--get-formatted-buffer-name title nil)
         source-or-query)))))

(defun org-roam-ql--get-formatted-title (title source-or-query)
  "Return the formatted title."
  ;; TODO: Think of a better way to get a default title
  (format "org-roam - %s" (or title (substring (format "%s" source-or-query) 0 10))))

(defun org-roam-ql--get-formatted-buffer-name (title source-or-query)
  "Return the formatted buffer name."
  (format "*%s*" (org-roam-ql--get-formatted-title title source-or-query)))

;; *****************************************************************************
;; Functions to work with org-ql-view
;; *****************************************************************************

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
    (let* ((nodes (apply #'append
                         (--map (apply #'org-roam-ql--nodes-cached
                                       (cdr it))
                                queries))))
      (org-roam-ql-clear-cache)
      (setq org-roam-ql--current-nodes nodes)
      ;; If results are empty buffer gets empty
      ;; `org-ql-view-buffers-files' is left alone to avoid org-ql
      ;; erroring with "Not an Org QL View buffer"
      (when nodes
        (setq org-ql-view-buffers-files (org-roam-ql--nodes-files nodes)))))
  (apply other-func rest))

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

;; *****************************************************************************
;; Functions to switch between org-roam/org-roam-ql buffers and
;; org-ql-view buffers
;; *****************************************************************************
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
          (let ((magit-section (plist-get
                                (text-properties-at (point))
                                'magit-section)))
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
    (org-roam-ql-search (buffer-name (current-buffer))
                        'org-ql
                        (--if-let header-line-format it ""))))

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
              (user-error (format "Non roam-node headings in query (in buffer %s)."
                                  (buffer-name))))))
        (setq line-output (forward-line))))
    (org-roam-ql--buffer-for-nodes nodes
                                   org-ql-view-title
                                   (format "*From ql: %s*" org-ql-view-title))))

;; *****************************************************************************
;; org-roam-ql mode and functions to build them
;; *****************************************************************************
(defvar org-roam-ql-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map org-roam-mode-map)
    (define-key map "v" #'org-roam-ql-buffer-dispatch)
    (define-key map [remap revert-buffer] #'org-roam-ql-refresh-buffer)
    map))

(define-derived-mode org-roam-ql-mode org-roam-mode "Org-roam-ql"
  "A major mode to display a list of nodes. Similar to org-roam-mode,
but doesn't default to the org-roam-current-node."
  :group 'org-roam-ql)

;;;###autoload
(defun org-roam-ql-refresh-buffer ()
  (interactive)
  (if (equal (buffer-name) org-roam-buffer)
      (if (not org-roam-ql-buffer-query)
          (org-roam-buffer-refresh)
        (let ((query org-roam-ql-buffer-query)
              (title (or org-roam-ql-buffer-title (format "%s - extended" (org-roam-node-title org-roam-current-node))))
              (in (or org-roam-ql-buffer-in "org-roam-db")))
          (setq org-roam-ql-buffer-query nil
                org-roam-ql-buffer-title nil
                org-roam-ql-buffer-in nil)
          (org-roam-buffer-refresh)
          (org-roam-ql-search `(id ,(org-roam-node-id org-roam-current-node))
                              'org-roam title query)))
    (org-roam-ql--refresh-buffer)))

(defun org-roam-ql--refresh-buffer (&optional buffer-name)
  (let* ((buffer-name (or buffer-name (buffer-name)))
         (query (pcase org-roam-ql-buffer-in
                  ("in-buffer" `(and (in-buffer ,buffer-name)
                                     ,org-roam-ql-buffer-query))
                  ("org-roam-db" org-roam-ql-buffer-query)
                  (_ (user-error "Invalid value for `org-roam-ql-buffer-in'")))))
    (org-roam-ql--buffer-for-nodes
     (org-roam-ql-nodes query)
     (if (s-equals-p org-roam-ql-buffer-in "in-buffer")
         (org-roam-ql--get-formatted-title
          (format "%s - extended" (s-replace "org-roam - " "" org-roam-ql-buffer-title)) nil)
       org-roam-ql-buffer-title)
     (if (s-equals-p org-roam-ql-buffer-in "in-buffer")
         (org-roam-ql--get-formatted-buffer-name
          (format "%s - extended" (s-replace "org-roam - " "" org-roam-ql-buffer-title)) nil)
       buffer-name)
     query)))

(defun org-roam-ql--render-buffer (sections title buffer-name source-or-query)
  "Render SECTIONS (list of functions) in an org-roam-ql buffer."
  ;; copied  from `org-roam-buffer-render-contents'
  (with-current-buffer (get-buffer-create buffer-name)
    (let ((inhibit-read-only t))
      (erase-buffer)
      (org-roam-ql-mode)
      (org-roam-buffer-set-header-line-format title)
      (setq org-roam-ql-buffer-query source-or-query
            org-roam-ql-buffer-title title
            org-roam-ql-buffer-in "org-roam-db")
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
       (magit-insert-heading ,heading)
       (dolist (entry
                ;;(seq-uniq  ;; removing duplicates as the whole subtree will be getting displayed
                ,nodes)
         (let ((pos (org-roam-node-point entry))
               (properties (org-roam-node-properties entry)))
           (org-roam-node-insert-section
            :source-node entry
            :point pos
            :properties properties))
         (insert ?\n))
       (run-hooks 'org-roam-buffer-postrender-functions))))

(defun org-roam-ql--buffer-for-nodes (nodes title buffer-name &optional source-or-query)
  "View nodes in org-roam-ql buffer"
  (org-roam-ql--render-buffer
   (list
    (org-roam-ql--nodes-section nodes "Nodes:"))
   title buffer-name (or source-or-query nodes)))

;; *****************************************************************************
;; org-roam-ql transient
;; *****************************************************************************
;; Copying alot it from org-ql-view and magit-transient

(defclass org-roam-ql--variable (transient-variable)
  ((default-value :initarg :default-value)))

(defclass org-roam-ql--variable:choices (org-roam-ql--variable) nil)

(defclass org-roam-ql--variable:sexp (org-roam-ql--variable) nil)

(cl-defmethod transient-init-value ((obj org-roam-ql--variable))
  ;; init value with the given default or the value of the related vairable
  (oset obj value (or (symbol-value (oref obj variable))
                      (and (slot-boundp obj 'default-value)
                           (oref obj default-value)))))

(cl-defmethod transient-infix-set ((obj org-roam-ql--variable) value)
  "Set org-roam-ql mode variable defined by OBJ to VALUE."
  (let* ((variable (oref obj variable))
         (default-value (and (slot-boundp obj 'default-value)
                             (oref obj default-value)))
         (value (or value default-value)))
    (oset obj value value)
    (set (make-local-variable (oref obj variable)) value)
    (unless (or value transient--prefix)
      (message "Unset %s" variable))))

(cl-defmethod transient-infix-read ((obj org-roam-ql--variable:choices))
  "Pick between CHOICES in OBJ when reading."
  (let ((choices (oref obj choices)))
    (if-let ((value (oref obj value)))
        (or (cadr (member value choices))
            (car choices))
      (car choices))))

(cl-defmethod transient-format-description ((obj org-roam-ql--variable))
  "Format of the OBJ's DESCRIPTION."
  (format "%s" (propertize (oref obj prompt) 'face 'transient-argument)))

(cl-defmethod transient-format-value ((obj org-roam-ql--variable))
  "Format of the OBJ's VALUE."
  (oref obj value))

(cl-defmethod transient-format-value ((obj org-roam-ql--variable:choices))
  "Format of the OBJ's VALUE for choices."
  (let ((value (oref obj value)))
    (format "{ %s }"
            (s-join " | " (--map (if (equalp it value)
                                     it
                                   (propertize it 'face 'transient-inactive-value))
                                 (oref obj choices))))))

(cl-defmethod transient-format-value ((obj org-roam-ql--variable:sexp))
  "Format of the OBJ's VALUE for sexpressions."
  ;; copied from `org-roam-ql'.
  (let ((value (format "%S" (oref obj value))))
    (with-temp-buffer
      (delay-mode-hooks
        (insert value)
        (funcall 'emacs-lisp-mode)
        (font-lock-ensure)
        (buffer-string)))))

(make-variable-buffer-local (defvar org-roam-ql-buffer-title nil "The current title of the buffer."))
(make-variable-buffer-local (defvar org-roam-ql-buffer-query nil "The current query of the buffer."))
(make-variable-buffer-local (defvar org-roam-ql-buffer-in nil
                              "Define which option to use - 'in-buffer' or 'org-roam-db'."))

(transient-define-prefix org-roam-ql-buffer-dispatch ()
  "Show Org QL View dispatcher."
  [["Edit"
    ("t" org-roam-ql-view--transient-title)
    ("q" org-roam-ql-view--transient-query)
    ("i" org-roam-ql-view--transient-in)]]
  [["View"
    ("r" "Refresh" org-roam-ql-refresh-buffer)]])

(defun org-roam-ql--format-transient-key-value (key value)
  "Return KEY and VALUE formatted for display in Transient."
  ;; `window-width' minus 15 is about right.  I think there's no way
  ;; to determine it automatically, because we can't know which column
  ;; Transient is starting at.
  (let ((max-width (- (window-width) 15)))
    (format "%s: %s" (propertize key 'face 'transient-argument)
            (s-truncate max-width (format "%s" value)))))

(transient-define-infix org-roam-ql-view--transient-title ()
  :class 'org-roam-ql--variable
  :argument ""
  :variable 'org-roam-ql-buffer-title
  :prompt "Title: "
  :always-read t
  :reader (lambda (prompt _initial-input history)
            ;; FIXME: Figure out how to integrate initial-input.
            (read-string prompt (when org-roam-ql-buffer-title
                                  (format "%s" org-roam-ql-buffer-title))
                         history)))

(transient-define-infix org-roam-ql-view--transient-query ()
  :class 'org-roam-ql--variable:sexp
  :argument ""
  :variable 'org-roam-ql-buffer-query
  :prompt "Query: "
  :always-read t
  :reader (lambda (prompt _initial-input history)
            ;; fixme: figure out how to integrate initial-input.
            (read (read-string prompt (when org-roam-ql-buffer-query
                                        (format "%S" org-roam-ql-buffer-query))
                               history))))

(transient-define-infix org-roam-ql-view--transient-in ()
  :class 'org-roam-ql--variable:choices
  :argument ""
  :variable 'org-roam-ql-buffer-in 
  :default-value "org-roam-db"
  :prompt "In: "
  :always-read t
  :choices '("in-buffer" "org-roam-db"))


;; Useful helper functions
(defun org-roam-ql-insert-node-title ()
  "Select a node and insert only its title. Can be used in the
minibuffer or when writting querries."
  (interactive)
  (insert (format "\"%s\"" (org-roam-node-title (org-roam-node-read nil nil nil t)))))

;; setup of org-roam-ql
(advice-add 'org-ql-view-refresh :around #'org-roam-ql--refresh)

(define-key org-roam-mode-map "v" #'org-roam-ql-buffer-dispatch)

(dolist (predicate
         '((file org-roam-node-file . org-roam-ql--predicate-s-match)
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
           (properties
            org-roam-node-properties . org-roam-ql--predicate-property-match)
           (tags org-roam-node-tags . org-roam-ql--predicate-tags-match)
           (refs org-roam-node-refs . org-roam-ql--predicate-s-match)
           (backlink-to
            org-roam-ql--extract-forwardlink-ids .
            org-roam-ql--predicate-backlinked-to)
           (backlink-from
            org-roam-ql--extract-backlink-source .
            org-roam-ql--predicate-backlinked-from)
           (in-buffer identity . org-roam-ql--predicate-in-query)
           (nodes-list identity . org-roam-ql--predicate-in-query)
           (function identity . org-roam-ql--predicate-in-query)
                     (funcall identity . org-roam-ql--predicate-funcall)))
  (org-roam-ql-defpred (car predicate) (cadr predicate) (cddr predicate)))

(provide 'org-roam-ql)

;;; org-roam-ql.el ends here
