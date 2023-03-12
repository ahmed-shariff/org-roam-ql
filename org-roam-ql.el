;;; org-roam-ql.el --- Intgrating org-roam and org-ql -*- lexical-binding: t -*-

;; Copyright (c) 2023 Shariff AM Faleel (shariff.mfa@outlook.com)

;; Author: Shariff AM Faleel
;; Version: 0.1
;; Package-Requires: ((emacs "28") (org-ql "0.7-pre") (org "9.0") (org-roam "2.2.2"))
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

(defvar org-roam-ql--current-nodes nil)

(defun org-roam-ql--get-nodes-from-query (source-or-query)
  "Convert SOURCE-OR-QUERY to org-roam-nodes.
For valid values of SOURCE-OR-QUERY see `org-roam-ql-select'."
  (cond
   ((-all-p #'org-roam-node-p source-or-query) source-or-query)
   ((and (listp source-or-query) (vectorp (car source-or-query)))
    (let ((query (car source-or-query))
          (args (cdr source-or-query)))
      (--map (org-roam-node-from-id (car it))
       (apply #'org-roam-db-query
             (if (equalp :select (aref query 0))
                 query
               (vconcat [:select id :from nodes :where] query))
             args))))
   ((functionp source-or-query) (funcall source-or-query))))

;;;###autoload
(defun org-roam-ql-view (source-or-query &optional title query super-groups)
  "Basically what `org-ql-search does', but for org-roam-nodes.
See `org-roam-ql--get-nodes-from-query' for what
SOURCE-OR-QUERY can be. TITLE is a title to associate with the view.
See `org-roam-search' for details on SUPER-GROUPS."
  (interactive (list (list (read-minibuffer "Query: "))
                     (read-string "Title: ")))
  (let* ((nodes (org-roam-ql--get-nodes-from-query source-or-query))
         (strings '())
         ;; TODO: Think of a better way to get a default title
         (title (format "org-roam - %s" (or title (substring source-or-query 0 10))))
         (buffer (format "%s %s*" org-ql-view-buffer-name-prefix title))
         (header (org-ql-view--header-line-format
                  :title title))
         (org-ql-view-buffers-files (org-roam-ql--nodes-files nodes))
         (org-ql-view-query (append
                             `(and (org-roam-query ,source-or-query))
                             query))
         (org-ql-view-sort nil)
         (org-ql-view-narrow nil)
         (org-ql-view-super-groups super-groups)
         (org-ql-view-title title))
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
      (org-ql-view-refresh))))

(defun org-roam-ql-select (source-or-query &optional ql-query action narrow sort)
  "Process SOURCE-OR-QUERY with org-roam-db and pass it to org-ql to be filtered with QL-QUERY.
ACTION NARROW and SORT are passed to `org-ql-select' as is.

SOURCE-OR-QUERY can be one of the following:
- A list of params that can be passed to `org-roam-db-query'. Expected
  to have the form (QUERY ARG1 ARG2 ARG3...). `org-roam-db-query' will
  called with the list or parameters as:
  (org-roam-db-query QUERY ARG1 ARG2 ARG3...). The first element in each
  row in the result from the query is expected to have the ID of a
  corresponding node, which will be conerted to a org-roam-node. QUERY
  can be a complete query. If the query is going to be of the form
  [:select [id] :from nodes :where (= todo \"TODO\")], you can omit the
  part till after :where. i.e., pass only [(= todo \"TODO\")] and the
  rest will get appended in the front.
- A list of org-roam-nodes
- A function that returns a list of org-roam-nodes"
  (let* ((nodes (org-roam-ql--get-nodes-from-query source-or-query))
         (buffers (org-roam-ql--nodes-files nodes))
         (query (append `(and (org-roam-query ,source-or-query)) ql-query)))
    (org-ql-select buffers query :action action :narrow narrow :sort sort)))

(defun org-roam-ql--nodes-files (nodes)
  "Returns the list of files from the list of NODES."
  (-uniq (mapcar #'org-roam-node-file nodes)))

(org-ql-defpred org-roam-query (query)
  "To be used with the org-roam-ql. Checks if a node is a result of a passed query."
  ;; :normalizers ((`(,predicate-names . ,query)
  ;;                `(-when-let (id (org-id-get (point) nil))
  ;;                   (member id (list ,@(-map #'org-roam-node-id org-roam-ql--current-nodes)))))))
  :preambles ((`(,predicate-names . ,query)
               (list :regexp (rx-to-string
                              `(seq bol (0+ space) ":ID:" (0+ space)
                                    (or ,@(-map
                                           #'org-roam-node-id
                                           org-roam-ql--current-nodes))
                                    eol))
                     :query t))))


;; (org-ql-defpred org-roam-query (query)
;;   :normalizers ((`(,predicate-names . ,query)
;;                  (let* ((nodes (org-roam-ql--get-nodes-from-query (car query)))
;;                         (node-ids (-map #'org-roam-node-id nodes)))
;;                  `(-when-let (id (org-id-get (point) nil))
;;                     (member id ,node-ids))))));;,(-map #'org-roam-node-id (org-roam-ql--get-nodes-from-query (car query))))))))

(defun org-roam-ql--refresh (other-func &rest rest)
  "When `org-ql-view' is refreshed, if this is created from a `org-roam-ql'
function, update the variables accordingly."
  (unless org-ql-view-buffers-files
    (user-error "Not an Org QL View buffer"))
  ;; FIXME: This is a super hacky way to extract the buffers-files from the query
  (-when-let (query (read (cadr (s-match "(org-roam-query \\(.*\\))" (format "%s" org-ql-view-query)))))
    (let* ((nodes (org-roam-ql--get-nodes-from-query query)))
      (setq org-roam-ql--current-nodes nodes
            org-ql-view-buffers-files (org-roam-ql--nodes-files nodes))))
  (apply other-func rest))

(advice-add 'org-ql-view-refresh :around #'org-roam-ql--refresh)

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
  "Return NODE as a string with text-properties set by its property list.
If NODE is nil, return an empty string."
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

;;;###autoload
(defun org-roam-ql-ql-buffer-from-roam-buffer ()
  "Convert a roam buffer to org-ql buffer."
  (interactive)
  (when (derived-mode-p 'org-roam-mode)
    (let (nodes)
      (goto-char 0)
      (while (condition-case err
                 (progn
                   (magit-section-forward)
                   t ;; keep the while loop going
                   )
               (user-error
                (if (equalp (error-message-string err) "No next section")
                    nil ;; end while loop
                  (signal (car err) (cdr err))))) ;; somthing else happened, re-throw
        (let ((magit-section (plist-get (text-properties-at (point)) 'magit-section)))
          (when (org-roam-node-section-p magit-section)
            (push (slot-value magit-section 'node) nodes))))
      ;; This allows any set of nodes to be displayed
      (org-roam-ql-view nodes (--if-let header-line-format it "")
                        ;;`(org-roam-backlink ,org-roam-buffer-current-node)))
                        `(member (org-id-get) (list ,@(-map #'org-roam-node-id nodes)))))))
      ;;(error "`org-roam-buffer-current-node' is nil"))))

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
  (let (mrk nodes)
    (save-excursion
      (goto-char (point-min))
      (while (equal (forward-line) 0)
	(when (setq mrk (get-text-property (point) 'org-hd-marker))
          (org-with-point-at mrk
            ;; pick only nodes
            (-if-let (id (org-id-get))
                (push (org-roam-node-from-id id) nodes)
              (user-error "Non roam-node headings in query."))))))
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
  "Returns a function that can be passed as a section for `okm-render-org-roam-buffer' with the NODES.
Nodes should be a list of org-roam nodes."
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

(provide 'org-roam-ql)

;;; org-roam-ql.el ends here
