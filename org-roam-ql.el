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

(defun org-roam-ql-view--get-nodes-from-query (source-or-query)
  "Convert SOURCE-OR-QUERY to org-roam-nodes.
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
(defun org-roam-ql-view (source-or-query title &optional query super-groups)
  "Basically what `org-ql-search does', but for org-roam-nodes.
See `org-roam-ql-view--get-nodes-from-querySOURCE-OR-QUERY' for what
SOURCE-OR-QUERY can be. TITLE is a title to associate with the view.
See `org-roam-search' for details on SUPER-GROUPS."
  (let* ((nodes (org-roam-ql-view--get-nodes-from-query source-or-query))
         (strings '())
         (title (format "org-roam - %s" title))
         (buffer (format "%s %s*" org-ql-view-buffer-name-prefix title))
         (header (org-ql-view--header-line-format
                  :title title))
         (org-ql-view-buffers-files (-uniq (mapcar #'org-roam-node-file nodes)))
         (org-ql-view-query (if query `(and (property "ID") ,query) '(property "ID")))
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
      (org-ql-view-refresh))))


;; ;; modified version of org-ql-view--display
;; ;; note needed.
;; (cl-defun org-roam-ql-view--display (&key (buffer org-ql-view-buffer) header string)
;;   "Display STRING in `org-ql-view' BUFFER.

;; BUFFER may be a buffer, or a string naming a buffer, which is
;; reused if it already exists.  `org-ql-view-buffer' is used by
;; default.

;; HEADER is a string displayed in the buffer's header line.

;; The following special variables, if non-nil, are set
;; buffer-locally to preserve their value in the buffer for
;; subsequent refreshing of the buffer: `org-ql-view-buffers-files',
;; `org-ql-view-query', `org-ql-view-sort', `org-ql-view-narrow',
;; `org-ql-view-super-groups', `org-ql-title.'"
;;   (declare (indent defun))
;;   (let* ((vars (list 'org-ql-view-buffers-files 'org-ql-view-query
;;                      'org-ql-view-sort 'org-ql-view-narrow
;;                      'org-ql-view-super-groups 'org-ql-view-title))
;;          ;; Save the values of variables which are set buffer-locally in the
;;          ;; results buffer, which we want to override and set buffer-locally again.
;;          (vals (cl-loop for symbol in vars
;;                         collect (cons symbol (symbol-value symbol))))
;;          (buffer (if (bufferp buffer)
;;                      buffer
;;                    (with-current-buffer (get-buffer-create (or buffer "*org-roam-ql-buffer*"))
;;                      (unless (eq major-mode 'org-agenda-mode)
;;                        (org-agenda-mode)
;;                        (setf buffer-read-only t))
;;                      (current-buffer)))))
;;     (with-current-buffer buffer
;;       (setq-local bookmark-make-record-function #'org-ql-view-bookmark-make-record)
;;       (use-local-map org-ql-view-map)
;;       ;; Prepare buffer, saving data for refreshing.
;;       (cl-loop for symbol in vars
;;                do (progn
;;                     (kill-local-variable symbol)
;;                     (set (make-local-variable symbol) (alist-get symbol vals nil nil #'equal))))
;;       (setf header-line-format header)
;;       ;; Clear buffer, insert entries, etc.
;;       (let ((inhibit-read-only t))
;;         (erase-buffer)
;;         (insert string)
;;         (pop-to-buffer (current-buffer));;org-ql-view-display-buffer-action)
;;         (org-agenda-finalize)
;;         (goto-char (point-min))))))

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
                           (org-add-props it nil 'face 'org-tag))))))
                     ;;(org-roam-node--format-entry (org-roam-node--process-display-format org-roam-node-display-template) node)))
                     ))
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
(defun org-roam-ql-to-roam-buffer ()
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
