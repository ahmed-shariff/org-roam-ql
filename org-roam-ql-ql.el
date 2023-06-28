;;; org-roam-ql-ql.el --- Intgrating org-roam and org-ql -*- lexical-binding: t -*-

;; Copyright (c) 2023 Shariff AM Faleel (shariff.mfa@outlook.com)

;; Author: Shariff AM Faleel
;; Version: 0.1
;; Package-Requires: ((emacs "28") (org-roam-ql "0.1") (org-ql "0.7-pre") (org "9.0") (org-roam "2.2.2") (s "1.13.1") (magit-section "3.3.0"))
;; Homepage: https://github.com/ahmed-shariff/org-roam-ql
;; Keywords: org-roam, query
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; TBD

;;; Code:

(require 'org-ql)
(require 'org-ql-view)
(require 'org-roam-ql)
(require 'org-roam-utils)
(require 'org-roam-node)
(require 'dash)
(require 's)


(defvar org-roam-ql--current-nodes nil)

(defun org-roam-ql--org-ql-search (source-or-query nodes title)
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
           ;; (org-ql-view-super-groups super-groups)
           (org-ql-view-title title))
      ;; Invalidating cache to allow detecting changes.
      (org-roam-ql-clear-cache)
      (if (not nodes)
          (user-error "Empty result for query.")
        (dolist-with-progress-reporter (node nodes)
            (format "Processing %s nodes" (length nodes))
          (push (org-roam-ql-view--format-node node) strings))
        ;; TODO: Is this necessary?
        ;; (when super-groups
        ;;   (let ((org-super-agenda-groups (cl-etypecase super-groups
        ;;                                    (symbol (symbol-value super-groups))
        ;;                                    (list super-groups))))
        ;;     (setf strings (org-super-agenda--group-items strings))))
        (org-ql-view--display :buffer buffer :header header
          :string (s-join "\n" strings))
        ;; (with-current-buffer buffer
        ;;   ;; HACK - to make the buffer get rendered properly.
        ;;   (org-ql-view-refresh))))))
        ))))

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
;; Functions to work with org-ql-view
;; *****************************************************************************

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

;; *****************************************************************************
;; Functions to switch between org-roam/org-roam-ql buffers and
;; org-ql-view buffers
;; *****************************************************************************
(defun org-roam-ql--nodes-from-roam-buffer (buffer)
  "Collect the org-roam-nodes from a ORG-ROAM-BUFFER."
  (with-current-buffer buffer
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
    (org-roam-ql--org-ql-search (buffer-name (current-buffer))
                                (org-roam-ql--nodes-from-roam-buffer (current-buffer))
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

(advice-add 'org-ql-view-refresh :around #'org-roam-ql--refresh)

(provide 'org-roam-ql-ql)

;;; org-roam-ql-ql.el ends here
