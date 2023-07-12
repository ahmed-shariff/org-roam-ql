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

(defvar org-roam-ql-ql--current-nodes nil)

(defun org-roam-ql--ql-view-buffer-for-nodes (nodes title buffer-name &optional source-or-query super-groups)
  "Display nodes in org-ql-view buffer."
  (with-temp-buffer
    (let* ((strings '())
           (buffer (org-roam-ql--get-formatted-buffer-name title))
           (header (org-ql-view--header-line-format
                    :title title))
           (org-ql-view-buffers-files (org-roam-ql--nodes-files nodes))
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
        (with-current-buffer buffer
          ;; HACK - to make the buffer get rendered properly.
          (org-ql-view-refresh))))))

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
                                           org-roam-ql-ql--current-nodes))
                                    eol))
                     :query t))))

(defun org-roam-ql-ql--get-roam-queries (query)
  "Recursively traverse and get the org-roam-query's from a org-ql query."
  (if (listp query)
      (if (equal (car query) 'org-roam-query)
          (list query)
        (apply #'append (-non-nil (--map (org-roam-ql-ql--get-roam-queries it) query))))
    nil))

(defun org-roam-ql--refresh (other-func &rest rest)
  "When `org-ql-view' is refreshed, if this is created from a
`org-roam-ql' function, update the variables accordingly."
  (unless org-ql-view-buffers-files
    (user-error "Not an Org QL View buffer"))
  (-when-let (queries (org-roam-ql-ql--get-roam-queries org-ql-view-query))
    (let* ((nodes (apply #'append
                         (--map (apply #'org-roam-ql--nodes-cached
                                       (cdr it))
                                queries))))
      (org-roam-ql-clear-cache)
      (setq org-roam-ql-ql--current-nodes nodes)
      ;; If results are empty buffer gets empty
      ;; `org-ql-view-buffers-files' is left alone to avoid org-ql
      ;; erroring with "Not an Org QL View buffer"
      (when nodes
        (setq org-ql-view-buffers-files (org-roam-ql--nodes-files nodes)))))
  (apply other-func rest)
  (when-let ((_ org-roam-ql-ql--current-nodes)
             (file-nodes (--filter (equal (org-roam-node-level it) 0) org-roam-ql-ql--current-nodes)))
    (let ((inhibit-read-only t))
      (goto-char (point-max))
      (insert
       (propertize
        (format "\n\n  WARNING: skipping %s file nodes"
                (length file-nodes))
        'face 'error)))))


;; *****************************************************************************
;; Functions to switch between org-roam/org-roam-ql buffers and
;; org-ql-view buffers
;; *****************************************************************************
;;;###autoload
(defun org-roam-ql-ql-buffer-from-roam-buffer ()
  "Convert a roam buffer to org-ql buffer."
  (interactive)
  (when (or (derived-mode-p 'org-agenda-mode) (derived-mode-p 'org-roam-mode))
    (let* ((b (buffer-name (current-buffer)))
           (title (org-roam-ql--get-formatted-title b nil "from org-ql-view")))
      (org-roam-ql--ql-view-buffer-for-nodes (cond
                                              ((derived-mode-p 'org-roam-mode)
                                               (org-roam-ql--nodes-from-roam-buffer (current-buffer)))
                                              ((derived-mode-p 'org-agenda-mode)
                                               (org-roam-ql--nodes-from-agenda-buffer (current-buffer))))
                                             title
                                             (org-roam-ql--get-formatted-buffer-name
                                              title)
                                             `(in-buffer ,b)))))

;;;###autoload
(defun org-roam-ql-roam-buffer-frmo-agenda-buffer ()
  "Convert a agenda reusult to a roam-buffer."
  (interactive)
  (unless org-ql-view-buffers-files
    (user-error "Not an Org QL View buffer"))
  (when (derived-mode-p 'org-agenda-mode)
    (org-roam-ql--agenda-buffer-for-nodes (org-roam-ql--nodes-from-agenda-buffer (current-buffer))
                                          (org-roam-ql--get-formatted-title b nil "from org-ql-view") 
                                          (org-roam-ql--get-formatted-buffer-name
                                           (org-roam-ql--get-formatted-title b nil "from org-ql-view"))
                                          `(in-buffer ,b))))

(advice-add 'org-ql-view-refresh :around #'org-roam-ql--refresh)

(provide 'org-roam-ql-ql)

;;; org-roam-ql-ql.el ends here
