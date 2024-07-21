;;; org-roam-ql-ql.el --- Intgrating org-roam and org-ql -*- lexical-binding: t -*-

;; Copyright (C) 2024 Shariff AM Faleel

;; Author: Shariff AM Faleel
;; Version: 0.1
;; Package-Requires: ((emacs "28") (org-roam-ql "0.1") (org-ql "0.7") (org-roam "2.2.0") (s "1.12.0") (transient "0.4"))
;; Homepage: https://github.com/ahmed-shariff/org-roam-ql
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; License:

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;;; Commentary:

;; This pacakge provides an interface to query an org-roam databases
;; and display it.

;;; code:

(require 'org-ql)
(require 'org-ql-view)
(require 'org-roam-ql)
(require 'org-roam-utils)
(require 'org-roam-node)
(require 'dash)
(require 's)
(require 'transient)

(defvar org-roam-ql-ql--current-nodes nil)

(defun org-roam-ql-ql--ql-view-buffer-for-nodes (nodes title buffer-name &optional source-or-query super-groups)
  "Display NODES in `org-ql-view' buffer with TITLE in buffer BUFFER-NAME.
See `org-roam-ql-nodes' for SOURCE-OR-QUERY.
See `org-super-agenda' for SUPER-GROUPS."
  (with-temp-buffer
    (let* ((strings '())
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
          (user-error "Empty result for query")
        (dolist-with-progress-reporter (node nodes)
            (format "Processing %s nodes" (length nodes))
          (push (org-roam-ql-view--format-node node) strings))
        (when super-groups
          (let ((org-super-agenda-groups (cl-etypecase super-groups
                                           (symbol (symbol-value super-groups))
                                           (list super-groups))))
            (setf strings (org-super-agenda--group-items strings))))
        (org-ql-view--display :buffer buffer-name :header header
          :string (s-join "\n" strings))
        (with-current-buffer buffer-name
          ;; HACK - to make the buffer get rendered properly.
          (org-ql-view-refresh))))))

;; *****************************************************************************
;; Functions to work with org-ql-view
;; *****************************************************************************

(org-ql-defpred org-roam-query (query)
  "To be used with the org-roam-ql. Checks if a node is a result of a passed query."
  ;; FIXME: Adding body because of Warning: Unused lexical argument `query'
  :body (cl-assert query)
  :preambles ((`(,predicate-names . ,query)
               (list :regexp (rx-to-string
                              `(seq bol (0+ space) ":ID:" (0+ space)
                                    (or ,@(-map
                                           #'org-roam-node-id
                                           org-roam-ql-ql--current-nodes))
                                    eol))
                     :query t))))

(defun org-roam-ql-ql--get-roam-queries (query)
  "Recursively traverse and get the org-roam-query's from a org-ql QUERY."
  (if (listp query)
      (if (equal (car query) 'org-roam-query)
          (list query)
        (apply #'append (-non-nil (--map (org-roam-ql-ql--get-roam-queries it) query))))
    nil))

(defun org-roam-ql-ql--refresh (other-func &rest rest)
  "Advice function to `org-ql-view-refresh'.
When `org-ql-view' is refreshed, if this is created from a
`org-roam-ql' function, update the variables accordingly.
OTHER-FUNC would be the `org-ql-view-refresh', REST if any additional
parameters were ever passed"
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
  (when-let ((-have-nodes org-roam-ql-ql--current-nodes)
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
      (org-roam-ql-ql--ql-view-buffer-for-nodes (cond
                                                 ((derived-mode-p 'org-roam-mode)
                                                  (org-roam-ql--nodes-from-roam-buffer (current-buffer)))
                                                 ((derived-mode-p 'org-agenda-mode)
                                                  (org-roam-ql--nodes-from-agenda-buffer (current-buffer))))
                                                title
                                                (org-roam-ql--get-formatted-buffer-name
                                                 title)
                                                `(in-buffer ,b)))))

;;;###autoload
(defun org-roam-ql-ql-roam-buffer-from-agenda-buffer ()
  "Convert a agenda reusult to a roam-buffer."
  (interactive)
  (unless org-ql-view-buffers-files
    (user-error "Not an Org QL View buffer"))
  (when (derived-mode-p 'org-agenda-mode)
    (let* ((b (buffer-name (current-buffer))))
      (org-roam-ql--agenda-buffer-for-nodes (org-roam-ql--nodes-from-agenda-buffer (current-buffer))
                                            (org-roam-ql--get-formatted-title b nil "from org-ql-view")
                                            (org-roam-ql--get-formatted-buffer-name
                                             (org-roam-ql--get-formatted-title b nil "from org-ql-view"))
                                            `(in-buffer ,b)))))

;;;###autoload
(defun org-roam-ql-ql-init ()
  "Integrate `org-roam-ql' into `org-ql'."
  (advice-add 'org-ql-view-refresh :around #'org-roam-ql-ql--refresh)

  (transient-insert-suffix
    'org-roam-ql-buffer-dispatch '(1 -1)
    [("Q" "View in org-ql buffer" org-roam-ql-ql-buffer-from-roam-buffer)])

  (transient-insert-suffix
    'org-ql-view-dispatch '(1)
    [["org-roam-ql"
      ("R" "View in org roam buffer" org-roam-ql-ql-roam-buffer-from-agenda-buffer)]]))

(provide 'org-roam-ql-ql)

;;; org-roam-ql-ql.el ends here
