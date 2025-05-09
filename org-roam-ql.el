;;; org-roam-ql.el --- Interface to query and view results from org-roam -*- lexical-binding: t -*-

;; Copyright (C) 2025 Shariff AM Faleel

;; Author: Shariff AM Faleel
;; Package-Requires: ((emacs "28") (org-roam "2.2.0") (s "1.12.0") (magit-section "3.3.0") (transient "0.4") (org-super-agenda "1.2") (dash "2.0"))
;; Version: 0.3-pre
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

;; Inspired by org-ql, this package provides an interface to easily
;; query and view the results from an org-roam database.

;;; Code:

(require 'magit-section)
(require 'transient)
(require 'org-super-agenda)
(require 'org-roam-utils)
(require 'org-roam-node)
(require 'dash)
(require 's)
(require 'bookmark)

(defgroup org-roam-ql nil
  "Customization for `org-roam-ql'."
  :group 'org
  :link '(url-link "https://github.com/ahmed-shariff/org-roam-ql"))

(defcustom org-roam-ql-default-org-roam-buffer-query
  #'org-roam-ql--default-query-for-roam-buffer
  "The query to use when org-roam-buffer is extended.  Can also be a
function that returns a query."
  :group 'org-roam-ql
  :type '(choice function sexp))

(defcustom org-roam-ql-buffer-postrender-functions
  org-roam-buffer-postrender-functions
  "Functions to run after a section of a `org-roam-ql-mode' buffer is
rendered.  Each function accepts no arguments, and is run with the
`org-roam-ql-mode' buffer as the current buffer."
  :group 'org-roam-ql
  :type 'hook)

(defcustom org-roam-ql-preview-function #'org-roam-ql-preview-default-function
  "The preview function used to generate the content of a nodes
preview in `org-roam-ql-mode' buffer.

Before calling this, the point will be set to `org-roam-node-point'.
Will be called with two arguments: (1) The corresponding node, and (2)
the source-or-query of the corresponding buffer. See
`org-roam-ql-nodes' for what the value of source-or-query can be."
  :group 'org-roam-ql
  :type 'function)

(defcustom org-roam-ql-preview-postprocess-functions org-roam-preview-postprocess-functions
  "A list of functions to postprocess the preview content.

This is the same as `org-roam-preview-postprocess-functions'.
Each function takes a single argument, the string for the preview
content, and returns the post-processed string. The functions are
applied in order of appearance in the list."
  :group 'org-roam-ql
  :type 'hook)

(defvar org-roam-ql--query-comparison-functions (make-hash-table) "Holds the comparison function for a query.")
(defvar org-roam-ql--query-expansion-functions (make-hash-table) "Holds the function to expand a query.")
(defvar org-roam-ql--sort-functions (make-hash-table :test 'equal) "Holds the function to sort nodes.")
(defvar org-roam-ql--saved-queries (make-hash-table :test 'equal) "Holds the saved queries.")
(defvar org-roam-ql--cache (make-hash-table))
(defvar org-roam-ql--db-mtime nil)
(defvar org-roam-ql--search-query-history '() "History of queries with `org-roam-ql-search'.")
(defvar-local org-roam-ql-buffer-title nil "The current title of the buffer.")
(defvar-local org-roam-ql-buffer-query nil "The current query of the buffer.")
(defvar-local org-roam-ql-buffer-sort nil "The current sort function of the buffer.")
(defvar-local org-roam-ql--buffer-displayed-query nil "The query which produced the results of the buffer.")
(defvar-local org-roam-ql-buffer-in nil
  "Define which option to use - `in-buffer' or `org-roam-db'.")

;; `bookmark.el' setup
(defvar bookmark-make-record-function)
(defvar bookmark-alist)
(declare-function bookmark-set "bookmark")

;;;###autoload
(defun org-roam-ql-nodes (source-or-query &optional sort-fn)
  "Convert SOURCE-OR-QUERY to org-roam-nodes.
if SORT-FN is provided, the returned values will be sorted with it.

SOURCE-OR-QUERY can be one of the following:
- A org-roam-ql query.
- A symbol or string referring to a saved query registered using
  `org-roam-ql-add-saved-query'. If a string is used, it will be
  interned to a symbol.
- A string name of a org-roam-ql bookmark.
- A `buffer-name' of a `org-roam-mode' buffer.
- A list of params that can be passed to `org-roam-db-query'.  Expected
  to have the form (QUERY ARG1 ARG2 ARG3...).  `org-roam-db-query' will
  called with the list or parameters as: (org-roam-db-query QUERY ARG1
  ARG2 ARG3...).  The first element in each row in the result from the
  query is expected to have the ID of a corresponding node, which will
  be conerted to a org-roam-node.  QUERY can be a complete query.  If
  the query is going to be of the form [:select [id] :from nodes
  :where (= todo \"TODO\")], you can omit the part till after
  :where i.e., pass only [(= todo \"TODO\")] and the rest will get
  appended in the front.
- A list of org-roam-nodes or an org-roam-node.
- A function that returns a list of org-roam-nodes.

SORT-FN can be a function that takes two org-roam-nodes, and
compatible with `seq-sort'.  Or it can be any regsitered sort
functions with `org-roam-ql-register-sort-fn'."
  (--> (pcase source-or-query
         ((pred org-roam-ql--check-if-list-of-org-roam-nodes-list)
          (-list source-or-query))
         ((and (app (org-roam-ql--check-if-saved-query) saved-query)
               (guard saved-query))
          (org-roam-ql--nodes-cached saved-query))
         ((and (app (org-roam-ql--check-if-bookmark) bookmark-query)
               (guard bookmark-query))
          (org-roam-ql--nodes-cached bookmark-query))
         ;; get-buffer returns a buffer if source-or-query is a buffer obj
         ;; or the name of a buffer
         ((pred org-roam-ql--check-if-org-roam-ql-buffer)
          (cond
           ((with-current-buffer source-or-query (derived-mode-p 'org-roam-mode))
            (org-roam-ql--nodes-from-roam-buffer (get-buffer source-or-query)))
           ((with-current-buffer source-or-query (derived-mode-p 'org-agenda-mode))
            (org-roam-ql--nodes-from-agenda-buffer (get-buffer source-or-query)))))
         ((pred org-roam-ql--check-if-org-roam-db-parameters)
          (let ((query (car source-or-query))
                (args (cdr source-or-query)))
            (--map (org-roam-node-from-id (car it))
                   (apply #'org-roam-db-query
                          (if (equal :select (aref query 0))
                              query
                            (vconcat [:select id :from nodes :where] query))
                          args))))
         ((and (pred listp) (pred org-roam-ql--check-if-valid-query))
          (org-roam-ql--expand-query source-or-query))
         ((pred functionp)
          (--when-let (funcall source-or-query)
            (if (and (listp it) (-all-p #'org-roam-node-p it))
                it
              (user-error "Function did not expand to list of nodes"))))
         (_ (user-error "Invalid source-or-query. Got  %S" source-or-query)))
       (if-let ((-sort-fn (when sort-fn
                            (or (and (functionp sort-fn) sort-fn)
                                (gethash sort-fn org-roam-ql--sort-functions)
                                (user-error (concat "SORT-FN is not a function registered"
                                                    "as a sort-function"
                                                    "(see `org-roam-ql-register-sort-fn')"))))))
           (seq-sort -sort-fn it)
         it)))

;;;###autoload
(defun org-roam-ql-add-saved-query (name docstring query)
  "Create saved QUERY with NAME and DOCSTRING.
QUERY can be an org-roam-ql query, a list of params that can be passed
to `org-roam-db-query' (see `org-roam-ql-nodes') or a function which
can be used with `org-roam-nodes'.
NAME can be a string or a symbol. Under the hood, it is stored as a
symbol. Hence if a string is passed, it will be saved with a symbol
with name NAME."
  (declare (indent defun) (doc-string 2))
  (let ((query-symbol (pcase name
                        ((pred symbolp)
                         name)
                        ((pred stringp)
                         (intern name))
                        (_ (user-error "NAME was not a string or symbol.")))))
    (cl-assert (or (org-roam-ql--check-if-org-roam-db-parameters query)
                   (and (listp query) (org-roam-ql--check-if-valid-query query))
                   (functionp query))
               t "QUERY is not valid.")
    (remhash query-symbol org-roam-ql--saved-queries)
    (puthash query-symbol
             (cons query docstring)
             org-roam-ql--saved-queries)))

;; Can we make org-roam-ql aware of any changes that can happen?
(defun org-roam-ql--nodes-cached (source-or-query)
  "Cache results of `org-roam-ql-nodes'.
See `org-roam-ql-nodes' for information on SOURCE-OR-QUERY.
Not caching or invalidating in the top level function as the
database/buffers can change.  Currently this is only used by the
internal functions"
  (let ((db-mtime (time-convert
                   (file-attribute-modification-time
                    (file-attributes (file-truename org-roam-db-location)))
                   'integer)))
    (unless (equal db-mtime org-roam-ql--db-mtime)
      (org-roam-ql-clear-cache)
      (setq org-roam-ql--db-mtime db-mtime)))
  (let ((cached-value (gethash source-or-query org-roam-ql--cache)))
    (if cached-value
        cached-value
      (puthash source-or-query
               (org-roam-ql-nodes source-or-query)
               org-roam-ql--cache))))

(defun org-roam-ql-clear-cache ()
  "Clear the org-roam-ql cache."
  (setq org-roam-ql--cache (make-hash-table)))


(defun org-roam-ql--node-list ()
  "Return all nodes stored as a list of `org-roam-node's.

Same as `org-roam-node-list', but doesn't treat the aliases as titles.
"
  (let ((rows (org-roam-db-query
               "SELECT
  id,
  file,
  filetitle,
  \"level\",
  todo,
  pos,
  priority ,
  scheduled ,
  deadline ,
  title,
  properties ,
  olp,
  atime,
  mtime,
  '(' || group_concat(tags, ' ') || ')' as tags,
  aliases,
  refs
FROM
  (
  SELECT
    id,
    file,
    filetitle,
    \"level\",
    todo,
    pos,
    priority ,
    scheduled ,
    deadline ,
    title,
    properties ,
    olp,
    atime,
    mtime,
    tags,
    '(' || group_concat(aliases, ' ') || ')' as aliases,
    refs
  FROM
    (
    SELECT
      nodes.id as id,
      nodes.file as file,
      nodes.\"level\" as \"level\",
      nodes.todo as todo,
      nodes.pos as pos,
      nodes.priority as priority,
      nodes.scheduled as scheduled,
      nodes.deadline as deadline,
      nodes.title as title,
      nodes.properties as properties,
      nodes.olp as olp,
      files.atime as atime,
      files.mtime as mtime,
      files.title as filetitle,
      tags.tag as tags,
      aliases.alias as aliases,
      '(' || group_concat(RTRIM (refs.\"type\", '\"') || ':' || LTRIM(refs.ref, '\"'), ' ') || ')' as refs
    FROM nodes
    LEFT JOIN files ON files.file = nodes.file
    LEFT JOIN tags ON tags.node_id = nodes.id
    LEFT JOIN aliases ON aliases.node_id = nodes.id
    LEFT JOIN refs ON refs.node_id = nodes.id
    GROUP BY nodes.id, tags.tag, aliases.alias )
  GROUP BY id, tags )
GROUP BY id")))
    (cl-loop for row in rows
             collect (pcase-let* ((`(
                                    ,id ,file ,file-title ,level ,todo ,pos ,priority ,scheduled ,deadline
                                    ,title ,properties ,olp ,atime ,mtime ,tags ,aliases ,refs)
                                  row)
                                 (all-titles (cons title aliases)))
                      (org-roam-node-create :id id
                                            :file file
                                            :file-title file-title
                                            :file-atime atime
                                            :file-mtime mtime
                                            :level level
                                            :point pos
                                            :todo todo
                                            :priority priority
                                            :scheduled scheduled
                                            :deadline deadline
                                            :title title
                                            :aliases aliases
                                            :properties properties
                                            :olp olp
                                            :tags tags
                                            :refs refs)))))

(defun org-roam-ql--compare-nodes (node1 node2)
  "Comparison function for nodes.
The nodes are considerered equal when they have the same id."
  (s-equals-p (org-roam-node-id node1) (org-roam-node-id node2)))

(defun org-roam-ql--expand-query (query)
  "Expand a org-roam-ql QUERY."
  ;; FIXME: two nodes are not equal? using this -compare-fn as workaround
  (let ((-compare-fn #'org-roam-ql--compare-nodes))
    (if (and (listp query) (member (car query) '(or and not)))
        (if (eq (car query) 'not)
            (org-roam-ql--nodes-cached
             (list [id :not :in $v1]
                   `[,@(-map #'org-roam-node-id (org-roam-ql--expand-query (cadr query)))]))
          (funcall
           #'-reduce
           (pcase (car query)
             ('or #'-union)
             ('and #'-intersection))
           (--map (org-roam-ql--expand-query it) (cdr query))))
      (let* ((query-key (and (listp query) (car query)))
             (query-expansion-function-info (gethash query-key org-roam-ql--query-expansion-functions))
             (query-comparison-function-info (gethash query-key org-roam-ql--query-comparison-functions)))
        (cond
         ((and query-key query-expansion-function-info)
          (org-roam-ql--nodes-cached (apply (cdr query-expansion-function-info) (cdr query))))
         ;; FIXME: Think of a better way to to this
         ((and query-key query-comparison-function-info)
          (org-roam-ql--process-comparison-function (cadr query-comparison-function-info)
                                                    (caddr query-comparison-function-info)
                                                    (cdr query)))
         ;; `org-roam-ql-nodes' will error if query is invalid
         (t (org-roam-ql--nodes-cached query)))))))

(defun org-roam-ql--process-comparison-function (extraction-function comparison-function query)
  "Process a predicate. See `org-roam-ql-defpred' for more info on
EXTRACTION-FUNCTION and COMPARISON-FUNCTION. QUERY will be passed to
the COMPARISON-FUNCTION."
  (--filter (let ((val (funcall extraction-function it)))
              (and val
                   (apply comparison-function (append (list val) query))))
            ;; Caching values
            (org-roam-ql--nodes-cached (org-roam-ql--node-list))))

;;;###autoload
(defun org-roam-ql-search (source-or-query &optional title sort-fn preview-fn)
  "Get nodes that match SOURCE-OR-QUERY and display in org-roam-ql
buffer.  See `org-roam-ql-nodes' for what SOURCE-OR-QUERY can be.
TITLE is a title to associate with the view.  Reesults will be
displayed in a org-roam-ql buffer.  SORT-FN is used for sorting the
results.  It can be a string name of a slot or a predicate function
which can be used to sort the nodes.  See `org-roam-nodes' for more
info on this.

PREVIEW-FN is a function used to generate the preview content on the
`org-roam-ql-mode' buffer. See `org-roam-ql-preview-function' for what
the arguments can be.  If PREVIEW-FN is nil, the value of
`org-roam-ql-preview-function' will be used. If it is non-nil, this
argument will be set as the buffer local value for
`org-roam-ql-preview-function' in the corresponding `org-roam-ql-mode'
buffer"
  (interactive (list (let ((query (org-roam-ql--read-query)))
                       (if (vectorp query)
                           (list query)
                         query))
                     (read-string "Title: ")))
  (let* ((nodes (org-roam-ql-nodes source-or-query sort-fn)))
    (org-roam-ql--roam-buffer-for-nodes
     nodes
     title
     (org-roam-ql--get-formatted-buffer-name
      (org-roam-ql--get-formatted-title title source-or-query))
     source-or-query
     sort-fn
     preview-fn)))

(defun org-roam-ql--get-formatted-title (title source-or-query &optional extended-kwd)
  "Return the formatted TITLE.
If TITLE is nil, Use SOURCE-OR-QUERY to generate a title.
When EXTENDED-KWD is provided, append that to the returned."
  ;; TODO: Think of a better way to get a default title
  (concat (format "org-roam - %s" (or (s-replace "org-roam - " "" (if (stringp title)
                                                                      title
                                                                    (prin1-to-string title)))
                                      (substring (format "%s" source-or-query) 0 10)))
          (when extended-kwd
            (format "- %s" extended-kwd))))

(defun org-roam-ql--get-formatted-buffer-name (title)
  "Return the formatted buffer name from the TITLE."
  (format "*%s*" title))

(defun org-roam-ql--nodes-files (nodes)
  "Return the list of files from the list of NODES."
  (-uniq (mapcar #'org-roam-node-file nodes)))

(defun org-roam-ql--check-if-list-of-org-roam-nodes-list (source-or-query)
  "Return non-nil if SOURCE-OR-QUERY is a list of org-roam-nodes."
  (or (org-roam-node-p source-or-query)
      (-all-p #'org-roam-node-p source-or-query)))

(defun org-roam-ql--check-if-saved-query (source-or-query)
  "Return the query if SOURCE-OR-QUERY is a saved query.
Otherwise return nil."
  (let* ((query-symbol (if (stringp source-or-query)
                           (intern source-or-query)
                         source-or-query))
         (saved-query (gethash query-symbol org-roam-ql--saved-queries)))
    (when saved-query
      (car saved-query))))

(defun org-roam-ql--check-if-bookmark (source-or-query)
  "Return the query if SOURCE-OR-QUERY is a org-roam-ql bookmark.
Otherwise return nil."
  (let ((bookmark (alist-get
                   source-or-query
                   (--filter
                    (equal #'org-roam-ql--bookmark-open
                           (bookmark-prop-get it 'handler))
                    bookmark-alist)
                   nil nil 'equal)))
    (when bookmark (alist-get 'query bookmark))))

(defun org-roam-ql--check-if-org-roam-ql-buffer (source-or-query)
  "Return non-nil if SOURCE-OR-QUERY is buffer org-roam-ql can understand.
This would be either an `org-agenda' buffer or a `org-roam' like buffer."
  (-when-let (buffer (and (or (stringp source-or-query)
                              (bufferp source-or-query))
                          (get-buffer source-or-query)))
    (with-current-buffer buffer (or (derived-mode-p 'org-agenda-mode)
                                    (derived-mode-p 'org-roam-mode)))))

(defun org-roam-ql--check-if-org-roam-db-parameters (source-or-query)
  "Test if SOURCE-OR-QUERY are valid parameters for `org-roam-db-query'."
  (and (listp source-or-query) (vectorp (car source-or-query))))

(defun org-roam-ql--check-if-valid-query (s-exp)
  "Check if S-EXP can be expanded to a roam-query."
  (if (listp s-exp)
      (or (and (member (car s-exp) '(or and not))
               (-all-p #'identity (--map (org-roam-ql--check-if-valid-query it) (cdr s-exp))))
          (or (gethash (car s-exp) org-roam-ql--query-comparison-functions)
              (gethash (car s-exp) org-roam-ql--query-expansion-functions)))
    (or (org-roam-ql--check-if-list-of-org-roam-nodes-list s-exp)
        (org-roam-ql--check-if-saved-query s-exp)
        (org-roam-ql--check-if-bookmark s-exp)
        (org-roam-ql--check-if-org-roam-ql-buffer s-exp)
        (org-roam-ql--check-if-org-roam-db-parameters s-exp))))

;; Copied from `simple.el' `read-experssion-map'
(defvar org-roam-ql--read-query-map
  (let ((m (make-sparse-keymap)))
    (define-key m "\M-\t" 'completion-at-point)
    (define-key m "\t" 'completion-at-point)
    (set-keymap-parent m minibuffer-local-map)
    m))

(defun org-roam-ql--read-query (&optional initial-input)
  "Read query from minibuffer.
Sets the history as well."
  ;; Copied from `simple.el' `read--expression'
  (minibuffer-with-setup-hook
      (lambda ()
        ;; KLUDGE: No idea why this is here!
        (set-syntax-table emacs-lisp-mode-syntax-table)
        (add-hook 'completion-at-point-functions
                  #'org-roam-ql--completion-at-point nil t))
    (let ((query (read-from-minibuffer "Query: " initial-input org-roam-ql--read-query-map nil
                                       'org-roam-ql--search-query-history)))
      (setf org-roam-ql--search-query-history (delete-dups
                                               (append
                                                org-roam-ql--search-query-history
                                                (list query))))
      (read query))))

(defvar org-roam-ql--valid-prev-chars-for-cap nil)

;; Copied from `elisp-completion-at-point'
(defun org-roam-ql--completion-at-point ()
  (with-syntax-table emacs-lisp-mode-syntax-table
    (let* ((pos (point))
	   (beg (condition-case nil
		    (save-excursion
		      (backward-sexp 1)
		      (skip-chars-forward "`',‘#")
		      (point))
		  (scan-error pos)))
	   (end
	    (unless (or (eq beg (point-max))
			(member (char-syntax (char-after beg))
                                '(?\" ?\()))
	      (condition-case nil
		  (save-excursion
		    (goto-char beg)
		    (forward-sexp 1)
                    (skip-chars-backward "'’")
		    (when (>= (point) pos)
		      (point)))
		(scan-error pos))))
           ;; t if in function position.
           (funpos (eq (char-before beg) ?\()))
      (cond
       ;; predicates
       ((and end funpos)
        (list beg end
              (lambda (string pred action)
                (cond
                 ((eq action 'metadata)
                  (cons
                   'metadata
                   `(,(cons '
                       annotation-function
                       (lambda (str)
                         (let* ((sym (intern-soft str)))
                           (let ((comparison-function (gethash sym org-roam-ql--query-comparison-functions))
                                 (expansion-function (gethash sym org-roam-ql--query-expansion-functions)))
                             (cond
                              (comparison-function
                               (concat (propertize " : " 'face 'shadow)
                                       (let ((fun-doc (elisp-get-fnsym-args-string (caddr comparison-function) 0)))
                                         (when fun-doc
                                           (concat (propertize (s-replace-regexp "([^ ]* " "("
                                                                                 fun-doc)
                                                               'face  '(:inherit shadow :weight extra-bold))
                                                   " - ")))
                                       (propertize (car comparison-function)
                                                   'face 'shadow)))
                              (expansion-function
                               (concat (propertize " : " 'face 'shadow)
                                       (let ((fun-doc (elisp-get-fnsym-args-string (cdr expansion-function) 0)))
                                         (when fun-doc
                                           (concat (propertize fun-doc
                                                               'face  '(:inherit shadow :weight extra-bold))
                                                   " - ")))
                                       (propertize (car expansion-function)
                                                   'face 'shadow)))))))))))
                 (t
                  (complete-with-action
                   action
                   (append
                    (hash-table-keys org-roam-ql--query-comparison-functions)
                    (hash-table-keys org-roam-ql--query-expansion-functions))
                   string pred))))))
       (t
        (let* ((beg (cond
                     ((or (eq pos (minibuffer-prompt-end))
                          (member (char-before pos)
                                  org-roam-ql--valid-prev-chars-for-cap)
                          (member (char-before pos) '(?\" ?\ )))
                      pos)
                     (t (condition-case nil
	                    (save-excursion
		              (goto-char pos)
		              (backward-sexp 1)
                              (skip-chars-forward
                               (apply #'string
                                      (append '(?\") org-roam-ql--valid-prev-chars-for-cap)))
		              (point))
	                  (scan-error pos)))))
               (end (condition-case nil
		        (save-excursion
		          (goto-char pos)
		          (forward-sexp 1)
		          (point))
		      (scan-error pos))))
          (list beg end
                (lambda (string pred action)
                  (let* ((wrap-in-quotes (not (eq (char-before beg) ?\")))
                         (saved-queries (--map
                                         (propertize (format "%s" it)
                                                     'type 'query
                                                     'data (gethash it org-roam-ql--saved-queries))
                                         (hash-table-keys org-roam-ql--saved-queries)))
                         (bookmarks (-non-nil
                                     (--map
                                      (when (equal #'org-roam-ql--bookmark-open (bookmark-prop-get it 'handler))
                                        (propertize (car it)
                                                    'type 'bookmark
                                                    'data (cons (bookmark-prop-get it 'query)
                                                                (bookmark-prop-get it 'title))))
                                      bookmark-alist)))
                         (buffers (-non-nil
                                   (--map
                                    (with-current-buffer it
                                      (when (derived-mode-p 'org-roam-mode)
                                        (propertize (buffer-name)
                                                    'type 'buffer
                                                    'data (if (equal (buffer-name) org-roam-buffer)
                                                              (cons org-roam-buffer "org-roam-buffer")
                                                            (with-current-buffer it
                                                              (cons org-roam-ql-buffer-query org-roam-ql-buffer-title))))))
                                    (buffer-list))))
                         (candidates (--map (if wrap-in-quotes (format "\"%s\""it) it) (append saved-queries bookmarks buffers)))
                         (width (-max (-map #'length candidates))))
                    (cond
                     ((eq action 'metadata)
                      (cons
                       'metadata
                       `(,(cons
                           'affixation-function
                           (lambda (cands)
                             (--map
                              (--> (if wrap-in-quotes
                                       (string-trim-left (string-trim-right it "\"") "\"")
                                     it)
                                   (when-let (data (get-text-property 1 'data it))
                                     (pcase (get-text-property 1 'type it)
                                       ('query
                                        (list (truncate-string-to-width it width 0 ?\ )
                                              (propertize "(saved)    " 'face '(:inherit shadow :weight extra-bold))
                                              (propertize (format " : %s - %s" (cdr data) (car data))
                                                          'face 'shadow)))
                                       ('bookmark
                                        (list (truncate-string-to-width it width 0 ?\ )
                                              (propertize "(bookmark) " 'face '(:inherit shadow :weight extra-bold))
                                              (propertize (format " : %s - %s" (cdr data) (car data))
                                                          'face 'shadow)))
                                       ('buffer
                                        (list (truncate-string-to-width it width 0 ?\ )
                                              (propertize "(buffer)   " 'face '(:inherit shadow :weight extra-bold))
                                              (propertize (format " : %s - %s" (cdr data) (car data))
                                                          'face 'shadow))))))
                              cands))))))
                     (t
                      (complete-with-action
                       action
                       candidates
                       string pred))))))))))))

;; *****************************************************************************
;; Functions for predicates and expansions
;; *****************************************************************************

;;;###autoload
(defun org-roam-ql-defpred (name docstring extraction-function comparison-function)
  "Create a org-roam-ql predicate with the NAME.
DOCSTRING is the docstring of the predicate.
The COMPARISON-FUNCTION is a function that returns non-nil if this
predicate doesn't fail for a given org-roam-node.  The first value
passed to this function would be the value from calling the
EXTRACTION-FUNCTION with the respective node, and the remainder of the
arguments from the predicate itself.

If any predicate or expansion function with same NAME exists, it will be
overwritten."
  (declare (indent defun) (doc-string 2))
  (remhash name org-roam-ql--query-expansion-functions)
  (puthash name
           (list docstring extraction-function comparison-function)
           org-roam-ql--query-comparison-functions))

;;;###autoload
(defun org-roam-ql-defexpansion (name docstring expansion-function)
  "Add an EXPANSION-FUNCTION identified by NAME in an org-roam-ql query.
DOCSTRING is the docstring of the predicate.
The EXPANSION-FUNCTION should take the parameters
passed in the query and return values that can be passed to
  `org-roam-nodes'

If any predicate or expansion function with same NAME exists, it will be
overwritten."
  (declare (indent defun) (doc-string 2))
  (remhash name org-roam-ql--query-comparison-functions)
  (puthash name (cons docstring expansion-function) org-roam-ql--query-expansion-functions))

(defun org-roam-ql--predicate-s-match (value regexp &optional exact)
  "Return non-nil if there is a REGEXP match in VALUE.
If EXACT, test if VALUE and REGEXP are equal strings.
Ignores the case when matching."
  (when (and value regexp)
    (if exact
        (string-equal-ignore-case value regexp)
      (s-match regexp value))))

(defmacro org-roam-ql--expand-s-like-function (slot-name-symbol extraction-function)
  "Return a function that can expand for string match using sql LIKE
or fallback to using predicate function if `use-regexp' is used."
  `(defun ,(intern (format "org-roam-ql--%s-s-like-function" slot-name-symbol))
       (match-string &optional exact use-regexp)
     ,(format "Expand to nodes with %s that MATCH-STRING. If EXACT is
non-nil, then will only do exact matches. If USE-REGEXP, then use
predicate function else use sql LIKE." slot-name-symbol)
     (if use-regexp
         (org-roam-ql--process-comparison-function ,extraction-function
                                                   #'org-roam-ql--predicate-s-match
                                                   (list match-string exact))
       (list [:select id :from nodes :where (like ,slot-name-symbol $s1)]
             (if exact
                 match-string
               (format "%%%s%%" match-string))))))

(defun org-roam-ql--file-title-expansion-function (match-string &optional exact use-regexp)
  "Expand to nodes with file-title that MATCH-STRING. If EXACT is
non-nil, then will only do exact matches. If USE-REGEXP, then use
predicate function else use sql LIKE."
  (if use-regexp
      (org-roam-ql--process-comparison-function #'org-roam-node-file-title
                                                #'org-roam-ql--predicate-s-match
                                                (list match-string exact))
    `([:select nodes:id
               :from nodes
               :left :join files
               :on (= files:file nodes:file)
               :where (like files:title $s1)]
      ,(if exact
           match-string
         (format "%%%s%%" match-string)))))

(defun org-roam-ql--aliases-expansion-function (match-string &optional exact use-regexp)
  "Expand to nodes with alias that MATCH-STRING. If EXACT is non-nil,
then will only do exact matches. If USE-REGEXP, then use predicate
function else use sql LIKE."
  (if use-regexp
      (org-roam-ql--process-comparison-function #'org-roam-node-aliases
                                                (lambda (extracted-value matchcc)
                                                  (--any-p (org-roam-ql--predicate-s-match it match-string exact)
                                                           extracted-value))
                                                (list match-string))
    `([:select node-id :from aliases :where (like alias $s1) :group-by node-id]
      ,(if exact
           match-string
         (format "%%%s%%" match-string)))))

(defun org-roam-ql--predicate-s-equals-p (value other)
  "Return non-nil if VALUE and OTHER are equal strings."
  (when (and value other)
    (s-equals-p value other)))

(defun org-roam-ql--expand-comparison-function (comparison-operator slot-name-symbol)
  "Return a function that can expand for match using sql =."
  (lambda (match)
    `([:select id :from nodes :where (,comparison-operator ,slot-name-symbol $s1)]
      ,match)))

;; TODO: Multivalue properties
(defun org-roam-ql--predicate-property-match (value prop prop-val)
  "Return non-nil if PROP is a key in the alist PROP-VAL.
And its value is a string equal to VALUE."
  (-when-let (val (assoc-string prop value t))
    (s-match prop-val (cdr val))))

(defun org-roam-ql--predicate-list-match (values &rest str-list)
  "Return non-nil if all strings in VALUES are in STR-LIST."
  (--all-p (member it values) (-flatten (-list str-list))))

(defun org-roam-ql--title-and-aliased-of-node (node)
  "Return the title and aliases of NODE as a list."
  `(,(org-roam-node-title node)
    ,@(org-roam-node-aliases node)))

(cl-defun org-roam-ql--expand-forwardlinks (source-or-query &key (type "id") (combine :and))
  "Expansion function for forwardlinks.
Return a list of nodes that have forward links to any nodes that
SOURCE-OR-QUERY resolves to, as a list.  TYPE is the type of the
link.  COMBINE can be :and or :or.  If :and, only nodes that have
forwardlinks to all results of source-or-query, else if have
forwardlinks to any of them."
  (org-roam-ql--expand-link source-or-query type combine nil))

(cl-defun org-roam-ql--expand-backlinks (source-or-query &key (type "id") (combine :and))
  "Expansion function for backlinks.
Returns a list of nodes that have back links to any nodes that
SOURCE-OR-QUERY resolves to, as a list.  TYPE is the type of the link.
COMBINE can be :and or :or.  If :and, only nodes that have backlinks
to all results of source-or-query, else if have backlinks to any of
them."
  (org-roam-ql--expand-link source-or-query type combine t))

(defun org-roam-ql--expand-link (source-or-query type combine is-backlink)
  "Expansion function for links.
Returns a list of nodes that have back links to any nodes that
SOURCE-OR-QUERY resolves to, as a list.  TYPE is the type of the link.
COMBINE can be :and or :or.  If :and, only nodes that have backlinks
to all results of source-or-query, else if have backlinks to any of
them.  If is-backlink is nil, return forward links, else return
backlinks"
  (let* ((-compare-fn #'org-roam-ql--compare-nodes)
         (query-nodes
          (-uniq
           (org-roam-ql--nodes-cached source-or-query)))
         (query-nodes-count (length query-nodes))
         (target-col (if is-backlink 'links:source 'links:dest))
         (test-col (if is-backlink 'links:dest 'links:source)))
    (->> (org-roam-db-query (apply #'vector
                                   (append `(:select [,target-col (funcall count :distinct ,test-col)]
                                                     :from links
                                                     :where (in ,test-col $v1))
                                           (when type
                                             `(:and (= type $s2)))
                                           `(:group-by ,target-col)))
                            (apply #'vector (-map #'org-roam-node-id query-nodes))
                            type)
         (--filter
          (pcase combine
            (:and (= (cadr it) query-nodes-count))
            (:or (>= (cadr it) 1))
            (_ (user-error "Keyword :combine should be :and or :or; got %s" combine))))
         (--map (org-roam-node-from-id (car it))))))

(defun org-roam-ql--expansion-identity (source-or-query)
  "Expansion function that passes the SOURCE-OR-QUERY as is."
  source-or-query)

(defun org-roam-ql--predicate-funcall (value f)
  "Return the value of calling the function F with VALUE as its parameter."
  (funcall f value))

(defun org-roam-ql--read-date-to-ts (read-date)
  "Convert org date prompt to timestamp."
  (time-convert
   (encode-time (org-read-date-analyze read-date
                                       (current-time)
                                       (decode-time (current-time))))
   'integer))

(defun org-roam-ql--time-convert-to-ts (time)
  "Convert TIME to ts.
This uses `org-time-string-to-seconds' or `time-convert' based on the type."
  (cond
   ((stringp time) (org-roam-ql--time-convert-to-ts
                    (encode-time (parse-time-string time))))
   ((and (not (null time)) (listp time)) (time-convert time 'integer))
   (t (user-error "Unknown value for `time'"))))

(defun org-roam-ql--predicate-compare-time (value comparison time-string)
  "Compare VALUE to TIME-STRING based on COMPARISON.
VALUE is a time-string (see `org-time-string-to-seconds' or valid
value for `time-convert').
TIME-STRING is any valid value for a org date/time prompt.
COMPARISON can be either '< or '>"
  (when value
    (let* ((val1 (org-roam-ql--time-convert-to-ts value))
           (val2 (org-roam-ql--read-date-to-ts time-string))
           (time-less (time-less-p val1 val2)))
      (pcase comparison
        ('< time-less)
        ('> (not time-less))
        (_ (user-error "Unknown value for `comparison'. Should be '< or '>"))))))

(defun org-roam-ql--predicate-time< (value time-string)
  "Check if VALUE less than TIME-STRING.
VALUE is a time-string (see `org-time-string-to-seconds' or valid
value for `time-convert').
TIME-STRING is any valid value for a org date/time prompt."
  (org-roam-ql--predicate-compare-time value '< time-string))

(defun org-roam-ql--predicate-time> (value time-string)
  "Check if VALUE less than TIME-STRING.
VALUE is a time-string (see `org-time-string-to-seconds' or valid
value for `time-convert').
TIME-STRING is any valid value for a org date/time prompt."
  (org-roam-ql--predicate-compare-time value '> time-string))

;; *****************************************************************************
;; Functions for sorting
;; *****************************************************************************

;;;###autoload
(defun org-roam-ql-register-sort-fn (function-name sort-function)
  "Register SORT-FUNCTION with name FUNCTION-NAME.
The function should take two org-roam-nodes and return a truth value,
which is used to sort, i.e., if non-nil, the first node would be
before the second node passed to the function.  Uses `seq-sort'.  If a
sort-function with the given name already exists, it would be
overwritten."
  (declare (indent defun))
  (puthash function-name sort-function org-roam-ql--sort-functions))

(defun org-roam-ql--sort-function-for-slot (slot-name comparison-function)
  "Add a sort function of SLOT-NAME of org-roam-node.
COMPARISON-FUNCTION is the symbol of function.
DOCSTRING is the documentation string to use for the function."
  (let ((getter (intern-soft
                 (format "org-roam-node-%s" slot-name)))
        (reverse-slot-name (format "%s-reverse" slot-name)))
    (org-roam-ql-register-sort-fn
      slot-name
      `(lambda (node1 node2)
         (,comparison-function (,getter node1) (,getter node2))))
    (org-roam-ql-register-sort-fn
      reverse-slot-name
      `(lambda (node1 node2)
         (,comparison-function (,getter node2) (,getter node1))))))

(defun org-roam-ql--sort-time-less (val1 val2)
  "Sort based on time-less-p."
  (if (or (null val1) (null val2))
      nil
    (time-less-p (org-roam-ql--time-convert-to-ts val1)
                 (org-roam-ql--time-convert-to-ts val2))))

;; *****************************************************************************
;; org-roam-ql mode and functions to build them and operate on them
;; *****************************************************************************
(defvar org-roam-ql-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map org-roam-mode-map)
    (define-key map "v" #'org-roam-ql-buffer-dispatch)
    (define-key map [remap revert-buffer] #'org-roam-ql-refresh-buffer)
    map)
  "Keymap for roam-buffer.  Extends `org-roam-mode-map'.")

(define-derived-mode org-roam-ql-mode org-roam-mode "Org-roam-ql"
  "A major mode to display a list of nodes.
Similar to `org-roam-mode', but doesn't default to the
`org-roam-buffer-current-node'."
  :group 'org-roam-ql
  (setq-local bookmark-make-record-function #'org-roam-ql--bookmark-make-record))

(defun org-roam-ql--refresh-buffer-with-display-function (display-function)
  "Refresh a buffer with a DISPLAY-FUNCTION."
  (let* ((buffer-name (buffer-name))
         subquery
         (query (pcase org-roam-ql-buffer-in
                  ("in-buffer" (let ((query org-roam-ql-buffer-query))
                                 (setq org-roam-ql-buffer-query
                                       org-roam-ql--buffer-displayed-query
                                       org-roam-ql-buffer-in "org-roam-db"
                                       subquery t)
                                 `(and ,org-roam-ql--buffer-displayed-query
                                       ,query)))
                  ("org-roam-db" org-roam-ql-buffer-query)
                  (_ (user-error "Invalid value for `org-roam-ql-buffer-in'")))))
    (funcall display-function
             (org-roam-ql-nodes query org-roam-ql-buffer-sort)
             (if subquery
                 (org-roam-ql--get-formatted-title org-roam-ql-buffer-title nil "extended")
               org-roam-ql-buffer-title)
             (if subquery
                 (org-roam-ql--get-formatted-buffer-name
                  (org-roam-ql--get-formatted-title org-roam-ql-buffer-title nil "extended"))
               buffer-name)
             query
             org-roam-ql-buffer-sort
             org-roam-ql-preview-function)))

(defun org-roam-ql--refresh-roam-buffer ()
  "Refresh a org-roam buffer."
  (if (equal (buffer-name) org-roam-buffer)
      (if (not org-roam-ql-buffer-query)
          (org-roam-buffer-refresh)
        (let ((query org-roam-ql-buffer-query)
              (title (or org-roam-ql-buffer-title (org-roam-ql--get-formatted-title
                                                   (org-roam-node-title org-roam-buffer-current-node) nil "extended"))))
          (setq org-roam-ql-buffer-query nil
                org-roam-ql-buffer-title nil
                org-roam-ql-buffer-in nil)
          (org-roam-buffer-refresh)
          (org-roam-ql-search `(and ,(pcase org-roam-ql-default-org-roam-buffer-query
                                       ((pred functionp) (funcall org-roam-ql-default-org-roam-buffer-query))
                                       (_ org-roam-ql-default-org-roam-buffer-query))
                                    ,query)
                              title)))
    (org-roam-ql--refresh-buffer-with-display-function #'org-roam-ql--roam-buffer-for-nodes)))

(defun org-roam-ql--render-roam-buffer (sections title buffer-name source-or-query sort-fn preview-fn)
  "Render SECTIONS (list of functions) in an org-roam-ql buffer.
TITLE is the title, BUFFER-NAME is the name for the buffer.
See `org-roam-nodes' for information on SOURCE-OR-QUERY.
See `org-roam-ql-nodes' for SORT-FN.
See `org-roam-ql-search' for PREVIEW-FN.
When all sections are inserted, will call hook
`org-roam-ql-buffer-postrender-functions'."
  ;; copied  from `org-roam-buffer-render-contents'
  (with-current-buffer (get-buffer-create buffer-name)
    (let ((inhibit-read-only t))
      (erase-buffer)
      (org-roam-ql-mode)
      (org-roam-buffer-set-header-line-format
       (org-roam-ql--get-formatted-title title source-or-query))
      (setq org-roam-ql-buffer-query source-or-query
            org-roam-ql--buffer-displayed-query source-or-query
            org-roam-ql-buffer-title title
            org-roam-ql-buffer-sort sort-fn
            org-roam-ql-buffer-in "org-roam-db")
      (when preview-fn
        (setq-local org-roam-ql-preview-function preview-fn))
      (magit-insert-section section (org-roam)
         (magit-insert-heading)
         (dolist (section sections)
           (funcall section)))
      (run-hooks 'org-roam-ql-buffer-postrender-functions)
      (goto-char 0))
    (display-buffer (current-buffer))))

(defun org-roam-ql--nodes-section (nodes &optional heading)
  "Return a function that generate magit-sections for NODES.
This can be passed as a section for `org-roam-ql--render-roam-buffer'
with the NODES.  Nodes should be a list of org-roam nodes.  HEADING is
the heading for the function `magit-section'.

This inserts nodes simlar to `org-roam-node-insert-section', but uses
`org-roam-ql-preview-function' to generate the preview content. The
return value of `org-roam-ql-preview-function' will be passed through
all values in `org-roam-ql-preview-postprocess-functions'."
  (lambda ()
    (magit-insert-section section (org-roam-nodes)
      (magit-insert-heading heading)
      (dolist (previewing-node nodes)
        (magit-insert-section section (org-roam-node-section)
          (magit-insert-heading (propertize (org-roam-node-title previewing-node)
                                            'font-lock-face 'org-roam-title))
          (oset section node previewing-node)
          (magit-insert-section section (org-roam-preview-section)
            ;; respecting buffer local value
            (let ((preview-fn org-roam-ql-preview-function))
              (insert (org-roam-fontify-like-in-org-mode
                       (save-excursion
                         (org-roam-with-temp-buffer (org-roam-node-file previewing-node)
                           (org-with-wide-buffer
                            (goto-char (org-roam-node-point previewing-node))
                            (let ((s (funcall preview-fn previewing-node org-roam-ql-buffer-query)))
                              (dolist (fn org-roam-ql-preview-postprocess-functions)
                                (setq s (funcall fn s)))
                              s)))))
                      "\n"))
            (oset section file (org-roam-node-file previewing-node))
            (oset section point (org-roam-node-point previewing-node))
            (insert ?\n)))
        (insert ?\n))
      (magit-insert-child-count section))))

(defun org-roam-ql--roam-buffer-for-nodes (nodes title buffer-name &optional source-or-query sort-fn preview-fn)
  "View nodes in org-roam-ql buffer.
See `org-roam-ql--render-roam-buffer' for TITLE BUFFER-NAME and SOURCE-OR-QUERY.
See `org-roam-ql--nodes-section' for NODES.
See `org-roam-ql-nodes' for SORT-FN.
See `org-roam-ql-search' for PREVIEW-FN."
  (org-roam-ql--render-roam-buffer
   (list
    (org-roam-ql--nodes-section nodes "Nodes:"))
   title buffer-name (or source-or-query nodes) sort-fn preview-fn))

(defun org-roam-ql--nodes-from-roam-buffer (buffer)
  "Collect the org-roam-nodes from a valid BUFFER."
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

(defun org-roam-ql--bookmark-open (bookmark)
  "Query and open org-roam-ql bookmark BOOKMARK."
  (let ((title (bookmark-prop-get bookmark 'title))
        (query (bookmark-prop-get bookmark 'query))
        (sort (bookmark-prop-get bookmark 'sort)))
    (org-roam-ql-search query title sort)))

(put 'org-roam-ql--bookmark-open 'bookmark-handler-type "org-roam-ql query")

(defun org-roam-ql--bookmark-make-record ()
  "Create a bookmark record for org-roam-ql-mode buffers.

See docs of `bookmark-make-record-function'."
  (let ((bookmark (cons nil (bookmark-make-record-default 'no-file 'no-context))))
    (if (equal (buffer-name) org-roam-buffer)
        (progn
          (bookmark-prop-set bookmark 'handler #'org-roam-ql--bookmark-open)
          (bookmark-prop-set bookmark 'title
                             (or org-roam-ql-buffer-title
                                 (org-roam-ql--get-formatted-title
                                  (org-roam-node-title org-roam-buffer-current-node) nil)))
          (bookmark-prop-set bookmark 'query
                             (pcase org-roam-ql-default-org-roam-buffer-query
                               ((pred functionp)
                                (funcall org-roam-ql-default-org-roam-buffer-query))
                               (_ org-roam-ql-default-org-roam-buffer-query)))
          (bookmark-prop-set bookmark 'sort (or org-roam-ql-buffer-sort "title")))
      (bookmark-prop-set bookmark 'handler   #'org-roam-ql--bookmark-open)
      (bookmark-prop-set bookmark 'title     org-roam-ql-buffer-title)
      (bookmark-prop-set bookmark 'query     org-roam-ql-buffer-query)
      (bookmark-prop-set bookmark 'sort      org-roam-ql-buffer-sort)
      bookmark)))

;; *****************************************************************************
;; org-roam-ql-agenda-view functions
;; *****************************************************************************
(defvar org-roam-ql--agenda-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map org-agenda-mode-map)
    (define-key map "g" #'org-roam-ql-refresh-buffer)
    (define-key map "r" #'org-roam-ql-refresh-buffer)
    (define-key map "q" #'bury-buffer)
    (define-key map "v" #'org-roam-ql-buffer-dispatch)
    ;; TODO? (define-key map (kbd "C-x C-s") #'org-ql-view-save)
    map)
  "Keymap for agenda-view of org-roam-ql.
Based on `org-agenda-mode-map'.")

;; KLUDGE: preview-fn is left _
(defun org-roam-ql--agenda-buffer-for-nodes (nodes title buffer-name &optional source-or-query sort-fn _ super-groups)
  "Display the nodes in a agenda like buffer.
See `org-roam-ql--render-roam-buffer' for TITLE BUFFER-NAME and SOURCE-OR-QUERY.
See `org-roam-ql--nodes-section' for NODES.
See `org-roam-ql-nodes' for SORT-FN.
See `org-super-agenda' for SUPER-GROUPS."
  (with-current-buffer (get-buffer-create buffer-name)
    (unless (eq major-mode 'org-agenda-mode)
      (org-agenda-mode)
      (setf buffer-read-only t))
    (use-local-map org-roam-ql--agenda-map)
    (let* ((strings '())
           (formatted-title (org-roam-ql--get-formatted-title title source-or-query))
           (header (concat (propertize "View: " 'face 'transient-argument)
                           formatted-title))
           (inhibit-read-only t))
      (erase-buffer)
      ;; XXX: Does the same thing irrespective of buffer!
      (org-roam-buffer-set-header-line-format header)
      (setq org-roam-ql-buffer-query source-or-query
            org-roam-ql--buffer-displayed-query source-or-query
            org-roam-ql-buffer-title title
            org-roam-ql-buffer-sort sort-fn
            org-roam-ql-buffer-in "org-roam-db")
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
        (insert (string-join strings "\n") "\n")
        (pop-to-buffer (current-buffer))
        (org-agenda-finalize)
        (goto-char (point-min))))))

(defun org-roam-ql--get-file-marker (node)
  "Return a marker for NODE."
  (org-roam-with-file (org-roam-node-file node) t
    ;; (with-current-buffer (find-file-noselect (org-roam-node-file node))
    ;; (with-plain-file (org-roam-node-file node) t
    (goto-char (org-roam-node-point node))
    (point-marker)))

;; copied from `org-ql-search'
(defun org-roam-ql-view--format-node (node)
  "Format NODE to a string.
Return NODE as a string with text-properties set by its property
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
                        (let* ((org-done-keywords org-done-keywords-for-agenda)
                               (face (org-get-todo-face todo)))
                          (when face
                            (add-text-properties 0 (length todo) (list 'face face) todo))
                          todo))
                      (when-let (priority (org-roam-node-priority node))
                        (let ((string (byte-to-string priority)))
                          (when (string-match "\\(\\[#\\(.\\)\\]\\)" string)
                            (let ((face (org-get-priority-face (string-to-char (match-string 2 string)))))
                              (org-add-props string nil 'face face 'font-lock-fontified t)))))
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
           ;; FIXME: Use proper prefix (from org-ql, what does this mean?)
           (concat "  " it)
           (org-add-props it properties
             'org-agenda-type 'search
             'todo-state (org-roam-node-todo node)
             'tags (org-roam-node-tags node)
             ;;'org-habit-p (org)
             )))))

(defun org-roam-ql--refresh-agenda-buffer ()
  "Refresh the agenda-based org-roam-ql buffer."
  (org-roam-ql--refresh-buffer-with-display-function #'org-roam-ql--agenda-buffer-for-nodes))

(defun org-roam-ql--nodes-from-agenda-buffer (buffer)
  "Return the list of nodes from the `org-agenda' BUFFER.
If there are entries that do not have an ID, it will signal an error"
  ;; Copied from `org-agenda-finalize'
  (with-current-buffer buffer
    (let (mrk nodes (line-output 0))
      (save-excursion
        (goto-char (point-min))
        (while (equal line-output 0)
          (when (setq mrk (get-text-property (point) 'org-hd-marker))
            (org-with-point-at mrk
              ;; pick only nodes
              (-if-let (id (org-id-get))
                  (push (org-roam-node-from-id id) nodes)
                (user-error "Non roam-node headings in query (in buffer %s).?"
                            (buffer-name)))))
          (setq line-output (forward-line))))
      nodes)))

;; *****************************************************************************
;; Functions to switch between org-roam/org-roam-ql buffers and
;; agenda-view buffers
;; *****************************************************************************
;;;###autoload
(defun org-roam-ql-agenda-buffer-from-roam-buffer ()
  "Convert a roam buffer to agenda buffer."
  (interactive)
  (if (derived-mode-p 'org-roam-mode)
      (org-roam-ql--agenda-buffer-for-nodes (org-roam-ql--nodes-from-roam-buffer (current-buffer))
                                            (org-roam-ql--get-formatted-title org-roam-ql-buffer-title nil "from roam buffer")
                                            (org-roam-ql--get-formatted-buffer-name
                                             (org-roam-ql--get-formatted-title org-roam-ql-buffer-title nil "from roam buffer"))
                                            org-roam-ql-buffer-query)
    (user-error "Not in a org-roam-ql agenda buffer")))

(defun org-roam-ql-roam-buffer-from-agenda-buffer ()
  "Convert a agenda-buffer reusult to a roam-buffer."
  (interactive)
  (org-roam-ql--roam-buffer-for-nodes (org-roam-ql--nodes-from-agenda-buffer (current-buffer))
                                      (org-roam-ql--get-formatted-title org-roam-ql-buffer-title nil "from agenda buffer")
                                      (org-roam-ql--get-formatted-buffer-name
                                       (org-roam-ql--get-formatted-title org-roam-ql-buffer-title nil "from agenda buffer"))
                                      (if org-roam-ql-buffer-query
                                          org-roam-ql-buffer-query
                                        `(in-buffer ,(buffer-name)))
                                      org-roam-ql-preview-function))

;; *****************************************************************************
;; org-roam-ql transient
;; *****************************************************************************
;; Copying alot it from org-ql-view and magit-transient

(defclass org-roam-ql--variable (transient-variable)
  ((default-value :initarg :default-value)))

(defclass org-roam-ql--variable--choices (org-roam-ql--variable) nil)

(defclass org-roam-ql--variable--sexp (org-roam-ql--variable) nil)

(cl-defmethod transient-init-value ((obj org-roam-ql--variable))
  "Initialize the transient value of OBJ."
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

(cl-defmethod transient-infix-read ((obj org-roam-ql--variable--choices))
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
  (or (oref obj value) ""))

(cl-defmethod transient-format-value ((obj org-roam-ql--variable--choices))
  "Format of the OBJ's VALUE for choices."
  (let ((value (oref obj value)))
    (format "{ %s }"
            (s-join " | " (--map (if (s-equals-p it value)
                                     it
                                   (propertize it 'face 'transient-inactive-value))
                                 (oref obj choices))))))

(cl-defmethod transient-format-value ((obj org-roam-ql--variable--sexp))
  "Format of the OBJ's VALUE for sexpressions."
  ;; copied from `org-ql'.
  (let ((value (format "%S" (oref obj value))))
    (with-temp-buffer
      (delay-mode-hooks
        (insert value)
        (funcall #'emacs-lisp-mode)
        (font-lock-ensure)
        (buffer-string)))))

(transient-define-prefix org-roam-ql-buffer-dispatch ()
  "Show `org-roam-ql' dispatcher."
  ["Edit"
   [("t" org-roam-ql-view--transient-title)
    ("q" org-roam-ql-view--transient-query)
    ("s" org-roam-ql-view--transient-sort)
    ("i" org-roam-ql-view--transient-in)]]
  ["View"
   [("r" "Refresh" org-roam-ql-refresh-buffer)]
   [:if-derived org-roam-mode
                ("S" "Show in agenda buffer" org-roam-ql-agenda-buffer-from-roam-buffer)
                ("b" "Bookmark (org-roam-ql)" bookmark-set)]
   [:if-not-derived org-roam-ql-mode
                    ("o" "Convert to org-roam-ql buffer"
                     org-roam-ql-convert-roam-buffer-to-roam-ql-buffer)]
   [:if-derived org-agenda-mode
                ("S" "Show in org-roam buffer" org-roam-ql-roam-buffer-from-agenda-buffer)]])

;;;###autoload
(defun org-roam-ql-refresh-buffer ()
  "Refresh org-roam-ql buffer (agenda/org-roam)."
  (interactive)
  (cond
   ((derived-mode-p 'org-roam-mode)
    (org-roam-ql--refresh-roam-buffer))
   ((derived-mode-p 'org-agenda-mode)
    (org-roam-ql--refresh-agenda-buffer))
   (t (user-error "Not in a org-roam buffer or agenda search buffer"))))

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
            (read-string prompt (when org-roam-ql-buffer-title
                                  (format "%s" org-roam-ql-buffer-title))
                         history)))

(transient-define-infix org-roam-ql-view--transient-query ()
  :class 'org-roam-ql--variable--sexp
  :argument ""
  :variable 'org-roam-ql-buffer-query
  :prompt "Query: "
  :always-read t
  :reader (lambda (&rest _)
            (org-roam-ql--read-query (when org-roam-ql-buffer-query
                                       (format "%S" org-roam-ql-buffer-query)))))

(transient-define-infix org-roam-ql-view--transient-sort ()
  :class 'org-roam-ql--variable--sexp
  :argument ""
  :variable 'org-roam-ql-buffer-sort
  :prompt "Sort-by: "
  :always-read t
  :default-value nil
  :reader (lambda (prompt initial-input history)
            (completing-read prompt
                             (cons nil (hash-table-keys org-roam-ql--sort-functions))
                             nil 'require-match initial-input history)))

(transient-define-infix org-roam-ql-view--transient-in ()
  :class 'org-roam-ql--variable--choices
  :argument ""
  :variable 'org-roam-ql-buffer-in
  :default-value "org-roam-db"
  :prompt "In: "
  :always-read t
  :choices '("in-buffer" "org-roam-db"))

;; *****************************************************************************
;; org dynamic block
;; *****************************************************************************

(defun org-dblock-write:org-roam-ql (params)
  "Write org block for org-roam-ql with PARAMS."
  (let ((query (plist-get params :query))
        (columns (plist-get params :columns))
        (sort (plist-get params :sort))
        (take (plist-get params :take))
        (no-link (plist-get params :no-link)))
    (if (and query columns)
        (-if-let (nodes (org-roam-ql-nodes query sort))
            (progn
              (when take
                (setq nodes (cl-etypecase take
                              ((and integer (satisfies cl-minusp)) (-take-last (abs take) nodes))
                              (integer (-take take nodes)))))
              (insert "|"
                      (if no-link "" "|")
                      (string-join (--map (pcase it
                                            ((pred symbolp) (capitalize (symbol-name it)))
                                            (`(,_ ,name) name))
                                          columns)
                                   " | ")
                      "|\n|-\n")
              (dolist (node nodes)
                (insert "|"
                        (if no-link
                            ""
                          (format "[[id:%s][link]]|" (org-roam-node-id node)))
                        (string-join (--map (let ((value (funcall (intern-soft
                                                                   (format "org-roam-node-%s" it))
                                                                  node)))
                                              (pcase value
                                                ((pred listp) (string-join (--map (format "%s" it) value) ","))
                                                (_ (format "%s" value))))
                                            columns)
                                     "|")
                        "|\n"))
              (delete-char -1)
              (org-table-align))
          (message "Query results is empty"))
      (user-error "Dynamic block needs to specify :query and :columns"))))

;; *****************************************************************************
;; org agenda custom command
;; *****************************************************************************
(defun org-roam-ql-agenda-block (query)
  "Insert items for QUERY into current buffer.
See `org-roam-ql-nodes' for more information on QUERY.  Intended to be
used as a user-defined function in `org-agenda-custom-commands'.
QUERY is the `match' item in the custom command form.  Currently this
doesn't respect agenda restrict."
  ;; Copying alot from org-ql-search-block
  (let (narrow-p old-beg old-end strings)
    (when-let* ((from (pcase org-agenda-restrict
                        ('nil (org-agenda-files nil 'ifmode))
                        (_ (prog1 org-agenda-restrict
                             (with-current-buffer org-agenda-restrict
			       ;; Narrow the buffer; remember to widen it later.
			       (setf old-beg (point-min) old-end (point-max)
                                     narrow-p t)
			       (narrow-to-region org-agenda-restrict-begin org-agenda-restrict-end))))))
                (nodes (org-roam-ql-nodes query)))
      (when narrow-p
        ;; Restore buffer's previous restrictions.
        (with-current-buffer from
          (narrow-to-region old-beg old-end)))
      (org-agenda-prepare)
      (org-agenda--insert-overriding-header (org-roam-ql--get-formatted-title nil query))
      ;; But we do call `org-agenda-finalize-entries', which allows `org-super-agenda' to work.
      (dolist-with-progress-reporter (node nodes)
          (format "Processing %s nodes" (length nodes))
        (push (org-roam-ql-view--format-node node) strings))
      (insert (org-agenda-finalize-entries strings) "\n"))))

(defun org-roam-ql-nodes-files (source-or-query)
  "Retuns a list of files of the corresponding SOURCE-OR-QUERY.
See `org-roam-ql-nodes' for more information on SOURCE-OR-QUERY."
  (-uniq (-map #'org-roam-node-file (org-roam-ql-nodes source-or-query))))

;; *****************************************************************************
;; Helper functions
;; *****************************************************************************
(defun org-roam-ql-insert-node-title ()
  "Select a node and insert only its title.
Can be used in the minibuffer or when writting querries."
  (interactive)
  (insert (format "\"%s\"" (org-roam-node-title (org-roam-node-read nil nil nil t)))))

(defun org-roam-ql--default-query-for-roam-buffer ()
  "Function used for `org-roam-ql-default-org-roam-buffer-query'."
  `(backlink-to (id ,(org-roam-node-id org-roam-buffer-current-node))))

(defun org-roam-ql-preview-default-function (_ _)
  "The deafult value for `org-roam-ql-preview-fn'.

Simple wrapper `org-roam-preview-function'."
  (funcall org-roam-preview-function))

;;;###autoload
(defun org-roam-ql-convert-roam-buffer-to-roam-ql-buffer ()
  "Convert a `org-roam-mode' buffer to a `org-roam-ql-mode' buffer."
  (interactive)
  (when (and (derived-mode-p 'org-roam-mode) (not (derived-mode-p 'org-roam-ql-mode)))
    (org-roam-ql-search
     (pcase org-roam-ql-default-org-roam-buffer-query
       ((pred functionp)
        (funcall org-roam-ql-default-org-roam-buffer-query))
       (_ org-roam-ql-default-org-roam-buffer-query))
     (or org-roam-ql-buffer-title
         (org-roam-ql--get-formatted-title
          (org-roam-node-title org-roam-buffer-current-node) nil))
     (or org-roam-ql-buffer-sort nil))))

;; *****************************************************************************
;; Setup of org-roam-ql
;; *****************************************************************************
(dolist (predicate
         '(
           ;; TODO: revise this
           (file-atime "Compare to `file-atime' of a node to arg based on comparison parsed (< or >)"  org-roam-node-file-atime . org-roam-ql--predicate-compare-time)
           (file-atime-is-before "Check if `file-atime' of a node is earlier (less) than arg"  org-roam-node-file-atime . org-roam-ql--predicate-time<)
           (file-atime-is-after "Check if `file-atime' of a node is later (greater) than arg"  org-roam-node-file-atime . org-roam-ql--predicate-time>)
           ;; TODO: revise this
           (file-mtime "Compare to `file-mtime' of a node to arg based on comparison parsed (< or >)"  org-roam-node-file-mtime . org-roam-ql--predicate-compare-time)
           (file-mtime-is-before "Check if `file-mtime' of a node is earlier (less) than arg"  org-roam-node-file-mtime . org-roam-ql--predicate-time<)
           (file-mtime-is-after "Check if `file-mtime' of a node is later (greater) than arg"  org-roam-node-file-mtime . org-roam-ql--predicate-time>)
           (title-or-alias "Compare to `title' or `aliases' of a node" org-roam-ql--title-and-aliased-of-node . org-roam-ql--predicate-list-match)
           ;; TODO: sql query
           (scheduled "Compare `scheduled' of a node to arg based on comparison parsed (< or >)"  org-roam-node-scheduled . org-roam-ql--predicate-compare-time)
           (scheduled-is-before "Check if `scheduled' of a node is earlier (less) than arg"  org-roam-node-scheduled . org-roam-ql--predicate-time<)
           (scheduled-is-after "Check if `scheduled' of a node is later (greater) than arg"  org-roam-node-scheduled . org-roam-ql--predicate-time>)
           ;; TODO: sql query
           (deadline "Compare `deadline' of a node to arg based on comparison parsed (< or >)"  org-roam-node-deadline . org-roam-ql--predicate-compare-time)
           (deadline-is-before "Check if `deadline' of a node is earlier (less) than arg"  org-roam-node-deadline . org-roam-ql--predicate-time<)
           (deadline-is-after "Check if `deadline' of a node is later (greater) than arg"  org-roam-node-deadline . org-roam-ql--predicate-time>)
           (properties "Compare to `properties' of a node"
                       org-roam-node-properties . org-roam-ql--predicate-property-match)
           ;; TODO: sql query
           (tags "Compare to `tags' of a node" org-roam-node-tags . org-roam-ql--predicate-list-match)
           (funcall "Function to test with a node" identity . org-roam-ql--predicate-funcall)))
  (org-roam-ql-defpred (car predicate) (cadr predicate) (caddr predicate) (cdddr predicate)))

(dolist (expansion-function
         `((id "Check if value is equal to `id' of a node" . ,(org-roam-ql--expand-comparison-function '= 'id))
           (level= "Check if value is equal to `level' of a node" . ,(org-roam-ql--expand-comparison-function '= 'level))
           (level< "Check if `level' of a node is less than value" . ,(org-roam-ql--expand-comparison-function '< 'level))
           (level> "Check if `level' of a node is greater than value" . ,(org-roam-ql--expand-comparison-function '> 'level))
           (level<> "Check if `level' of a node is not equal to value" . ,(org-roam-ql--expand-comparison-function '<> 'level))
           (point= "Check if value is equal to `point' of a node" . ,(org-roam-ql--expand-comparison-function '= 'point))
           (point< "Check if `point' of a node is less than value" . ,(org-roam-ql--expand-comparison-function '< 'point))
           (point> "Check if `point' of a node is greater than value" . ,(org-roam-ql--expand-comparison-function '> 'point))
           (point<> "Check if `point' of a node is not equal to value" . ,(org-roam-ql--expand-comparison-function '<> 'point))
           (file "Check if `file' of a node is LIKE the value" . ,(org-roam-ql--expand-s-like-function file #'org-roam-node-file))
           (file-title "Check if `file-title' of a node is LIKE the value" . org-roam-ql--file-title-expansion-function)
           (todo "Check if `todo' of a node is LIKE the value" . ,(org-roam-ql--expand-s-like-function todo #'org-roam-node-todo))
           (title "Check if `title' of a node is LIKE the value" . ,(org-roam-ql--expand-s-like-function title #'org-roam-node-title))
           (aliases "Check if `aliases' of a node is LIKE the value" . org-roam-ql--aliases-expansion-function)
           (priority "Compare to `priority' of a node." ,(org-roam-ql--expand-s-like-function priority #'org-roam-node-priority))
           (refs "Check if `refs' of a node is LIKE the value" . ,(org-roam-ql--expand-s-like-function refs #'org-roam-node-refs))
           (backlink-to "Backlinks to results of QUERY." . org-roam-ql--expand-backlinks)
           (backlink-from "Forewardlinks to results of QUERY" . org-roam-ql--expand-forwardlinks)
           (in-buffer "In BUFFER" . org-roam-ql--expansion-identity)
           (nodes-list "List of nodes" . org-roam-ql--expansion-identity)
           (function "FUNCTION that returns list of nodes" . org-roam-ql--expansion-identity)))
  (org-roam-ql-defexpansion (car expansion-function) (cadr expansion-function) (cddr expansion-function)))

(dolist (sort-function
         '(("title" . string<)
           ("file" . string<)
           ("file-title" . string<)
           ("level" . <)
           ("point" . <)
           ("scheduled" . org-roam-ql--sort-time-less)
           ("deadline" . org-roam-ql--sort-time-less)
           ("file-atime" . org-roam-ql--sort-time-less)
           ("file-mtime" . org-roam-ql--sort-time-less)))
  (org-roam-ql--sort-function-for-slot (car sort-function) (cdr sort-function)))

(provide 'org-roam-ql)

;;; org-roam-ql.el ends here
