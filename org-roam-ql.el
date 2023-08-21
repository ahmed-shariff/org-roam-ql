;;; org-roam-ql.el --- Interface to query and view results from org-roam -*- lexical-binding: t -*-

;; Copyright (c) 2023 Shariff AM Faleel (shariff.mfa@outlook.com)

;; Permission is hereby granted, free of charge, to any person obtaining a copy
;; of this software and associated documentation files (the "Software"), to deal
;; in the Software without restriction, including without limitation the rights
;; to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
;; copies of the Software, and to permit persons to whom the Software is
;; furnished to do so, subject to the following conditions:
;;
;; The above copyright notice and this permission notice shall be included in all
;; copies or substantial portions of the Software.
;;
;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;; NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS
;; BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN
;; ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
;; CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
;; SOFTWARE.

;; Author: Shariff AM Faleel
;; Version: 0.1
;; Package-Requires: ((emacs "28") (org-roam "2.2.0") (s "1.12.0") (magit-section "3.3.0") (transient "0.4") (org-super-agenda "1.2"))
;; Homepage: https://github.com/ahmed-shariff/org-roam-ql
;; SPDX-License-Identifier: MIT

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

;; FIXME: What is this docstring!
(defvar org-roam-ql--query-comparison-functions (make-hash-table) "Holds the function to check different elements of the roam-query.")
(defvar org-roam-ql--query-expansion-functions (make-hash-table) "Holds the function to expand a query.")
(defvar org-roam-ql--cache (make-hash-table))
(defvar org-roam-ql--search-query-history '() "History of queries with `org-roam-ql-search'.")
(defvar-local org-roam-ql-buffer-title nil "The current title of the buffer.")
(defvar-local org-roam-ql-buffer-query nil "The current query of the buffer.")
(defvar-local org-roam-ql--buffer-displayed-query nil "The query which produced the results of the buffer.")
(defvar-local org-roam-ql-buffer-in nil
  "Define which option to use - 'in-buffer' or 'org-roam-db'.")

;;;###autoload
(defun org-roam-ql-nodes (source-or-query)
  "Convert SOURCE-OR-QUERY to org-roam-nodes.
SOURCE-OR-QUERY can be one of the following:
- A org-roam-ql query.
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
- An org-roam-query.
- A function that returns a list of org-roam-nodes."
  (cond
   ;; TODO: think of a better way to display the nodes in the query
   ;; without showing it all. Perhaps use only ids?
   ((org-roam-ql--check-if-list-of-org-roam-nodes-list source-or-query)
    (-list source-or-query))
   ;; get-buffer returns a buffer if source-or-query is a buffer obj
   ;; or the name of a buffer
   ((org-roam-ql--check-if-org-roam-ql-buffer source-or-query)
    (cond
     ((with-current-buffer source-or-query (derived-mode-p 'org-roam-mode))
      (org-roam-ql--nodes-from-roam-buffer (get-buffer source-or-query)))
     ((with-current-buffer source-or-query (derived-mode-p 'org-agenda-mode))
      (org-roam-ql--nodes-from-agenda-buffer (get-buffer source-or-query)))))
   ((org-roam-ql--check-if-org-roam-db-parameters source-or-query)
    (let ((query (car source-or-query))
          (args (cdr source-or-query)))
      (--map (org-roam-node-from-id (car it))
             (apply #'org-roam-db-query
                    (if (equal :select (aref query 0))
                        query
                      (vconcat [:select id :from nodes :where] query))
                    args))))
   ((and (listp source-or-query) (org-roam-ql--check-if-valid-query source-or-query))
    (org-roam-ql--expand-query source-or-query))
   ((functionp source-or-query) (--when-let (funcall source-or-query)
                                  (if (and (listp it) (-all-p #'org-roam-node-p it))
                                      it
                                    (user-error "Function did not expand to list of nodes"))))
   (t (user-error "Invalid source-or-query"))))

;; Can we make org-roam-ql aware of any changes that can happen?
(defun org-roam-ql--nodes-cached (source-or-query)
  "Cache results of `org-roam-ql-nodes'.
See `org-roam-ql-nodes' for information on SOURCE-OR-QUERY.
Not caching or invalidating in the top level function as the
database/buffers can change.  Currently this is only used by the
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

(defun org-roam-ql--expand-query (query)
  "Expand a org-roam-ql QUERY."
  (if (and (listp query) (member (car query) '(or and not)))
      ;; FIXME: two nodes are not equal? using this -compare-fn as workaround
      (let ((-compare-fn (lambda (node1 node2)
                           (s-equals-p (org-roam-node-id node1) (org-roam-node-id node2)))))
        (if (eq (car query) 'not)
            (-difference
             ;; Caching values
             (org-roam-ql--nodes-cached (org-roam-node-list))
             (org-roam-ql--expand-query (cadr query)))
          (funcall
           #'-reduce
           (pcase (car query)
             ('or #'-union)
             ('and #'-intersection))
           (--map (org-roam-ql--expand-query it) (cdr query)))))
    (let* ((query-key (and (listp query) (car query)))
           (query-expansion-function (gethash query-key org-roam-ql--query-expansion-functions))
           (query-comparison-function-info (gethash query-key org-roam-ql--query-comparison-functions)))
      (cond
       ((and query-key query-expansion-function)
        (org-roam-ql--nodes-cached (apply query-expansion-function (cdr query))))
       ;; FIXME: Think of a better way to to this
       ((and query-key query-comparison-function-info)
        (--filter (let ((val (funcall (car query-comparison-function-info) it)))
                    (and val
                         (apply (cdr query-comparison-function-info) (append (list val) (cdr query)))))
                  ;; Caching values
                  (org-roam-ql--nodes-cached (org-roam-node-list))))
       ;; `org-roam-ql-nodes' will error if query is invalid
       (t (org-roam-ql--nodes-cached query))))))

;;;###autoload
(defun org-roam-ql-search (source-or-query &optional title)
  "Basically what `org-ql-search does', but for org-roam-nodes.
See `org-roam-ql-nodes' for what SOURCE-OR-QUERY can be.  TITLE is a title
to associate with the view.  DISPLAY-IN is expected to be a symbol,
either `'org-ql' or `'org-roam'.  If its `org-ql', the results from the
SOURCE-OR-QUERY will be displayed in `org-ql's agenda buffer.  If its
`org-roam', will be displayed in a org-roam-ql buffer."
  (interactive (list (let ((query (org-roam-ql--read-query)))
                       (if (vectorp query)
                           (list query)
                         query))
                     (read-string "Title: ")))
  (let* ((nodes (org-roam-ql-nodes source-or-query)))
    (org-roam-ql--roam-buffer-for-nodes
     nodes
     title
     (org-roam-ql--get-formatted-buffer-name
      (org-roam-ql--get-formatted-title title source-or-query))
     source-or-query)))

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

;; FIXME: To be performant this can be done by constructing the
;; results instead of going through org-ql?
;;;###autoload
;; (defun org-roam-ql-select (source-or-query &optional ql-query action narrow sort)
;;   "Process SOURCE-OR-QUERY with org-roam-db and pass it to org-ql to
;; be filtered with QL-QUERY.  ACTION NARROW and SORT are passed to
;; `org-ql-select' as is.

;; See `org-roam-ql-nodes' for the values that can be passed to
;; SOURCE-OR-QUERY."
;;   (let* ((nodes (org-roam-ql-nodes source-or-query))
;;          (buffers (org-roam-ql--nodes-files nodes))
;;          (query (append `(and (org-roam-query ,source-or-query)) ql-query)))
;;     (org-roam-ql-clear-cache)
;;     (when buffers
;;       (org-ql-select buffers query :action action :narrow narrow :sort sort))))

(defun org-roam-ql--nodes-files (nodes)
  "Return the list of files from the list of NODES."
  (-uniq (mapcar #'org-roam-node-file nodes)))

(defun org-roam-ql--check-if-list-of-org-roam-nodes-list (source-or-query)
  "Return non-nil if SOURCE-OR-QUERY is a list of org-roam-nodes."
  (or (org-roam-node-p source-or-query)
      (-all-p #'org-roam-node-p source-or-query)))

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
        (org-roam-ql--check-if-org-roam-ql-buffer s-exp)
        (org-roam-ql--check-if-org-roam-db-parameters s-exp))))

(defun org-roam-ql--read-query (&optional initial-input)
  "Read query from minibuffer.
Sets the history as well."
  (let* ((query (read-string "Query: " initial-input 'org-roam-ql--search-query-history)))
    (setf org-roam-ql--search-query-history (delete-dups
                                             (append
                                              org-roam-ql--search-query-history
                                              (list query))))
    (read query)))
;; *****************************************************************************
;; Functions for predicates and expansions
;; *****************************************************************************

;;;###autoload
(defmacro org-roam-ql-defpred (name extraction-function comparison-function)
  "Create a org-roam-ql predicate with the NAME.
The COMPARISON-FUNCTION is a function that returns non-nil if this
predicate doesn't fail for a given org-roam-node.  The first value
passed to this function would be the value from calling the
EXTRACTION-FUNCTION with the respective node, and the remainder of the
arguments from the predicate itself."
  `(puthash ,name
            (cons ,extraction-function ,comparison-function)
            org-roam-ql--query-comparison-functions))

(defmacro org-roam-ql-defexpansion (name expansion-function)
  "Add an EXPANSION-FUNCTION identified by NAME in an org-roam-ql query.
The EXPANSION-FUNCTION should take the parameters
passed in the query and return values that can be passed to
  `org-roam-nodes'"
  `(puthash ,name ,expansion-function org-roam-ql--query-expansion-functions))

(defun org-roam-ql--predicate-s-match (value regexp &optional exact)
  "Return non-nil if there is a REGEXP match in VALUE.
If EXACT, test if VALUE and REGEXP are equal strings."
  (when (and value regexp)
    (if exact
        (s-equals-p value regexp)
      (s-match regexp value))))

(defun org-roam-ql--predicate-s-equals-p (value other)
  "Return non-nil if VALUE and OTHER are equal strings."
  (when (and value other)
    (s-equals-p value other)))

;; TODO: Multivalue properties
(defun org-roam-ql--predicate-property-match (value prop prop-val)
  "Return non-nil if PROP is a key in the alist PROP-VAL.
And its value is a string equal to VALUE."
  (-when-let (val (assoc prop value))
    (s-match prop-val (cdr val))))

(defun org-roam-ql--predicate-tags-match (values &rest tags)
  "Return non-nil if all strings in VALUES are in the list of strings TAGS."
  (--all-p (member it values) (-list tags)))

(cl-defun org-roam-ql--expand-forwardlinks (source-or-query &key (type "id") (combine :and))
  "Expansion function for forwardlinks.
Return parameters that cab be passed to org-roam-nodes that would
return a list of nodes that have forward links to any nodes that
SOURCE-OR-QUERY resolves to, as a list.  TYPE is the type of the
link.  COMBINE can be :and or :or.  If :and, only nodes that have
forwardlinks to all results of source-or-query, else if have
forwardlinks to any of them."
  (list (apply #'vector
               (append '(:select :distinct links:dest
                                 :from links
                                 :where (in links:source $v1))
                       (when type
                         `(,(pcase combine
                              (:and :and)
                              (:or :or)
                              (_ (user-error "Keyword :combine should be :and or :or; got %s" combine)))
                           (= type $s2)))))
        (apply #'vector (-map #'org-roam-node-id (org-roam-ql--nodes-cached source-or-query)))
        type))

(cl-defun org-roam-ql--expand-backlinks (source-or-query &key (type "id") (combine :and))
  "Expansion function for backlinks.
Returns parameters org-roam-nodes that would return a list of nodes
that have back links to any nodes that SOURCE-OR-QUERY resolves to, as
a list.  TYPE is the type of the link.  COMBINE can be :and or :or.
If :and, only nodes that have backlinks to all results of
source-or-query, else if have backlinks to any of them."
  (list (apply #'vector
               (append '(:select :distinct links:source
                                 :from links
                                 :where (in links:dest $v1))
                       (when type
                         `(,(pcase combine
                              (:and :and)
                              (:or :or)
                              (_ (user-error "Keyword :combine should be :and or :or; got %s" combine)))
                           (= type $s2)))))
        (apply #'vector (-map #'org-roam-node-id (org-roam-ql--nodes-cached source-or-query)))
        type))

(defun org-roam-ql--expansion-identity (source-or-query)
  "Expansion function that passes the SOURCE-OR-QUERY as is."
  source-or-query)

(defun org-roam-ql--predicate-funcall (value f)
  "Return the value of calling the function F with VALUE as its parameter."
  (funcall f value))

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
  :group 'org-roam-ql)

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
             (org-roam-ql-nodes query)
             (if subquery
                 (org-roam-ql--get-formatted-title org-roam-ql-buffer-title nil "extended")
               org-roam-ql-buffer-title)
             (if subquery
                 (org-roam-ql--get-formatted-buffer-name
                  (org-roam-ql--get-formatted-title org-roam-ql-buffer-title nil "extended"))
               buffer-name)
             query)))

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
          (org-roam-ql-search `(and (backlink-to (id ,(org-roam-node-id org-roam-buffer-current-node)))
                                    ,query)
                              title)))
    (org-roam-ql--refresh-buffer-with-display-function #'org-roam-ql--roam-buffer-for-nodes)))

(defun org-roam-ql--render-roam-buffer (sections title buffer-name source-or-query)
  "Render SECTIONS (list of functions) in an org-roam-ql buffer.
TITLE is the title, BUFFER-NAME is the name for the buffer.  See
`org-roam-nodes' for information on SOURCE-OR-QUERY."
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
            org-roam-ql-buffer-in "org-roam-db")
      (insert ?\n)
      (dolist (section sections)
        (funcall section))
      (goto-char 0))
    (display-buffer (current-buffer))))

(defmacro org-roam-ql--nodes-section (nodes &optional heading)
  "Return a function that generate magit-sections for NODES.
This can be passed as a section for `okm-render-org-roam-buffer' with
the NODES.  Nodes should be a list of org-roam nodes.  HEADING is the
heading for the function `magit-section'"
  `(lambda ()
     (magit-insert-section (org-roam)
       (magit-insert-heading ,heading)
       (dolist (entry
                ,nodes)
         (let ((pos (org-roam-node-point entry))
               (properties (org-roam-node-properties entry)))
           (org-roam-node-insert-section
            :source-node entry
            :point pos
            :properties properties))
         (insert ?\n))
       (run-hooks 'org-roam-buffer-postrender-functions))))

(defun org-roam-ql--roam-buffer-for-nodes (nodes title buffer-name &optional source-or-query)
  "View nodes in org-roam-ql buffer.
See `org-roam-ql--render-roam-buffer' for TITLE BUFFER-NAME and SOURCE-OR-QUERY.
See `org-roam-ql--nodes-section' for NODES."
  (org-roam-ql--render-roam-buffer
   (list
    (org-roam-ql--nodes-section nodes "Nodes:"))
   title buffer-name (or source-or-query nodes)))

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

(defun org-roam-ql--agenda-buffer-for-nodes (nodes title buffer-name &optional source-or-query super-groups)
  "Display the nodes in a agenda like buffer.
See `org-roam-ql--render-roam-buffer' for TITLE BUFFER-NAME and SOURCE-OR-QUERY.
See `org-roam-ql--nodes-section' for NODES.
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
                                        `(in-buffer ,(buffer-name)))))

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
  (oref obj value))

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
    ("i" org-roam-ql-view--transient-in)]]
  ["View"
   [("r" "Refresh" org-roam-ql-refresh-buffer)]
   [:if-derived org-roam-mode
                ("S" "Show in agenda buffer" org-roam-ql-agenda-buffer-from-roam-buffer)]
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
  :reader (lambda (prompt _initial-input history)
            (org-roam-ql--read-query (when org-roam-ql-buffer-query
                                       (format "%S" org-roam-ql-buffer-query)))))

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
        (take (plist-get params :take))
        (no-link (plist-get params :no-link)))
    (if (and query columns)
        (-if-let (nodes (org-roam-ql-nodes query))
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

;; Useful helper functions
(defun org-roam-ql-insert-node-title ()
  "Select a node and insert only its title.
Can be used in the minibuffer or when writting querries."
  (interactive)
  (insert (format "\"%s\"" (org-roam-node-title (org-roam-node-read nil nil nil t)))))

;; setup of org-roam-ql
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
           (funcall identity . org-roam-ql--predicate-funcall)))
  (org-roam-ql-defpred (car predicate) (cadr predicate) (cddr predicate)))

(dolist (expansion-function
         '((backlink-to . org-roam-ql--expand-backlinks)
           (backlink-from . org-roam-ql--expand-forwardlinks)
           (in-buffer . org-roam-ql--expansion-identity)
           (nodes-list . org-roam-ql--expansion-identity)
           (function . org-roam-ql--expansion-identity)))
  (org-roam-ql-defexpansion (car expansion-function) (cdr expansion-function)))

(provide 'org-roam-ql)

;;; org-roam-ql.el ends here
