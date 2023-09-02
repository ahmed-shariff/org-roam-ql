;;; org-roam-ql.el --- Tests for org-roam-ql -*- lexical-binding: t -*-

;; Copyright (C) 2023 Shariff AM Faleel

;; Author: Shariff AM Faleel
;; Package-Requires: ((buttercup))

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Setup sandbox directory (named sandbox) with: `make sandbox=sandbox v=vvv init'
;; Run tests with: `make sandbox=sandbox quick`

;;; Code:

(require 'buttercup)
(require 'org-roam-ql)
(require 'org-roam-ql-ql)
(require 'dash)

(describe "org-roam-ql"
  :var ((org-roam-directory (expand-file-name "tests/roam-nodes"
                                              (locate-dominating-file default-directory ".git"))))
  (org-roam-db-sync)
  (describe "Test the s-exp query"
    (it "with the todo"
      (expect (org-roam-ql--check-if-valid-query '(todo "TODO"))
      :not :to-be nil))
    (it "with nested 'and and 'or"
      (expect (org-roam-ql--check-if-valid-query '(or (todo "done") (and (todo "todo") (scheduled "something")))) :not :to-be nil))
    (it "fails when unexpected term in query"
      (expect (org-roam-ql--check-if-valid-query '(or (todo "DONE") (and (todo "TODO") (what-is-this "something")))) :to-be nil)))

  (describe "Test org-roam-ql-nodes"
    (it "with list of nodes"
      (let ((nodes (cl-subseq (org-roam-node-list) 0 3)))
        (expect (org-roam-ql-nodes nodes) :to-equal nodes)))
    (it "with one node"
      (let ((nodes (car (org-roam-node-list))))
        (expect (org-roam-ql-nodes nodes) :to-equal (list nodes))))
    (it "with sql-db query"
      (expect (org-roam-ql-nodes '([(like title "%Node a%")])) :to-equal (--filter (s-match "Node a" (org-roam-node-title it)) (org-roam-node-list))))
    (describe "with roam predicate"
      (it "todo (cannot be interpreted as function)"
        (expect (org-roam-ql-nodes '(todo "DONE")) :to-equal (--filter (--when-let (org-roam-node-todo it) (s-match "DONE" it)) (org-roam-node-list))))
      (it "or (could be interpreted as function)"
        (expect (sort (-map #'org-roam-node-id (org-roam-ql-nodes '(or (tags "interesting") (todo "DONE")))) #'string>)
                :to-equal (sort (-map #'org-roam-node-id
                                      (--filter
                                       (let ((todo-state (org-roam-node-todo it))
                                             (tags (org-roam-node-tags it)))
                                         (or (and todo-state
                                                  (s-match "DONE" todo-state))
                                             (member "interesting" tags)))
                                       (org-roam-node-list)))
                                #'string>)))
      (it "not"
        (expect (sort (-map #'org-roam-node-id (org-roam-ql-nodes '(not (tags "interesting")))) #'string>)
                :to-equal (sort (-map #'org-roam-node-id
                                      (--filter
                                       (not (member "interesting" (org-roam-node-tags it)))
                                       (org-roam-node-list)))
                                #'string>))))
    (describe "with function"
      (it "returning list of nodes"
        (expect (org-roam-ql-nodes (lambda () (list (car (org-roam-node-list))))) :to-equal (list (car (org-roam-node-list)))))
      (it "not returning list of nodes"
        (expect (org-roam-ql-nodes (lambda () 'somthing-else)) :to-throw 'user-error))
      (it "returning nil"
        (expect (org-roam-ql-nodes (lambda () nil)) :to-equal nil)))
    ;; FIXME: Not sure why passing just the buffer name is not working here!
    (describe "with roam buffers"
      (let* ((nodes (--filter (s-match "test2.org" (org-roam-node-file it)) (org-roam-node-list)))
             (buffer-name "test-buffer"))
        (org-roam-ql--roam-buffer-for-nodes nodes "test buffer" buffer-name nodes)
        (it "as a string"
          (expect (-map #'org-roam-node-id (with-current-buffer buffer-name
                                             (org-roam-ql-nodes buffer-name)))
                  :to-have-same-items-as (-map #'org-roam-node-id nodes)))
        (it "as a predicate"
          (expect (-map #'org-roam-node-id (with-current-buffer buffer-name
                                             (org-roam-ql-nodes `(in-buffer ,buffer-name))))
                  :to-have-same-items-as (-map #'org-roam-node-id nodes)))))
    ;; FIXME: Not sure why passing just the buffer name is not working here!
    (describe "with agenda buffers"
      (let* ((nodes (--filter (s-match "test2.org" (org-roam-node-file it)) (org-roam-node-list)))
             (buffer-name "test-buffer"))
        (org-roam-ql--agenda-buffer-for-nodes nodes "test buffer" buffer-name nodes)
        (it "as a string"
          (expect (-map #'org-roam-node-id (with-current-buffer buffer-name
                                             (org-roam-ql-nodes buffer-name)))
                  :to-have-same-items-as (-map #'org-roam-node-id nodes)))
        (it "as a predicate"
          (expect (-map #'org-roam-node-id (with-current-buffer buffer-name
                                             (org-roam-ql-nodes `(in-buffer ,buffer-name))))
                  :to-have-same-items-as (-map #'org-roam-node-id nodes)))))
    (describe "with invalid inputs"
      (it "string"
        (expect (org-roam-ql-nodes "a random string") :to-throw 'user-error))))

  (describe "Test displaying content"
    :var* ((query '(or (todo "DONE") (tags "interesting")))
           (query-result (org-roam-ql-nodes query))
           (query-result-ids (-map #'org-roam-node-id query-result)))
    (it "in agenda buffer"
      (org-roam-ql--agenda-buffer-for-nodes query-result "agenda-test" "agenda-test-buffer" query)
      (expect (-map #'org-roam-node-id (org-roam-ql--nodes-from-agenda-buffer (get-buffer "agenda-test-buffer"))) :to-have-same-items-as query-result-ids))
    (it "in roam buffer"
      (org-roam-ql--roam-buffer-for-nodes query-result "roam-test" "roam-test-buffer" query)
      (expect (-map #'org-roam-node-id (org-roam-ql--nodes-from-roam-buffer (get-buffer "roam-test-buffer"))) :to-have-same-items-as query-result-ids))
    (let ((roam-buffer-name (org-roam-ql--get-formatted-buffer-name (org-roam-ql--get-formatted-title "test-title" nil)))
          (agenda-buffer-name (org-roam-ql--get-formatted-buffer-name
                               (org-roam-ql--get-formatted-title "test-title" nil "from roam buffer")))
          (second-roam-buffer-name (org-roam-ql--get-formatted-buffer-name
                                    (org-roam-ql--get-formatted-title
                                     (org-roam-ql--get-formatted-title "test-title" nil "from roam buffer")
                                     nil "from agenda buffer")))
          roam-buffer agenda-buffer second-roam-buffer)
      (org-roam-ql-search query "test-title")
      (it "switching from roam to agenda buffer"
        (with-current-buffer roam-buffer-name
          (org-roam-ql-agenda-buffer-from-roam-buffer))
        (expect (-map #'org-roam-node-id (org-roam-ql--nodes-from-agenda-buffer (get-buffer agenda-buffer-name))) :to-have-same-items-as query-result-ids))
      (it "switching from agenda to roam buffer"
        (with-current-buffer agenda-buffer-name
          (org-roam-ql-roam-buffer-from-agenda-buffer))
        (expect (-map #'org-roam-node-id (org-roam-ql--nodes-from-roam-buffer (get-buffer second-roam-buffer-name))) :to-have-same-items-as query-result-ids))))

  (describe "Test org-roam-ql-ql--get-roam-queries"
    (it "without any ql-queries"
      (expect (org-roam-ql-ql--get-roam-queries '(or (todo "TODO") (tags "tag1" "tag2") (and (title "a") (tags "tag3"))))
              :to-equal nil))
    (it "with multiple nested queries"
      (expect (org-roam-ql-ql--get-roam-queries '(or (org-roam-query (todo "TODO")) (and (org-roam-query (tags "tag1" "tag2")) (scheduled "something"))))
              :to-equal '((org-roam-query (todo "TODO")) (org-roam-query (tags "tag1" "tag2"))))))


  ;; (describe "Tets query expansion"
  ;;   (it "with only todo"
  ;;     (expect (macroexpand-1 '(org-roam-ql--expand-query-function (todo "TODO")))
  ;;             :to-equal `(let ((val (org-roam-node-todo it))) (and val (s-match "TODO" val)))))
  ;;   (it "with only tags"
  ;;     (expect (macroexpand-1 '(org-roam-ql--expand-query-function (tags "ATTACH")))
  ;;             :to-equal `(let ((val (org-roam-node-tags it)))
  ;;                          (and val (funcall (lambda (tags &rest values)
  ;;                                              (--all-p (member it tags) values))
  ;;                                            val "ATTACH")))))
  ;;   (it "with 'or"
  ;;     (expect (macroexpand-1 '(org-roam-ql--expand-query (or (todo "TODO") (and (schedule '(1)) (todo "DONE")))))
  ;;             :to-equal
  ;;             `(or
  ;;               (org-roam-ql--expand-query (todo "TODO"))
  ;;               (org-roam-ql--expand-query (and (schedule '(1)) (todo "DONE"))))))
  ;;   (it "with 'and"
  ;;     (expect (macroexpand-1 '(org-roam-ql--expand-query (and (todo "TODO") (or (schedule '(1)) (todo "DONE")))))
  ;;             :to-equal
  ;;             `(and
  ;;               (org-roam-ql--expand-query (todo "TODO"))
  ;;               (org-roam-ql--expand-query (or (schedule '(1)) (todo "DONE"))))))))
)
