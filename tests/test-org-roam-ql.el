;;; org-roam-ql.el --- Tests for org-roam-ql -*- lexical-binding: t -*-

;; Setup sandbox directory (named sandbox) with: `make sandbox=sandbox v=vvv init'
;; Run tests with: `make sandbox=sandbox quick`

(require 'buttercup)
(require 'org-roam-ql)

(describe "Test the s-exp query"
  (it "with the todo"
    (expect (org-roam-ql--check-if-valid-query '(todo "TODO")))
    :not :to-be nil)
  (it "with nested 'and and 'or"
    (expect (org-roam-ql--check-if-valid-query '(or (todo "done") (and (todo "todo") (scheduled "something"))))))
  (it "fails when unexpected term in query"
    (expect (org-roam-ql--check-if-valid-query '(or (todo "DONE") (and (todo "TODO") (what-is-this "something")))))))
