;;; org-roam-ql.el --- Tests for org-roam-ql -*- lexical-binding: t -*-

;; Setup sandbox directory (named sandbox) with: `make sandbox=sandbox v=vvv init'
;; Run tests with: `make sandbox=sandbox quick`

(require 'buttercup)
(require 'org-roam-ql)
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
      (expect (org-roam-ql--check-if-valid-query '(or (todo "done") (and (todo "todo") (scheduled "something"))))))
    (it "fails when unexpected term in query"
      (expect (org-roam-ql--check-if-valid-query '(or (todo "DONE") (and (todo "TODO") (what-is-this "something")))))))

  (describe "Test org-roam-ql-nodes"
    (it "with list of nodes"
      (let ((nodes (car (org-roam-node-list))))
        (expect (org-roam-ql-nodes nodes) :to-equal nodes)))
    (it "with sql-db query"
      (expect (org-roam-ql-nodes '([(like title "%Node a%")])) :to-equal (--filter (s-match "Node a" (org-roam-node-title it)) (org-roam-node-list))))
    (it "with roam-predicate"
      (expect (org-roam-ql-nodes '(todo "DONE")) :to-equal (--filter (s-match "DONE" (org-roam-node-todo it)) (org-roam-node-list))))
    (it "with function"
      (expect (org-roam-ql-nodes (lambda (car (org-roam-node-list)))) :to-equal (car (org-roam-node-list)))))

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
