;;; org-roam-ql.el --- Tests for org-roam-ql -*- lexical-binding: t -*-

;; Setup sandbox directory (named sandbox) with: `make sandbox=sandbox v=vvv init'
;; Run tests with: `make sandbox=sandbox quick`

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

  (describe "Test org-roam-ql--get-queries"
    (it "without any ql-queries"
      (expect (org-roam-ql--get-queries '(or (todo "TODO") (tags "tag1" "tag2") (and (title "a") (tags "tag3"))))
              :to-equal nil))
    (it "with multiple nested queries"
      (expect (org-roam-ql--get-queries '(or (org-roam-query (todo "TODO")) (and (org-roam-query (tags "tag1" "tag2")) (scheduled "something"))))
              :to-equal '((org-roam-query (todo "TODO")) (org-roam-query (tags "tag1" "tag2"))))))

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
                                #'string>))))
    (describe "with function"
      (it "returning list of nodes"
        (expect (org-roam-ql-nodes (lambda () (list (car (org-roam-node-list))))) :to-equal (list (car (org-roam-node-list)))))
      (it "not returning list of nodes"
        (expect (org-roam-ql-nodes (lambda () 'somthing-else)) :to-throw 'user-error))
      (it "returning nil"
        (expect (org-roam-ql-nodes (lambda () nil)) :to-equal nil)))
    (describe "with buffers"
      (let* ((nodes (--filter (s-match "test2.org" (org-roam-node-file it)) (org-roam-node-list)))
             (buffer-name "test-buffer"))
        (org-roam-ql--render-buffer (list (org-roam-ql--nodes-section nodes)) "test buffer" buffer-name nodes)
        (it "as a string"
          (expect (--map #'org-roam-node-id (org-roam-ql-nodes buffer-name)) :to-equal (--map #'org-roam-node-id nodes)))
        (it "as a predicate"
          (expect (--map #'org-roam-node-id (org-roam-ql-nodes `(in-buffer ,buffer-name))) :to-equal (--map #'org-roam-node-id nodes)))))
    (describe "with invalid inputs"
      (it "string"
        (expect (org-roam-ql-nodes "a random string") :to-throw 'user-error))))

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
