;; -*- mode: eask; lexical-binding: t -*-

(package "org-roam-ql"
         "0.3pre"
         "Interface to query and view results from org-roam")

(website-url "https://github.com/ahmed-shariff/org-roam-ql")
(keywords )

(package-file "org-roam-ql.el")
(files "org-roam-ql.el" "org-roam-ql-ql.el")

(script "test" "echo \"Error: no test specified\" && exit 1")

(source "gnu")
(source "melpa")
(source 'melpa-stable)

(depends-on "emacs" "29")
(depends-on "magit-section")
(depends-on "transient")
(depends-on "dash")
(depends-on "org-roam")
(depends-on "s")
(depends-on "org-ql")

(development
 (depends-on "buttercup"))

(add-hook 'eask-before-command-hook
           (lambda ()
             (eval-after-load 'org-roam
               (progn
                 (setq org-roam-db-location (expand-file-name ".eask/tmp.db" (eask-working-directory)))
                 (message "Using roam db location: %s" org-roam-db-location)))))
