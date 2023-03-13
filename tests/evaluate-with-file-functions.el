;;; This is copied from my post on profiling the different alternatives of `org-roam-with-file' trying to figure out the bottlenecks

(straight-use-package 'org)
(straight-use-package 'org-roam)

(defun run-elp (func sources)
  "Instrument org and FUNC and iterate on SOURCES with FUNC.
FUNC is a sumbol representing a function that takes one parameter.
SOURCES is a list of element that will be processed by FUNC"
  (elp-instrument-package "org")
  (elp-instrument-function func)
  (elp-reset-all)
  (mapcar func sources)
  (elp-results))

(defmacro with-plain-file (file keep-buf-p &rest body)
  "Same as `org-roam-with-file', but doesn't start `org-roam'."
  (declare (indent 2) (debug t))
  `(let* (new-buf
          (auto-mode-alist nil)
          (find-file-hook nil)
          (buf (or
                (and (not ,file)
                     (current-buffer)) ;If FILE is nil, use current buffer
                (find-buffer-visiting ,file) ; If FILE is already visited, find buffer
                (progn
                  (setq new-buf t)
                  (find-file-noselect ,file)))) ; Else, visit FILE and return buffer
          res)
     (with-current-buffer buf
       (setq res (progn ,@body))
       (unless (and new-buf (not ,keep-buf-p))
         (save-buffer)))
     (if (and new-buf (not ,keep-buf-p))
         (when (find-buffer-visiting ,file)
           (kill-buffer (find-buffer-visiting ,file))))
     res))

(defun test-org-load-files (func &optional restart)
  (let ((test-dir "~/temp/org-mode-test/")
        files)
    (message "Tests running")
    (when (and (file-exists-p test-dir) restart)
      (dolist (f (directory-files (file-truename test-dir))) (unless (member f '("." "..")) (delete-file f)))
      (delete-directory (file-truename test-dir) t))

    (if (or restart (not (file-exists-p test-dir)))
        (progn
          (make-directory (file-truename test-dir))
          ;; generating a bunch of file for testing
          (dolist (num (number-sequence 1 25 1))
            (let ((auto-mode-alist nil)
                  (find-file-hook nil)
                  (id (org-id-new))
                  (f (file-truename (format "~/temp/org-roam-test/test_%s.org" num))))
              (push f files)
              (with-current-buffer (find-file-noselect f)
                (erase-buffer)
                (insert (format "* This is the heading in file number %s
  :PROPERTIES:
  :ID:       %s
  :TEST_PROP_1: %s
  :TEST_PROP_2: id:%s
  :END:" num id num id))
                (save-buffer)
                (kill-buffer (find-buffer-visiting f))))))
      (progn
        (mapcar (lambda (f) (let ((f (find-buffer-visiting f)))
                              (em f)
                              (when f
                                (kill-buffer f))))
                (setq files (f-glob "*.org" test-dir)))))

    (run-elp func files)
    (with-current-buffer "*ELP Profiling Results*"
      (write-file (format "~/elp_results_%s" func (format-time-string "%Y-%m-%dT%H-%M-%S%-z"))))))

(defun --test-org-roam-with-file (f)
  (org-roam-with-file f t
    (goto-char 3)
    (point-marker)))

(defun --test-with-current-buffer (f)
  (with-current-buffer (find-file-noselect f)
    (goto-char 3)
    (point-marker)))

(defun --test-with-plain-file (f)
  (with-plain-file f t
    (goto-char 3)
    (point-marker)))

(setq org-roam-directory (file-truename "~/temp/org-mode-test/"))
(setq org-roam-node-display-template (concat "{title:*} &quot;</span> (<span class="hljs-name">propertize</span> <span class="hljs-string">&quot;{tags:10}" 'face 'org-tag)))
(org-roam-db-autosync-mode)

(with-eval-after-load 'org-roam
  ;; running twice to so that the first time around module loading won't effect times
  (dolist (func '(--test-org-roam-with-file
                  --test-with-current-buffer
                  --test-with-plain-file))
    (test-org-load-files func t))

  (dolist (func '(--test-org-roam-with-file
                  --test-with-current-buffer
                  --test-with-plain-file))
    (test-org-load-files func t)))
