;; -*- lexical-binding: t -*-

(defun my-go//shorten-vendored-package-names (packages)
  (sort
   (delete-dups
    (mapcar
     (lambda (pkg)
       (car (last (split-string pkg "/vendor/"))))
     packages))
   #'string<))

(defun spacemacs/go-run-benchmark-current-function ()
  (interactive)
  (if (string-match "_test\\.go" buffer-file-name)
      (save-excursion
        (re-search-backward "^func[ ]+\\(?:([[:alnum:]]*?[ ]?[*]?[[:alnum:]]+)[ ]+\\)?\\(Benchmark[[:alnum:]_]+\\)(.*)")
        (spacemacs/go-run-tests (concat "-bench='" (match-string-no-properties 1) "$'")))
    (message "Must be in a _test.go file to run go-run-benchmark-current-function")))

(defun spacemacs/go-run-package-benchmarks ()
  (interactive)
  (spacemacs/go-run-tests "-bench=."))

(defun my-go//disable-eldoc ()
  (eldoc-mode -1))

(defun my--ad-save-match-data (orig-fun &rest args)
  (save-match-data
    (apply orig-fun args)))

(defun my-go--ffa-trailing-separator ()
  (setq-local fill-function-arguments-trailing-separator t))
