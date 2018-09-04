;; -*- lexical-binding: t -*-

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
