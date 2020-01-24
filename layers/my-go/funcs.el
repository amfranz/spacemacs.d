;; -*- lexical-binding: t -*-

(defun my-go/go-packages-gopkgs ()
  "Return a list of all Go packages, using `gopkgs'."
  (delete-consecutive-dups
   (sort
    (mapcar
     (lambda (package)
       (if (string-match "/vendor/" package)
           (substring package (match-end 0))
         package))
     (process-lines "gopkgs" "-workDir" "."))
    #'string<)))

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

(defun my-go//ffa-trailing-separator ()
  (setq-local fill-function-arguments-indent-after-fill t)
  (setq-local fill-function-arguments-trailing-separator t))

(defun my-go//reenable-go-build ()
  (setq flycheck-disabled-checkers (delq 'go-build flycheck-disabled-checkers)
        flycheck-checker 'go-build))

(defun my-go//configure-flycheck ()
  ;; Make flycheck less eager to lint. Invoking the linter after every keystroke
  ;; makes the editor sluggish and the laptop hot.
  (setq-local flycheck-check-syntax-automatically '(save mode-enabled)))
