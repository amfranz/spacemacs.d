(defconst mfa-dtrt-indent-packages '(dtrt-indent))

(defun mfa-dtrt-indent/init-dtrt-indent ()
  (use-package dtrt-indent
    :config
    (progn
      (defun dtrt-indent--propagate-to-evil-shift-width ()
        "Propagate adjustment of indent level to evils shift width."
        (let ((indent-offset-variable (nth 2 (dtrt-indent--search-hook-mapping major-mode))))
          (when (local-variable-p indent-offset-variable)
            (set (make-local-variable 'evil-shift-width) (symbol-value indent-offset-variable)))))
      (advice-add 'dtrt-indent-try-set-offset :after
                  #'dtrt-indent--propagate-to-evil-shift-width)

      ;; Override for the function of the same name as defined by the package.
      ;; Only adjust indent for modes listed in dtrt-indent-hook-mapping-list.
      (defun dtrt-indent-find-file-hook ()
        "Try adjusting indentation offset when a file is loaded."
        (when (and dtrt-indent-mode
                   (not (eq 'default (car (dtrt-indent--search-hook-mapping major-mode)))))
          (dtrt-indent-try-set-offset)))

      (dtrt-indent-mode t))))
