(defconst mfa-make-mode-packages '(make-mode))

(defun mfa-make-mode/post-init-make-mode ()
  (with-eval-after-load 'make-mode
    ;; Make tabs visible in Makefiles.
    (add-hook 'makefile-mode-hook
              (lambda ()
                (require 'whitespace)
                (setq buffer-display-table (or buffer-display-table (make-display-table)))
                (aset buffer-display-table ?\t (vconcat (vector (make-glyph-code ?» 'whitespace-tab)) (make-vector (1- tab-width) ? ))))
              t)

    ;; Adjust Makefile indentation.
    (add-hook 'makefile-mode-hook (lambda ()
                                    (setq tab-width 4)))))
