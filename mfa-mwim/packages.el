(defconst mfa-mwim-packages '(mwim))

(defun mfa-mwim/init-mwim ()
  (use-package mwim
    :defer t
    :init
    (progn
      (global-set-key (kbd "C-a") #'mwim-beginning-of-code-or-line)
      (global-set-key (kbd "C-e") #'mwim-end-of-code-or-line)
      (global-set-key (kbd "<home>") #'mwim-beginning-of-line-or-code)
      (global-set-key (kbd "<end>") #'mwim-end-of-line-or-code)
      (with-eval-after-load 'evil
        (define-key evil-normal-state-map (kbd "C-e") #'mwim-end-of-line-or-code)
        (define-key evil-motion-state-map (kbd "C-e") #'mwim-end-of-line-or-code)
        (define-key evil-insert-state-map (kbd "C-e") #'mwim-end-of-line-or-code)
        (define-key evil-visual-state-map (kbd "C-e") #'mwim-end-of-line-or-code)))))
