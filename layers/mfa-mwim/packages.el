(defconst mfa-mwim-packages '(mwim))

(defun mfa-mwim/init-mwim ()
  (use-package mwim
    :defer t
    :init
    (progn
      (global-set-key (kbd "C-a") #'mwim-beginning)
      (global-set-key (kbd "C-e") #'mwim-end)
      (global-set-key (kbd "<home>") #'mwim-beginning)
      (global-set-key (kbd "<end>") #'mwim-end)
      (with-eval-after-load 'evil
        (define-key evil-normal-state-map (kbd "C-e") #'mwim-end)
        (define-key evil-motion-state-map (kbd "C-e") #'mwim-end)
        (define-key evil-insert-state-map (kbd "C-e") #'mwim-end)
        (define-key evil-visual-state-map (kbd "C-e") #'mwim-end)
        (evil-declare-motion 'mwim-beginning)
        (evil-declare-motion 'mwim-end)))))
