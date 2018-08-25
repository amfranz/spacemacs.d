(defconst mfa-mwim-packages '(mwim))

(defun mfa-mwim/post-init-mwim ()
  ;; Delay these key bindings to override the defaults
  (add-hook 'emacs-startup-hook
            (lambda ()
              (global-set-key (kbd "C-a") #'mwim-beginning)
              (global-set-key (kbd "C-e") #'mwim-end)
              (global-set-key (kbd "<home>") #'mwim-beginning)
              (global-set-key (kbd "<end>") #'mwim-end)
              (with-eval-after-load 'evil
                (define-key evil-normal-state-map (kbd "C-e") #'mwim-end)
                (define-key evil-motion-state-map (kbd "C-e") #'mwim-end)
                (define-key evil-visual-state-map (kbd "C-e") #'mwim-end)
                (evil-declare-motion 'mwim-beginning)
                (evil-declare-motion 'mwim-end)))))
