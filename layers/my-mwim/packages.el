;; -*- lexical-binding: t -*-

(defconst my-mwim-packages '(mwim))

(defun my-mwim/post-init-mwim ()
  (global-set-key (kbd "C-a") #'mwim-beginning)
  (global-set-key (kbd "C-e") #'mwim-end)
  (global-set-key (kbd "<home>") #'mwim-beginning)
  (global-set-key (kbd "<end>") #'mwim-end)
  (with-eval-after-load 'evil
    (define-key evil-motion-state-map (kbd "C-e") #'mwim-end)
    (evil-declare-motion 'mwim-beginning)
    (evil-declare-motion 'mwim-end)))
