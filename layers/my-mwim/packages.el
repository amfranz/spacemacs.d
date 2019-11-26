;; -*- lexical-binding: t -*-

(defconst my-mwim-packages '(mwim))

(defun my-mwim/post-init-mwim ()
  (let ((move-beg-fun (lookup-key (current-global-map) (kbd "C-a")))
        (move-end-fun (lookup-key (current-global-map) (kbd "C-e"))))
    (global-set-key (kbd "<home>") move-beg-fun)
    (global-set-key (kbd "<end>") move-end-fun)
    (with-eval-after-load 'evil
      (define-key evil-motion-state-map (kbd "C-e") move-end-fun)
      (dolist (move-fun '(mwim
                          mwim-beginning
                          mwim-beginning-of-code
                          mwim-beginning-of-code-or-line
                          mwim-beginning-of-code-or-line-or-comment
                          mwim-beginning-of-comment
                          mwim-beginning-of-line
                          mwim-beginning-of-line-function
                          mwim-beginning-of-line-or-code
                          mwim-end
                          mwim-end-of-code
                          mwim-end-of-code-or-line
                          mwim-end-of-line
                          mwim-end-of-line-function
                          mwim-end-of-line-or-code))
        (evil-declare-motion move-fun)))))
