;; -*- lexical-binding: t -*-

(defconst my-mwim-packages '(mwim))

(defun my-mwim/post-init-mwim ()
  (let* ((gmap (current-global-map))
         (move-beg-fun (lookup-key gmap (kbd "C-a")))
         (move-end-fun (lookup-key gmap (kbd "C-e"))))
    (define-key gmap (kbd "<home>") move-beg-fun)
    (define-key gmap (kbd "<end>") move-end-fun))
  (with-eval-after-load 'evil-maps
    (define-key evil-motion-state-map (kbd "C-e") nil))
  (with-eval-after-load 'evil
    (mapc #'evil-declare-motion
          '(mwim
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
            mwim-end-of-line-or-code))))
