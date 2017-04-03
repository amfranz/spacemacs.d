(defconst mfa-evil-vimish-fold-packages '(evil-vimish-fold))

(defun mfa-evil-vimish-fold/init-evil-vimish-fold ()
  (use-package evil-vimish-fold
    :init
    (progn
      (when (configuration-layer/package-usedp 'avy)
        (define-key evil-normal-state-map "zv" #'vimish-fold-avy))

      (defun vimish-fold-delete-all-confirm ()
        (interactive)
        (and (y-or-n-p "Really delete all folds in this buffer?")
             (vimish-fold-delete-all)))

      (define-key evil-normal-state-map "zD" #'vimish-fold-delete-all-confirm))
    :config
    (progn
      (setq vimish-fold-dir (concat spacemacs-cache-directory "vimish-fold"))

      (spacemacs|hide-lighter evil-vimish-fold-mode)

      (defun vimish-fold-avy ()
        "Fold region of text between point and line selected with avy."
        (interactive)
        (let ((beg (point))
              (end (let ((vlm (bound-and-true-p visual-line-mode))
                         (avy-all-windows nil))
                     ;; (if (not vlm) (visual-line-mode))
                     (call-interactively #'avy-goto-line)
                     ;; (if (not vlm) (visual-line-mode -1))
                     (point))))
          (vimish-fold beg end)))

      ;; TODO fix this properly - this is merely an ugly workaround
      ;; (defun avy--with-visual-line-mode (orig-fun &rest args)
      ;;   "Enable visual-line-mode while using avy."
      ;;   (let ((nvlm (not (bound-and-true-p visual-line-mode))))
      ;;     (prog2
      ;;         (if nvlm (visual-line-mode t))
      ;;         (let ((error t))
      ;;           (unwind-protect
      ;;               (prog1 (apply orig-fun args)
      ;;                 (setq error nil))
      ;;             (when (and error nvlm)
      ;;               (visual-line-mode -1)))))))

      ;; (dolist (avyfn '(avy-goto-line
      ;;                  avy-goto-char
      ;;                  avy-goto-char-2
      ;;                  avy-goto-char-in-line
      ;;                  avy-isearch
      ;;                  avy-goto-word-0
      ;;                  avy-goto-word-1
      ;;                  avy-goto-subword-0
      ;;                  avy-goto-word-or-subword-1))
      ;;   (advice-add avyfn :around 'avy--with-visual-line-mode))

      (evil-vimish-fold-mode 1))))
