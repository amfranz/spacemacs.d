(defconst my-highlight-symbols-packages '(highlight-escape-sequences highlight-numbers))

(defun my-highlight-symbols/init-highlight-escape-sequences ()
  (use-package highlight-escape-sequences
    :defer t
    :init
    (add-hook 'prog-mode-hook (lambda ()
                                (require 'highlight-escape-sequences)))
    :config
    (progn
      ;; Make face the same as builtin face
      (put 'font-lock-regexp-grouping-backslash 'face-alias 'font-lock-builtin-face)

      ;; Add extra modes
      (push `(coffee-mode . ,hes-js-escape-sequence-re) hes-mode-alist)
      (push `(enh-ruby-mode . ,hes-ruby-escape-sequence-keywords) hes-mode-alist)

      ;; Enable globally
      (hes-mode 1))))

(defun my-highlight-symbols/post-init-highlight-numbers ()
  (remove-hook 'prog-mode-hook #'highlight-numbers-mode))
