;; -*- lexical-binding: t -*-

(defconst my-ruby-packages '(rbenv rubocop ruby-mode))

(defun my-ruby/post-init-rbenv ()
  ;; It is unnecessary for rbenv to show the Ruby version in the modeline.
  (setq rbenv-show-active-ruby-in-modeline nil))

(defun my-ruby/post-init-rubocop ()
  (spacemacs|hide-lighter rubocop-mode))

(defun my-ruby/post-init-ruby-mode ()
  ;; Ruby mode customizations
  (with-eval-after-load 'ruby-mode
    (add-hook 'ruby-mode-hook (lambda ()
                                (setq evil-shift-width ruby-indent-level))))

  ;; Additional file mode associations.
  (add-to-list 'auto-mode-alist '("\\.rake\\'" . ruby-mode))
  (add-to-list 'auto-mode-alist '("\\.gemspec\\'" . ruby-mode))
  (add-to-list 'auto-mode-alist '("\\.ru\\'" . ruby-mode))
  (add-to-list 'auto-mode-alist '("Capfile\\'" . ruby-mode))
  (add-to-list 'auto-mode-alist '("Gemfile\\'" . ruby-mode))
  (add-to-list 'auto-mode-alist '("Rakefile\\'" . ruby-mode))
  (add-to-list 'auto-mode-alist '("Vagrantfile\\'" . ruby-mode))

  ;; Additional interpreter associations.
  (add-to-list 'interpreter-mode-alist '("rake" . ruby-mode)))
