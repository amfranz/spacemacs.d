;; -*- lexical-binding: t -*-

(defconst my-yasnippet-packages '(helm-c-yasnippet
                                  yasnippet
                                  yasnippet-classic-snippets))

(defun my-yasnippet/post-init-helm-c-yasnippet ()
  ;; The message we disable here displays the key binding for the snippet. It is
  ;; pretty useless because most snippets do not have a key binding.
  (setq helm-yas-display-msg-after-complete nil)

  ;; This avoids a warning about overwriting `helm-c-yas-space-match-any-greedy'
  ;; by aliasing it. This is because Spacemacs is setting the value using a
  ;; deprecated name for the variable. Here we "migrate" the value to the new
  ;; neame of the variable to avoid the warning.
  (setq helm-yas-space-match-any-greedy helm-c-yas-space-match-any-greedy)
  (makunbound 'helm-c-yas-space-match-any-greedy)

  ;; Preserve the point when saving a snippet. Loading the snippet from the
  ;; buffer causes the point to jump which is unnecessary and distracting.
  (advice-add 'yas-maybe-load-snippet-buffer
              :around #'my-yasnippet//preserve-point)

  ;; The following patch makes it so that if a region is selected it will be
  ;; used for the snippet just like `yas-insert-snippet' would do.
  ;;
  ;; It is unfortunate to have to patch it this way - this fix should
  ;; probably the submitted back to the `helm-c-yasnippet' project.
  ;;
  (el-patch-feature helm-c-yasnippet)
  (with-eval-after-load 'helm-c-yasnippet
    (eval '(el-patch-defun helm-yas-get-cmp-context ()
             "Return list (initial-input point-start point-end)
like `yas--current-key'"
             (el-patch-wrap 3
               (if (use-region-p)
                   (cl-values "" (region-beginning) (region-end))
                 (let ((start (point))
                       (end (point))
                       (syntax "w_"))
                   (el-patch-splice 2 1
                     (condition-case nil
                         (save-excursion
                           (el-patch-remove
                             (when mark-active
                               (error "")))
                           (skip-syntax-backward syntax)
                           (setq start (point))
                           (cl-values (buffer-substring-no-properties start end) start end))
                       (error (cl-values "" (point) (point))))))))))))

(defun my-yasnippet/post-init-yasnippet ()
  ;; Load `yasnippet' in `text-mode', but not if `text-mode' is used for the
  ;; scratch buffer. This lets us initialize the scratch buffer with `text-mode'
  ;; while avoiding `yasnippet' to be loaded eagerly at startup.
  (add-hook 'text-mode-hook #'my-yasnippet//load-yasnippet-unless-scratch)

  ;; Avoids an error if `hippie-expand' is used before `yasnippet' is loaded.
  (autoload 'yas-hippie-try-expand "yasnippet"))

(defun my-yasnippet/init-yasnippet-classic-snippets ()
  (use-package yasnippet-classic-snippets
    :defer t))
