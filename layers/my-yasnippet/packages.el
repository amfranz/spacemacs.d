;; -*- lexical-binding: t -*-

(defconst my-yasnippet-packages '(helm-c-yasnippet yasnippet yasnippet-classic-snippets))

(defun my-yasnippet/post-init-helm-c-yasnippet ()
  ;; This message displays the key binding for the snippet, but the message is
  ;; pretty useless because most snippets do not have a key binding.
  (setq helm-yas-display-msg-after-complete nil)

  ;; Workaround for snippet expansion when in evil visual mode.
  ;; For details, see https://github.com/emacs-evil/evil/issues/254
  (defun my-yasnippet//leave-visual-state ()
    (when (evil-visual-state-p)
      (let ((p (point))
            (m (mark)))
        (evil-normal-state)
        (goto-char p)
        (set-mark m))))
  (add-hook 'yas-before-expand-snippet-hook
            #'my-yasnippet//leave-visual-state)

  ;; Enter insert mode automatically when moving the point to a snippet field.
  (defun my-yasnippet//enter-insert-state (&rest ignored)
    (unless undo-in-progress
      (when (evil-normal-state-p)
        (evil-insert-state))))
  (advice-add 'yas--move-to-field :after
              #'my-yasnippet//enter-insert-state)

  ;; Leave insert mode automatically when reaching the exit point of a snippet.
  (defun my-yasnippet//exit-insert-state (&rest ignored)
    (unless undo-in-progress ;; required?
      (when (evil-insert-state-p)
        ;; FIXME: breaks company completion
        ;; (evil-normal-state)
        )))
  (advice-add 'yas-exit-snippet :after
              #'my-yasnippet//exit-insert-state)

  ;; Preserve the point when saving a snippet. Loading the snippet from the
  ;; buffer causes the point to jump which is unnecessary and distracting.
  (defun my-yasnippet//preserve-point (orig-fun &rest args)
    (save-excursion
      (apply orig-fun args)))
  (advice-add 'yas-maybe-load-snippet-buffer
              :around #'my-yasnippet//preserve-point)

  ;; The following patch fixes these issues in `helm-c-yasnippet': (1) filter
  ;; the list of snippets by their conditions, and (2) if a region is selected
  ;; it will be used for the snippet like `yas-insert-snippet' would do.
  ;;
  ;; It is unfortunate to have to patch it this way - these fixes should
  ;; probably the submitted back to the helm-c-yasnippet project.
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
                       (error (cl-values "" (point) (point))))))))))
    (eval '(el-patch-defun helm-yas-build-cur-snippets-alist (&optional table)
             (let ((yas-choose-keys-first nil)
                   (yas-choose-tables-first nil)
                   (el-patch-remove (yas-buffer-local-condition 'always)))
               (let* ((result-alist '((candidates) (transformed) (template-key-alist)
                                      (template-file-alist) (template-expand-env-alist)))
                      (cur-tables
                       (if table
                           (list table)
                         (yas--get-snippet-tables)))
                      (hash-value-alist nil))
                 (let ((hashes (cl-loop for table in cur-tables
                                        collect (yas--table-hash table))))
                   (cl-loop for hash in hashes
                            do (maphash (lambda (k v)
                                          (let (a)
                                            (maphash (lambda (_n te)
                                                       (setq a (append (list (cons k te)) a)))
                                                     v)
                                            (setq hash-value-alist (append a hash-value-alist))))
                                        hash))
                   (cl-loop with transformed
                            with templates
                            with template-key-alist
                            with template-file-alist
                            with template-expand-env-alist
                            for lst in (el-patch-wrap 1 (yas--filter-templates-by-condition hash-value-alist))
                            for key = (car lst)
                            for template-struct = (cdr lst)
                            for name = (yas--template-name template-struct) ;`yas--template-name'
                            for template = (yas--template-content template-struct) ;`yas--template-content'
                            for file = (yas--template-load-file template-struct) ;`yas--template-content'
                            for expand-env = (yas--template-expand-env template-struct)
                            do (progn (push template templates)
                                      (push `(,name . ,template) transformed)
                                      (push `(,template . ,key) template-key-alist)
                                      (push `(,template . ,file) template-file-alist)
                                      (push `(,template . ,expand-env) template-expand-env-alist)
                                      )
                            finally (progn (push `(candidates . ,templates) result-alist)
                                           (push `(transformed . ,transformed) result-alist)
                                           (push `(template-file-alist . ,template-file-alist) result-alist)
                                           (push `(template-key-alist . ,template-key-alist) result-alist)
                                           (push `(template-expand-env-alist . ,template-expand-env-alist) result-alist)
                                           ))
                   result-alist)
                 ))))
    ))

(defun my-yasnippet/post-init-yasnippet()
  (add-hook 'text-mode-hook #'my-yasnippet//load-yasnippet-unless-scratch)
  (autoload 'yas-hippie-try-expand "yasnippet"))

(defun my-yasnippet/init-yasnippet-classic-snippets ()
  (use-package yasnippet-classic-snippets
    :defer t))
