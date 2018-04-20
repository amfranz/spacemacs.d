(defconst mfa-yasnippet-packages '(helm-c-yasnippet yasnippet yasnippet-snippets))

(defun mfa-yasnippet/post-init-helm-c-yasnippet ()
  ;; This message displays the key binding for the snippet, but the message is
  ;; pretty useless because most snippets do not have a key binding.
  (setq helm-yas-display-msg-after-complete nil)

  ;; Workaround for snippet expansion when in evil visual mode.
  ;; For details, see https://github.com/emacs-evil/evil/issues/254
  (defun mfa-yasnippet//leave-visual-state ()
    (when (evil-visual-state-p)
      (let ((p (point))
            (m (mark)))
        (evil-normal-state)
        (goto-char p)
        (set-mark m))))
  (add-hook 'yas-before-expand-snippet-hook
            #'mfa-yasnippet//leave-visual-state)

  ;; Enter insert mode automatically when moving the point to a snippet field.
  (defun mfa-yasnippet//enter-insert-state (&rest ignored)
    (unless undo-in-progress
      (when (evil-normal-state-p)
        (evil-insert-state))))
  (advice-add 'yas--move-to-field :after
              #'mfa-yasnippet//enter-insert-state)

  ;; Leave insert mode automatically when reaching the exit point of a snippet.
  (defun mfa-yasnippet//exit-insert-state (&rest ignored)
    (unless undo-in-progress ;; required?
      (when (evil-insert-state-p)
        (evil-normal-state))))
  (advice-add 'yas-exit-snippet :after
              #'mfa-yasnippet//exit-insert-state)

  ;; The following patch fixes these issues in helm-c-yasnippet: (1) evaluate
  ;; the `expand-env` attribute in snippets, (2) filter the list of snippets by
  ;; their conditions, and (3) if a region is selected it will be used for the
  ;; snippet like yas-snippet-insert would do.
  ;;
  ;; It is unfortunate to have to patch it this way - this fix should probably
  ;; the submitted back to the helm-c-yasnippet project.
  (el-patch-feature helm-c-yasnippet)
  (with-eval-after-load 'helm-c-yasnippet
    (eval '(el-patch-defun helm-yas-get-cmp-context ()
             "Return list (initial-input point-start point-end)
like `yas--current-key'"
             (el-patch-swap
               (let ((start (point))
                     (end (point))
                     (syntax "w_"))
                 (condition-case nil
                     (save-excursion
                       (when mark-active
                         (error ""))
                       (skip-syntax-backward syntax)
                       (setq start (point))
                       (cl-values (buffer-substring-no-properties start end) start end))
                   (error (cl-values "" (point) (point)))))
               (if (use-region-p)
                   (cl-values "" (region-beginning) (region-end))
                 (let ((start (point))
                       (end (point))
                       (syntax "w_"))
                   (save-excursion
                     (skip-syntax-backward syntax)
                     (setq start (point))
                     (cl-values (buffer-substring-no-properties start end) start end)))))))
    (eval '(el-patch-defun helm-yas-build-cur-snippets-alist (&optional table)
             (let ((yas-choose-keys-first nil)
                   (yas-choose-tables-first nil)
                   (el-patch-remove (yas-buffer-local-condition 'always)))
               (let* ((result-alist '((candidates) (transformed) (template-key-alist) (template-file-alist) (el-patch-add (template-struct-alist))))
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
                            (el-patch-add with template-struct-alist)
                            for lst in (el-patch-swap hash-value-alist (yas--filter-templates-by-condition hash-value-alist))
                            for key = (car lst)
                            for template-struct = (cdr lst)
                            for name = (yas--template-name template-struct) ;`yas--template-name'
                            for template = (yas--template-content template-struct) ;`yas--template-content'
                            for file = (yas--template-load-file template-struct) ;`yas--template-content'
                            do (progn (push template templates)
                                      (push `(,name . ,template) transformed)
                                      (push `(,template . ,key) template-key-alist)
                                      (push `(,template . ,file) template-file-alist)
                                      (el-patch-add (push `(,template . ,template-struct) template-struct-alist))
                                      )
                            finally (progn (push `(candidates . ,templates) result-alist)
                                           (push `(transformed . ,transformed) result-alist)
                                           (push `(template-file-alist . ,template-file-alist) result-alist)
                                           (el-patch-add (push `(template-struct-alist . ,template-struct-alist) result-alist))
                                           (push `(template-key-alist . ,template-key-alist) result-alist)))
                   result-alist)
                 ))))
    (defun helm-yas-get-struct-by-template (template)
      (assoc-default template (assoc-default 'template-struct-alist helm-yas-cur-snippets-alist)))
    (setf (alist-get 'action helm-source-yasnippet)
          (mapcar (lambda (item)
                    (if (string-equal (car item) "Insert snippet")
                        '("Insert snippet" . (lambda (template)
                                               (let ((struct (helm-yas-get-struct-by-template template)))
                                                 (yas-expand-snippet (yas--template-content struct) helm-yas-point-start helm-yas-point-end (yas--template-expand-env struct)))
                                               (when helm-yas-display-msg-after-complete
                                                 (message "this snippet is bound to [ %s ]"
                                                          (helm-yas-get-key-by-template template helm-yas-cur-snippets-alist)))))
                      item))
                  (alist-get 'action helm-source-yasnippet)))))

(defun mfa-yasnippet/post-init-yasnippet()
  (setq yas-snippet-dirs (delete 'yas-installed-snippets-dir yas-snippet-dirs))
  (autoload 'yas-hippie-try-expand "yasnippet"))

(defun mfa-yasnippet/init-yasnippet-snippets ()
  (use-package yasnippet-snippets
    :defer t))
