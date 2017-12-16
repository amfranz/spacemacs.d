(defconst mfa-yasnippet-packages '(helm-c-yasnippet))

(defun mfa-yasnippet/post-init-helm-c-yasnippet ()
  (setq helm-yas-display-msg-after-complete nil)

  (el-patch-feature helm-c-yasnippet)
  (with-eval-after-load 'helm-c-yasnippet

    (eval '(el-patch-defun helm-yas-build-cur-snippets-alist (&optional table)
             (let ((yas-choose-keys-first nil)
                   (yas-choose-tables-first nil)
                   (yas-buffer-local-condition 'always))
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
                            for lst in hash-value-alist
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
                                                 (yas-expand-snippet (yas--template-content struct) nil nil (yas--template-expand-env struct)))
                                               (when helm-yas-display-msg-after-complete
                                                 (message "this snippet is bound to [ %s ]"
                                                          (helm-yas-get-key-by-template template helm-yas-cur-snippets-alist)))))
                      item))
                  (alist-get 'action helm-source-yasnippet)))

    ))
