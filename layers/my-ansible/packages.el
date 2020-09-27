;; -*- lexical-binding: t -*-

(defconst my-ansible-packages '(ansible
                                ansible-doc
                                company
                                company-ansible
                                expand-region
                                flycheck
                                flycheck-yamllint
                                molecule
                                yaml-mode))

(defun my-ansible/post-init-ansible ()
  ;; Customize the YAML files in which Ansible mode will be enabled. The value
  ;; of this regular expression is based on the default value provided by
  ;; Spacemacs. The differences are:
  ;;   1) use `rx' to make the regular expression more readable
  ;;   2) avoid capturing groups since they are not going to be needed
  ;;   3) anchor the end of the regular expression
  ;;   4) the addition of the "plays" directory
  (setq spacemacs--ansible-filename-re
        (rx "/"
            (or "main"
                "site"
                "encrypted"
                (and (or "roles"
                         "tasks"
                         "handlers"
                         "vars"
                         "defaults"
                         "meta"
                         "group_vars"
                         "host_vars"
                         "plays")
                     "/" (one-or-more not-newline)))
            ".y" (optional "a") "ml"
            string-end))

  ;; Ugly ugly hack that addresses https://github.com/k1LoW/emacs-ansible/issues/5
  (let* ((pkg-dir (package-desc-dir (cadr (assq 'ansible package-alist))))
         (txt-dir (concat pkg-dir "/snippets/text-mode"))
         (yml-dir (concat pkg-dir "/snippets/yaml-mode")))
    (when (file-directory-p txt-dir)
      (when (file-directory-p yml-dir)
        (delete-directory yml-dir t))
      (rename-file txt-dir yml-dir)))

  ;; Automatically decrypt files containing encrypted Ansible Vault content when
  ;; they are opened, and automatically encrypt them when they are saved.
  (add-hook 'yaml-mode-local-vars-hook
            #'my-ansible//auto-decrypt-encrypt-vault)
  (advice-add 'ansible-vault :filter-args
              #'my-ansible//encrypt-with-default-vault-id)
  (advice-add 'ansible-encrypt-buffer :before
              #'my-ansible//save-coord)
  (advice-add 'ansible-decrypt-buffer :after
              #'my-ansible//restore-coord)

  ;; Make imenu useful by teaching it parsing rules to extract the names of
  ;; top level variables and tasks in Ansible playbooks and roles.
  (add-hook 'ansible-hook
            #'my-ansible//update-imenu-expression)

  ;; Register jump handlers for Ansible role templates, files and variables.
  (add-to-list 'spacemacs-jump-handlers-yaml-mode #'my-ansible/jump-to-template)
  (add-to-list 'spacemacs-jump-handlers-yaml-mode #'my-ansible/jump-to-variable)

  (el-patch-feature ansible)
  (with-eval-after-load 'ansible
    (eval
     '(el-patch-define-minor-mode ansible
        "Ansible minor mode."
        :lighter " Ansible"
        :group 'ansible
        (if ansible
            (progn
              ;; Avoid adding to `minor-mode-map-alist' when the entry is
              ;; already present.
              (el-patch-swap
                (setq minor-mode-map-alist
                      (cons (cons 'ansible ansible-key-map)
                            minor-mode-map-alist))
                (add-to-list 'minor-mode-map-alist (cons 'ansible ansible-key-map)))
              (ansible-dict-initialize)
              (ansible-remove-font-lock)
              (ansible-add-font-lock)
              ;; Make integration into yasnippet behave consistent regardless
              ;; whether or not `yasnippet' has been loaded yet. (part 1)
              (el-patch-remove
                (when (featurep 'yasnippet)
                  (add-to-list 'yas-snippet-dirs ansible-snip-dir t)
                  (yas-load-directory ansible-snip-dir)))
              ;; The snippets have been moved to `yaml-mode', there is no need
              ;; to unload them any more. (part 1)
              (el-patch-remove
                (add-hook 'kill-buffer-hook #'ansible-maybe-unload-snippets nil t))
              (run-hooks 'ansible-hook))
          (ansible-remove-font-lock)
          ;; The snippets have been moved to `yaml-mode', there is no need to
          ;; unload them any more. (part 2)
          (el-patch-remove
            (ansible-maybe-unload-snippets 0))))
     t)

    ;; Make integration into yasnippet behave consistent regardless
    ;; whether or not `yasnippet' has been loaded yet. (part 2)
    (with-eval-after-load 'yasnippet
      (add-to-list 'yas-snippet-dirs ansible-snip-dir t))
    (when (bound-and-true-p yas-global-mode)
      (yas-load-directory ansible-snip-dir))

    ;; Make the modeline less verbose.
    (spacemacs|hide-lighter ansible)

    ;; Additional keybindings for Ansible playbooks and roles.
    (spacemacs/set-leader-keys-for-minor-mode 'ansible
      "rd" #'my-ansible/decrypt-region
      "re" #'my-ansible/encrypt-region
      "u" #'my-ansible/upgrade-syntax)

    ;; Syntax highlight Jinja code blocks in Ansible playbooks and roles. This
    ;; is based on the existing rules to highlight Jinja expressions.
    (add-to-list 'ansible-playbook-font-lock
                 '("\\({%\\)\\(.*?\\)\\(%}\\)"
                   (1 font-lock-builtin-face t)
                   (2 font-lock-function-name-face t)
                   (3 font-lock-builtin-face t)))))

(defun my-ansible/post-init-ansible-doc ()
  (with-eval-after-load 'ansible-doc
    (evilified-state-evilify-map ansible-doc-module-mode-map
      :mode ansible-doc-module-mode)))

(defun my-ansible/post-init-company ()
  (with-eval-after-load 'company-dabbrev-code
    (add-to-list 'company-dabbrev-code-modes 'yaml-mode)))

(defun my-ansible/post-init-company-ansible ()
  (remove-hook 'yaml-mode-hook
               'spacemacs/ansible-company-maybe-enable)
  (add-hook 'yaml-mode-hook
            'my-ansible//ansible-company-maybe-enable t))

(defun my-ansible/post-init-expand-region ()
  (with-eval-after-load 'expand-region
    (er/enable-mode-expansions 'yaml-mode #'er/add-yaml-mode-expansions)))

(defun my-ansible/post-init-flycheck ()
  (add-hook 'yaml-mode-hook #'my-ansible//flycheck-setup))

(defun my-ansible/init-flycheck-yamllint ()
  (use-package flycheck-yamllint
    :defer t))

(defun my-yaml--indent ()
  (when (looking-at "\\( *\\)[^ #\n]")
    (- (match-end 1) (match-beginning 1))))

(defun my-yaml--any-indent-backwards ()
  (let (indent)
    (while (not (or indent (bobp)))
      (forward-line -1)
      (setq indent (my-yaml--indent)))
    indent))

(defun my-yaml--lesser-indent-backwards (guide)
  (let ((indent (my-yaml--any-indent-backwards)))
    (while (and indent (>= indent guide))
      (setq indent (my-yaml--any-indent-backwards)))
    indent))

;; TODO: handle comments as well as other block literal types
(defun my-yaml--looking-at-block-start-p ()
  (seq-some #'looking-at-p '(" *[a-zA-Z0-9_\\-\\.]+: |"
                             " *- *\\(?:[a-zA-Z0-9_\\-\\.]+:\\)? |")))

(defun my-yaml--block-start ()
  (let (block-start block-indent)
    (goto-char (point-at-bol))
    (let ((indent (or (my-yaml--indent)
                      (my-yaml--any-indent-backwards))))
      (while indent
        (when (my-yaml--looking-at-block-start-p)
          (setq block-start (point)
                block-indent indent))
        (setq indent (my-yaml--lesser-indent-backwards indent))))
    (when block-start
      (cons block-start block-indent))))

(defun my-yaml--block-end (guide)
  (let* ((block-end (point))
         (indent (my-yaml--any-indent-forwards (point-max))))
    (while (and indent (> indent guide))
      (setq block-end (point))
      (setq indent (my-yaml--any-indent-forwards (point-max))))
    (goto-char block-end)
    (point-at-eol)))

(defun my-yaml--surrounding-block ()
  (when-let* ((start (my-yaml--block-start)))
    (goto-char (car start))
    (cons (car start) (my-yaml--block-end (cdr start)))))

(defun my-yaml--any-indent-forwards (bound)
  (let (indent)
    (while (not (or indent (>= (point) bound)))
      (forward-line 1)
      (setq indent (my-yaml--indent)))
    indent))

(defun my-yaml--line-end-position-at (pos)
  (let ((saved-point (point))
        end-point)
    (goto-char pos)
    (goto-char (line-end-position))
    (setq end-point (point))
    (goto-char saved-point)
    end-point))

(defun my-yaml--font-lock-block-literals (bound)
  ;; (let ((block-bounds (save-excursion (my-yaml--surrounding-block))))
  ;;   (when block-bounds
  ;;     (when (< (car block-bounds) (point)))
  ;;     ))
  (message "called for %s until %s" (point) bound)
  (let (block-start block-last)
    (while (not (or block-last (>= (point) bound)))
      (if (not (my-yaml--looking-at-block-start-p))
          (forward-line 1)
        (let* ((guide (my-yaml--indent))
               (indent (my-yaml--any-indent-forwards bound)))
          (setq block-start (point))
          (while (and indent (> indent guide))
            (setq block-last (point))
            (setq indent (my-yaml--any-indent-forwards bound))))))
    (when block-last
      (let ((block-end (min bound (my-yaml--line-end-position-at block-last))))
        (message "returning %s %s with point at %d" block-start block-end (point))
        (put-text-property block-start block-end 'yaml-block-literal t)
        (put-text-property block-start block-end 'font-lock-multiline t)
        (set-match-data (list block-start block-end)))
      t)))

(defun my-ansible//font-lock-extend-region ()
  (eval-when-compile
    (defvar font-lock-beg)
    (defvar font-lock-end))
  (let (altered)
    (save-excursion
      (goto-char font-lock-beg)
      (when-let* ((block-bounds (my-yaml--surrounding-block)))
        (unless (= font-lock-beg (car block-bounds))
          (message "altered font-lock-beg to %d" (car block-bounds))
          (setq font-lock-beg (car block-bounds)
                altered t)))
      (goto-char font-lock-end)
      (when-let* ((block-bounds (my-yaml--surrounding-block)))
        (unless (= font-lock-end (cdr block-bounds))
          (message "altered font-lock-end to %d" (cdr block-bounds))
          (setq font-lock-end (cdr block-bounds)
                altered t))))
    altered)
  ;; (save-excursion
  ;;   (let ((needle1 " *[a-zA-Z0-9_\\-\\.]+: |")
  ;;         (needle2 " *- *\\(?:[a-zA-Z0-9_\\-\\.]+:\\)? |")
  ;;         (orig-point (point))
  ;;         block-ind)
  ;;     (beginning-of-line)
  ;;     (looking-at " *")
  ;;     (let ((indent (- (match-end 0)
  ;;                      (match-beginning 0))))
  ;;       (while (> indent 0)
  ;;         (let ((line-indent indent))
  ;;           (while (>= line-indent indent)
  ;;             (forward-line -1)
  ;;             (looking-at " *")
  ;;             (setq line-indent (- (match-end 0)
  ;;                                  (match-beginning 0))))
  ;;           (when (or (looking-at-p needle1)
  ;;                     (looking-at-p needle2))
  ;;             (setq font-lock-beg (point)
  ;;                   block-ind line-indent))
  ;;           (setq indent line-indent))))
  ;;     (when block-ind
  ;;       (goto-char orig-point)
  ;;       (let ((line-indent block-ind))
  ;;         (while (>= line-indent block-ind)
  ;;           (forward-line 1)
  ;;           (looking-at " *")
  ;;           (setq line-indent (- (match-end 0)
  ;;                                (match-beginning 0)))))
  ;;       (setq font-lock-end (point)))))
  )

;; (defun my-ansible/test-font-lock-extend-region ()
;;   (interactive)
;;   (when (my-ansible//font-lock-extend-region)
;;     (set-mark font-lock-beg)
;;     (goto-char font-lock-end)
;;     (backward-char)
;;     (activate-mark)))

(defun my-ansible/test-font-lock-extend-region ()
  (interactive)
  (when (yaml-font-lock-block-literals (point-max))
    (message "%s" (match-data)))
  ;; (if-let* ((bounds (my-yaml--surrounding-block)))
  ;;     (progn
  ;;       (set-mark (car bounds))
  ;;       (goto-char (cdr bounds))
  ;;       (activate-mark))
  ;;   (message "nothing found"))
  )

;; (advice-add 'yaml-font-lock-block-literals :override #'my-yaml-font-lock-block-literals)
;; (advice-remove 'yaml-font-lock-block-literals 'my-yaml-font-lock-block-literals)

(defun my-ansible//font-lock-setup ()
  ;; Preserve highlight of multiline Jinja2 blocks.
  (setq font-lock-multiline t)
  ;; (push #'my-ansible//font-lock-extend-region font-lock-extend-region-functions)
  )

(defun my-ansible/init-molecule ()
  (use-package molecule
    :defer t
    :init
    (progn
      (with-eval-after-load 'tramp
        (add-to-list 'tramp-methods
                     `("molecule"
                       (tramp-login-program ,(concat dotspacemacs-directory "bin/molecule-tramp"))
                       (tramp-login-args (("%u") ("%h")))
                       (tramp-remote-shell "/bin/sh")
                       (tramp-remote-shell-args ("-c")))))
      (with-eval-after-load 'recentf
        (add-to-list 'recentf-exclude "\\`/molecule:")))))

;; single quoted strings:
;; - 'this \ backslash also does not need to be escaped'
;; - 'just like the " double quote'
;; - 'to express one single quote, use '' two of them'
(defun yaml-syntax-skip-single-quoted-string (end)
  (while (and (re-search-forward "'" end 'move-to-end)
              (char-equal ?' (char-after))
              (> end (point))
              (or (forward-char) t))))

;; double quoted:
;; - "here we can use predefined escape sequences like \t \n \b"
;; - "or generic escape sequences \x0b \u0041 \U00000041"
;; - "the double quote \" needs to be escaped"
;; - "just like the \\ backslash"
;; - "the single quote ' and other characters must not be escaped"
(defun yaml-syntax-skip-double-quoted-string (end)
  (while (and (re-search-forward "[\\\\\"]" end 'move-to-end)
              (char-equal ?\\ (char-before))
              (> end (point))
              (or (forward-char) t))))

(defun my-ansible/post-init-yaml-mode ()
  (add-to-list 'auto-mode-alist '("\\.yamllint\\'" . yaml-mode))

  ;; This replaces the use of `forward-sexp' with equivalent functionality.
  ;; `forward-sexp' uses syntax properties set by `syntax-propertize-function',
  ;; it elicits inconsistent behavior when used within. Due to implementation
  ;; details, it makes syntax highlighting behave differently depending on where
  ;; the point is. The tell-tale sign is that the `font-lock-string-face'
  ;; sometimes spans multiple lines past the end of the actual string.
  (el-patch-feature yaml-mode)
  (with-eval-after-load 'yaml-mode
    (eval
     '(el-patch-defun yaml-mode-syntax-propertize-function (beg end)
        "Override buffer's syntax table for special syntactic constructs."
        ;; Unhighlight foo#bar tokens between BEG and END.
        (save-excursion
          (goto-char beg)
          (while (search-forward "#" end t)
            (save-excursion
              (forward-char -1)
              ;; both ^# and [ \t]# are comments
              (when (and (not (bolp))
                         (not (memq (preceding-char) '(?\s ?\t))))
                (put-text-property (point) (1+ (point))
                                   'syntax-table (string-to-syntax "_"))))))

        (save-excursion
          (goto-char beg)
          (while (and
                  (> end (point))
                  (re-search-forward "['\"]" end t))
            (when (get-text-property (point) 'yaml-block-literal)
              (put-text-property (1- (point)) (point)
                                 'syntax-table (string-to-syntax "w")))
            (let* ((pt (point))
                   (sps (save-excursion (syntax-ppss (1- pt)))))
              (when (not (nth 8 sps))
                (cond
                 ((and (char-equal ?' (char-before (1- pt)))
                       (char-equal ?' (char-before pt)))
                  (put-text-property (- pt 2) pt
                                     'syntax-table (string-to-syntax "w"))
                  ;; Workaround for https://debbugs.gnu.org/41195.
                  (let ((syntax-propertize--done syntax-propertize--done))
                    ;; Carefully invalidate the last cached ppss.
                    (syntax-ppss-flush-cache (- pt 2))))
                 ;; If quote is detected as a syntactic string start but appeared
                 ;; after a non-whitespace character, then mark it as syntactic word.
                 ((and (char-before (1- pt))
                       (char-equal ?w (char-syntax (char-before (1- pt)))))
                  (put-text-property (1- pt) pt
                                     'syntax-table (string-to-syntax "w")))
                 (el-patch-swap
                   (t
                    ;; We're right after a quote that opens a string literal.
                    ;; Skip over it (big speedup for long JSON strings).
                    (goto-char (1- pt))
                    (condition-case nil
                        (forward-sexp)
                      (scan-error
                       (goto-char end))))
                   (t
                    ;; We're right after a quote that opens a string literal.
                    ;; Skip over it (big speedup for long JSON strings).
                    (if (char-equal ?' (char-before))
                        (yaml-syntax-skip-single-quoted-string end)
                      (yaml-syntax-skip-double-quoted-string end))))))))))))

  ;; Attempt to fix syntax highlighting of multi-line Jinja expressions.
  (add-hook 'yaml-mode-hook #'my-ansible//font-lock-setup)

  ;; Fontify URLs in Ansible buffers and make them interactive.
  (add-hook 'yaml-mode-hook #'goto-address-prog-mode)

  (with-eval-after-load 'yaml-mode
    (spacemacs/declare-prefix-for-mode 'yaml-mode "mr" "region")
    (spacemacs/set-leader-keys-for-major-mode 'yaml-mode
      "w" #'my-ansible/test-font-lock-extend-region
      "t" #'describe-text-properties)

    ;; Fixes auto-completion of words containing "_" or "-" by company. Without
    ;; this, entering "_" or "-" of a completion candidate would close the
    ;; company popup.
    (modify-syntax-entry ?_ "w" yaml-mode-syntax-table)
    (modify-syntax-entry ?- "w" yaml-mode-syntax-table)

    ;; This fixes an indentation oddity. For example:
    ;;
    ;;    - name: some description
    ;;        | <- suggested indentation is a level too far
    ;;
    ;; This happens because the subexpression [^#]* is allowed to extend the
    ;; match across newlines which is unintended as the expression is only meant
    ;; to match against a single line. The indentation oddity occurs when the
    ;; expression matches with the start of a nested map farther down the
    ;; buffer.
    ;;
    ;; This alters the original expression by changing the subexpression [^#]*
    ;; to [^#\n]*.
    ;;
    (setq yaml-nested-map-re
          (concat "[^#\n]*: *\\(?:&.*\\|{ *\\|" yaml-tag-re " *\\)?$"))))
