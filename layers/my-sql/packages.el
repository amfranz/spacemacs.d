;; -*- lexical-binding: t -*-

(defconst my-sql-packages '((sql :location built-in)
                            (sqlformat :location (recipe
                                                  :fetcher git
                                                  :url "https://framagit.org/steckerhalter/sqlformat.el"))))

(defun my-sql/post-init-sql ()
  (with-eval-after-load 'sql
    ;; My prompt format is configured to "user@host [schema]> " in ~/.my.cnf,
    ;; we need to give Emacs hints how to parse it.
    (sql-set-product-feature 'mysql :prompt-regexp "\\(^\\|\\)([_a-zA-Z]*@[_a-zA-Z]*) \\[(?[_a-zA-Z]*)?\\]> ")

    ;; The control character that signals the shell to beep (^G) confuses the
    ;; logic that tries to identify the prompt. Without this, after triggering a
    ;; beep (eg. entering a query with a syntax error), the next prompt will not
    ;; be detected and therefore invisible in the buffer.
    (setq sql-mysql-options '("--no-beep"))

    ;; Fix parsing of PostgreSQL prompt.
    (add-hook 'sql-login-hook #'my-sql//my-sql-login-hook)))

(defun my-sql/init-sqlformat ()
  (use-package sqlformat
    :defer t
    :init
    (with-eval-after-load 'sql
      ;; sqlformat is an alternative to sqlfmt (which Spacemacs binds to "=").
      (spacemacs/safe-set-leader-keys-for-major-mode 'sql-mode "f" #'sqlformat))))
