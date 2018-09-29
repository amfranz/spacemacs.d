;; -*- lexical-binding: t -*-

(defconst mfa-sql-packages '(sql
                             (sqlformat :location (recipe
                                                   :fetcher git
                                                   :url "https://framagit.org/steckerhalter/sqlformat.el"))))

(defun mfa-sql/post-init-sql ()
  (with-eval-after-load 'sql
    (spacemacs/declare-prefix-for-mode 'sql-mode "m=" "format")
    (spacemacs/safe-set-leader-keys-for-major-mode 'sql-mode
      "=s" #'sqlformat)))

(defun mfa-sql/init-sqlformat ()
  (use-package sqlformat
    :defer t))
