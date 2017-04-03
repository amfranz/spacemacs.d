(defconst mfa-sqlformat-packages '((sqlformat :location (recipe
                                                         :fetcher github
                                                         :repo "steckerhalter/sqlformat.el"))))

(defun mfa-sqlformat/init-sqlformat ()
  (use-package sqlformat
    :defer t
    :init
    (progn
      (autoload 'sqlformat "sqlformat" "Use sqlformat to reformat a query in an SQL buffer." t nil)
      (spacemacs/set-leader-keys-for-major-mode 'sql-mode "f" #'sqlformat))))
