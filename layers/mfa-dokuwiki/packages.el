(defconst mfa-dokuwiki-packages '(dokuwiki dokuwiki-mode))

(defun mfa-dokuwiki/init-dokuwiki ()
  (use-package dokuwiki
    :commands (dokuwiki-get-wiki-title
               dokuwiki-list-pages
               dokuwiki-open-page
               dokuwiki-save-page)
    :init
    (progn
      (dolist (command '(dokuwiki-get-wiki-title
                         dokuwiki-list-pages
                         dokuwiki-open-page
                         dokuwiki-save-page))
        (advice-add command :around #'mfa-dokuwiki//apply-session-cookie))
      (advice-add 'dokuwiki-open-page :after #'mfa-dokuwiki//after-open-page)
      (spacemacs/declare-prefix "ow" "dokuwiki")
      (spacemacs/set-leader-keys
        "owc" #'mfa-dokuwiki/configure
        "owl" #'dokuwiki-list-pages
        "owo" #'dokuwiki-open-page
        "ows" #'dokuwiki-save-page))))

(defun mfa-dokuwiki/init-dokuwiki-mode ()
  (use-package dokuwiki-mode
    :defer t))
