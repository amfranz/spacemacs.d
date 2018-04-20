;; -*- lexical-binding: t -*-

(defconst mfa-dokuwiki-packages '(company dokuwiki dokuwiki-mode outline-magic))

(defun mfa-dokuwiki/post-init-company ()
  (spacemacs|add-company-hook dokuwiki-mode))

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
        "owr" #'mfa-dokuwiki/reopen-page
        "ows" #'dokuwiki-save-page))))

(defun mfa-dokuwiki/init-dokuwiki-mode ()
  (use-package dokuwiki-mode
    :mode "\\.dwiki\\'"
    :config
    (progn
      (add-hook 'dokuwiki-mode-hook #'mfa-dokuwiki//configure-line-wrap)
      (add-hook 'dokuwiki-mode-hook #'mfa-dokuwiki//configure-imenu-index))))

(defun mfa-dokuwiki/init-outline-magic ()
  (use-package outline-magic
    :after (dokuwiki-mode)))
