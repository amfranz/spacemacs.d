(defconst mfa-mu4e-packages '(mu4e))

(defun mfa-mu4e/post-init-mu4e ()
  (setq mu4e-change-filenames-when-moving t ;; required when using mbsync
        mu4e-compose-signature-auto-include nil
        mu4e-get-mail-command "mbsync -a"
        mu4e-html2text-command #'mu4e-shr2text
        mu4e-maildir (expand-file-name "~/.mail")
        mu4e-update-interval nil
        mu4e-view-show-addresses t
        mu4e-view-show-images t)

  (setq mu4e-maildir-shortcuts
        '(("/amfranz@gmail.com/INBOX" . ?g)))

  (setq mu4e-bookmarks
        `(("flag:unread AND NOT flag:trashed" "Unread messages" ?u)
          ("date:today..now" "Today's messages" ?t)
          ("date:7d..now" "Last 7 days" ?w)
          ("mime:image/*" "Messages with images" ?p)
          (,(mapconcat 'identity
                       (mapcar
                        (lambda (maildir)
                          (concat "maildir:" (car maildir)))
                        mu4e-maildir-shortcuts) " OR ")
           "All inboxes" ?i)))

  (with-eval-after-load 'mu4e
    (setq mu4e-account-alist
          '(("amfranz@gmail.com"
             (mu4e-sent-messages-behavior delete)
             (mu4e-drafts-folder "/amfranz@gmail.com/Drafts")
             (mu4e-sent-folder "/amfranz@gmail.com/Sent Mail")
             (mu4e-trash-folder "/amfranz@gmail.com/Trash")
             (user-full-name "Michael Franz Aigner")
             (user-mail-address "amfranz@gmail.com"))))
    (mu4e/mail-account-reset)

    (add-to-list 'mu4e-view-actions
                 '("xView in XWidget" . mu4e-action-view-with-xwidget) t)

    (require 'mu4e-contrib)
    (mu4e-alert-set-default-style 'notifications))

  (setq send-mail-function #'smtpmail-send-it
        message-send-mail-function #'smtpmail-send-it
        smtpmail-default-smtp-server "smtp.gmail.com"
        smtpmail-smtp-server "smtp.gmail.com"
        smtpmail-smtp-service 587
        smtpmail-stream-type 'starttls))
