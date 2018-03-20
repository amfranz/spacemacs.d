(defvar dokuwiki-session-cookie nil
  "The session cookie to use for DokuWiki authentication.")

(defun mfa-dokuwiki/configure (domain cookie)
  "Configures the DokuWiki XML-RPC connection."
  (interactive "sDomain: \nsCookie: ")
  (setq dokuwiki-xml-rpc-url (concat "https://" domain "/lib/exe/xmlrpc.php")
        dokuwiki-session-cookie cookie))

(defun mfa-dokuwiki//apply-session-cookie (orig-fun &rest args)
  "Adds a session cookie to a DokuWiki XML-RPC request."
  (if (or (not dokuwiki-session-cookie) dokuwiki--has-successfully-logged-in)
      (apply orig-fun args)
    (let ((dokuwiki--has-successfully-logged-in t)
          (xml-rpc-request-extra-headers (append
                                          (list (cons "Cookie" dokuwiki-session-cookie))
                                          xml-rpc-request-extra-headers)))
      (apply orig-fun args))))

(defun mfa-dokuwiki//after-open-page (&rest args)
  "Configures the buffer after it was populated with a DokuWiki page."
  (goto-char (point-min))
  (dokuwiki-mode))
