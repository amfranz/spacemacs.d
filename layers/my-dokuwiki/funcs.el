;; -*- lexical-binding: t -*-

(defvar dokuwiki-xml-rpc-domain nil
  "The domain name of the DokuWiki site.")

(defvar dokuwiki-session-cookie nil
  "The session cookie to use for DokuWiki authentication.")

(defun my-dokuwiki/configure (domain cookie)
  "Configures the DokuWiki XML-RPC connection."
  (interactive "sDomain: \nsCookie: ")
  (setq dokuwiki-xml-rpc-url (concat "https://" domain "/lib/exe/xmlrpc.php")
        dokuwiki-session-cookie cookie))

(defun my-dokuwiki//apply-session-cookie (orig-fun &rest args)
  "Adds a session cookie to a DokuWiki XML-RPC request."
  (if (or (not dokuwiki-session-cookie) dokuwiki--has-successfully-logged-in)
      (apply orig-fun args)
    (let ((dokuwiki--has-successfully-logged-in t)
          (xml-rpc-request-extra-headers (append
                                          (list (cons "Cookie" dokuwiki-session-cookie))
                                          xml-rpc-request-extra-headers)))
      (apply orig-fun args))))

(defun my-dokuwiki//after-open-page (&rest args)
  "Configures the buffer after it was populated with a DokuWiki page."
  (unless (eq major-mode 'dokuwiki-mode)
    (goto-char (point-min))
    (dokuwiki-mode)))

(defun my-dokuwiki//imenu-create-index-function-parser (depth bound prefix index)
  (let ((end (point)))
    (while (search-backward-regexp (format "^=\\{%d\\} \\(.+\\) =\\{%d\\}$" depth depth) bound t)
      (let ((beg (match-beginning 0))
            (path (append prefix (list (match-string-no-properties 1)))))
        (when (> depth 3)
          (save-excursion
            (goto-char end)
            (setq index (my-dokuwiki//imenu-create-index-function-parser
                         (- depth 1) beg path index))))
        (push (cons (string-join path " / ") beg) index)
        (setq end beg))))
  index)

(defun my-dokuwiki//imenu-create-index-function ()
  (save-restriction
    (widen)
    (save-excursion
      (goto-char (point-max))
      (list (cons "Heading" (my-dokuwiki//imenu-create-index-function-parser 6 nil nil nil))))))

(defun my-dokuwiki//configure-imenu-index ()
  (setq imenu-create-index-function #'my-dokuwiki//imenu-create-index-function))

(defun my-dokuwiki/reopen-page ()
  (interactive)
  (let ((name (buffer-name)))
    (unless (and (eq major-mode 'dokuwiki-mode)
                 (string-suffix-p ".dwiki" name))
      (user-error "Not a dokuwiki page"))
    (let ((prev-window-start (window-start))
          (prev-point (point)))
      (dokuwiki-open-page (string-remove-suffix ".dwiki" name))
      (set-window-start nil prev-window-start)
      (goto-char prev-point))))

(defun my-dokuwiki//customize-export-backend-ad (args)
  (when (eq 'wk (car args))
    (setcar (plist-get args :menu-entry) ?d))
  args)

(defun my-dokuwiki/diff-page ()
  (interactive)
  (let ((name (buffer-name)))
    (unless (and (eq major-mode 'dokuwiki-mode)
                 (string-suffix-p ".dwiki" name))
      (user-error "Not a dokuwiki page"))
    (let* ((page-name (string-remove-suffix ".dwiki" name))
           (upstream-name (concat page-name ".upstream.dwiki"))
           (upstream-buffer (get-buffer-create upstream-name)))
      (with-current-buffer upstream-buffer
        (dokuwiki-open-page page-name)) ;; todo needs buffer arg
      (ediff-buffers name upstream-name))))
