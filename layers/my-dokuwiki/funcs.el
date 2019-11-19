;; -*- lexical-binding: t -*-

(defvar dokuwiki-xml-rpc-domain nil
  "The domain name of the DokuWiki site.")

(defvar dokuwiki-session-cookie nil
  "The session cookie to use for DokuWiki authentication.")

(defun my-dokuwiki/configure (domain cookie)
  "Configures the DokuWiki XML-RPC connection."
  (interactive "sDomain: \nsCookie: ")
  (setq dokuwiki-xml-rpc-domain domain
        dokuwiki-xml-rpc-url (concat "https://" domain "/lib/exe/xmlrpc.php")
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

(defun my-dokuwiki/revert-page ()
  "Revert the current buffers content to the current content on the Wiki."
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

(defun my-dokuwiki/browse-at-remote ()
  "Opens the browser with the Wiki page of the current buffer."
  (interactive)
  (let ((name (buffer-name)))
    (unless (and (eq major-mode 'dokuwiki-mode)
                 (string-suffix-p ".dwiki" name))
      (user-error "Not a dokuwiki page"))
    (browse-url (concat "https://" dokuwiki-xml-rpc-domain "/"
                        (replace-regexp-in-string
                         ":" "/" (string-remove-suffix ".dwiki" name))))))

(with-eval-after-load 'el-patch
  (el-patch-feature dokuwiki)
  (with-eval-after-load 'dokuwiki
    (eval
     '(el-patch-defun dokuwiki-open-page
        (el-patch-swap
          (page-name-or-url)
          (page-name-or-url &optional in-this-buffer))
        (el-patch-swap
          "Opens a page from the wiki.

PAGE-NAME-OR-URL: The page id or url to open.

To open a page in a particular namespace add the namespace name before
the page-name.  For example, \"namespace:wiki-page\" to open the
\"wiki-page\" page inside the \"namespace\" namespace.

If the specified page does not exist, it creates a new page once the
buffer is saved."
          "Opens a page from the wiki.

PAGE-NAME-OR-URL: The page id or url to open.

IN-THIS-BUFFER: If nil, create a new buffer named after the wiki page and make
the buffer active. If non-nil, replace the content of the current buffer.

To open a page in a particular namespace add the namespace name before
the page-name.  For example, \"namespace:wiki-page\" to open the
\"wiki-page\" page inside the \"namespace\" namespace.

If the specified page does not exist, it creates a new page once the
buffer is saved.")
        (interactive "sEnter page name: ")
        (if (not dokuwiki--has-successfully-logged-in)
            (user-error "Login first before opening a page")
          (let* ((page-name (car (last (split-string page-name-or-url "/"))))
                 (page-content (xml-rpc-method-call dokuwiki-xml-rpc-url 'wiki.getPage page-name)))
            (message "Page name is \"%s\"" page-name)
            (if (not page-content)
                (message "Page not found in wiki. Creating a new buffer with page name \"%s\"" page-name)
              (message "Page exists. Creating buffer for existing page \"%s\"" page-name))
            (el-patch-wrap 2 0
              (unless in-this-buffer
                (get-buffer-create (concat page-name ".dwiki"))
                (switch-to-buffer (concat page-name ".dwiki"))))
            (erase-buffer)
            (when page-content
              (insert page-content))))))))

(defun my-dokuwiki/diff-page ()
  "Shows the difference between the current buffer and the page on the Wiki."
  (interactive)
  (let ((name (buffer-name)))
    (unless (and (eq major-mode 'dokuwiki-mode)
                 (string-suffix-p ".dwiki" name))
      (user-error "Not a dokuwiki page"))
    (let* ((page-name (string-remove-suffix ".dwiki" name))
           (upstream-name (concat page-name ".upstream.dwiki"))
           (upstream-buffer (get-buffer-create upstream-name)))
      (with-current-buffer upstream-buffer
        (dokuwiki-open-page page-name t))
      (ediff-buffers name upstream-name))))
