;; -*- lexical-binding: t -*-

(defconst mfa-dokuwiki-packages '(company dokuwiki dokuwiki-mode outline-magic ox-wk))

(defun mfa-dokuwiki/post-init-company ()
  (spacemacs|add-company-backends :modes dokuwiki-mode))

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
      (add-hook 'dokuwiki-mode-hook #'spacemacs/toggle-visual-line-navigation-on)
      (add-hook 'dokuwiki-mode-hook #'mfa-dokuwiki//configure-imenu-index))))

(defun mfa-dokuwiki/init-outline-magic ()
  (use-package outline-magic
    :after (dokuwiki-mode)))

(defun mfa-dokuwiki/init-ox-wk ()
  (use-package ox-wk
    :after org
    :init
    (progn
      (el-patch-feature ox-wk)
      (advice-add 'org-export-define-derived-backend :filter-args
                  #'mfa-dokuwiki//customize-export-backend-ad))
    :config
    (el-patch-defun ox-wk-export-to-wiki
      (&optional async subtreep visible-only body-only ext-plist)
      "Export current buffer to a Wiki sytntax text file.

If narrowing is active in the current buffer, only export its
narrowed part.

If a region is active, export that region.

A non-nil optional argument ASYNC means the process should happen
asynchronously.  The resulting file should be accessible through
the `org-export-stack' interface.

When optional argument SUBTREEP is non-nil, export the sub-tree
at point, extracting information from the headline properties
first.

When optional argument VISIBLE-ONLY is non-nil, don't export
contents of hidden elements.

When optional argument BODY-ONLY is non-nil, strip title, table
of contents and footnote definitions from output.

EXT-PLIST, when provided, is a property list with external
parameters overriding Org default settings, but still inferior to
file-local settings.

Return output file's name."
      (interactive)
      (let ((outfile (org-export-output-file-name
                      (el-patch-swap ".txt" ".dwiki")
                      subtreep))
            (org-export-coding-system (el-patch-swap "utf-8" 'utf-8)))
        (org-export-to-file 'wk outfile
          async subtreep visible-only body-only ext-plist)))))
