;; -*- lexical-binding: t -*-

(defconst my-company-packages '(company-box))

(defun my-company/post-init-company-box ()
  ;; Use icons provided by `icons-in-terminal' for company candidates. I prefer
  ;; their look over `all-the-icons'.
  (add-to-list 'load-path (expand-file-name "~/.local/share/icons-in-terminal/"))
  (autoload 'company-box-icons-icons-in-terminal "icons-in-terminal")
  (setq company-box-icons-alist 'company-box-icons-icons-in-terminal)

  ;; Make `company-box' candidates adhere to the color theme instead of using a
  ;; hardcoded bright white text color.
  (add-hook 'spacemacs-post-theme-change-hook
            #'my-company//adjust-company-box-theme-faces)
  (my-company//adjust-company-box-theme-faces))
