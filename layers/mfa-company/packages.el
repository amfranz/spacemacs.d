;; -*- lexical-binding: t -*-

(defconst mfa-company-packages '(company-box))

(defun mfa-company/init-company-box ()
  (use-package company-box
    :hook (company-mode . mfa-company//enable-company-box-mode)
    :diminish
    :config
    (progn
      (add-to-list 'load-path (expand-file-name "~/.local/share/icons-in-terminal/"))

      (require 'icons-in-terminal)

      (defun my--adjust-company-box-theme-faces ()
        (when (memq 'zenburn custom-enabled-themes)
          (custom-theme-set-faces
           'zenburn
           '(company-box-candidate ((t (:inherit default)))))))
      (add-hook 'spacemacs-post-theme-change-hook
                #'my--adjust-company-box-theme-faces)
      (when (memq 'zenburn custom-enabled-themes)
        (my--adjust-company-box-theme-faces))

      (setq company-box-icons-alist 'company-box-icons-icons-in-terminal)

      (setq company-box-icons-unknown 'fa_question_circle)

      (setq company-box-icons-elisp
            '((fa_tag :face font-lock-function-name-face) ;; Function
              (fa_cog :face font-lock-variable-name-face) ;; Variable
              (fa_cube :face font-lock-constant-face) ;; Feature
              (md_color_lens :face font-lock-doc-face))) ;; Face

      (setq company-box-icons-yasnippet 'fa_bookmark)

      (setq company-box-icons-lsp
            '((1 . fa_text_height) ;; Text
              (2 . (fa_tags :face font-lock-function-name-face)) ;; Method
              (3 . (fa_tag :face font-lock-function-name-face)) ;; Function
              (4 . (fa_tag :face font-lock-function-name-face)) ;; Constructor
              (5 . (fa_cog :foreground "#FF9800")) ;; Field
              (6 . (fa_cog :foreground "#FF9800")) ;; Variable
              (7 . (fa_cube :foreground "#7C4DFF")) ;; Class
              (8 . (fa_cube :foreground "#7C4DFF")) ;; Interface
              (9 . (fa_cube :foreground "#7C4DFF")) ;; Module
              (10 . (fa_cog :foreground "#FF9800")) ;; Property
              (11 . md_settings_system_daydream) ;; Unit
              (12 . (fa_cog :foreground "#FF9800")) ;; Value
              (13 . (md_storage :face font-lock-type-face)) ;; Enum
              (14 . (md_closed_caption :foreground "#009688")) ;; Keyword
              (15 . md_closed_caption) ;; Snippet
              (16 . (md_color_lens :face font-lock-doc-face)) ;; Color
              (17 . fa_file_text_o) ;; File
              (18 . md_refresh) ;; Reference
              (19 . fa_folder_open) ;; Folder
              (20 . (md_closed_caption :foreground "#009688")) ;; EnumMember
              (21 . (fa_square :face font-lock-constant-face)) ;; Constant
              (22 . (fa_cube :face font-lock-type-face)) ;; Struct
              (23 . fa_calendar) ;; Event
              (24 . fa_square_o) ;; Operator
              (25 . fa_arrows)) ;; TypeParameter
            )

      (setq company-box-doc-enable nil))))
