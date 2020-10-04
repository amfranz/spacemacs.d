;; -*- lexical-binding: t -*-

(defconst my-dired-packages '(all-the-icons
                              treemacs-icons-dired
                              ;; FIXME: broken
                              ;; diff-hl
                              (dired :location built-in)
                              (dired+ :location (recipe :fetcher wiki
                                                        :files ("dired+.el")))
                              dired-filter
                              dired-subtree
                              (dired-x :location built-in)
                              (image-dired :location built-in)
                              (wdired :location built-in)
                              ;; This conflicts with the location in `spacemacs-modeline-packages'
                              ;; which fetches an older version of `font-lock+' from emacsmirror.
                              ;; (font-lock+ :location (recipe :fetcher url
                              ;;                               :url "https://www.emacswiki.org/emacs/download/font-lock+.el"))
                              font-lock+))

;; TODO: Move this to another layer. The Spacemacs auto-completion layer owns
;;       the `all-the-icons' package now.
(defun my-dired/post-init-all-the-icons ()
  ;; Suggested by https://github.com/domtronn/all-the-icons.el#slow-rendering
  (setq inhibit-compacting-font-caches t))

(defun my-dired/post-init-treemacs-icons-dired ()
  (el-patch-feature treemacs-icons-dired)
  (with-eval-after-load 'treemacs-icons-dired
    (eval
     '(el-patch-defun treemacs-icons-dired--display-icons-for-subdir (path pos)
        "Display icons for subdir PATH at given POS."
        (unless (member path treemacs-icons-dired--covered-subdirs)
          (add-to-list 'treemacs-icons-dired--covered-subdirs path)
          (treemacs-with-writable-buffer
           (save-excursion
             (goto-char pos)
             (forward-line 2)
             (treemacs-block
              (while (not (eobp))
                (if (dired-move-to-filename nil)
                    (let* ((file (dired-get-filename nil t))
                           (icon (if (file-directory-p file)
                                     treemacs-icon-dir-closed
                                   (treemacs-icon-for-file file))))
                      (el-patch-swap
                        (insert icon)
                        ;; If the stars align, due to packages being loaded
                        ;; on-demand, this function may be called before the
                        ;; icons are loaded. In such case icon is nil. We make
                        ;; this change to avoid errors due to (insert nil) or
                        ;; (propertize nil ...).
                        (when icon (insert (propertize icon 'icon t)))))
                  (treemacs-return nil))
                (forward-line 1))))))))))

;; Disabled because it triggers an error every time after a directory is created
;; in dired:
;;
;;     error in process sentinel: vc-exec-after: Unexpected process state
;;     error in process sentinel: Unexpected process state
;;
;; (defun my-dired/post-init-diff-hl ()
;;   (add-hook 'dired-mode-hook #'diff-hl-dired-mode))

(defun my-dired/post-init-dired ()
  ;; Customize sort order of files in dired.
  (setq dired-listing-switches "-ahlv --group-directories-first")

  ;; Avoid unnecessary prompts by dired.
  (setq dired-recursive-copies 'always
        dired-recursive-deletes 'always)

  ;; Show symlink targets, even if other details are hidden.
  (setq dired-hide-details-hide-symlink-targets nil)

  ;; Bind a key to open dired in the users home directory.
  (spacemacs/safe-set-leader-keys "j~" #'dired-home)

  ;; An easy to reach binding to enter `dired-mode' in `default-directory'.
  (define-key evil-motion-state-map (kbd "-") #'dired-jump)

  ;; Enabe automatic revert mode for dired buffers.
  (add-hook 'global-auto-revert-mode-hook #'my-dired//dired-auto-revert-hooks)
  (when (featurep 'autorevert)
    (my-dired//dired-auto-revert-hooks))

  (with-eval-after-load 'dired
    (evilified-state-evilify-map dired-mode-map
      :mode dired-mode

      ;; The binding "G" (`dired-do-chgrp') can not be automatically evilified
      ;; and there will be a warning about when the `dired' feature is loaded.
      ;; To avoid this warning, we will move the bindings for `dired-do-chgrp',
      ;; `dired-do-chmod' and `dired-do-chown' under the major mode leader key.
      :pre-bindings
      "G" nil
      "M" nil
      "O" nil

      :bindings
      "j" #'diredp-next-line
      "k" #'diredp-previous-line
      "-" #'diredp-up-directory
      "=" #'my-dired/ediff-marked-pair

      (kbd "C-j") #'diredp-visit-next-file
      (kbd "C-k") #'diredp-visit-previous-file
      "]]" #'diredp-next-subdir
      "[[" #'diredp-prev-subdir
      "~" #'dired-home
      "f" (if (configuration-layer/layer-used-p 'ivy)
              #'counsel-find-file
            #'helm-find-files)
      "gr" #'revert-buffer

      ;; Fine-grained control over the window in which to visit a file.
      "oo" #'dired-find-file-other-window
      "oh" #'my-dired/find-file-horizontal-split
      "ov" #'my-dired/find-file-vertical-split
      "oaa" #'my-dired/find-file-ace
      "oah" #'my-dired/find-file-ace-horizontal-split
      "oav" #'my-dired/find-file-ace-vertical-split

      ;; Exchange "c" (compress) with "C" (copy). Copy is used more often, it
      ;; should be on the binding that is easier to reach. Ideally the binding
      ;; for copy should also mirror the binding for rename, which is "r".
      "c" #'dired-do-copy
      "C" #'dired-do-compress-to)

    (spacemacs/declare-prefix-for-mode 'dired-mode "g" "tags")
    (spacemacs/declare-prefix-for-mode 'dired-mode "T" "toggle")

    (spacemacs/declare-prefix-for-mode 'dired-mode "ms" "subdir")
    (spacemacs/set-leader-keys-for-major-mode 'dired-mode
      "sj" #'dired-goto-subdir
      "sd" #'my-dired/kill-subdir)

    (spacemacs/declare-prefix-for-mode 'dired-mode "ma" "attrs")
    (spacemacs/set-leader-keys-for-major-mode 'dired-mode
      "ag" #'dired-do-chgrp
      "am" #'dired-do-chmod
      "ao" #'dired-do-chown
      "w" #'wdired-change-to-wdired-mode)))

(defun my-dired/init-wdired ()
  (with-eval-after-load 'wdired
    (evil-define-key nil wdired-mode-map
      [remap evil-write] #'wdired-finish-edit)
    (evil-define-key 'normal wdired-mode-map
      ",k" #'wdired-abort-changes
      ",c" #'wdired-finish-edit
      (kbd "<escape>") #'wdired-exit)))

(defun my-dired/init-dired+ ()
  (use-package dired+
    :hook (dired-mode . my-dired//require-dired+)
    :config
    (progn
      ;; `dired+' overuses color in my opinion, let's scale it down a bit.
      (unless (listp font-lock-maximum-decoration)
        (setq font-lock-maximum-decoration `((t . ,font-lock-maximum-decoration))))
      (add-to-list 'font-lock-maximum-decoration '(dired-mode . 1)))))

(defun my-dired/init-dired-filter ()
  (use-package dired-filter
    :hook (dired-mode . my-dired//maybe-dired-filter-mode)
    :diminish
    :init
    (progn
      (when (featurep 'dired-filter)
        (lwarn 'spacemacs :warning
               "The feature `dired-filter' was loaded too early, \
the customization to `dired-filter-prefix' did not take effect."))
      (setq dired-filter-prefix nil))
    :config
    (progn
      (setq dired-filter-verbose nil)
      (spacemacs/declare-prefix-for-mode 'dired-mode "mf" "filter")
      (spacemacs/declare-prefix-for-mode 'dired-mode "mfi" "ignore")
      (spacemacs/declare-prefix-for-mode 'dired-mode "mm" "mark")
      (spacemacs/declare-prefix-for-mode 'dired-mode "mmi" "ignore")
      (spacemacs/safe-set-leader-keys-for-major-mode 'dired-mode
        "f" dired-filter-map
        "m" dired-filter-mark-map))))

(defun my-dired/init-dired-subtree ()
  (use-package dired-subtree
    :defer t
    :init
    (progn
      (when (configuration-layer/package-used-p 'treemacs-icons-dired)
        (add-hook 'treemacs-icons-dired-mode-hook
                  #'my-dired//dired-subtree--insert-treemacs-icons))
      (with-eval-after-load 'dired
        (evil-define-key 'evilified dired-mode-map
          (kbd "TAB") #'dired-subtree-toggle)))
    :config
    (setq dired-subtree-use-backgrounds nil)))

(defun my-dired/post-init-dired-x ()
  ;; Hide temporary files by Jetbrains IDEs
  (add-to-list 'completion-ignored-extensions "___jb_tmp___")
  (with-eval-after-load 'dired-x
    (add-to-list 'dired-omit-extensions "___jb_tmp___"))

  ;; Hide Python bytecode files and directories
  (with-eval-after-load 'dired-x
    (setq dired-omit-files (concat dired-omit-files "\\|^__pycache__$")))

  ;; Hide dired-omit-mode lighter, it is distracting.
  (with-eval-after-load 'dired-x
    (defun dired-x--diminish-dired-omit-mode ()
      (diminish 'dired-omit-mode))
    (advice-add 'dired-omit-startup
                :after #'dired-x--diminish-dired-omit-mode)))

(defun my-dired/init-image-dired ()
  (setq image-dired-dir (concat spacemacs-cache-directory "image-dired/")))

;; TODO: Move this to another layer. The Spacemacs auto-completion layer owns
;;       the `all-the-icons' package now.
(defun my-dired/post-init-font-lock+ ()
  ;; `font-lock+' is needed for the icons in `dired-mode' buffers to be
  ;; colorized by `all-the-icons-dired'. We only need it for this purpose, but
  ;; considering this feature potentially modifies font lock functionality in
  ;; other buffers as well we load it early instead of deferring until after
  ;; `dired-mode' is loaded to ensure a consistent UI experience.
  (with-eval-after-load 'font-lock
    (require 'font-lock+)))
