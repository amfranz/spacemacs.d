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
  ;; This is a simplified and more reliable `dired-after-readin-hook' for
  ;; `treemacs-icons-dired'.
  ;;
  ;; Both `all-the-icons-dired' as well as `treemacs-icons-dired' seem to be
  ;; implemented ignorant of the fact that when `dired-after-readin-hook' is
  ;; called, the buffer is narrowed over the relevant entries. The following
  ;; explains the problems in the context `treemacs-icons-dired', but most of it
  ;; applies to `all-the-icons-dired' as well.
  ;;
  ;; The logic in `treemacs-icons-dired--display' which serves to remember the
  ;; list of subdirs for which icons have already been inserted is unnecessary,
  ;; because `dired-after-readin-hook' is called by `dired-insert-subdir' with
  ;; the region narrowed over the new subdir. All that needs to be done is to add
  ;; icons for the files within the narrowed region, there is no bookkeeping
  ;; in necessary in regards to which files or subdirs already got icons.
  ;;
  ;; Using the narrowed region is not just simpler, it also fixes some edge cases
  ;; that are currently broken. For example, if `auto-revert-mode' is NOT used,
  ;; in which case the dired buffer will not refresh when the content of the
  ;; directory changes, dired functions like `dired-create-directory' or
  ;; `dired-create-empty-file' will insert an entry for the file they create into
  ;; the existing dired buffer. Then they call `dired-after-readin-hook' with the
  ;; buffer narrowed over only the one new entry. Again, all that needs to be
  ;; done is to an icon for the file within the narrowed region. The upstream
  ;; logic in the `treemacs-icons-dired' package will not add an icon, because
  ;; according to its own bookkeeping, the subdir this new entry is in already
  ;; got icons.
  ;;
  (el-patch-feature treemacs-icons-dired)
  (with-eval-after-load 'treemacs-icons-dired
    ;; We need to patch the existing function, we can't just create a new one. The
    ;; reason is because `treemacs-icons-dired-mode', when enabled, calls _this_
    ;; function in all `dired-mode' buffers for the purpose of populating them with
    ;; icons. It is therefore too late to patch the icon insertion functionality
    ;; using `treemacs-icons-dired-mode-hook', because when that is invoked there
    ;; may already be buffers that have been populated with icons by the upstream
    ;; logic. To avoid that, we are patching the existing function instead.
    (eval
     '(el-patch-defun treemacs-icons-dired--display ()
        "Display the icons of files in a dired buffer."
        (el-patch-swap
          (when (and (display-graphic-p)
                     (not treemacs-icons-dired-displayed)
                     dired-subdir-alist)
            (setq-local treemacs-icons-dired-displayed t)
            (setq-local treemacs-icons (treemacs-theme->gui-icons treemacs--current-theme))
            (pcase-dolist (`(,path . ,pos) dired-subdir-alist)
              (treemacs-icons-dired--display-icons-for-subdir path pos)))
          (when (display-graphic-p)
            (treemacs-with-writable-buffer
             (save-excursion
               (goto-char (point-min))
               (while (not (eobp))
                 (when (dired-move-to-filename nil)
                   (unless (get-text-property (1- (point)) 'icon)
                     (when-let ((file (dired-get-filename nil t))
                                (icon (if (file-directory-p file)
                                          treemacs-icon-dir-closed
                                        (treemacs-icon-for-file file))))
                       (insert (propertize icon 'icon t)))))
                 (forward-line))))))))

    ;; Since no special bookkeeping is necessary to remember which subdirs already
    ;; got icons, these advices are unnecessary.
    (advice-remove #'dired-revert #'treemacs-icons-dired--reset)
    (advice-remove #'dired-insert-subdir #'treemacs-icons-dired--insert-subdir-advice)
    (advice-remove #'dired-kill-subdir #'treemacs-icons-dired--kill-subdir-advice)
    (with-eval-after-load 'dired+
      (when (fboundp 'diredp-insert-subdirs)
        (advice-remove #'diredp-insert-subdirs #'treemacs-icons-dired--insert-subdir-advice)))

    ;; Unbind obsolete functions. This ensures that if they are called, it will be
    ;; noticed.
    (fmakunbound 'treemacs-icons-dired--display-icons-for-subdir)
    (fmakunbound 'treemacs-icons-dired--insert-subdir-advice)
    (fmakunbound 'treemacs-icons-dired--kill-subdir-advice)
    (fmakunbound 'treemacs-icons-dired--reset)))

;; (defun my-dired/post-init-diff-hl ()
;;   (add-hook 'dired-mode-hook #'diff-hl-dired-mode))

(defun my-dired/post-init-dired ()
  ;; Customize sort order of files in dired.
  (setq dired-listing-switches "-ahlv --group-directories-first")

  ;; Avoid unnecessary prompts by dired.
  (setq dired-recursive-copies 'always
        dired-recursive-deletes 'top)

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
      (add-hook 'dired-subtree-after-insert-hook
                #'treemacs-dired-subtree-insert-icons)
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
