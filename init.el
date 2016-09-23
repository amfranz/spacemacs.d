;; -*- mode: emacs-lisp -*-
;; This file is loaded by Spacemacs at startup.
;; It must be stored in your home directory.

(defun dotspacemacs/layers ()
  "Configuration Layers declaration.
You should not put any user code in this function besides modifying the variable
values."
  (setq-default
   ;; Base distribution to use. This is a layer contained in the directory
   ;; `+distribution'. For now available distributions are `spacemacs-base'
   ;; or `spacemacs'. (default 'spacemacs)
   dotspacemacs-distribution 'spacemacs
   ;; Lazy installation of layers (i.e. layers are installed only when a file
   ;; with a supported type is opened). Possible values are `all', `unused'
   ;; and `nil'. `unused' will lazy install only unused layers (i.e. layers
   ;; not listed in variable `dotspacemacs-configuration-layers'), `all' will
   ;; lazy install any layer that support lazy installation even the layers
   ;; listed in `dotspacemacs-configuration-layers'. `nil' disable the lazy
   ;; installation feature and you have to explicitly list a layer in the
   ;; variable `dotspacemacs-configuration-layers' to install it.
   ;; (default 'unused)
   dotspacemacs-enable-lazy-installation 'unused
   ;; If non-nil then Spacemacs will ask for confirmation before installing
   ;; a layer lazily. (default t)
   dotspacemacs-ask-for-lazy-installation t
   ;; If non-nil layers with lazy install support are lazy installed.
   ;; List of additional paths where to look for configuration layers.
   ;; Paths must have a trailing slash (i.e. `~/.mycontribs/')
   dotspacemacs-configuration-layer-path '()
   ;; List of configuration layers to load.
   dotspacemacs-configuration-layers
   '(
     ;; ----------------------------------------------------------------
     ;; Example of useful layers you may want to use right away.
     ;; Uncomment some layer names and press <SPC f e R> (Vim style) or
     ;; <M-m f e R> (Emacs style) to install them.
     ;; ----------------------------------------------------------------
     ;; helm
     ;; auto-completion
     ;; better-defaults
     ;; emacs-lisp
     ;; git
     ;; markdown
     ;; org
     ;; (shell :variables
     ;;        shell-default-height 30
     ;;        shell-default-position 'bottom)
     ;; spell-checking
     ;; syntax-checking
     ;; version-control

     ;; layers by spacemacs
     ansible
     auto-completion
     clojure
     (c-c++ :variables
            c-c++-enable-clang-support t)
     colors
     (dash :variables
           helm-dash-docset-newpath "~/.local/share/Zeal/Zeal/docsets")
     docker
     git
     github
     emacs-lisp
     emoji
     evil-snipe
     (go :variables
         go-tab-width 4)
     graphviz
     helm
     html
     javascript
     lua
     markdown
     nlinum
     (org :variables
          org-enable-github-support t)
     php
     plantuml
     prodigy
     python
     ;; rebox
     restclient
     ruby
     search-engine
     (shell :variables
            shell-default-shell 'multi-term
            shell-enable-smart-eshell t)
     shell-scripts
     spacemacs-layouts
     sql
     (syntax-checking :variables
                      syntax-checking-enable-tooltips nil)
     vinegar
     version-control
     yaml

     ;; my custom layers
     mfa-alpine
     mfa-angularjs
     mfa-ansible
     mfa-artist
     mfa-ascii
     mfa-bookmarks
     mfa-checkbashisms
     mfa-dash
     mfa-defproject
     mfa-devdocs
     mfa-diff
     mfa-dtrt-indent
     mfa-dynamic-ruler
     mfa-editorconfig
     mfa-environment
     mfa-ethan-wspace
     mfa-evil
     mfa-evil-mc
     mfa-evil-quickscope
     mfa-evil-textobj-anyblock
     mfa-evil-vimish-fold
     mfa-fancy-narrow
     mfa-google-this
     mfa-highlight-symbols
     mfa-javascript
     mfa-json
     mfa-make-mode
     mfa-mwim
     mfa-neotree
     mfa-org
     mfa-pass
     mfa-poporg
     mfa-projectile
     mfa-python
     mfa-ruby
     mfa-shell
     mfa-shell-scripts
     mfa-string-edit
     mfa-string-inflection
     mfa-xml

     ;; TODO
     ;; bpr
     ;; fzf
     ;; multi-line
     )
   ;; List of additional packages that will be installed without being
   ;; wrapped in a layer. If you need some configuration for these
   ;; packages, then consider creating a layer. You can also put the
   ;; configuration in `dotspacemacs/user-config'.
   dotspacemacs-additional-packages '()
   ;; A list of packages that cannot be updated.
   dotspacemacs-frozen-packages '()
   ;; A list of packages that will not be installed and loaded.
   dotspacemacs-excluded-packages '()
   ;; Defines the behaviour of Spacemacs when downloading packages.
   ;; Possible values are `used', `used-but-keep-unused' and `all'. `used' will
   ;; download only explicitly used packages and remove any unused packages as
   ;; well as their dependencies. `used-but-keep-unused' will download only the
   ;; used packages but won't delete them if they become unused. `all' will
   ;; download all the packages regardless if they are used or not and packages
   ;; won't be deleted by Spacemacs. (default is `used')
   dotspacemacs-download-packages 'used))

(defun dotspacemacs/init ()
  "Initialization function.
This function is called at the very startup of Spacemacs initialization
before layers configuration.
You should not put any user code in there besides modifying the variable
values."
  ;; This setq-default sexp is an exhaustive list of all the supported
  ;; spacemacs settings.
  (setq-default
   ;; If non nil ELPA repositories are contacted via HTTPS whenever it's
   ;; possible. Set it to nil if you have no way to use HTTPS in your
   ;; environment, otherwise it is strongly recommended to let it set to t.
   ;; This variable has no effect if Emacs is launched with the parameter
   ;; `--insecure' which forces the value of this variable to nil.
   ;; (default t)
   dotspacemacs-elpa-https t
   ;; Maximum allowed time in seconds to contact an ELPA repository.
   dotspacemacs-elpa-timeout 5
   ;; If non nil then spacemacs will check for updates at startup
   ;; when the current branch is not `develop'. Note that checking for
   ;; new versions works via git commands, thus it calls GitHub services
   ;; whenever you start Emacs. (default nil)
   dotspacemacs-check-for-update nil
   ;; One of `vim', `emacs' or `hybrid'.
   ;; `hybrid' is like `vim' except that `insert state' is replaced by the
   ;; `hybrid state' with `emacs' key bindings. The value can also be a list
   ;; with `:variables' keyword (similar to layers). Check the editing styles
   ;; section of the documentation for details on available variables.
   ;; (default 'vim)
   dotspacemacs-editing-style 'vim
   ;; If non nil output loading progress in `*Messages*' buffer. (default nil)
   dotspacemacs-verbose-loading nil
   ;; Specify the startup banner. Default value is `official', it displays
   ;; the official spacemacs logo. An integer value is the index of text
   ;; banner, `random' chooses a random text banner in `core/banners'
   ;; directory. A string value must be a path to an image format supported
   ;; by your Emacs build.
   ;; If the value is nil then no banner is displayed. (default 'official)
   dotspacemacs-startup-banner nil
   ;; List of items to show in startup buffer or an association list of
   ;; the form `(list-type . list-size)`. If nil then it is disabled.
   ;; Possible values for list-type are:
   ;; `recents' `bookmarks' `projects' `agenda' `todos'."
   dotspacemacs-startup-lists '((recents . 5)
                                (projects . 7))
   ;; Default major mode of the scratch buffer (default `text-mode')
   dotspacemacs-scratch-mode 'text-mode
   ;; List of themes, the first of the list is loaded when spacemacs starts.
   ;; Press <SPC> T n to cycle to the next theme in the list (works great
   ;; with 2 themes variants, one dark and one light)
   dotspacemacs-themes '(zenburn
                         solarized-light
                         solarized-dark
                         spacemacs-light
                         spacemacs-dark
                         leuven
                         monokai)
   ;; If non nil the cursor color matches the state color in GUI Emacs.
   dotspacemacs-colorize-cursor-according-to-state t
   ;; Default font. `powerline-scale' allows to quickly tweak the mode-line
   ;; size to make separators look not too crappy.
   dotspacemacs-default-font '("DejaVu Sans Mono"
                               :size 13
                               :weight normal
                               :width normal
                               :powerline-scale 1.1)
   ;; The leader key
   dotspacemacs-leader-key "SPC"
   ;; The leader key accessible in `emacs state' and `insert state'
   ;; (default "M-m")
   dotspacemacs-emacs-leader-key "M-m"
   ;; Major mode leader key is a shortcut key which is the equivalent of
   ;; pressing `<leader> m`. Set it to `nil` to disable it. (default ",")
   dotspacemacs-major-mode-leader-key ","
   ;; Major mode leader key accessible in `emacs state' and `insert state'.
   ;; (default "C-M-m)
   dotspacemacs-major-mode-emacs-leader-key "C-M-m"
   ;; The key used for Emacs commands (M-x) (after pressing on the leader key).
   ;; (default "SPC")
   dotspacemacs-emacs-command-key ":"
   ;; These variables control whether separate commands are bound in the GUI to
   ;; the key pairs C-i, TAB and C-m, RET.
   ;; Setting it to a non-nil value, allows for separate commands under <C-i>
   ;; and TAB or <C-m> and RET.
   ;; In the terminal, these pairs are generally indistinguishable, so this only
   ;; works in the GUI. (default nil)
   dotspacemacs-distinguish-gui-tab nil
   ;; If non nil `Y' is remapped to `y$' in Evil states. (default nil)
   dotspacemacs-remap-Y-to-y$ nil
   ;; If non-nil, the shift mappings `<' and `>' retain visual state if used
   ;; there. (default t)
   dotspacemacs-retain-visual-state-on-shift t
   ;; If non-nil, J and K move lines up and down when in visual mode.
   ;; (default nil)
   dotspacemacs-visual-line-move-text nil
   ;; If non nil, inverse the meaning of `g' in `:substitute' Evil ex-command.
   ;; (default nil)
   dotspacemacs-ex-substitute-global nil
   ;; Name of the default layout (default "Default")
   dotspacemacs-default-layout-name "Default"
   ;; If non nil the default layout name is displayed in the mode-line.
   ;; (default nil)
   dotspacemacs-display-default-layout nil
   ;; If non nil then the last auto saved layouts are resume automatically upon
   ;; start. (default nil)
   dotspacemacs-auto-resume-layouts nil
   ;; Size (in MB) above which spacemacs will prompt to open the large file
   ;; literally to avoid performance issues. Opening a file literally means that
   ;; no major mode or minor modes are active. (default is 1)
   dotspacemacs-large-file-size 2
   ;; Location where to auto-save files. Possible values are `original' to
   ;; auto-save the file in-place, `cache' to auto-save the file to another
   ;; file stored in the cache directory and `nil' to disable auto-saving.
   ;; (default 'cache)
   dotspacemacs-auto-save-file-location 'cache
   ;; Maximum number of rollback slots to keep in the cache. (default 5)
   dotspacemacs-max-rollback-slots 5
   ;; If non nil, `helm' will try to minimize the space it uses. (default nil)
   dotspacemacs-helm-resize nil
   ;; if non nil, the helm header is hidden when there is only one source.
   ;; (default nil)
   dotspacemacs-helm-no-header nil
   ;; define the position to display `helm', options are `bottom', `top',
   ;; `left', or `right'. (default 'bottom)
   dotspacemacs-helm-position 'bottom
   ;; If non nil the paste micro-state is enabled. When enabled pressing `p`
   ;; several times cycle between the kill ring content. (default nil)
   dotspacemacs-enable-paste-transient-state nil
   ;; Which-key delay in seconds. The which-key buffer is the popup listing
   ;; the commands bound to the current keystroke sequence. (default 0.4)
   dotspacemacs-which-key-delay 0.4
   ;; Which-key frame position. Possible values are `right', `bottom' and
   ;; `right-then-bottom'. right-then-bottom tries to display the frame to the
   ;; right; if there is insufficient space it displays it at the bottom.
   ;; (default 'bottom)
   dotspacemacs-which-key-position 'bottom
   ;; If non nil a progress bar is displayed when spacemacs is loading. This
   ;; may increase the boot time on some systems and emacs builds, set it to
   ;; nil to boost the loading time. (default t)
   dotspacemacs-loading-progress-bar nil
   ;; If non nil the frame is fullscreen when Emacs starts up. (default nil)
   ;; (Emacs 24.4+ only)
   dotspacemacs-fullscreen-at-startup nil
   ;; If non nil `spacemacs/toggle-fullscreen' will not use native fullscreen.
   ;; Use to disable fullscreen animations in OSX. (default nil)
   dotspacemacs-fullscreen-use-non-native nil
   ;; If non nil the frame is maximized when Emacs starts up.
   ;; Takes effect only if `dotspacemacs-fullscreen-at-startup' is nil.
   ;; (default nil) (Emacs 24.4+ only)
   dotspacemacs-maximized-at-startup t
   ;; A value from the range (0..100), in increasing opacity, which describes
   ;; the transparency level of a frame when it's active or selected.
   ;; Transparency can be toggled through `toggle-transparency'. (default 90)
   dotspacemacs-active-transparency 90
   ;; A value from the range (0..100), in increasing opacity, which describes
   ;; the transparency level of a frame when it's inactive or deselected.
   ;; Transparency can be toggled through `toggle-transparency'. (default 90)
   dotspacemacs-inactive-transparency 90
   ;; If non nil show the titles of transient states. (default t)
   dotspacemacs-show-transient-state-title t
   ;; If non nil show the color guide hint for transient state keys. (default t)
   dotspacemacs-show-transient-state-color-guide t
   ;; If non nil unicode symbols are displayed in the mode line. (default t)
   dotspacemacs-mode-line-unicode-symbols t
   ;; If non nil smooth scrolling (native-scrolling) is enabled. Smooth
   ;; scrolling overrides the default behavior of Emacs which recenters point
   ;; when it reaches the top or bottom of the screen. (default t)
   dotspacemacs-smooth-scrolling t
   ;; If non nil line numbers are turned on in all `prog-mode' and `text-mode'
   ;; derivatives. If set to `relative', also turns on relative line numbers.
   ;; (default nil)
   dotspacemacs-line-numbers nil
   ;; Code folding method. Possible values are `evil' and `origami'.
   ;; (default 'evil)
   dotspacemacs-folding-method 'origami
   ;; If non-nil smartparens-strict-mode will be enabled in programming modes.
   ;; (default nil)
   dotspacemacs-smartparens-strict-mode nil
   ;; If non-nil pressing the closing parenthesis `)' key in insert mode passes
   ;; over any automatically added closing parenthesis, bracket, quote, etc…
   ;; This can be temporary disabled by pressing `C-q' before `)'. (default nil)
   dotspacemacs-smart-closing-parenthesis nil
   ;; Select a scope to highlight delimiters. Possible values are `any',
   ;; `current', `all' or `nil'. Default is `all' (highlight any scope and
   ;; emphasis the current one). (default 'all)
   dotspacemacs-highlight-delimiters 'all
   ;; If non nil, advise quit functions to keep server open when quitting.
   ;; (default nil)
   dotspacemacs-persistent-server nil
   ;; List of search tool executable names. Spacemacs uses the first installed
   ;; tool of the list. Supported tools are `ag', `pt', `ack' and `grep'.
   ;; (default '("ag" "pt" "ack" "grep"))
   dotspacemacs-search-tools '("ag" "pt" "ack" "grep")
   ;; The default package repository used if no explicit repository has been
   ;; specified with an installed package.
   ;; Not used for now. (default nil)
   dotspacemacs-default-package-repository nil
   ;; Delete whitespace while saving buffer. Possible values are `all'
   ;; to aggressively delete empty line and long sequences of whitespace,
   ;; `trailing' to delete only the whitespace at end of lines, `changed'to
   ;; delete only whitespace for changed lines or `nil' to disable cleanup.
   ;; (default nil)
   dotspacemacs-whitespace-cleanup nil
   ))

(defun dotspacemacs/user-init ()
  "Initialization function for user code.
It is called immediately after `dotspacemacs/init', before layer configuration
executes.
 This function is mostly useful for variables that need to be set
before packages are loaded. If you are unsure, you should try in setting them in
`dotspacemacs/user-config' first."

  ;; Tweak the garbage collector.
  (defun disable-gc-hook ()
    (setq gc-cons-threshold most-positive-fixnum))
  (defun enable-gc-hook ()
    (setq gc-cons-threshold 20000000)) ; 20 MB
  (add-hook 'minibuffer-setup-hook #'disable-gc-hook)
  (add-hook 'minibuffer-exit-hook #'enable-gc-hook)
  (add-hook 'after-init-hook #'enable-gc-hook)
  (disable-gc-hook)

  ;; Prefer to load non-compiled elisp files if they are newer than their
  ;; compiled equivalents. This is a prophylactic against loading outdated
  ;; compiled files. Requires Emacs >= 24.4.
  (setq load-prefer-newer t)

  ;; Tramp by default tries to look for the availability of various config
  ;; options by running ssh against the host `host.does.not.exist'
  ;; This can be prevented by defining the options before loading tramp.
  (setq tramp-ssh-controlmaster-options
        "-o ControlMaster=auto -o ControlPath='tramp.%%r@%%h:%%p' -o ControlPersist=no")

  ;; Enables C-n and C-p to cycle through previous searches.
  ;; This needs to be set before evil is loaded or it won't take effect.
  (setq evil-search-module 'evil-search)

  ;; Import additional variables from the shell profile.
  (with-eval-after-load 'exec-path-from-shell
    ;; Go
    (dolist (item '("GOBIN" "GOPATH" "GOROOT" "GO15VENDOREXPERIMENT"))
      (add-to-list 'exec-path-from-shell-variables item))
    ;; RVM
    (dolist (item '("GEM_HOME" "GEM_PATH"))
      (add-to-list 'exec-path-from-shell-variables item))
    ;; NVM
    (dolist (item '("NVM_BIN" "NVM_DIR" "NVM_PATH"))
      (add-to-list 'exec-path-from-shell-variables item)))
  )

(defun dotspacemacs/user-config ()
  "Configuration function for user code.
This function is called at the very end of Spacemacs initialization after
layers configuration.
This is the place where most of your configurations should be done. Unless it is
explicitly specified that a variable should be set before a package is loaded,
you should place your code here."

  ;; Flash the frame to indicate a bell.
  (setq visible-bell t)

  ;; Lets trust myself to not create problematic symlinks.
  (setq vc-follow-symlinks t)

  ;; Disable lockfiles (.#*).
  (setq create-lockfiles nil)

  ;; Do not add temporary files to the recentf list.
  (with-eval-after-load 'recentf
    (push "\\`/tmp/" recentf-exclude)
    (push "\\`/var/tmp/" recentf-exclude))

  ;; Mention the active buffer name in the title bar.
  (when window-system
    (setq frame-title-format '(buffer-file-name "Emacs: %b (%f)" "Emacs: %b")))

  ;; Various customization of helm.
  (setq helm-default-external-file-browser "xdg-open"
        helm-home-url "https://www.google.com/"
        helm-M-x-fuzzy-match t
        helm-raise-command "wmctrl -xa %s")

  ;; Enable adaptive helm sources.
  (require 'helm-adaptive)
  (setq helm-adaptive-history-file
        (concat spacemacs-cache-directory "helm-adaptive-history"))
  (helm-adaptive-mode 1)

  ;; Various customization of tramp.
  (setq tramp-default-user "maigner"
        tramp-default-method "ssh")

  ;; Open a dired buffer when switching to a project.
  (setq projectile-switch-project-action #'projectile-dired)

  ;; Mark files as executable if they contain a shebang.
  (add-hook 'after-save-hook #'executable-make-buffer-file-executable-if-script-p)

  ;; Having this disabled is annoying, totally messes up visual indentation.
  (setq-default truncate-lines t)

  ;; Tell paradox that we won't give it a GitHub token.
  (setq paradox-github-token t)

  ;; Reduce the list of supported VC backends. This improves performance as it
  ;; does less when trying to autodetect source control meta-data.
  (setq vc-handled-backends '(Git))

  ;; Disable highlight current line.
  (when global-hl-line-mode
    (global-hl-line-mode -1))

  ;; Disable xterm mouse mode, it makes it harder to copy-paste.
  ;; xterm-mouse-mode also causes issues in daemon mode.
  (when xterm-mouse-mode
    (xterm-mouse-mode -1))

  ;; When the name of the server socket has been customized, child shells need
  ;; to be told the new name so that emacsclient will always connect to the
  ;; Emacs instance it has been invoked from.
  (with-eval-after-load 'server
    (setenv "EMACS_SERVER_NAME" server-name))

  ;; Register directories with executables dedicated specifically for use by Emacs.
  (dolist (layer-path (cons configuration-layer-private-directory dotspacemacs-configuration-layer-path))
    (let ((layer-bin-path (concat layer-path "bin/")))
      (when (file-directory-p layer-bin-path)
        (let ((absolute-layer-bin-path (expand-file-name layer-bin-path)))
          (setenv "PATH" (concat
                          (replace-regexp-in-string "/$" "" absolute-layer-bin-path)
                          path-separator (getenv "PATH")))
          (push absolute-layer-bin-path exec-path)))))

  ;; A key binding to open a full buffer-size shell.
  (defun spacemacs/default-shell ()
    "Open the default shell."
    (interactive)
    (call-interactively shell-default-shell))
  (spacemacs/set-leader-keys "\"" #'spacemacs/default-shell)

  ;; For *most* languages I work with 2 space indent is the norm.
  (setq-default evil-shift-width 2)

  ;; Additional file mode associations.
  (add-to-list 'auto-mode-alist '("\\.gp\\'" . gnuplot-mode))
  (add-to-list 'auto-mode-alist '("\\.gpi\\'" . gnuplot-mode))

  ;; Remap helm-mini to helm-buffers-list.
  ;; Recent files are still available via "fr".
  (spacemacs/set-leader-keys "bb" #'helm-buffers-list)

  ;; Miscellaneous key bindings to built-in functions.
  (spacemacs/set-leader-keys "gd" #'vc-diff)
  (spacemacs/set-leader-keys "nm" #'next-match)
  (spacemacs/set-leader-keys "ou" #'spacemacs/avy-open-url)
  (spacemacs/set-leader-keys "qe" #'server-edit)
  (spacemacs/set-leader-keys "wx" #'kill-buffer-and-window)

  ;; Key bindings for auto-fill.
  (spacemacs/declare-prefix "of" "auto-fill")
  (spacemacs/set-leader-keys "ofa" #'auto-fill-mode)
  (spacemacs/set-leader-keys "ofc" #'set-fill-column)
  (spacemacs/set-leader-keys "ofp" #'set-fill-prefix)

  ;; Key bindings for compile.
  (spacemacs/declare-prefix "ce" "compile-errors")
  (spacemacs/set-leader-keys "cef" #'first-error)
  (spacemacs/set-leader-keys "cen" #'next-error)
  (spacemacs/set-leader-keys "cep" #'previous-error)

  (spacemacs|add-toggle subword-mode
    :status subword-mode
    :on (subword-mode)
    :off (subword-mode -1)
    :documentation "Toggle subword movement and editing (subword mode)."
    :evil-leader "tj")

  (spacemacs|add-toggle superword-mode
    :status superword-mode
    :on (superword-mode)
    :off (superword-mode -1)
    :documentation "Toggle superword movement and editing (superword mode)."
    :evil-leader "tJ")

  (defun show-and-copy-project-dirname ()
    "Show and copy the full path to the directory of the current file in the minibuffer."
    (interactive)
    (let ((project-root (projectile-project-root)))
      (message (kill-new project-root))))
  (spacemacs/set-leader-keys "pY" #'show-and-copy-project-dirname)

  (defun show-and-copy-buffer-dirname ()
    "Show and copy the full path to the directory of the current file in the minibuffer."
    (interactive)
    ;; list-buffers-directory is the variable set in dired buffers
    (let ((file-name (or (buffer-file-name) list-buffers-directory)))
      (if file-name
          (message (kill-new (if (file-directory-p file-name)
                                 file-name
                               (file-name-directory file-name))))
        (error "Buffer not visiting a file"))))
  (spacemacs/set-leader-keys "fY" #'show-and-copy-buffer-dirname)

  ;; http://stackoverflow.com/questions/30697523/how-to-get-emacs-to-sort-lines-by-length
  (defun sort-lines-by-length (reverse beg end)
    "Sort lines by length."
    (interactive "P\nr")
    (save-excursion
      (save-restriction
        (narrow-to-region beg end)
        (goto-char (point-min))
        (let ;; To make `end-of-line' and etc. to ignore fields.
            ((inhibit-field-text-motion t))
          (sort-subr reverse 'forward-line 'end-of-line nil nil
                     (lambda (l1 l2)
                       (apply #'< (mapcar (lambda (range) (- (cdr range) (car range)))
                                          (list l1 l2)))))))))
  (spacemacs/set-leader-keys "xll" #'sort-lines-by-length)
  (spacemacs/set-leader-keys "xln" #'sort-numeric-fields)

  ;; Make Flycheck a little bit less eager to lint.
  (with-eval-after-load 'flycheck
    (setq flycheck-check-syntax-automatically
          (delete 'new-line flycheck-check-syntax-automatically)))

  ;; Scroll the compilation buffer to the first error.
  (setq compilation-scroll-output 'first-error)

  (with-eval-after-load 'compile
    (defun close-compile-buffer-if-successful (buffer string)
      "Bury a compilation buffer if succeeded without warnings "
      (if (and
           (string-match "compilation" (buffer-name buffer))
           (string-match "finished" string)
           (not
            (with-current-buffer buffer
              (goto-char (point-min))
              (search-forward "exited abnormally" nil t))))
          (spacemacs/close-compilation-window)))
    (add-hook 'compilation-finish-functions #'close-compile-buffer-if-successful))

  ;; Support for colors with ANSI escape sequences compile output buffer.
  (with-eval-after-load 'compile
    (autoload 'ansi-color-apply-on-region "ansi-color")
    (defun mfa-colorize-compilation-buffer ()
      (when (eq major-mode 'compilation-mode)
        (ansi-color-apply-on-region compilation-filter-start (point-max))))
    (add-hook 'compilation-filter-hook #'mfa-colorize-compilation-buffer))

  ;; Use helm as a replacement for browse-kill-ring.
  (defadvice yank-pop (around browse-kill-ring-maybe (arg) activate)
    "If last action was not a yank, run `browse-kill-ring' instead."
    ; yank-pop has an (interactive "*p") form which does not allow
    ; it to run in a read-only buffer.  We want browse-kill-ring to
    ; be allowed to run in a read only buffer, so we change the
    ; interactive form here.  In that case, we need to
    ; barf-if-buffer-read-only if we're going to call yank-pop with
    ; ad-do-it
    (interactive "p")
    (if (not (eq last-command 'yank))
        (helm-show-kill-ring)
      (barf-if-buffer-read-only)
      ad-do-it))
  (with-eval-after-load 'evil-common
    (defadvice evil-paste-pop (around evil-browse-kill-ring-maybe (arg) activate)
      "If last action was not a yank, run `browse-kill-ring' instead."
      ; evil-paste-pop has an (interactive "*p") form which does not allow
      ; it to run in a read-only buffer.  We want browse-kill-ring to
      ; be allowed to run in a read only buffer, so we change the
      ; interactive form here.  In that case, we need to
      ; barf-if-buffer-read-only if we're going to call evil-paste-pop with
      ; ad-do-it
      (interactive "p")
      (if (not (memq last-command '(yank evil-paste-before evil-paste-pop evil-paste-after evil-visual-paste)))
          (helm-show-kill-ring)
        (barf-if-buffer-read-only)
        ad-do-it)))

  ;; Teach Emacs how to colorize Maven output.
  (with-eval-after-load 'compile
    (push '(maven-warning "^\\[WARN\\(?:ING\\)?\\] \\(\\(.+?\\):\\[\\([0-9]+\\),\\([0-9]+\\)\\]\\)" 2 3 4 1 1) compilation-error-regexp-alist-alist)
    (push 'maven-warning compilation-error-regexp-alist)
    (push '(maven-error "^\\[ERROR\\] \\(\\(.+?\\):\\[\\([0-9]+\\),\\([0-9]+\\)\\]\\)" 2 3 4 2 1) compilation-error-regexp-alist-alist)
    (push 'maven-error compilation-error-regexp-alist))

  ;; Give dired the same "select window by prefix" functionality as neotree.
  (defun dired-find-file-by-prefix (&optional arg)
    (interactive "P")
    (let ((file (dired-get-file-for-visit))
          (dired-window (selected-window)))
      (when (and (integerp arg)
                 (boundp 'window-numbering-mode)
                 (symbol-value window-numbering-mode)
                 (fboundp 'select-window-by-number))
        (select-window-by-number arg))
      (if (not (file-directory-p file))
          (find-file file)
        (let ((find-file-run-dired t))
          (if (eq dired-window (selected-window))
              (find-alternate-file file)
            (find-file file))))))
  (with-eval-after-load 'dired
    (evil-define-key 'evilified dired-mode-map
      (kbd "RET") #'dired-find-file-by-prefix))

  ;; X clipboard support for Emacs in terminal.
  (defvar xclip-saved-icf nil
    "Saved value of `interprogram-cut-function'.")
  (defvar xclip-saved-ipf nil
    "Saved value of `interprogram-paste-function'.")
  (defvar xclip-saved-xsec nil
    "Saved value of `x-select-enable-clipboard'.")
  (defun xclip-cut-function (text)
    (if window-system
        (x-select-text text)
      (when (getenv "DISPLAY"))
        (with-temp-buffer
          (insert text)
          (call-process-region (point-min) (point-max)
                               "xclip" nil 0 nil "-silent" "-i" "-selection" "clipboard"))))
  (defun xclip-paste-function ()
    (if window-system
        (x-selection-value)
      (when (getenv "DISPLAY")
        (let ((xclip-output (shell-command-to-string "xclip -o -selection clipboard")))
          (unless (string= (car kill-ring) xclip-output)
            xclip-output)))))
  (define-minor-mode xclip-mode
    "Turn on `xclip-mode'.
  When called interactively with no prefix argument this command
  toggles the mode.  With a prefix argument, it enables the mode
  if the argument is positive and otherwise disables the mode.
  When called from Lisp, this command enables the mode if the
  argument is omitted or nil, and toggles the mode if the argument
  is 'toggle."
   nil " X" nil
    :global t
    (cond
     (xclip-mode
      (setq xclip-saved-icf interprogram-cut-function)
      (setq xclip-saved-ipf interprogram-paste-function)
      (when (boundp 'x-select-enable-clipboard)
        (setq xclip-saved-xsec x-select-enable-clipboard))
      (setq interprogram-cut-function 'xclip-cut-function)
      (setq interprogram-paste-function 'xclip-paste-function)
      (setq x-select-enable-clipboard t))
     (t
      (setq interprogram-cut-function xclip-saved-icf)
      (setq interprogram-paste-function xclip-saved-ipf)
      (when (boundp 'x-select-enable-clipboard)
        (setq x-select-enable-clipboard xclip-saved-xsec))
      (setq xclip-saved-icf nil)
      (setq xclip-saved-ipf nil)
      (setq xclip-saved-xsec nil))))
  (spacemacs|add-toggle xclip-mode
    :status xclip-mode
    :on (xclip-mode)
    :off (xclip-mode -1)
    :documentation "Enable X clipboard support on the terminal."
    :evil-leader "tx")
  (spacemacs|hide-lighter xclip-mode)
  (when (and (not window-system) (getenv "DISPLAY"))
    (xclip-mode))
  )

;; Do not write anything past this comment. This is where Emacs will
;; auto-generate custom variable definitions.
