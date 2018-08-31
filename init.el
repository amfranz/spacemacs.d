;; -*- mode: emacs-lisp; lexical-binding: t -*-
;; This file is loaded by Spacemacs at startup.
;; It must be stored in your home directory.


(defun dotspacemacs/layers ()
  "Layer configuration:
This function should only modify configuration layer settings."
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
     ;; Uncomment some layer names and press `SPC f e R' (Vim style) or
     ;; `M-m f e R' (Emacs style) to install them.
     ;; ----------------------------------------------------------------
     ;; helm
     ;; auto-completion
     ;; better-defaults
     ;; emacs-lisp
     ;; git
     ;; markdown
     ;; neotree
     ;; org
     ;; (shell :variables
     ;;        shell-default-height 30
     ;;        shell-default-position 'bottom)
     ;; spell-checking
     ;; syntax-checking
     ;; version-control

     ;; layers by spacemacs
     (ansible :variables
              ;;
              ;; We do not really turn off automatic decryption of the vault
              ;; files, we just don't let Spacemacs register the hooks and
              ;; instead we'll do it ourselves in the mfa-ansible layer.
              ;;
              ;; We do it ourselves because Spacemacs is hooking the initial
              ;; automatic decryption into the activation of ansible mode. This
              ;; too early, at this point directory-local variables are not
              ;; loaded yet, so an ansible::vault-password-file set as directory
              ;; local variable won't work.
              ;;
              ;; Our own logic hooks the initial decryption into the
              ;; hack-local-variables-hook instead, a hook which is invoked
              ;; after the directory local variables have been loaded.
              ;;
              ansible-auto-encrypt-descrypt nil)
     (auto-completion :variables
                      ;; There is no good way to insert a newline when the popup
                      ;; is present. Let's disable the completion on RET.
                      auto-completion-return-key-behavior nil
                      ;; Navigating org mode tables can be difficult, if you
                      ;; press TAB to go to the next cell it will autocomplete
                      ;; instead. Let's disable the completion on TAB.
                      auto-completion-tab-key-behavior nil
                      ;; Since neither RET nor TAB will trigger completion any
                      ;; more, the interaction with autocompletion has to be
                      ;; done via the Vim/Evil keybindings:
                      ;;   C-n => next
                      ;;   C-p => previous
                      ;;   C-l => complete
                      )
     better-defaults
     bm
     (c-c++ :variables
            c-c++-enable-c++11 t
            c-c++-enable-clang-support t)
     clojure
     colors
     copy-as-format
     cscope
     csv
     (dash :variables
           helm-dash-docset-newpath "~/.local/share/Zeal/Zeal/docsets")
     docker
     emacs-lisp
     erlang
     evil-snipe
     (git :variables
          git-magit-status-fullscreen t)
     github
     (go :variables
         go-format-before-save t
         go-tab-width 4
         go-use-gometalinter t
         godoc-at-point-function 'godoc-gogetdoc)
     graphviz
     (gtags :variables
            gtags-enable-by-default nil)
     helm
     (html :variables
           web-fmt-tool 'prettier)
     (ibuffer :variables
              ibuffer-group-buffers-by 'projects)
     imenu-list
     (javascript :variables
                 javascript-fmt-tool 'prettier
                 js-indent-level 2)
     (json :variables
           json-fmt-tool 'prettier)
     lua
     (markdown :variables
               markdown-live-preview-engine 'vmd)
     ;; mu4e
     nginx
     (org :variables
          org-enable-bootstrap-support t
          org-enable-github-support t
          org-enable-reveal-js-support t
          org-enable-org-journal-support t
          org-projectile-file "notes.org")
     php
     plantuml
     prettier
     prodigy
     python
     restclient
     (ruby :variables
           ruby-test-runner 'rspec
           ruby-version-manager 'rbenv)
     (rust :variables
           rust-format-on-save t)
     search-engine
     (shell :variables
            shell-default-shell 'eshell)
     shell-scripts
     (sql :variables
          sql-capitalize-keywords t)
     syntax-checking
     systemd
     treemacs
     typescript
     vimscript
     vinegar
     version-control
     yaml

     ;; my custom layers
     mfa-alpine
     mfa-angularjs
     mfa-ansible
     mfa-artist
     mfa-atomic-chrome
     mfa-bookmarks
     mfa-checkbashisms
     mfa-dash
     mfa-devdocs
     mfa-diff
     mfa-dired
     mfa-direnv
     mfa-dokuwiki
     mfa-dtrt-indent
     mfa-dynamic-ruler
     mfa-editorconfig
     mfa-el-patch
     mfa-environment
     mfa-evil
     mfa-evil-mc
     mfa-evil-quickscope
     mfa-evil-textobj-anyblock
     mfa-fancy-narrow
     mfa-fill-function-arguments
     mfa-go
     mfa-google-this
     mfa-hcl
     mfa-highlight-symbols
     mfa-javascript
     mfa-journal
     mfa-json
     mfa-lice
     mfa-make-mode
     mfa-markdown
     ;; mfa-mu4e
     mfa-multi-line
     mfa-mwim
     mfa-org
     mfa-org-trello
     mfa-pass
     mfa-prodigy
     mfa-projectile
     mfa-python
     mfa-quickrun
     mfa-ruby
     mfa-shell
     mfa-shell-scripts
     mfa-string-edit
     mfa-tmux
     mfa-sqlformat
     mfa-treemacs
     mfa-vagrant
     mfa-vdiff
     mfa-xml
     mfa-yasnippet

     ;; TODO
     ;; bpr
     ;; fzf
     )

   ;; List of additional packages that will be installed without being
   ;; wrapped in a layer. If you need some configuration for these
   ;; packages, then consider creating a layer. You can also put the
   ;; configuration in `dotspacemacs/user-config'.
   ;; To use a local version of a package, use the `:location' property:
   ;; '(your-package :location "~/path/to/your-package/")
   ;; Also include the dependencies as they will not be resolved automatically.
   dotspacemacs-additional-packages '()

   ;; A list of packages that cannot be updated.
   dotspacemacs-frozen-packages '()

   ;; A list of packages that will not be installed and loaded.
   dotspacemacs-excluded-packages '()

   ;; Defines the behaviour of Spacemacs when installing packages.
   ;; Possible values are `used-only', `used-but-keep-unused' and `all'.
   ;; `used-only' installs only explicitly used packages and deletes any unused
   ;; packages as well as their unused dependencies. `used-but-keep-unused'
   ;; installs only the used packages but won't delete unused ones. `all'
   ;; installs *all* packages supported by Spacemacs and never uninstalls them.
   ;; (default is `used-only')
   dotspacemacs-install-packages 'used-only))

(defun dotspacemacs/init ()
  "Initialization:
This function is called at the very beginning of Spacemacs startup,
before layer configuration.
It should only modify the values of Spacemacs settings."
  ;; This setq-default sexp is an exhaustive list of all the supported
  ;; spacemacs settings.
  (setq-default
   ;; If non-nil then enable support for the portable dumper. You'll need
   ;; to compile Emacs 27 from source following the instructions in file
   ;; EXPERIMENTAL.org at to root of the git repository.
   ;; (default nil)
   dotspacemacs-enable-emacs-pdumper nil

   ;; File path pointing to emacs 27.1 executable compiled with support
   ;; for the portable dumper (this is currently the branch pdumper).
   ;; (default "emacs-27.0.50")
   dotspacemacs-emacs-pdumper-executable-file "emacs-27.0.50"

   ;; Name of the Spacemacs dump file. This is the file will be created by the
   ;; portable dumper in the cache directory under dumps sub-directory.
   ;; To load it when starting Emacs add the parameter `--dump-file'
   ;; when invoking Emacs 27.1 executable on the command line, for instance:
   ;;   ./emacs --dump-file=~/.emacs.d/.cache/dumps/spacemacs.pdmp
   ;; (default spacemacs.pdmp)
   dotspacemacs-emacs-dumper-dump-file "spacemacs.pdmp"

   ;; If non-nil ELPA repositories are contacted via HTTPS whenever it's
   ;; possible. Set it to nil if you have no way to use HTTPS in your
   ;; environment, otherwise it is strongly recommended to let it set to t.
   ;; This variable has no effect if Emacs is launched with the parameter
   ;; `--insecure' which forces the value of this variable to nil.
   ;; (default t)
   dotspacemacs-elpa-https t

   ;; Maximum allowed time in seconds to contact an ELPA repository.
   ;; (default 5)
   dotspacemacs-elpa-timeout 5

   ;; Set `gc-cons-threshold' and `gc-cons-percentage' when startup finishes.
   ;; This is an advanced option and should not be changed unless you suspect
   ;; performance issues due to garbage collection operations.
   ;; (default '(100000000 0.1))
   dotspacemacs-gc-cons `(,most-positive-fixnum 0.1)

   ;; If non-nil then Spacelpa repository is the primary source to install
   ;; a locked version of packages. If nil then Spacemacs will install the
   ;; latest version of packages from MELPA. (default nil)
   dotspacemacs-use-spacelpa nil

   ;; If non-nil then verify the signature for downloaded Spacelpa archives.
   ;; (default nil)
   dotspacemacs-verify-spacelpa-archives nil

   ;; If non-nil then spacemacs will check for updates at startup
   ;; when the current branch is not `develop'. Note that checking for
   ;; new versions works via git commands, thus it calls GitHub services
   ;; whenever you start Emacs. (default nil)
   dotspacemacs-check-for-update nil

   ;; If non-nil, a form that evaluates to a package directory. For example, to
   ;; use different package directories for different Emacs versions, set this
   ;; to `emacs-version'. (default 'emacs-version)
   dotspacemacs-elpa-subdirectory nil

   ;; One of `vim', `emacs' or `hybrid'.
   ;; `hybrid' is like `vim' except that `insert state' is replaced by the
   ;; `hybrid state' with `emacs' key bindings. The value can also be a list
   ;; with `:variables' keyword (similar to layers). Check the editing styles
   ;; section of the documentation for details on available variables.
   ;; (default 'vim)
   dotspacemacs-editing-style 'vim

   ;; If non-nil output loading progress in `*Messages*' buffer. (default nil)
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
   ;; `recents' `bookmarks' `projects' `agenda' `todos'.
   ;; List sizes may be nil, in which case
   ;; `spacemacs-buffer-startup-lists-length' takes effect.
   dotspacemacs-startup-lists '((recents . 5)
                                (projects . 7))

   ;; True if the home buffer should respond to resize events. (default t)
   dotspacemacs-startup-buffer-responsive t

   ;; Default major mode of the scratch buffer (default `text-mode')
   dotspacemacs-scratch-mode 'text-mode

   ;; Initial message in the scratch buffer, such as "Welcome to Spacemacs!"
   ;; (default nil)
   dotspacemacs-initial-scratch-message nil

   ;; List of themes, the first of the list is loaded when spacemacs starts.
   ;; Press `SPC T n' to cycle to the next theme in the list (works great
   ;; with 2 themes variants, one dark and one light)
   dotspacemacs-themes '(zenburn
                         leuven
                         spacemacs-dark
                         spacemacs-light)

   ;; Set the theme for the Spaceline. Supported themes are `spacemacs',
   ;; `all-the-icons', `custom', `vim-powerline' and `vanilla'. The first three
   ;; are spaceline themes. `vanilla' is default Emacs mode-line. `custom' is a
   ;; user defined themes, refer to the DOCUMENTATION.org for more info on how
   ;; to create your own spaceline theme. Value can be a symbol or list with\
   ;; additional properties.
   ;; (default '(spacemacs :separator wave :separator-scale 1.5))
   dotspacemacs-mode-line-theme '(spacemacs :separator wave :separator-scale 1.2)

   ;; If non-nil the cursor color matches the state color in GUI Emacs.
   ;; (default t)
   dotspacemacs-colorize-cursor-according-to-state t

   ;; Default font, or prioritized list of fonts. `powerline-scale' allows to
   ;; quickly tweak the mode-line size to make separators look not too crappy.
   dotspacemacs-default-font `("DejaVu Sans Mono for Powerline"
                               :size ,(* (dpi-adjusted-font-size) (display-scaling-factor))
                               :weight normal
                               :width normal)

   ;; The leader key (default "SPC")
   dotspacemacs-leader-key "SPC"

   ;; The key used for Emacs commands `M-x' (after pressing on the leader key).
   ;; (default "SPC")
   dotspacemacs-emacs-command-key "SPC"

   ;; The key used for Vim Ex commands (default ":")
   dotspacemacs-ex-command-key ":"

   ;; The leader key accessible in `emacs state' and `insert state'
   ;; (default "M-m")
   dotspacemacs-emacs-leader-key "M-m"

   ;; Major mode leader key is a shortcut key which is the equivalent of
   ;; pressing `<leader> m`. Set it to `nil` to disable it. (default ",")
   dotspacemacs-major-mode-leader-key ","

   ;; Major mode leader key accessible in `emacs state' and `insert state'.
   ;; (default "C-M-m")
   dotspacemacs-major-mode-emacs-leader-key "C-M-m"

   ;; These variables control whether separate commands are bound in the GUI to
   ;; the key pairs `C-i', `TAB' and `C-m', `RET'.
   ;; Setting it to a non-nil value, allows for separate commands under `C-i'
   ;; and TAB or `C-m' and `RET'.
   ;; In the terminal, these pairs are generally indistinguishable, so this only
   ;; works in the GUI. (default nil)
   dotspacemacs-distinguish-gui-tab t

   ;; Name of the default layout (default "Default")
   dotspacemacs-default-layout-name "Default"

   ;; If non-nil the default layout name is displayed in the mode-line.
   ;; (default nil)
   dotspacemacs-display-default-layout nil

   ;; If non-nil then the last auto saved layouts are resumed automatically upon
   ;; start. (default nil)
   dotspacemacs-auto-resume-layouts nil

   ;; If non-nil, auto-generate layout name when creating new layouts. Only has
   ;; effect when using the "jump to layout by number" commands. (default nil)
   dotspacemacs-auto-generate-layout-names nil

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

   ;; If non-nil, the paste transient-state is enabled. While enabled, after you
   ;; paste something, pressing `C-j' and `C-k' several times cycles through the
   ;; elements in the `kill-ring'. (default nil)
   dotspacemacs-enable-paste-transient-state t

   ;; Which-key delay in seconds. The which-key buffer is the popup listing
   ;; the commands bound to the current keystroke sequence. (default 0.4)
   dotspacemacs-which-key-delay 0.4

   ;; Which-key frame position. Possible values are `right', `bottom' and
   ;; `right-then-bottom'. right-then-bottom tries to display the frame to the
   ;; right; if there is insufficient space it displays it at the bottom.
   ;; (default 'bottom)
   dotspacemacs-which-key-position 'bottom

   ;; Control where `switch-to-buffer' displays the buffer. If nil,
   ;; `switch-to-buffer' displays the buffer in the current window even if
   ;; another same-purpose window is available. If non-nil, `switch-to-buffer'
   ;; displays the buffer in a same-purpose window even if the buffer can be
   ;; displayed in the current window. (default nil)
   dotspacemacs-switch-to-buffer-prefers-purpose nil

   ;; If non-nil a progress bar is displayed when spacemacs is loading. This
   ;; may increase the boot time on some systems and emacs builds, set it to
   ;; nil to boost the loading time. (default t)
   dotspacemacs-loading-progress-bar nil

   ;; If non-nil the frame is fullscreen when Emacs starts up. (default nil)
   ;; (Emacs 24.4+ only)
   dotspacemacs-fullscreen-at-startup nil

   ;; If non-nil `spacemacs/toggle-fullscreen' will not use native fullscreen.
   ;; Use to disable fullscreen animations in OSX. (default nil)
   dotspacemacs-fullscreen-use-non-native nil

   ;; If non-nil the frame is maximized when Emacs starts up.
   ;; Takes effect only if `dotspacemacs-fullscreen-at-startup' is nil.
   ;; (default nil) (Emacs 24.4+ only)
   dotspacemacs-maximized-at-startup nil

   ;; A value from the range (0..100), in increasing opacity, which describes
   ;; the transparency level of a frame when it's active or selected.
   ;; Transparency can be toggled through `toggle-transparency'. (default 90)
   dotspacemacs-active-transparency 90

   ;; A value from the range (0..100), in increasing opacity, which describes
   ;; the transparency level of a frame when it's inactive or deselected.
   ;; Transparency can be toggled through `toggle-transparency'. (default 90)
   dotspacemacs-inactive-transparency 90

   ;; If non-nil show the titles of transient states. (default t)
   dotspacemacs-show-transient-state-title t

   ;; If non-nil show the color guide hint for transient state keys. (default t)
   dotspacemacs-show-transient-state-color-guide t

   ;; If non-nil unicode symbols are displayed in the mode line.
   ;; If you use Emacs as a daemon and wants unicode characters only in GUI set
   ;; the value to quoted `display-graphic-p'. (default t)
   dotspacemacs-mode-line-unicode-symbols (or (display-graphic-p)
                                              (string-equal (getenv "EMACS_SOCKET_NAME") "server-x"))

   ;; If non-nil smooth scrolling (native-scrolling) is enabled. Smooth
   ;; scrolling overrides the default behavior of Emacs which recenters point
   ;; when it reaches the top or bottom of the screen. (default t)
   dotspacemacs-smooth-scrolling t

   ;; Control line numbers activation.
   ;; If set to `t' or `relative' line numbers are turned on in all `prog-mode' and
   ;; `text-mode' derivatives. If set to `relative', line numbers are relative.
   ;; This variable can also be set to a property list for finer control:
   ;; '(:relative nil
   ;;   :disabled-for-modes dired-mode
   ;;                       doc-view-mode
   ;;                       markdown-mode
   ;;                       org-mode
   ;;                       pdf-view-mode
   ;;                       text-mode
   ;;   :size-limit-kb 1000)
   ;; (default nil)
   dotspacemacs-line-numbers nil

   ;; Code folding method. Possible values are `evil' and `origami'.
   ;; (default 'evil)
   dotspacemacs-folding-method 'origami

   ;; If non-nil `smartparens-strict-mode' will be enabled in programming modes.
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

   ;; If non-nil, start an Emacs server if one is not already running.
   ;; (default nil)
   dotspacemacs-enable-server (getenv "EMACS_SOCKET_NAME")

   ;; Set the emacs server socket location.
   ;; If nil, uses whatever the Emacs default is, otherwise a directory path
   ;; like \"~/.emacs.d/server\". It has no effect if
   ;; `dotspacemacs-enable-server' is nil.
   ;; (default nil)
   dotspacemacs-server-socket-dir nil

   ;; If non-nil, advise quit functions to keep server open when quitting.
   ;; (default nil)
   dotspacemacs-persistent-server nil

   ;; List of search tool executable names. Spacemacs uses the first installed
   ;; tool of the list. Supported tools are `rg', `ag', `pt', `ack' and `grep'.
   ;; (default '("rg" "ag" "pt" "ack" "grep"))
   dotspacemacs-search-tools '("rg" "ag" "pt" "ack" "grep")

   ;; Format specification for setting the frame title.
   ;; %a - the `abbreviated-file-name', or `buffer-name'
   ;; %t - `projectile-project-name'
   ;; %I - `invocation-name'
   ;; %S - `system-name'
   ;; %U - contents of $USER
   ;; %b - buffer name
   ;; %f - visited file name
   ;; %F - frame name
   ;; %s - process status
   ;; %p - percent of buffer above top of window, or Top, Bot or All
   ;; %P - percent of buffer above bottom of window, perhaps plus Top, or Bot or All
   ;; %m - mode name
   ;; %n - Narrow if appropriate
   ;; %z - mnemonics of buffer, terminal, and keyboard coding systems
   ;; %Z - like %z, but including the end-of-line format
   ;; (default "%I@%S")
   dotspacemacs-frame-title-format "%I@%S"

   ;; Format specification for setting the icon title format
   ;; (default nil - same as frame-title-format)
   dotspacemacs-icon-title-format nil

   ;; Delete whitespace while saving buffer. Possible values are `all'
   ;; to aggressively delete empty line and long sequences of whitespace,
   ;; `trailing' to delete only the whitespace at end of lines, `changed' to
   ;; delete only whitespace for changed lines or `nil' to disable cleanup.
   ;; (default nil)
   dotspacemacs-whitespace-cleanup 'changed

   ;; Either nil or a number of seconds. If non-nil zone out after the specified
   ;; number of seconds. (default nil)
   dotspacemacs-zone-out-when-idle nil

   ;; Run `spacemacs/prettify-org-buffer' when
   ;; visiting README.org files of Spacemacs.
   ;; (default nil)
   dotspacemacs-pretty-docs nil))

(defun dotspacemacs/user-env ()
  "Environment variables setup.
This function defines the environment variables for your Emacs session. By
default it calls `spacemacs/load-spacemacs-env' which loads the environment
variables declared in `~/.spacemacs.env' or `~/.spacemacs.d/.spacemacs.env'.
See the header of this file for more information.")

(defun dotspacemacs/user-init ()
  "Initialization for user code:
This function is called immediately after `dotspacemacs/init', before layer
configuration.
It is mostly for variables that should be set before packages are loaded.
If you are unsure, try setting them in `dotspacemacs/user-config' first."

  ;; Prefer to load non-compiled elisp files if they are newer than their
  ;; compiled equivalents. This is a prophylactic against loading outdated
  ;; compiled files. Requires Emacs >= 24.4.
  (setq load-prefer-newer t)

  ;; Add this projects library directory to the load path.
  (add-to-list 'load-path (concat dotspacemacs-directory "lib/"))

  ;; Garbage collect only during idle times.
  (require 'gc-idle)
  (add-hook 'spacemacs-post-user-config-hook #'gc-idle-enable)
  (seq-do #'gc-idle-exempt '(configuration-layer//install-packages
                             spacemacs/recompile-elpa))

  ;; Make frames larger than the conservative default size.
  (let ((goal-height 47)
        (goal-width 164))
    (setq default-frame-alist `((width . ,goal-width)
                                (height . ,goal-height)))
    ;; Also resize the initial frame to the goal size, but enlarge the frame in
    ;; all directions so that the center of the window does remain where it is.
    (when (display-graphic-p)
      (set-frame-position
       nil
       (max 0
            (-
             (car (frame-position))
             (*
              (/ (- goal-width (frame-width)) 2)
              (/ (frame-pixel-width) (frame-width)))))
       (max 0
            (-
             (cdr (frame-position))
             (*
              (/ (- goal-height (frame-height)) 2)
              (/ (frame-pixel-height) (frame-height))))))
      (set-frame-size nil goal-width goal-height)))

  ;; Disable vertical scroll bars that appear on non-initial frames.
  ;; https://emacs.stackexchange.com/questions/23773/disable-scrollbar-on-new-frame
  (push '(vertical-scroll-bars . nil) default-frame-alist)

  ;; Keep customizations in a separate file that is not under version control.
  (setq custom-file (concat dotspacemacs-directory "custom.el"))
  (load custom-file)

  ;; Tramp by default tries to look for the availability of various config
  ;; options by running ssh against the host `host.does.not.exist'
  ;; This can be prevented by defining the options before loading tramp.
  (setq tramp-ssh-controlmaster-options
        "-o ControlMaster=auto -o ControlPath='tramp.%%r@%%h:%%p' -o ControlPersist=no")

  ;; Enables C-n and C-p to cycle through previous searches.
  ;; This needs to be set before evil is loaded or it won't take effect.
  (setq evil-search-module 'evil-search)

  (defun eterm-256color-package--description-file (dir)
    "Fixes the guess of the package description file for the `eterm-256color' package.

  This is a replacement for `package--description-file' in `subr.el'. The only
  change is that the regular expression is anchored at the end."
    (concat (let ((subdir (file-name-nondirectory
                           (directory-file-name dir))))
              (if (string-match "\\([^.].*?\\)-\\([0-9]+\\(?:[.][0-9]+\\|\\(?:pre\\|beta\\|alpha\\)[0-9]+\\)*\\)\\'" subdir)
                  (match-string 1 subdir) subdir))
            "-pkg.el"))
  (advice-add 'package--description-file :override #'eterm-256color-package--description-file))

(defun dotspacemacs/user-load ()
  "Library to load while dumping.
This function is called only while dumping Spacemacs configuration. You can
`require' or `load' the libraries of your choice that will be included in the
dump.")

(defun dotspacemacs/user-config ()
  "Configuration for user code:
This function is called at the very end of Spacemacs startup, after layer
configuration.
Put your configuration code here, except for variables that should be set
before packages are loaded."

  ;; This avoids graphical artifacts in the mode line with the first graphical
  ;; client. Computing the mode line height does font measurements, which are
  ;; not working properly until the display system is initialized.
  (when (daemonp)
    (defun adjust-powerline-height (frame)
      (with-selected-frame frame
        (when (display-graphic-p)
          (setq powerline-height (spacemacs/compute-mode-line-height))
          (require 'spaceline)
          (spaceline-compile)
          (remove-hook 'after-make-frame-functions #'adjust-powerline-height))))
    (add-hook 'after-make-frame-functions #'adjust-powerline-height))

  ;; Flash the frame to indicate a bell.
  (setq visible-bell t)

  ;; Lets trust myself to not create problematic symlinks.
  (setq vc-follow-symlinks t)

  ;; Disable lockfiles (.#*).
  (setq create-lockfiles nil)

  ;; Customize file backups.
  (setq backup-by-copying t
        backup-directory-alist `(("." . ,(concat spacemacs-cache-directory "backups/")))
        delete-old-versions t
        kept-new-versions 2
        kept-old-versions 2
        version-control t)

  ;; Do not add temporary files to the recentf list.
  (with-eval-after-load 'recentf
    (setq recentf-exclude
          (append `("\\`/dev/"
                    "\\`/tmp/"
                    "\\`/var/tmp/")
                  recentf-exclude)))

  ;; Mention the active buffer name in the title bar.
  (defun frame-title ()
    (concat
     (buffer-name)
     (when (or buffer-file-name (eq major-mode 'dired-mode))
       (concat " (" (string-remove-suffix "/" (abbreviate-file-name default-directory)) ")"))
     " - Emacs"))
  (require 's)
  (setq frame-title-format '((:eval (s-replace "%" "%%" (frame-title)))))
  (defun xterm-supports-title ()
    (and (not (display-graphic-p))
         (let ((term (frame-parameter nil 'tty-type)))
           (or (string-prefix-p "xterm" term)
               (string-prefix-p "konsole" term)
               (string-prefix-p "screen" term)
               (string-prefix-p "tmux" term)))))
  (defun xterm-update-title ()
    (when (xterm-supports-title)
      (let ((old-title (frame-parameter nil 'xterm-title))
            (new-title (frame-title)))
        (unless (string-equal old-title new-title)
          (send-string-to-terminal (concat "\033]0;" new-title "\007"))
          (modify-frame-parameters nil `((xterm-title . ,new-title)))))))
  (add-hook 'buffer-list-update-hook #'xterm-update-title)

  ;; Various customization of helm.
  (setq helm-default-external-file-browser "xdg-open"
        helm-home-url "https://www.google.com/"
        helm-M-x-fuzzy-match t
        helm-raise-command "wmctrl -xa %s"
        helm-always-two-windows nil)

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

  ;; Adjust thresholds for sensible window splitting.
  (setq split-height-threshold 40
        split-width-threshold 120)

  ;; Tell paradox that we won't give it a GitHub token.
  (setq paradox-github-token t)

  ;; Reduce the list of supported VC backends. This improves performance as it
  ;; does less when trying to autodetect source control meta-data.
  (setq vc-handled-backends '(Git))

  ;; Hide dired-omit-mode lighter, it is distracting.
  (with-eval-after-load 'dired-x
    (defun mfa//hide-dired-omit-mode-lighter ()
      (spacemacs|hide-lighter dired-omit-mode))
    (advice-add 'dired-omit-startup
                :after #'mfa//hide-dired-omit-mode-lighter))

  ;; Disable highlight current line, it is distracting.
  (when global-hl-line-mode
    (global-hl-line-mode -1))

  ;; Add ~/spacemacs.d/bin/ to the executable search path.
  (let ((bin-path (concat dotspacemacs-directory "bin/")))
    (setenv "PATH" (concat bin-path path-separator (getenv "PATH")))
    (push bin-path exec-path))

  ;; For *most* languages I work with 2 space indent is the norm.
  (setq-default evil-shift-width 2)

  ;; Additional file mode associations.
  (add-to-list 'auto-mode-alist '("\\.gp\\'" . gnuplot-mode))
  (add-to-list 'auto-mode-alist '("\\.gpi\\'" . gnuplot-mode))

  ;; Remap helm-mini to helm-buffers-list.
  ;; Recent files are still available via "fr".
  (spacemacs/set-leader-keys "bb" #'helm-buffers-list)

  ;; Miscellaneous key bindings to built-in functions.
  (spacemacs/set-leader-keys "qe" #'server-edit)

  ;; Key bindings for auto-fill.
  (spacemacs/declare-prefix "of" "auto-fill")
  (spacemacs/set-leader-keys "ofa" #'auto-fill-mode)
  (spacemacs/set-leader-keys "ofc" #'set-fill-column)
  (spacemacs/set-leader-keys "ofp" #'set-fill-prefix)

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

  (defun projectile-copy-directory-path ()
    "Show and copy the full path to the current project directory in the minibuffer."
    (interactive)
    (let ((project-root (projectile-project-root)))
      (message "%s" (kill-new project-root))))
  (spacemacs/set-leader-keys "fyp" #'projectile-copy-directory-path)

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

  ;; This will cause the value of go-tab-width to carry over to evil-shift-width.
  (push '(go-mode . go-tab-width) spacemacs--indent-variable-alist)

  ;; Disable ws-butler for go source code, go fmt will do the job instead.
  (with-eval-after-load 'ws-butler
    (push 'go-mode ws-butler-global-exempt-modes))

  ;; Make Flycheck a little bit less eager to lint. This is mostly due to
  ;; gometalinter which is eating CPU cycles like they are candy.
  (with-eval-after-load 'flycheck
    (setq flycheck-check-syntax-automatically '(save mode-enabled)))

  ;; Scroll the compilation buffer to the first error.
  (setq compilation-scroll-output 'first-error)

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
    ;; yank-pop has an (interactive "*p") form which does not allow
    ;; it to run in a read-only buffer.  We want browse-kill-ring to
    ;; be allowed to run in a read only buffer, so we change the
    ;; interactive form here.  In that case, we need to
    ;; barf-if-buffer-read-only if we're going to call yank-pop with
    ;; ad-do-it
    (interactive "p")
    (if (not (eq last-command 'yank))
        (helm-show-kill-ring)
      (barf-if-buffer-read-only)
      ad-do-it))
  (with-eval-after-load 'evil-common
    (defadvice evil-paste-pop (around evil-browse-kill-ring-maybe (arg) activate)
      "If last action was not a yank, run `browse-kill-ring' instead."
      ;; evil-paste-pop has an (interactive "*p") form which does not allow
      ;; it to run in a read-only buffer.  We want browse-kill-ring to
      ;; be allowed to run in a read only buffer, so we change the
      ;; interactive form here.  In that case, we need to
      ;; barf-if-buffer-read-only if we're going to call evil-paste-pop with
      ;; ad-do-it
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
      (when (getenv "DISPLAY")
        (with-temp-buffer
          (insert text)
          (call-process-region (point-min) (point-max)
                               "xsel" nil 0 nil "--input" "--clipboard")))))
  (defun xclip-paste-function ()
    (if window-system
        (x-selection-value)
      (when (getenv "DISPLAY")
        (let ((xclip-output (shell-command-to-string "xsel --output --clipboard")))
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

  ;; Use a solid bar Unicode character as vertical border.
  (set-display-table-slot standard-display-table 'vertical-border #x2502)
  (with-eval-after-load 'page-break-lines
    (defun mfa//buffer-display-table-vertical-border-advice (window)
      (with-current-buffer (window-buffer window)
        (when buffer-display-table
          (set-display-table-slot buffer-display-table 'vertical-border #x2502))))
    (advice-add 'page-break-lines--update-display-table
                :after #'mfa//buffer-display-table-vertical-border-advice))

  ;; Force eshell to quit if it gets stuck with "text is read-only"
  (defun kill-eshell ()
    (interactive)
    (let ((inhibit-read-only t))
      (kill-this-buffer)))

  ;; Augment sort lines to support case insensitive sort with prefix argument.
  (defun spacemacs/sort-lines (invert-case)
    "Sort lines in region or current buffer"
    (interactive "P")
    (require 'sort) ;; to ensure sort-fold-case is defined
    (let ((beg (if (region-active-p) (region-beginning) (point-min)))
          (end (if (region-active-p) (region-end) (point-max)))
          (sort-fold-case (if invert-case (not sort-fold-case) sort-fold-case)))
      (sort-lines nil beg end)))

  ;; Required to be able to paste from clipboard in visual mode
  ;; http://emacs.stackexchange.com/questions/14940/emacs-doesnt-paste-in-evils-visual-mode-with-every-os-clipboard
  (fset 'evil-visual-update-x-selection 'ignore)

  ;; Add support for running go benchmarks
  (with-eval-after-load 'go-mode
    (defun spacemacs/go-run-test-current-function ()
      (interactive)
      (if (string-match "_test\\.go" buffer-file-name)
          (save-excursion
            (re-search-backward "^func[ ]+\\(?:([[:alnum:]]*?[ ]?[*]?[[:alnum:]]+)[ ]+\\)?\\(\\(Test\\|Benchmark\\)[[:alnum:]_]+\\)(.*)")
            (let ((test-method
                   (if (string= (match-string-no-properties 2) "Benchmark")
                       "-bench"
                     (if go-use-gocheck-for-testing
                         "-check.f"
                       "-run"))))
              (spacemacs/go-run-tests (concat test-method "='" (match-string-no-properties 1) "'"))))
        (message "Must be in a _test.go file to run go-run-test-current-function")))

    (defun spacemacs/go-run-package-benchmarks ()
      (interactive)
      (spacemacs/go-run-tests "-bench=."))

    (spacemacs/set-leader-keys-for-major-mode 'go-mode
      "tb" #'spacemacs/go-run-package-benchmarks))

  (defun magit-diff-this-file ()
    (interactive)
    (let ((file-name (buffer-file-name)))
      (if file-name
          (magit-diff "master" nil (list buffer-file-name))
        (error "Buffer not visiting a file"))))
  (spacemacs/set-leader-keys "gd" #'magit-diff-this-file)

  (push '(flycheck-clang-language-standard . "c++11") safe-local-variable-values)
  (push '(flycheck-clang-pedantic-errors . t) safe-local-variable-values)

  (with-eval-after-load 'core-configuration-layer
    (defun configuration-layer//preload-restart-emacs (&rest args)
      "Preloads the restart-emacs package before updating packages which
potentially deletes it, after which it can not be autoloaded any more."
      (require 'restart-emacs))
    (advice-add 'configuration-layer/update-packages :before
                'configuration-layer//preload-restart-emacs))

  ;; workaround for https://github.com/syl20bnr/spacemacs/issues/8027
  (require 'ansible)
  (require 'ansible-doc)

  (defun open-terminal ()
    (interactive)
    (let ((process-environment (cons "EMACS_SOCKET_NAME" initial-environment)))
      (call-process "konsole" nil 0 nil "--workdir" (expand-file-name default-directory))))

  (defun projectile-open-terminal ()
    (interactive)
    (let ((default-directory (projectile-project-root)))
      (open-terminal)))
  (spacemacs/set-leader-keys "ot" #'open-terminal)
  (spacemacs/declare-prefix "op" "projects")
  (spacemacs/set-leader-keys "opt" #'projectile-open-terminal)

  (defun open-file-manager-in-directory ()
    (interactive)
    (let ((process-environment (cons "EMACS_SOCKET_NAME" initial-environment)))
      (call-process "xdg-open" nil 0 nil (expand-file-name default-directory))))
  (spacemacs/set-leader-keys "od" #'open-file-manager-in-directory)

  ;; The GTK system tooltips do not take HiDPI into account, thus placing the tooltips incorrectly.
  (setq x-gtk-use-system-tooltips nil)

  (setq winum-scope 'frame-local)

  (defun my-scratch-lisp-interaction ()
    "create a scratch buffer in lisp interaction mode"
    (interactive)
    (switch-to-buffer (get-buffer-create "*scratch*"))
    (lisp-interaction-mode))
  (spacemacs/set-leader-keys "o'" #'my-scratch-lisp-interaction)

  (setq helm-ag-base-command "rg --no-heading --ignore-case")

  (defun set-require-final-newline ()
    (set (make-local-variable 'require-final-newline)
         mode-require-final-newline))
  (add-hook 'conf-mode-hook #'set-require-final-newline)

  ;; Workaround for broken xterm-paste. Yank needs to be called interactively.
  (with-eval-after-load 'xterm
    (defun xterm-paste ()
      "Handle the start of a terminal paste operation."
      (interactive)
      (let* ((pasted-text (xterm--pasted-text))
             (interprogram-paste-function (lambda () pasted-text)))
        (call-interactively #'yank))))

  (defun insert-date (arg)
    (interactive "P")
    (insert (if arg
                (format-time-string "%Y-%m-%d %H:%M:%S")
              (format-time-string "%Y-%m-%d"))))
  (spacemacs/set-leader-keys "id" #'insert-date)

  ;; Ensure files visited by emacsclient a fullscreen experience.
  ;; TODO only trigger when a new frame was requested by the client.
  (add-hook 'server-visit-hook #'delete-other-windows)

  (spacemacs|use-package-add-hook fill-column-indicator
    :post-config
    (with-eval-after-load 'zenburn-theme
      (zenburn-with-color-variables
        (custom-theme-set-variables
         'zenburn
         ;; The choice Spacemacs makes for fci-rule-color is too dark for the
         ;; zenburn theme, this adjusts the rule to be a brighter color.
         `(fci-rule-color ,zenburn-bg+3)))))

  ;; Fixes issues caused by auto-revert-buffers during rebases. For details, see
  ;; https://github.com/magit/magit/issues/2708
  (defvar postpone-auto-revert-buffers nil)
  (defvar postpone-auto-revert-interval nil)

  (defun postpone-auto-revert-buffers (orig-fun &rest args)
    "Delay `auto-revert-buffers' if `postpone-auto-revert-buffers' is non-nil."
    (if postpone-auto-revert-buffers
        ;; Do not run `auto-revert-buffers', but make its timer run more
        ;; frequently in the meantime, so that it will run promptly once
        ;; it's safe.  Remember the original `auto-revert-interval'.
        (unless postpone-auto-revert-interval
          (setq postpone-auto-revert-interval auto-revert-interval)
          (setq auto-revert-interval 0.5)
          (auto-revert-set-timer))
      ;; We are no longer postponed, so restore the original
      ;; `auto-revert-interval', and run `auto-revert-buffers'.
      (when postpone-auto-revert-interval
        (setq auto-revert-interval postpone-auto-revert-interval)
        (setq postpone-auto-revert-interval nil)
        (auto-revert-set-timer))
      (apply orig-fun args))) ;; Run `auto-revert-buffers'.

  (defun postpone-auto-revert-buffers-on (&rest args)
    (setq postpone-auto-revert-buffers t))

  (defun postpone-auto-revert-buffers-off (&rest args)
    (setq postpone-auto-revert-buffers nil))

  (advice-add 'auto-revert-buffers :around #'postpone-auto-revert-buffers)
  (advice-add 'magit-process-filter :before #'postpone-auto-revert-buffers-on)
  (advice-add 'magit-process-finish :before #'postpone-auto-revert-buffers-off)

  (with-eval-after-load 'zenburn-theme
    (zenburn-with-color-variables
      (custom-theme-set-faces
       'zenburn
       ;; This is aliased to lazy-highlight by spacemacs, but the alias does
       ;; only work in the initial frame. This makes it work in subsequent
       ;; frames as well, as long as the theme is zenburn that is.
       `(evil-search-highlight-persist-highlight-face
         ((t (:foreground ,zenburn-yellow-2 :weight bold :background ,zenburn-bg-05)))))))

  ;; Some glyphs in this font can cause Emacs to crash.
  ;; https://bugs.debian.org/cgi-bin/bugreport.cgi?bug=892611
  ;; https://github.com/syl20bnr/spacemacs/issues/10695
  ;; https://lists.nongnu.org/archive/html/bug-gnu-emacs/2018-01/msg00260.html
  (add-to-list 'face-ignored-fonts "Noto Color Emoji"))
