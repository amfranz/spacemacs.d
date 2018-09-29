;; -*- mode: emacs-lisp; lexical-binding: t -*-
;; This file is loaded by Spacemacs at startup.
;; It must be stored in your home directory.

;; Add this projects library directory to the load path.
(push (concat dotspacemacs-directory "lib/") load-path)

;; Register autoloads for this projects library directory.
(require 'my-autoloads)

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
     git
     github
     (go :variables
         go-format-before-save t
         go-tab-width 4
         go-use-golangci-lint t)
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
          org-enable-org-journal-support t
          org-enable-reveal-js-support t
          org-projectile-file "notes.org"
          org-want-todo-bindings t)
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
     mfa-go
     mfa-google-this
     mfa-hcl
     mfa-highlight-symbols
     mfa-javascript
     mfa-journal
     mfa-json
     mfa-lice
     mfa-lisp
     mfa-magit
     mfa-make-mode
     mfa-markdown
     ;; mfa-mu4e
     mfa-multi-line
     mfa-mwim
     mfa-org
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
     mfa-sql
     mfa-treemacs
     mfa-vagrant
     mfa-vdiff
     mfa-visual-ascii-mode
     mfa-xml
     mfa-yasnippet)

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
   dotspacemacs-elpa-subdirectory 'emacs-version

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
   dotspacemacs-themes '(zenburn)

   ;; Set the theme for the Spaceline. Supported themes are `spacemacs',
   ;; `all-the-icons', `custom', `doom', `vim-powerline' and `vanilla'. The
   ;; first three are spaceline themes. `doom' is the doom-emacs mode-line.
   ;; `vanilla' is default Emacs mode-line. `custom' is a user defined themes,
   ;; refer to the DOCUMENTATION.org for more info on how to create your own
   ;; spaceline theme. Value can be a symbol or list with additional properties.
   ;; (default '(spacemacs :separator wave :separator-scale 1.5))
   dotspacemacs-mode-line-theme `(spacemacs
                                  :separator ,(if (display-assume-graphic-p)
                                                  'wave
                                                'utf-8)
                                  :separator-scale 1.0)

   ;; If non-nil the cursor color matches the state color in GUI Emacs.
   ;; (default t)
   dotspacemacs-colorize-cursor-according-to-state t

   ;; Default font, or prioritized list of fonts. `powerline-scale' allows to
   ;; quickly tweak the mode-line size to make separators look not too crappy.
   dotspacemacs-default-font `("DejaVu Sans Mono for Powerline"
                               :size ,(if (display-graphic-p)
                                          (display-adjusted-font-size)
                                        13)
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
   dotspacemacs-mode-line-unicode-symbols (display-assume-graphic-p)

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
   dotspacemacs-enable-server nil

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

  ;; Garbage collect only during idle times.
  (add-hook 'spacemacs-post-user-config-hook #'gc-idle-enable)

  ;; Turn garbage collection on while installing packages and recompiling ELPA
  ;; packages. This avoids Emacs to hang for a long time after many packages get
  ;; installed.
  (seq-do #'gc-idle-exempt '(byte-recompile-directory
                             configuration-layer//install-packages))

  ;; Allow resizing frames with pixel-precision, do not enforce frame sizes to
  ;; be multiples of character width or height.
  (setq frame-resize-pixelwise t)

  ;; Make frames larger than the conservative default size.
  (let ((goal-width 164)
        (goal-height 47))
    (push `(width . ,goal-width) default-frame-alist)
    (push `(height . ,goal-height) default-frame-alist)
    ;; Also resize the initial frame to the goal size, but enlarge the frame in
    ;; all directions so that the center of the window does remain where it is.
    (when (display-graphic-p)
      (let ((frame (selected-frame))
            (position (frame-position))
            (width (frame-width))
            (height (frame-height)))
        (set-frame-position
         frame
         (max 0 (- (car position)
                   (/ (* (frame-pixel-width) (- goal-width width))
                      (* width 2))))
         (max 0 (- (cdr position)
                   (/ (* (frame-pixel-height) (- goal-height height))
                      (* height 2)))))
        (set-frame-size frame goal-width goal-height))))

  ;; Disable lockfiles (.#*). This needs to be set early to avoid creating lock
  ;; files for files opened during Spacemacs startup sequence.
  (setq create-lockfiles nil)

  ;; Tramp by default tries to look for the availability of various config
  ;; options by running ssh against the host `host.does.not.exist'
  ;; This can be prevented by defining the options before loading tramp.
  (setq tramp-ssh-controlmaster-options
        "-o ControlMaster=auto -o ControlPath='tramp.%%r@%%h:%%p' -o ControlPersist=no")

  ;; TODO submit this upstream
  ;; Fixes the guess of the package description file for the `eterm-256color' package.
  ;; This is a replacement for `package--description-file' in `subr.el'. The only
  ;; change is that the regular expression is anchored at the end."
  (defun eterm-256color-package--description-file (dir)
    (concat (let ((subdir (file-name-nondirectory
                           (directory-file-name dir))))
              (if (string-match "\\([^.].*?\\)-\\([0-9]+\\(?:[.][0-9]+\\|\\(?:pre\\|beta\\|alpha\\)[0-9]+\\)*\\)\\'" subdir)
                  (match-string 1 subdir) subdir))
            "-pkg.el"))
  (advice-add 'package--description-file :override
              #'eterm-256color-package--description-file)

  ;; Even though Spacemacs has a theming layer that can customize theme *faces*,
  ;; it does not have support to customize theme *variables*. We need to roll
  ;; our own.
  (defun my--adjust-theme-variables ()
    (when (eq 'zenburn spacemacs--cur-theme)
      (zenburn-with-color-variables
        (custom-theme-set-variables
         'zenburn
         `(fci-rule-color ,zenburn-bg+3)))))
  (add-hook 'spacemacs-post-theme-change-hook
            #'my--adjust-theme-variables)

  ;; Keep customizations in a separate file that is not under version control.
  ;; This needs to be set in `dotspacemacs/user-init' to prevent Spacemacs
  ;; copying the custom settings back into this file.
  (setq custom-file (concat spacemacs-cache-directory "custom.el")))

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

  ;; The final order of `after-frame-functions' is important:
  ;;   1) adjust default font size
  ;;   2) adjust powerline height
  ;;   3) after display system init

  ;; Move the logic that runs the after-display functions registered with
  ;; `spacemacs|do-after-display-system-init' from being an after-advice of
  ;; `server-create-window-system-frame' to `after-make-frame-functions'. This
  ;; gets rid of some odd issues, eg. the Spacemacs buffer not being current in
  ;; the initial frame of a daemon instance.
  (ad-disable-advice 'server-create-window-system-frame
                     'after 'spacemacs-init-display)
  (ad-activate 'server-create-window-system-frame)
  (defun spacemacs--after-display-system-init (frame)
    (with-selected-frame frame
      (when (display-graphic-p)
        (remove-hook 'after-make-frame-functions
                     #'spacemacs--after-display-system-init)
        (dolist (fn (reverse spacemacs--after-display-system-init-list))
          (funcall fn)))))
  (add-hook 'after-make-frame-functions #'spacemacs--after-display-system-init)

  ;; This avoids graphical artifacts in the mode line with the first graphical
  ;; client. Computing the mode line height does font measurements, which are
  ;; not working properly until the display system is initialized.
  (unless (display-graphic-p)
    (defun my--adjust-powerline-height (frame)
      (with-selected-frame frame
        (when (display-graphic-p)
          (setq powerline-height (spacemacs/compute-mode-line-height))
          (remove-hook 'after-make-frame-functions #'my--adjust-powerline-height))))
    (add-hook 'after-make-frame-functions #'my--adjust-powerline-height))

  ;; Adjust the font size to a HiDPI screen, if needed. This will either be done
  ;; immediately if possible, or after the first graphical frame got created
  ;; when this is an Emacs daemon instance.
  (unless (display-graphic-p)
    (defun my--adjust-default-font-size (frame)
      (with-selected-frame frame
        (when (display-graphic-p)
          (setq dotspacemacs-default-font
                (cons (car dotspacemacs-default-font)
                      (plist-put (cdr dotspacemacs-default-font)
                                 :size (display-adjusted-font-size))))
          (remove-hook 'after-make-frame-functions #'my--adjust-default-font-size))))
    (add-hook 'after-make-frame-functions #'my--adjust-default-font-size))

  (defun configuration-layer//preload-restart-emacs (&rest args)
    "Preloads the restart-emacs package before updating packages which
potentially deletes it, after which it can not be autoloaded any more."
    (require 'restart-emacs))
  (advice-add 'configuration-layer/update-packages :before
              'configuration-layer//preload-restart-emacs)

  ;; Customize the frame title. Also pass the customized frame title to
  ;; terminals that support it.
  (defun my-frame-title ()
    (concat
     (buffer-name)
     (when (or buffer-file-name (derived-mode-p 'dired-mode))
       (concat " ("
               (string-remove-suffix "/" (abbreviate-file-name default-directory))
               ")"))
     " - Emacs"))
  (setq frame-title-format
        (setq icon-title-format
              '((:eval (replace-regexp-in-string "%" "%%" (my-frame-title) t t)))))
  (defun my-tty--supports-title ()
    (and (not (display-graphic-p))
         (let ((term (frame-parameter nil 'tty-type)))
           (or (string-prefix-p "xterm" term)
               (string-prefix-p "konsole" term)
               (string-prefix-p "screen" term)
               (string-prefix-p "tmux" term)))))
  (defun my-tty-update-title ()
    (when (my-tty--supports-title)
      (let ((old-title (frame-parameter nil 'xterm-title))
            (new-title (my-frame-title)))
        (unless (string-equal old-title new-title)
          (send-string-to-terminal (concat "\033]0;" new-title "\007"))
          (modify-frame-parameters nil `((xterm-title . ,new-title)))))))
  (add-hook 'window-configuration-change-hook #'my-tty-update-title)

  ;; Add ~/.spacemacs.d/bin/ to the executable search path.
  (let ((bin-path (concat dotspacemacs-directory "bin/")))
    (setenv "PATH" (concat bin-path path-separator (getenv "PATH")))
    (push bin-path exec-path))

  ;; Some glyphs in this font can cause Emacs to crash.
  ;; https://bugs.debian.org/cgi-bin/bugreport.cgi?bug=892611
  ;; https://github.com/syl20bnr/spacemacs/issues/10695
  ;; https://lists.nongnu.org/archive/html/bug-gnu-emacs/2018-01/msg00260.html
  (push "Noto Color Emoji" face-ignored-fonts)

  ;; Prevent the text selected in visual mode to automatically get copied into
  ;; the clipboard. Without this it would be pretty cumbersome to paste text
  ;; from external applications.
  ;; http://emacs.stackexchange.com/questions/14940/emacs-doesnt-paste-in-evils-visual-mode-with-every-os-clipboard
  (fset 'evil-visual-update-x-selection 'ignore)

  ;; Lets trust myself to not create problematic symlinks.
  (setq vc-follow-symlinks t)

  ;; Do not add temporary files to the recentf list.
  (with-eval-after-load 'recentf
    (setq recentf-exclude
          (append '("\\`/dev/shm/"
                    "\\`/tmp/"
                    "\\`/var/tmp/")
                  recentf-exclude)))

  ;; Various customization of helm.
  (setq helm-default-external-file-browser "xdg-open"
        helm-raise-command "wmctrl -xa %s")

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

  ;; Namespace window numbers by frame.
  (setq winum-scope 'frame-local)

  ;; Reduce the list of supported VC backends. This improves performance as it
  ;; does less when trying to autodetect source control metadata.
  (setq vc-handled-backends '(Git))

  ;; Hide the useless helm lighter.
  (with-eval-after-load 'helm-mode
    (diminish 'helm-mode))

  ;; Hide dired-omit-mode lighter, it is distracting.
  (with-eval-after-load 'dired-x
    (defun dired-x--diminish-dired-omit-mode ()
      (diminish 'dired-omit-mode))
    (advice-add 'dired-omit-startup
                :after #'dired-x--diminish-dired-omit-mode))

  ;; Disable highlight current line, it is distracting.
  (when global-hl-line-mode
    (global-hl-line-mode -1))

  ;; Additional miscellaneous file mode associations.
  (push '("\\.gp\\'" . gnuplot-mode) auto-mode-alist)
  (push '("\\.gpi\\'" . gnuplot-mode) auto-mode-alist)

  ;; Remap helm-mini to helm-buffers-list.
  ;; Recent files are still available via "fr".
  (spacemacs/set-leader-keys "bb" #'helm-buffers-list)

  ;; Move `other-frame' to a less easy-to-reach key because I hardly ever need
  ;; it and bind its key to `ace-select-window' instead, which is more useful.
  (spacemacs/set-leader-keys
    "wo" #'ace-select-window
    "wO" #'other-frame)

  ;; Additional miscellaneous key bindings. These might conflict with key
  ;; bindings set up by Spacemacs, so extra checks are performed to verify they
  ;; are not yet bound.
  (spacemacs/safe-set-leader-keys
    "fyp" #'projectile-copy-directory-path
    "gd" #'magit-diff-this-file
    "id" #'insert-date
    "qe" #'server-edit
    "xll" #'sort-lines-by-length
    "xln" #'sort-numeric-fields)

  ;; Additional miscellaneous key bindings. These are bound under the prefix "o"
  ;; which Spacemacs specifically reserves for use by the user, so there should
  ;; not be any conflicts.
  (spacemacs/declare-prefix "op" "projects")
  (spacemacs/safe-set-leader-keys
    "o'" #'lisp-sandbox
    "od" #'open-file-manager
    "ot" #'open-terminal
    "opt" #'projectile-open-terminal)

  ;; Key binding to toggle sort-fold-case.
  (spacemacs/warn-if-leader-key-bound "tS")
  (spacemacs|add-toggle sort-fold-case
    :status (bound-and-true-p sort-fold-case)
    :on (setq sort-fold-case t)
    :off (setq sort-fold-case nil)
    :evil-leader "tS")

  ;; Require a final newline even in conf-mode (default is nil).
  (defun set-require-final-newline ()
    (set (make-local-variable 'require-final-newline)
         mode-require-final-newline))
  (add-hook 'conf-mode-hook #'set-require-final-newline)

  ;; Make Flycheck a little bit less eager to lint. This is mostly due to
  ;; gometalinter which is eating CPU cycles like they are candy.
  (setq flycheck-check-syntax-automatically '(save mode-enabled))

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
                               "xclip" nil 0 nil "-silent" "-i" "-selection" "clipboard")))))
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
  (unless (display-assume-graphic-p)
    (xclip-mode))

  ;; In `c++-mode' the HTML tag surround pair is pretty much useless. It is far
  ;; more useful to have angle bracket surround pairs.
  (with-eval-after-load 'evil-surround
    (defun my-evil-surround-pairs-c++-mode ()
      (push '(?< . ("< " . " >")) evil-surround-pairs-alist))
    (add-lazy-hook 'c++-mode #'my-evil-surround-pairs-c++-mode))

  ;; `evil-surround' should use "'" as end delimiter for "`" in lisp modes. This
  ;; brings it in line with the behavior of `smartparens', which does the same.
  (with-eval-after-load 'evil-surround
    (defun my-evil-surround-pairs-emacs-lisp-mode ()
      (push '(?` . ("`" . "'")) evil-surround-pairs-alist))
    (add-lazy-hook 'emacs-lisp-mode #'my-evil-surround-pairs-emacs-lisp-mode))

  ;; Use a solid bar Unicode character as vertical border.
  (set-display-table-slot standard-display-table 'vertical-border #x2502)

  ;; `page-break-lines-mode' creates an empty buffer-local display table that
  ;; supersedes the standard display table. The following advice applies the
  ;; vertical border customization to this buffer-local display table as well.
  (defun mfa//buffer-display-table-vertical-border-advice (window)
    (with-current-buffer (window-buffer window)
      (when buffer-display-table
        (set-display-table-slot buffer-display-table 'vertical-border #x2502))))
  (advice-add 'page-break-lines--update-display-table
              :after #'mfa//buffer-display-table-vertical-border-advice)

  ;; Disable automatic indentation of pasted content in `sql-mode'.
  ;; SQL indentation styles vary wide, and it always gets it wrong as I try to
  ;; stick to the particular indentation style that's used by the project I am
  ;; working on.
  (add-to-list 'spacemacs-indent-sensitive-modes 'sql-mode)

  ;; Fix for paste with prefix argument, which per documentation is supposed to
  ;; turn off auto-indent for the paste, but that doesn't work since a while.
  ;; https://github.com/syl20bnr/spacemacs/issues/4219#issuecomment-246074883
  (spacemacs|advise-commands
   "indent" (yank yank-pop evil-paste-before evil-paste-after) around
   "If current mode is not one of spacemacs-indent-sensitive-modes
 indent yanked text (with universal arg don't indent)."
   (let ((prefix (ad-get-arg 0)))
     (ad-set-arg 0 (unless (equal '(4) prefix) prefix))
     (evil-start-undo-step)
     ad-do-it
     (if (and (not (equal '(4) prefix))
              (not (member major-mode spacemacs-indent-sensitive-modes))
              (or (derived-mode-p 'prog-mode)
                  (member major-mode spacemacs-indent-sensitive-modes)))
         (let ((transient-mark-mode nil)
               (save-undo buffer-undo-list))
           (spacemacs/yank-advised-indent-function (region-beginning)
                                                   (region-end))))
     (evil-end-undo-step)
     (ad-set-arg 0 prefix)))

  ;; Configure `helm' to never pop up in a separate frame nor to excessively
  ;; alter the window configuration to force two windows only. This was a
  ;; problem with `helm-eshell' history completion.
  (setq helm-always-two-windows nil
        helm-show-completion-display-function #'spacemacs//display-helm-window)

  ;; The GTK system tooltips do not take HiDPI into account, thus placing the
  ;; tooltips incorrectly. Apart from that, the Gtk tooltip looks uglier than
  ;; its non-Gtk counterpart.
  (setq x-gtk-use-system-tooltips nil)

  ;; Reduce the size of the compilation window, to prevent it from taking away
  ;; too much valuable screen estate when it pops up.
  (setq compilation-window-height 10)

  ;; Commands like `evil-show-registers' pop up a window but leave it opened
  ;; when you quit the buffer using "q". This binding alters that behavior so
  ;; that the window will get closed in addition to killing the buffer.
  (define-key evil-list-view-mode-map "q" #'kill-buffer-and-window)

  ;; The `my-utils' library is my place to put features that can be autoloaded
  ;; when the user invokes them, to reduce initial startup time. This is a
  ;; safety check that verifies that the library did not get eagerly loaded.
  (defun my-verify-utils-lazy-load ()
    (when (featurep 'my-utils)
      (lwarn 'spacemacs :warning "The `my-utils' feature was loaded too early")))
  (add-hook 'spacemacs-post-user-config-hook #'my-verify-utils-lazy-load t)

  (defun my-find-custom-file ()
    (interactive)
    (find-file-existing custom-file))
  (spacemacs/safe-set-leader-keys "fec" #'my-find-custom-file)

  ;; Apply persisted custom settings. This needs to be the very last step to
  ;; make sure that any customization applied by the custom file will not get
  ;; undone by later stages of the Emacs startup sequence.
  (defun my-load-custom-file ()
    (when (file-exists-p custom-file)
      (load custom-file)))
  (add-hook 'spacemacs-post-user-config-hook #'my-load-custom-file t))
