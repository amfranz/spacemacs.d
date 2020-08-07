;; -*- mode: emacs-lisp; lexical-binding: t -*-
;; This file is loaded by Spacemacs at startup.
;; It must be stored in your home directory.

;; Add this projects shared library directory to the load path.
(add-to-list 'load-path (concat dotspacemacs-directory "lib/"))

;; Register autoloads for this projects shared library directory.
(require 'my-autoloads)
(require 'my-theming)

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
   dotspacemacs-enable-lazy-installation nil

   ;; If non-nil then Spacemacs will ask for confirmation before installing
   ;; a layer lazily. (default t)
   dotspacemacs-ask-for-lazy-installation t

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
     ;; auto-completion
     ;; better-defaults
     ;; emacs-lisp
     ;; git
     ;; helm
     ;; markdown
     ;; multiple-cursors
     ;; org
     ;; (shell :variables
     ;;        shell-default-height 30
     ;;        shell-default-position 'bottom)
     ;; spell-checking
     ;; syntax-checking
     ;; treemacs
     ;; version-control

     ;; layers by spacemacs
     (ansible :variables
              ;;
              ;; We do not really turn off automatic decryption of the vault
              ;; files, we just don't let Spacemacs register the hooks and
              ;; instead we'll do it ourselves in the my-ansible layer.
              ;;
              ;; We do it ourselves because Spacemacs is hooking the initial
              ;; automatic decryption into the activation of ansible mode. This
              ;; too early, at this point directory-local variables are not
              ;; loaded yet, so an `ansible-vault-password-file' set as
              ;; directory local variable won't work.
              ;;
              ;; Our own logic hooks the initial decryption into the
              ;; hack-local-variables-hook instead, a hook which is invoked
              ;; after the directory local variables have been loaded.
              ;;
              ansible-auto-encrypt-decrypt nil)

     ;; With these settings neither RET nor TAB will trigger completion any
     ;; more. The way to interact with the autocompletion popup is by using
     ;; these Vim/Evil friendly keybindings:
     ;;   C-j => next
     ;;   C-k => previous
     ;;   C-l => complete
     ;;   C-h => help
     (auto-completion :variables
                      ;; There is no good way to insert a newline when the popup
                      ;; is present. Let's disable completion on RET.
                      auto-completion-return-key-behavior nil
                      ;; Navigating org mode tables can be difficult, if you
                      ;; press TAB to go to the next cell it will complete
                      ;; instead. Let's disable completion on TAB.
                      auto-completion-tab-key-behavior nil
                      ;; `company-box' provides a nicer UI.
                      auto-completion-use-company-box t
                      ;; The help tooltip appearing automatically is nice but
                      ;; unnecessary most of the time, manual invocation will do
                      ;; just as well.
                      ;; Note: The reason we choose nil instead of 'manual is
                      ;; because `company-box' already customizes `company' as
                      ;; necessary to make the help text appear in a tooltip, we
                      ;; don't need Spacemacs to assist any further.
                      auto-completion-enable-help-tooltip nil
                      ;; The only reason a list of cancel keywords even exists
                      ;; is because sometimes RET triggering completion goes
                      ;; against the users intent. Since we disabled completion
                      ;; on RET, this is a non-issue for us. By configuring an
                      ;; empty list of cancel keywords we essentially disable
                      ;; the feature.
                      company-mode-completion-cancel-keywords nil)

     better-defaults
     bm
     (c-c++ :variables
            c-c++-enable-clang-format-on-save t
            c-c++-default-mode-for-headers 'c-or-c++-mode
            c-c++-backend 'lsp-ccls)
     (clojure :packages (not clojure-cheatsheet))
     colors
     copy-as-format
     cscope
     csv
     dap
     (dash :variables
           dash-docs-docset-newpath "~/.local/share/Zeal/Zeal/docsets")
     docker
     emacs-lisp
     erlang
     evil-snipe
     git
     (go :variables
         go-format-before-save t
         go-use-golangci-lint t)
     graphviz
     helm
     (html :variables
           web-fmt-tool 'prettier)
     (ibuffer :variables
              ibuffer-group-buffers-by 'projects)
     imenu-list
     (java :variables
           java-backend 'lsp)
     (javascript :variables
                 javascript-fmt-tool 'prettier
                 js-indent-level 2)
     (json :variables
           json-fmt-tool 'prettier)
     (lsp :variables
          lsp-ui-doc-enable nil
          lsp-ui-sideline-enable nil)
     lua
     (markdown :variables
               markdown-live-preview-engine 'vmd)
     ;; mu4e
     multiple-cursors
     nginx
     (org :variables
          org-enable-bootstrap-support t
          org-enable-github-support t
          org-enable-org-journal-support t
          org-enable-reveal-js-support t
          org-want-todo-bindings t)
     pass
     perl5
     plantuml
     prettier
     prodigy
     python
     restclient
     (ruby :variables
           ruby-test-runner 'rspec
           ruby-version-manager 'rbenv)
     (rust :variables
           rust-backend 'lsp
           rust-format-on-save t)
     search-engine
     (shell :variables
            shell-default-shell 'vterm)
     shell-scripts
     sql
     syntax-checking
     systemd
     treemacs
     typescript
     vimscript
     (version-control :variables
                      version-control-diff-tool 'diff-hl)
     yaml

     ;; my custom layers
     my-alpine
     my-ansible
     my-atomic-chrome
     my-bookmarks
     my-bpr
     my-company
     my-compile
     my-cpp
     my-dash
     my-dired
     my-direnv
     my-dokuwiki
     my-dtrt-indent
     my-editorconfig
     my-el-patch
     my-environment
     my-evil
     my-evil-quickscope
     my-evil-textobj-anyblock
     my-fancy-narrow
     my-go
     my-google-this
     my-hcl
     my-help
     my-highlight-symbols
     my-java
     my-javascript
     my-journal
     my-json
     my-layouts
     my-lice
     my-lisp
     my-lsp
     my-magit
     my-make-mode
     my-markdown
     ;; my-mu4e
     my-multi-line
     my-mwim
     my-org
     my-pass
     my-prodigy
     my-projectile
     my-python
     my-quickrun
     my-restclient
     my-ruby
     my-rust
     my-shell
     my-shell-scripts
     my-string-edit
     my-tmux
     my-sql
     my-treemacs
     my-xclipboard
     my-xml
     my-yadm
     my-yasnippet
     my-ztree)

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

   ;; Name of executable file pointing to emacs 27+. This executable must be
   ;; in your PATH.
   ;; (default "emacs")
   dotspacemacs-emacs-pdumper-executable-file "emacs"

   ;; Name of the Spacemacs dump file. This is the file will be created by the
   ;; portable dumper in the cache directory under dumps sub-directory.
   ;; To load it when starting Emacs add the parameter `--dump-file'
   ;; when invoking Emacs 27.1 executable on the command line, for instance:
   ;;   ./emacs --dump-file=$HOME/.emacs.d/.cache/dumps/spacemacs-27.1.pdmp
   ;; (default spacemacs-27.1.pdmp)
   dotspacemacs-emacs-dumper-dump-file (format "spacemacs-%s.pdmp" emacs-version)

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

   ;; Set `read-process-output-max' when startup finishes.
   ;; This defines how much data is read from a foreign process.
   ;; Setting this >= 1 MB should increase performance for lsp servers
   ;; in emacs 27.
   ;; (default (* 1024 1024))
   dotspacemacs-read-process-output-max (* 1024 1024)

   ;; If non-nil then Spacelpa repository is the primary source to install
   ;; a locked version of packages. If nil then Spacemacs will install the
   ;; latest version of packages from MELPA. (default nil)
   dotspacemacs-use-spacelpa nil

   ;; If non-nil then verify the signature for downloaded Spacelpa archives.
   ;; (default t)
   dotspacemacs-verify-spacelpa-archives t

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

   ;; If non-nil show the version string in the Spacemacs buffer. It will
   ;; appear as (spacemacs version)@(emacs version)
   ;; (default t)
   dotspacemacs-startup-buffer-show-version t

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

   ;; Default major mode for a new empty buffer. Possible values are mode
   ;; names such as `text-mode'; and `nil' to use Fundamental mode.
   ;; (default `text-mode')
   dotspacemacs-new-empty-buffer-major-mode 'text-mode

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

   ;; Default font or prioritized list of fonts.
   dotspacemacs-default-font `("DejaVu Sans Mono for Powerline"
                               :size ,(if (display-graphic-p)
                                          (display-adjusted-font-size)
                                        14)
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
   ;; (default "C-M-m" for terminal mode, "<M-return>" for GUI mode).
   ;; Thus M-RET should work as leader key in both GUI and terminal modes.
   ;; C-M-m also should work in terminal mode, but not in GUI mode.
   dotspacemacs-major-mode-emacs-leader-key (if window-system "<M-return>" "C-M-m")

   ;; These variables control whether separate commands are bound in the GUI to
   ;; the key pairs `C-i', `TAB' and `C-m', `RET'.
   ;; Setting it to a non-nil value, allows for separate commands under `C-i'
   ;; and TAB or `C-m' and `RET'.
   ;; In the terminal, these pairs are generally indistinguishable, so this only
   ;; works in the GUI. (default nil)
   dotspacemacs-distinguish-gui-tab (display-assume-graphic-p)

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
   dotspacemacs-enable-paste-transient-state nil

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

   ;; If non-nil the frame is undecorated when Emacs starts up. Combine this
   ;; variable with `dotspacemacs-maximized-at-startup' in OSX to obtain
   ;; borderless fullscreen. (default nil)
   dotspacemacs-undecorated-at-startup nil

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
   ;; If set to `t', `relative' or `visual' then line numbers are enabled in all
   ;; `prog-mode' and `text-mode' derivatives. If set to `relative', line
   ;; numbers are relative. If set to `visual', line numbers are also relative,
   ;; but lines are only visual lines are counted. For example, folded lines
   ;; will not be counted and wrapped lines are counted as multiple lines.
   ;; This variable can also be set to a property list for finer control:
   ;; '(:relative nil
   ;;   :visual nil
   ;;   :disabled-for-modes dired-mode
   ;;                       doc-view-mode
   ;;                       markdown-mode
   ;;                       org-mode
   ;;                       pdf-view-mode
   ;;                       text-mode
   ;;   :size-limit-kb 1000)
   ;; When used in a plist, `visual' takes precedence over `relative'.
   ;; (default nil)
   dotspacemacs-line-numbers nil

   ;; Code folding method. Possible values are `evil' and `origami'.
   ;; (default 'evil)
   dotspacemacs-folding-method 'origami

   ;; If non-nil `smartparens-strict-mode' will be enabled in programming modes.
   ;; (default nil)
   dotspacemacs-smartparens-strict-mode nil

   ;; If non-nil pressing the closing parenthesis `)' key in insert mode passes
   ;; over any automatically added closing parenthesis, bracket, quote, etc...
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

   ;; If non nil activate `clean-aindent-mode' which tries to correct
   ;; virtual indentation of simple modes. This can interfer with mode specific
   ;; indent handling like has been reported for `go-mode'.
   ;; If it does deactivate it here.
   ;; (default t)
   dotspacemacs-use-clean-aindent-mode nil

   ;; Either nil or a number of seconds. If non-nil zone out after the specified
   ;; number of seconds. (default nil)
   dotspacemacs-zone-out-when-idle nil

   ;; Run `spacemacs/prettify-org-buffer' when
   ;; visiting README.org files of Spacemacs.
   ;; (default nil)
   dotspacemacs-pretty-docs t

   ;; If nil the home buffer shows the full path of agenda items
   ;; and todos. If non nil only the file name is shown.
   dotspacemacs-home-shorten-agenda-source nil))

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

  ;; Emacs 27 changed the default value of `custom--inhibit-theme-enable' to
  ;; apply-only-user. Currently this makes some faces appear incorrectly, for
  ;; example the base colors in vterm. For the time being, let's reset it to
  ;; nil, which was the previous default value.
  (when (>= emacs-major-version 27)
    (setq custom--inhibit-theme-enable nil))

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

  ;; evil-collection will complain if this is not set before evil is loaded.
  (setq evil-want-keybinding nil)

  ;; TODO: submit this upstream
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

  ;; `custom-file' needs to be set in `dotspacemacs/user-init' to prevent
  ;; Spacemacs copying custom settings back into this file.
  ;;
  ;; The reason it is set to a garbage file instead of the real custom file is
  ;; to avoid the real custom file to get reset to an empty state. This will
  ;; happen when new packages get installed, because that mechanism will call
  ;; `custom-save-all' to persist the new value of `package-selected-packages'.
  ;; If at that time the custom file was not loaded yet, it will get overwritten
  ;; with an essentially empty custom file.
  ;;
  ;; To avoid this, we will initally set `custom-file' to a garbage file. When
  ;; new packages get installed, `custom-save-all' will overwrite the garbage
  ;; file instead of the real custom file.
  (setq custom-file (concat spacemacs-cache-directory "garbage.el")))

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
     (when-let (persp (and (fboundp 'get-frame-persp) (get-frame-persp)))
       (let ((name (persp-name persp)))
         ;; This is the same logic to abbreviate the layout name as in `spaceline.el'.
         (if (file-directory-p name)
             (setq name (file-name-nondirectory (directory-file-name name))))
         (concat "[" name "] ")))
     (buffer-name)
     (when (or buffer-file-name (derived-mode-p 'dired-mode))
       (concat " (" (string-remove-suffix "/" (abbreviate-file-name default-directory)) ")"))
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
  (add-to-list 'face-ignored-fonts "Noto Color Emoji")

  ;; Prevent the text selected in visual mode to automatically get copied into
  ;; the clipboard. Without this it would be pretty cumbersome to paste text
  ;; from external applications.
  ;; http://emacs.stackexchange.com/questions/14940/emacs-doesnt-paste-in-evils-visual-mode-with-every-os-clipboard
  (fset 'evil-visual-update-x-selection 'ignore)

  ;; Lets trust myself to not create problematic symlinks.
  (setq vc-follow-symlinks t)

  ;; Avoid duplicate entries in history (eg. the helm command history).
  (setq history-delete-duplicates t)

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

  ;; This is reported to speed up helm frames under Emacs 26.x, for details see
  ;; https://github.com/emacs-helm/helm/issues/1976. A user reports that this
  ;; makes Emacs on Windows unusable, so we'll restrict this to Linux.
  (when (and (= emacs-major-version 26) (string-equal system-type "gnu/linux"))
    (setq x-wait-for-event-timeout nil))

  ;; Draw block cursor as wide as the glyph under it. This makes it obvious
  ;; whether the cursor is currently over a TAB or a SPC character.
  (setq x-stretch-cursor t)

  ;; Open a dired buffer when switching to a project.
  (setq projectile-switch-project-action #'projectile-dired)

  ;; Mark files as executable if they contain a shebang.
  (add-hook 'after-save-hook #'executable-make-buffer-file-executable-if-script-p)

  ;; Having this disabled is annoying, totally messes up visual indentation.
  (setq-default truncate-lines t)

  ;; Adjust thresholds for sensible window splitting.
  ;; Typical dimensions of a full-frame window: 240x59
  (setq split-height-threshold 30
        split-width-threshold 140)

  ;; Prefer horizontal splits over vertical ones.
  (defun my-ad-split-window-sensibly (orig-fun &optional window)
    (let ((window (or window (selected-window))))
      (or (and (window-splittable-p window t)
               ;; Split window horizontally.
               (with-selected-window window
                 (split-window-right)))
          (and (window-splittable-p window)
               ;; Split window vertically.
               (with-selected-window window
                 (split-window-below)))
          ;; For all other cases, fall back
          ;; to the logic built into Emacs.
          (funcall orig-fun window))))
  ;; Can't simply set `split-window-preferred-function' because of
  ;; `markdown-mode', `graphviz-dot-mode', `window-purpose-mode', et al
  (advice-add 'split-window-sensibly :around #'my-ad-split-window-sensibly)

  ;; Tell paradox that we won't give it a GitHub token.
  (setq paradox-github-token t)

  ;; Namespace window numbers by frame.
  (setq winum-scope 'frame-local)

  ;; Reduce the list of supported VC backends. This improves performance as it
  ;; does less when trying to autodetect source control metadata.
  (setq vc-handled-backends '(Git))

  ;; Display a fringe indicator as hint that lines are wrapped.
  (setq visual-line-fringe-indicators '(nil right-curly-arrow))

  ;; Highlight `hl-todo' keywords in `conf-mode' as well.
  ;; By default only `prog-mode' and `text-mode' are included.
  (with-eval-after-load 'hl-todo
    (add-to-list 'hl-todo-include-modes 'conf-mode)
    (add-to-list 'hl-todo-text-modes 'conf-mode))

  ;; Hide the useless helm lighter.
  (with-eval-after-load 'helm-mode
    (diminish 'helm-mode))

  ;; Disable highlight current line, it is distracting.
  (when (bound-and-true-p global-hl-line-mode)
    (global-hl-line-mode -1))

  ;; Additional miscellaneous file mode associations.
  (add-to-list 'auto-mode-alist '("\\.gp\\'" . gnuplot-mode))
  (add-to-list 'auto-mode-alist '("\\.gpi\\'" . gnuplot-mode))

  ;; Recent files are still available via "fr".
  (spacemacs/replace-leader-key "bb" #'lazy-helm/helm-mini #'helm-buffers-list)

  ;; The latter, to me, makes more sense to be at that key binding. The former
  ;; also already has another binding ("SPC F o").
  (spacemacs/replace-leader-key "wo" #'other-frame #'ace-select-window)

  ;; This binding is a good mnemonic for this function. The former also already
  ;; has another binding ("SPC w F").
  (spacemacs/replace-leader-key "Fn" #'make-frame #'select-frame-by-name)

  ;; Additional miscellaneous key bindings. These might conflict with key
  ;; bindings set up by Spacemacs, so extra checks are performed to verify they
  ;; are not yet bound.
  (spacemacs/safe-set-leader-keys
    "fyp" #'projectile-copy-project-root
    "ef" #'spacemacs/first-error
    "gdb" #'magit-diff-buffer-file
    "gdu" #'magit-diff-upstream
    "id" #'my-insert-date-or-time
    "qe" #'server-edit
    "xli" #'sort-lines-insert
    "xll" #'sort-lines-by-length
    "xln" #'sort-numeric-fields
    "xl0" #'renumber-list
    "xrb" #'re-builder)

  ;; Additional miscellaneous key bindings. These are bound under the prefix "o"
  ;; which Spacemacs specifically reserves for use by the user, so there should
  ;; not be any conflicts.
  (spacemacs/declare-prefix "op" "projects")
  (spacemacs/safe-set-leader-keys
    "o'" #'lisp-sandbox
    "od" #'open-file-manager
    "oT" #'open-terminal
    "ot" #'spacemacs/shell-pop-ansi-term
    "opT" #'projectile-open-terminal
    "opt" #'projectile-open-shell)

  ;; Key binding to toggle sort-fold-case.
  (spacemacs/warn-if-leader-key-bound "tS" #'spacemacs/toggle-sort-fold-case)
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

  ;; Enable highlighting of URLs in configuration files.
  (add-hook 'conf-mode-hook #'goto-address-prog-mode)

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

  ;; Use a solid bar Unicode character as vertical border.
  (set-display-table-slot standard-display-table 'vertical-border #x2502)

  ;; `page-break-lines-mode' creates an empty buffer-local display table that
  ;; supersedes the standard display table. The following advice applies the
  ;; vertical border customization to this buffer-local display table as well.
  (defun my//buffer-display-table-vertical-border-advice (window)
    (with-current-buffer (window-buffer window)
      (when buffer-display-table
        (set-display-table-slot buffer-display-table 'vertical-border #x2502))))
  (advice-add 'page-break-lines--update-display-table
              :after #'my//buffer-display-table-vertical-border-advice)

  ;; Configure `helm' to never pop up in a separate frame nor to excessively
  ;; alter the window configuration to force two windows only. This was a
  ;; problem with `helm-eshell' history completion.
  (setq helm-always-two-windows nil
        helm-show-completion-display-function #'spacemacs//display-helm-window)

  ;; set to t by helm-read-file-name-handler-1
  (defun my-helm-always-two-windows-off (orig-fun &rest args)
    (let (helm-always-two-windows)
      (apply orig-fun args)))
  (advice-add 'helm-read-file-name :around #'my-helm-always-two-windows-off)

  ;; The GTK system tooltips do not take HiDPI into account, thus placing the
  ;; tooltips incorrectly. Apart from that, the Gtk tooltip looks uglier than
  ;; its non-Gtk counterpart.
  (setq x-gtk-use-system-tooltips nil)

  ;; Do not automatically hide the tooltip containing the message by flycheck.
  ;; The default was to hide it after 5 seconds.
  (setq flycheck-pos-tip-timeout -1)

  ;; Reduce the size of the compilation window, to prevent it from taking away
  ;; too much valuable screen estate when it pops up.
  (setq compilation-window-height 15)

  ;; Disable undo/redo-in-region, it occasionally exhibits buggy behavior and
  ;; not useful enough to justify having to deal with the issues.
  ;; See https://emacs.stackexchange.com/questions/37393/disable-undo-tree-on-a-region-of-text
  (setq undo-tree-enable-undo-in-region nil)

  ;; Commands like `evil-show-registers' pop up a window but leave it opened
  ;; when you quit the buffer using "q". This binding alters that behavior so
  ;; that the window will get closed in addition to killing the buffer.
  (define-key evil-list-view-mode-map "q" #'kill-buffer-and-window)

  ;; Customize Evil to use the Emacs heuristics for recording undoable changes,
  ;; instead of trying to emulate Vim exactly. The Emacs heuristics are more
  ;; useful, they make it possible to undo buffer changes that happened during
  ;; an insert state partially. The Vim behavior would be to undo all the
  ;; changes of an insert state session in one step.
  ;; See https://emacs.stackexchange.com/questions/3358/how-can-i-get-undo-behavior-in-evil-similar-to-vims
  (setq evil-want-fine-undo t)

  ;; Several paste operations in a row with an active visual selection should
  ;; paste the same text. This is the most intuitive behavior for me, and I use
  ;; this operation pretty often. For swapping text there is is still 'gx'.
  (setq evil-kill-on-visual-paste nil)
  (defun evil-delete--kill-on-visual-paste (args)
    (if (and (not evil-kill-on-visual-paste)
             (eq this-command 'evil-visual-paste))
        (append args '(?_))
      args))
  (advice-add 'evil-delete :filter-args
              #'evil-delete--kill-on-visual-paste)

  ;; Make paste via Super-v work. I configured my terminal program with the same
  ;; key binding. This makes Super-v a universal paste, which I hope will lead
  ;; less mental overhead for such a common operation.
  (define-key evil-normal-state-map (kbd "s-v") #'evil-paste-before)
  (define-key evil-insert-state-map (kbd "s-v") #'evil-paste-before)
  (define-key evil-ex-completion-map (kbd "s-v") #'evil-paste-before)
  (define-key minibuffer-local-map (kbd "s-v") #'evil-paste-before)

  ;; Replace the key binding for `spacemacs/recompile-elpa' ("SPC f e c") with
  ;; the more useful binding to edit the `custom-file'. This mirrors the
  ;; keybindings to edit the `user-init-file' ("SPC f e i") and the
  ;; `dotspacemacs-filepath' ("SPC f e d").
  (spacemacs/replace-leader-key "fec"
    #'spacemacs/recompile-elpa #'my-find-custom-file)

  (defun my-find-spacemacs-dir ()
    "Edit the `user-emacs-directory', in the current window."
    (interactive)
    (find-file-existing user-emacs-directory))
  (spacemacs/safe-set-leader-keys "fes" #'my-find-spacemacs-dir)

  ;; The first call to `completing-read' might not use helm because it is not
  ;; loaded yet due to the lazy-load mechanics. This forces helm to be loaded
  ;; before the first load to `completing-read'.
  (defun my-require-before-call (feat func)
    (unless (featurep feat)
      (let ((req-func (intern (concat (symbol-name func) "--require-"
                                      (symbol-name feat)))))
        (fset req-func (lambda (&rest ignore) (require feat)))
        (advice-add func :before req-func)
        (with-eval-after-load feat
          (advice-remove func req-func)
          (fmakunbound req-func)))))
  (my-require-before-call 'helm 'completing-read)

  ;; cw (change word) in iedit-mode swallows whitespace after word. causes
  ;; iedit-mode to quit. normal mode does not exhibit this behavior.
  ;; https://github.com/emacs-evil/evil/blob/dc936936666595afdbdbb4cc44c1f82e74c6802c/evil-commands.el#L309-L311
  (with-eval-after-load 'evil-vars
    (add-to-list 'evil-change-commands 'evil-iedit-state/evil-change))

  (defun my-projectile-goto-git-info-exclude (&optional arg)
    (interactive "P")
    (if-let (project-path (projectile-project-root))
        (let ((git-info-exclude (concat project-path ".git/info/exclude")))
          (if (file-exists-p git-info-exclude)
              (if arg
                  (find-file-other-window git-info-exclude)
                (find-file git-info-exclude))
            (message "WARNING: Current project is not managed by git!")))
      (message "WARNING: Current buffer is not part of a project!")))
  (spacemacs/safe-set-leader-keys "pi" #'my-projectile-goto-git-info-exclude)

  (defun my-capture-switch-to-buffer (buffer)
    (select-window (pupo/display-function buffer nil) t))
  (defun my-ad-ignore-delete-other-windows (orig-fun &rest args)
    (cl-letf (((symbol-function 'delete-other-windows) #'ignore)
              ((symbol-function 'org-switch-to-buffer-other-window) #'my-capture-switch-to-buffer))
      (apply orig-fun args)))
  (advice-add 'org-capture-place-template
              :around #'my-ad-ignore-delete-other-windows)

  (defun my-select-mru-window ()
    (interactive)
    (when-let ((window (get-mru-window nil t t)))
      (select-window window)))
  (spacemacs/set-leader-keys "ww" #'my-select-mru-window)

  ;; Sort ripgrep search results. This is a trade-off. Sorting means that
  ;; ripgrep can't parallelize the search. On the other hand sorting prevents
  ;; the search results from "jumping around" in the helm window, which bothers
  ;; me very much.
  ;; The jumping of the results happens because of the "live" behavior of the
  ;; search, whereas after every keypress it performs the search again. Even if
  ;; after another keypress the same list of results is produced, they might
  ;; suddenly appear in a different order because of the randomness of thread
  ;; timing.
  (defun my-ad-helm-do-ag-rg-sort-files (orig-fun &rest args)
    (let ((helm-ag-base-command (if (string-prefix-p "rg " helm-ag-base-command)
                                    (concat helm-ag-base-command " --sort path")
                                  helm-ag-base-command)))
      (apply orig-fun args)))
  (advice-add 'helm-do-ag :around #'my-ad-helm-do-ag-rg-sort-files)

  ;; Center the buffer after visiting a file via `helm-ag'.
  (el-patch-feature helm-ag)
  (with-eval-after-load 'helm-ag
    (eval
     '(el-patch-defun helm-ag--find-file-action (candidate find-func this-file &optional persistent)
        "Not documented, CANDIDATE, FIND-FUNC, THIS-FILE, PERSISTENT."
        (when (memq 'pt helm-ag--command-features)
          ;; 'pt' always show filename if matched file is only one.
          (setq this-file nil))
        (let* ((file-line (helm-grep-split-line candidate))
               (filename (or this-file (cl-first file-line) candidate))
               (line (if this-file
                         (cl-first (split-string candidate ":"))
                       (cl-second file-line)))
               (default-directory (or helm-ag--default-directory
                                      helm-ag--last-default-directory
                                      default-directory)))
          (unless persistent
            (setq helm-ag--last-default-directory default-directory))
          (funcall find-func filename)
          (goto-char (point-min))
          (when line
            (forward-line (1- (string-to-number line)))
            (el-patch-add (recenter)))
          (ignore-errors
            (and (re-search-forward helm-ag--last-query (line-end-position) t)
                 (helm-goto-char (match-beginning 0))))))))

  ;; Workaround for display issues with squished font glyphs in tooltip windows.
  (setq pos-tip-border-width 0)
  (with-eval-after-load 'tooltip
    (setcdr (assq 'border-width tooltip-frame-parameters) 0))

  ;; Enable tooltips in graphical environments.
  (spacemacs|do-after-display-system-init
   (tooltip-mode))

  (spacemacs/safe-set-leader-keys
    "fa" #'ff-find-other-file)

  ;; Prevent "SPC q f" from the last remaining frame invisible if the persistent
  ;; server is not running. Without a persistent server it's not possible any
  ;; more to interact with the Emacs instance once its last remaining frame is
  ;; invisible.
  (defun kill-frame-or-emacs ()
    "If there is only one remaining window and no persistent server is active,
prompt to save changed buffers and exit Spacemacs. In all other cases, kill the
current frame but keep Emacs running."
    (interactive)
    (if (or (and (fboundp 'server-running-p) (server-running-p))
            (delete-frame-enabled-p))
        (spacemacs/frame-killer)
      (spacemacs/prompt-kill-emacs)))
  (spacemacs/set-leader-keys "qf" #'kill-frame-or-emacs)

  ;; Add the :extend attribute, introduced by Emacs 27, to faces that need it.
  (when (>= emacs-major-version 27)
    (with-eval-after-load 'company
      (dolist (face '(company-tooltip-selection))
        (set-face-attribute face nil :extend t)))
    (with-eval-after-load 'helm
      (dolist (face '(helm-selection))
        (set-face-attribute face nil :extend t))))

  ;; This will make the value of a file-local tab-width setting to carry over to evil-shift-width.
  (add-to-list 'spacemacs--indent-variable-alist '(conf-mode . tab-width))

  ;; This will make the value of a file-local tab-width setting to carry over to evil-shift-width.
  (add-to-list 'spacemacs--indent-variable-alist '(asm-mode . tab-width))

  ;; Customize the color of the fill column indicator, the color chosen by
  ;; `zenburn-theme' is too similar to the background color.
  (spacemacs/after-load-theme 'zenburn
    (zenburn-with-color-variables
      ;; `display-fill-column-indicator' is available in Emacs 27+
      (if (boundp 'display-fill-column-indicator)
          (custom-theme-alter-faces
           'zenburn `(fill-column-indicator ((,class
                                              :foreground ,zenburn-bg+3
                                              :weight semilight))))
        (custom-theme-alter-variables
         'zenburn `(fci-rule-color ,zenburn-bg+3)))))

  ;; Remove the box around `org-mode' checkboxes. The box causes the line to be
  ;; taller and thus vertically shifts the text below, which is distracting.
  (spacemacs/after-load-theme 'zenburn
    (zenburn-with-color-variables
      (custom-theme-alter-faces
       'zenburn `(org-checkbox
                  ((t :background ,zenburn-bg+2
                      :foreground ,zenburn-fg+1
                      :box nil))))))
  ;; An easier way to enter the `evil-numbers' transient state.
  (spacemacs/safe-set-leader-keys
    "nn" #'spacemacs/evil-numbers-transient-state/body)

  ;; Fixes errors when using escaped brackets in the search terms when using
  ;; helm-ag+ripgrep. See https://github.com/syohex/emacs-helm-ag/pull/309
  (el-patch-feature helm-ag)
  (with-eval-after-load 'helm-ag
    (eval
     '(el-patch-defun helm-ag--do-ag-highlight-patterns (input)
        (if (el-patch-swap (memq 'pcre helm-ag--command-features)
                           (or (memq 'pcre helm-ag--command-features)
                               (memq 're2 helm-ag--command-features)))
            (cl-loop with regexp = (helm-ag--pcre-to-elisp-regexp input)
                     for pattern in (helm-ag--split-string regexp)
                     when (helm-ag--validate-regexp pattern)
                     collect pattern)
          (list (helm-ag--join-patterns input))))))

  ;; Apply persisted custom settings. This needs to be the very last step to
  ;; make sure that any customization applied by the custom file will not get
  ;; undone by later stages of the Emacs startup sequence.
  (add-hook 'spacemacs-post-user-config-hook #'my-load-custom-file 'append))
