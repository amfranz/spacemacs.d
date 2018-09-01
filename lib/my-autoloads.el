;;; my-autoloads.el --- automatically extracted autoloads
;;
;;; Code:


;;;### (autoloads nil "display" "display.el" (0 0 0 0))
;;; Generated autoloads from display.el

(autoload 'display-assume-graphic-p "display" "\
My Emacs instances are dedicated to either the graphical environment or the
terminal. My customizations differ slightly between the two. This functions
returns whether this Emacs instance is dedicated to the graphical environment.

\(fn)" nil nil)

(autoload 'display-scaling-factor "display" "\
Reads the display scaling factor from the Cinnamon dconf database.
This will return 2 on Hi-DPI displays, 1 otherwise.

\(fn)" nil nil)

(autoload 'display-adjusted-font-size "display" "\
Calculates a recommended size in pixels for the default font based on the DPI
of the monitor.

\(fn)" nil nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "display" '("display--pixels-per-inch")))

;;;***

;;;### (autoloads nil "gc-idle" "gc-idle.el" (0 0 0 0))
;;; Generated autoloads from gc-idle.el

(autoload 'gc-idle-enable "gc-idle" "\
Configure garbage collection to occur when the user is idle.

\(fn)" nil nil)

(autoload 'gc-idle-exempt "gc-idle" "\


\(fn FUN)" nil nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "gc-idle" '("gc-idle-")))

;;;***

;;;### (autoloads nil "my-config" "my-config.el" (0 0 0 0))
;;; Generated autoloads from my-config.el

(autoload 'spacemacs/safe-set-leader-keys "my-config" "\
Like `spacemacs/set-leader-keys' but emits a warning if the key is already bound.

\(fn &rest BINDINGS)" nil nil)

;;;***

;;;### (autoloads nil "my-utils" "my-utils.el" (0 0 0 0))
;;; Generated autoloads from my-utils.el

(autoload 'insert-date "my-utils" "\
Inserts todays date into the current buffer.
With PREFIX time will be included as well.

\(fn ARG)" t nil)

(autoload 'kill-eshell "my-utils" "\
Forces eshell to quit if it gets stuck with 'text is read-only'.

\(fn)" t nil)

(autoload 'lisp-sandbox "my-utils" "\
Create a scratch buffer in lisp interaction mode

\(fn)" t nil)

(autoload 'magit-diff-this-file "my-utils" "\


\(fn)" t nil)

(autoload 'projectile-copy-directory-path "my-utils" "\
Show and copy the full path to the current project directory in the minibuffer.

\(fn)" t nil)

(autoload 'sort-lines-by-length "my-utils" "\
Sort lines by length.

\(fn REVERSE BEG END)" t nil)

(autoload 'open-terminal "my-utils" "\


\(fn)" t nil)

(autoload 'projectile-open-terminal "my-utils" "\


\(fn)" t nil)

(autoload 'open-file-manager "my-utils" "\


\(fn)" t nil)

;;;***

(provide 'my-autoloads)
;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; my-autoloads.el ends here
