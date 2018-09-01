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

;;;### (autoloads nil "my-lisp" "my-lisp.el" (0 0 0 0))
;;; Generated autoloads from my-lisp.el

(autoload 'lisp-sandbox "my-lisp" "\
Create a scratch buffer in lisp interaction mode

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
