;;; my-autoloads.el --- automatically extracted autoloads
;;
;;; Code:


;;;### (autoloads nil "display" "display.el" (0 0 0 0))
;;; Generated autoloads from display.el

(autoload 'display-scaling-factor "display" "\
Reads the display scaling factor from the Cinnamon dconf database.
This will return 2 on Hi-DPI displays, 1 otherwise.

\(fn)" nil nil)

(autoload 'display-adjusted-font-size "display" "\
Returns my preferred font size of 14 point multiplied by the screen scaling
factor set by the desktop environment.

\(fn)" nil nil)

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

(autoload 'my-load-custom-file "my-config" "\
Load and apply persisted custom settings.

This function is supposed to be invoked only once, at the very end of the Emacs
startup sequence. It is possible that additional packages have been installed
earlier during startup sequence, in which case `package-selected-packages' has
already been customized. Its value is important for `package-autoremove' to be
able to function correctly.

For this reason, if the variable `package-selected-packages' has a value, the
current value will be preserved even if the custom file contains another value
for it. Additionally, the custom file will be updated with this new value." nil nil)

(autoload 'spacemacs/replace-leader-key "my-config" "\
Similar to `spacemacs/set-leader-keys' for a single binding, but emits a
warning if the key is bound to neither ORIG nor DEF.

\(fn KEY ORIG DEF)" nil nil)

(autoload 'spacemacs/warn-if-leader-key-bound "my-config" "\
Emit a warning if the leader key is already bound to a different defun.

\(fn KEY DEF)" nil nil)

(autoload 'spacemacs/safe-set-leader-keys "my-config" "\
Like `spacemacs/set-leader-keys' but emits a warning if the key is already
bound to a different defun.

\(fn &rest BINDINGS)" nil nil)

(autoload 'spacemacs/warn-if-leader-key-for-major-mode-bound "my-config" "\
Emit a warning if the leader key for a major mode is already bound to a
different defun.

\(fn MODE KEY DEF)" nil nil)

(autoload 'spacemacs/safe-set-leader-keys-for-major-mode "my-config" "\
Like `spacemacs/set-leader-keys-for-major-mode' but emits a warning if the
key is already bound to a different defun.

\(fn MODE &rest BINDINGS)" nil nil)

(autoload 'add-lazy-hook "my-config" "\
Like `add-hook' but also runs the hook immediately in all buffers derived
from the given mode.

This function should be used instead of `add-hook' whenever it is possible that
buffers of that mode already exist, for example in logic that is lazy loaded via
`with-eval-after-load' or `eval-after-load'.

\(fn MODE FUN)" nil nil)

(register-definition-prefixes "my-config" '("spacemacs//lookup-leader-key"))

;;;***

;;;### (autoloads nil "my-theming" "my-theming.el" (0 0 0 0))
;;; Generated autoloads from my-theming.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "my-theming" '(#("custom-theme-alter-" 0 19 (fontified nil face font-lock-function-name-face)) #("spacemacs/" 0 10 (fontified t face font-lock-function-name-face)))))

;;;***

;;;### (autoloads nil "my-utils" "my-utils.el" (0 0 0 0))
;;; Generated autoloads from my-utils.el

(autoload 'my-find-custom-file "my-utils" "\
Edit the `custom-file', in the current window." t nil)

(autoload 'my-insert-date-or-time "my-utils" "\
Inserts the current time and/or date into the current buffer.

The timestamp will be formatted by `format-time-string'. The user will be asked
which format string to use." t nil)

(autoload 'lisp-sandbox "my-utils" "\
Switch to a scratch buffer with major mode `lisp-interaction-mode.'" t nil)

(autoload 'projectile-copy-project-root "my-utils" "\
Show and copy the full path to the current project directory in the
minibuffer." t nil)

(autoload 'sort-lines-by-length "my-utils" "\
Sort lines by length.

\(fn REVERSE BEG END)" t nil)

(autoload 'sort-lines-insert "my-utils" "\


\(fn VALUE)" t nil)

(autoload 'open-terminal "my-utils" nil t nil)

(autoload 'projectile-open-terminal "my-utils" nil t nil)

(autoload 'projectile-open-shell "my-utils" nil t nil)

(autoload 'open-file-manager "my-utils" nil t nil)

(autoload 'dedup-safe-local-variables "my-utils" nil nil nil)

(autoload 'renumber-list "my-utils" "\
Renumber the list items in the current region.

\(fn START END)" t nil)

(autoload 'read-string-with-history "my-utils" "\


\(fn PROMPT HISTORY)" nil nil)

(register-definition-prefixes "my-utils" '("lisp-sandbox-buffer-name"))

;;;***

(provide 'my-autoloads)
;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; my-autoloads.el ends here
