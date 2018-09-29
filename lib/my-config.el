;; -*- lexical-binding: t -*-
;;
;; This file contains functions that are likely needed during the the startup
;; sequence to help configure Emacs.
;;

;;;###autoload
(defun my-load-custom-file ()
  "Load and apply persisted custom settings.

This function is supposed to be invoked only once, at the very end of the Emacs
startup sequence. It is possible that additional packages have been installed
earlier during startup sequence, in which case `package-selected-packages' has
already been customized. Its value is important for `package-autoremove' to be
able to function correctly.

For this reason, if the variable `package-selected-packages' has a value, the
current value will be preserved even if the custom file contains another value
for it. Additionally, the custom file will be updated with this new value."
  (setq custom-file (concat spacemacs-cache-directory "custom.el"))
  (when (file-exists-p custom-file)
    (let ((selected-pkgs package-selected-packages))
      (load custom-file)
      (when selected-pkgs
        (setq package-selected-packages selected-pkgs)
        (let ((save-silently t))
          (customize-save-variable 'package-selected-packages package-selected-packages))))))

;;;###autoload
(defun spacemacs/warn-if-leader-key-bound (key)
  "Emit a warning if the leader key is already bound."
  (when-let* ((def (lookup-key spacemacs-default-map (kbd key)))
              ((not (integerp def))))
    (lwarn 'spacemacs :warning
           "Leader key `%s' is already bound to `%s'"
           key def)))

;;;###autoload
(defun spacemacs/safe-set-leader-keys (&rest bindings)
  "Like `spacemacs/set-leader-keys' but emits a warning if the key is already
bound."
  (let* ((bindcpy (copy-list bindings))
         (key (pop bindcpy))
         (def (pop bindcpy)))
    (while key
      (spacemacs/warn-if-leader-key-bound key)
      (setq key (pop bindcpy) def (pop bindcpy))))
  (apply #'spacemacs/set-leader-keys bindings))
(put 'spacemacs/safe-set-leader-keys 'lisp-indent-function 'defun)

;;;###autoload
(defun spacemacs/warn-if-leader-key-for-major-mode-bound (mode key)
  "Emit a warning if the leader key for a major mode is already bound."
  (when-let* ((sym (intern (format "spacemacs-%s-map" mode)))
              (map (symbol-value sym))
              (def (lookup-key map (kbd key)))
              ((not (integerp def))))
    (lwarn 'spacemacs :warning
           "Leader key `%s' for major mode `%s' is already bound to `%s'"
           key mode def)))

;;;###autoload
(defun spacemacs/safe-set-leader-keys-for-major-mode (mode &rest bindings)
  "Like `spacemacs/set-leader-keys-for-major-mode' but emits a warning if the
key is already bound."
  (let* ((bindcpy (copy-list bindings))
         (key (pop bindcpy))
         (def (pop bindcpy)))
    (while key
      (spacemacs/warn-if-leader-key-for-major-mode-bound mode key)
      (setq key (pop bindcpy) def (pop bindcpy))))
  (apply #'spacemacs/set-leader-keys-for-major-mode mode bindings))
(put 'spacemacs/safe-set-leader-keys-for-major-mode 'lisp-indent-function 'defun)

;;;###autoload
(defun add-lazy-hook (mode fun)
  "Like `add-hook' but also runs the hook immediately in all buffers derived
from the given mode.

This function should be used instead of `add-hook' whenever it is possible that
buffers of that mode already exist, for example in logic that is lazy loaded via
`with-eval-after-load' or `eval-after-load'."
  (let ((hook (intern (concat (symbol-name mode) "-hook"))))
    (add-hook hook fun))
  (dolist (buffer (buffer-list))
    (with-current-buffer buffer
      (when (derived-mode-p mode)
        (funcall fun)))))

(provide 'my-config)
