;; -*- lexical-binding: t -*-
;;
;; This file contains functions that are likely needed during the the startup
;; sequence to help configure Emacs.
;;

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

(provide 'my-config)
