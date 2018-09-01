;; -*- lexical-binding: t -*-
;;
;; This file contains functions that are likely needed during the the startup
;; sequence to help configure Emacs.
;;

;;;###autoload
(defun spacemacs/warn-if-leader-key-bound (key)
  (when-let ((def (lookup-key spacemacs-default-map (kbd key))))
    (lwarn 'spacemacs :warning
           "Leader key `%s' is already bound to `%s'"
           key def)))

;;;###autoload
(defun spacemacs/safe-set-leader-keys (&rest bindings)
  "Like `spacemacs/set-leader-keys' but emits a warning if the key is already bound."
  (let* ((bindcpy (copy-list bindings))
         (key (pop bindcpy))
         (def (pop bindcpy)))
    (while key
      (spacemacs/warn-if-leader-key-bound key)
      (setq key (pop bindcpy) def (pop bindcpy))))
  (apply #'spacemacs/set-leader-keys bindings))
(put 'spacemacs/safe-set-leader-keys 'lisp-indent-function 'defun)

(provide 'my-config)
