;; -*- lexical-binding: t -*-
;;
;; This file contains functions that are likely needed during the the startup
;; sequence to help configure Emacs.
;;

;;;###autoload
(defun spacemacs/safe-set-leader-keys (&rest bindings)
  "Like `spacemacs/set-leader-keys' but emits a warning if the key is already bound."
  (let* ((bindcpy (copy-list bindings))
         (key (pop bindcpy))
         (def (pop bindcpy)))
    (while key
      (when-let ((prev-def (lookup-key spacemacs-default-map (kbd key))))
        (lwarn 'spacemacs :warning
               "Warning: overriding leader key binding for `%s' with `%s', previous definition was `%s'"
               key def prev-def))
      (setq key (pop bindcpy) def (pop bindcpy))))
  (apply #'spacemacs/set-leader-keys bindings))

(provide 'my-config)
