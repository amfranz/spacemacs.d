(defconst mfa-chrome-packages '(edit-server))

(defun mfa-chrome/post-init-edit-server ()
  (with-eval-after-load 'edit-server
    (add-hook 'edit-server-edit-mode-hook
              (lambda ()
                (delete-other-windows)
                (spacemacs/toggle-maximize-frame-on)))))
