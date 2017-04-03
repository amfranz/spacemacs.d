(defconst mfa-defproject-packages '(defproject))

(defun mfa-defproject/init-defproject ()
  (use-package defproject
    :config
    (let ((projects-el (concat configuration-layer-private-directory "projects.el")))
      (when (file-exists-p projects-el)
            (load-file projects-el)))))
