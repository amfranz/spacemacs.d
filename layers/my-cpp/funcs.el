;; -*- lexical-binding: t -*-

;; https://github.com/Lindydancer/cmake-font-lock/issues/6
(defun my-cpp//cmake-font-lock-activate()
  "Activate advanced CMake colorization.

To activate this every time a CMake file is opened, use the following:

    (add-hook 'cmake-mode-hook 'cmake-font-lock-activate)"
  (interactive)
  (cmake-font-lock-setup)
  (when (boundp 'font-lock-set-defaults)
    (kill-local-variable 'font-lock-set-defaults))
  (when (and (boundp 'font-lock-mode) font-lock-mode)
    (font-lock-mode -1)
    (font-lock-mode 1)))

(defun my-cpp//ccls-use-project-build-directory ()
  "If a project build folder exists with a compilation database exists, store
the build cache there and make use of the compilation datbase."
  (when-let ((root-dir (projectile-project-root)))
    (let ((build-dir (concat root-dir "build/")))
      (when (file-exists-p (concat build-dir "compile_commands.json"))
        ;; The `use-package' config hook that Spacemacs installs sets the
        ;; default value of `ccls-initialization-options' with `setq'. If at
        ;; that point the variable is buffer-local this would set the
        ;; buffer-local value instead of the default value. The purpose of the
        ;; `require' is to force the `use-package' config hook to run first
        ;; which will `setq' the default value, afterwards we can safely make
        ;; the variable buffer-local.
        (require 'ccls)
        (setq-local ccls-initialization-options
                    (list :cache '(:directory "build/.ccls-cache")
                          :compilationDatabaseDirectory "build"))))))

(defun my-lsp//ccls-add-library-folders-fn (args)
  "Set library-folders-fn in the CCLS lsp-client definition."
  (let ((client (car args)))
    (when (eq 'ccls (lsp--client-server-id client))
      ;; `eval' is a workaround for some weirdness with `defstruct', the macro
      ;; expansion warps the code into a non-functional state. This only happens
      ;; if the `lsp--client' structure has not been defined yet which is the
      ;; case because this file is loaded before `lsp-mode'.
      (eval
       '(setf (lsp--client-library-folders-fn client)
              (lambda (_workspace) lsp-ccls-library-directories))
       `((client . ,client)))))
  args)

;; See https://github.com/Fuco1/smartparens/issues/783
(defun my-cpp//disable-escape-quotes-after-insert ()
  (setq-local sp-escape-quotes-after-insert nil))
