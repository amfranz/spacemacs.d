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

(defun my-cpp//find-build-dir ()
  "Finds the build directory of the current project by looking for
compile_commands.json in common build directory locations. Returns the absolute
path to the build directory if found."
  (when-let ((project-dir (projectile-project-root)))
    (let (confirmed-build-dir
          (candidate-dirs (list (projectile-compilation-dir)
                                (concat project-dir "build/"))))
      (while (and candidate-dirs (not confirmed-build-dir))
        (let* ((subject-dir (pop candidate-dirs)))
          (when (file-exists-p (concat subject-dir "compile_commands.json"))
            (setq confirmed-build-dir subject-dir))))
      confirmed-build-dir)))

(defun my-cpp//clangd-use-project-build-directory ()
  "If a project build folder exists with a compilation database exists, store
the build cache there and make use of the compilation database."
  (when-let ((build-dir (my-cpp//find-build-dir)))
    (setq-local lsp-clients-clangd-args
                (list (concat "-compile-commands-dir=" build-dir)))))

(defun my-cpp//ccls-use-project-build-directory ()
  "If a project build folder exists with a compilation database exists, store
the build cache there and make use of the compilation database."
  (when-let ((build-dir (my-cpp//find-build-dir)))
    ;; The `use-package' config hook that Spacemacs installs sets the
    ;; default value of `ccls-initialization-options' with `setq'. If at
    ;; that point the variable is buffer-local this would set the
    ;; buffer-local value instead of the default value. The purpose of the
    ;; `require' is to force the `use-package' config hook to run first
    ;; which will `setq' the default value, afterwards we can safely make
    ;; the variable buffer-local.
    (require 'ccls)
    (setq-local ccls-initialization-options
                (list :cache `(:directory ,(concat build-dir ".ccls-cache/"))
                      :compilationDatabaseDirectory build-dir))))

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

(defun my-cpp//lsp-ui-flycheck-configure ()
  ;; Make flycheck less eager to lint.
  ;; Linting after every keystroke is making the editor sluggish.
  (setq-local flycheck-idle-change-delay 2)
  (setq-local flycheck-idle-buffer-switch-delay 2)
  (setq-local flycheck-check-syntax-automatically
              '(idle-change idle-buffer-switch mode-enabled)))
