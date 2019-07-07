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
