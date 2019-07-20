;; -*- lexical-binding: t -*-

(defvar lsp-ccls-library-directories '("/usr/include/" "/usr/local/include/")
  "List of directories which will considered to be libraries.")

;; Use `c++-mode' for Qt4/5 headers. These mode can not be chosen based on file
;; extension, because these files have no extension.
(add-to-list 'auto-mode-alist '("/qt[45]/Q[^/]+/Q[^/]+\\'" . c++-mode))

;; Auto-indent after paste in C++ makes life very difficult.
(add-to-list 'spacemacs-indent-sensitive-modes 'c++-mode)
