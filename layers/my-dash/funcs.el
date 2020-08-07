;; -*- lexical-binding: t -*-

;; Inspired by `zeal-at-point-mode-alist'
(defcustom dash-docs-mode-alist
  `((c++-mode . ("C++"))
    (c-mode . ("C"))
    (cmake-mode . ("CMake"))
    (go-mode . ("Go"))
    (python-mode . ("Python_3"))
    (rust-mode . ("Rust")))
  "Alist which maps major modes to Dash docsets.
Each entry is of the form (MAJOR-MODE . DOCSET-TAGS) where
MAJOR-MODE is a symbol and DOCSET-TAGs is a list containing
the names of one or more Dash docsets."
  :type '(repeat (cons (symbol :tag "Major mode name")
                       (repeat (string :tag "Docset"))))
  :group 'dash-docs)

;; Make this variable safe for use in `dir-locals.el'.
(defvar-local dash-docs-docsets nil
  "A subset of docsets to activate in the current buffer.")
(put 'dash-docs-docsets 'safe-local-variable 'listp)

;; If the user did not configure a list of docsets to activate, choose a default
;; based on major mode. To avoid errors when searching, only docsets that are
;; actually installed will be activated.
(defun my-dash//set-dash-docset-default (&optional installed-docsets)
  (unless dash-docs-docsets
    (if-let (candidates (cdr (assq major-mode dash-docs-mode-alist)))
        (setq-local dash-docs-docsets
                    (-intersection (or installed-docsets
                                       (dash-docs-installed-docsets))
                                   candidates)))))

(defun my-dash//configure-dash-docs ()
  ;; Don't populate `dash-docs-common-docsets'
  (setq dash-autoload-common-docsets nil)
  (with-eval-after-load 'dash-docs
    (unless (string-blank-p dash-docs-docset-newpath)
      (setq dash-docs-docsets-path
            (expand-file-name dash-docs-docset-newpath))))

  ;; Choose a default docset based on the major mode
  (with-eval-after-load 'dash-docs
    (require 'zeal-at-point)
    (add-hook 'hack-local-variables-hook #'my-dash//set-dash-docset-default)
    (let ((installed-docsets (dash-docs-installed-docsets)))
      (dolist (buffer (buffer-list))
        (with-current-buffer buffer
          (my-dash//set-dash-docset-default installed-docsets)))))

  ;; May speed up some queries
  (setq dash-docs-enable-debugging nil))
