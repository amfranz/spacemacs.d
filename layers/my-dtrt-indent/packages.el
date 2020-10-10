;; -*- lexical-binding: t -*-

(defconst my-dtrt-indent-packages '(dtrt-indent))

(defun my-dtrt-indent/init-dtrt-indent ()
  (use-package dtrt-indent
    :diminish
    :defer t
    :init
    (dolist (mode dtrt-indent-mode-whitelist)
      (add-hook (intern (concat (symbol-name mode) "-hook"))
                #'dtrt-indent//maybe-enable))
    :config
    (setq dtrt-indent-verbosity 0)))
