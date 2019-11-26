;; -*- lexical-binding: t -*-

(defconst my-dtrt-indent-packages '(dtrt-indent))

(defun my-dtrt-indent/init-dtrt-indent ()
  (use-package dtrt-indent
    :diminish
    :defer t
    :init
    (progn
      ;; Enable `dtrt-indent' mode in all major modes it is whitelisted for.
      (dolist (mode dtrt-indent-mode-whitelist)
        (add-hook (intern (concat (symbol-name mode) "-hook"))
                  #'dtrt-indent//maybe-enable))

      ;; Propagate indent level to `evil-shift-width'.
      (advice-add 'dtrt-indent-try-set-offset :after
                  #'dtrt-indent//adjust-evil-shift-width))))
