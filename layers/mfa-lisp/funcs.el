;; -*- lexical-binding: t -*-

(defun mfa-lisp/mark-emacs-lisp-list-with-prefix ()
  "Mark pairs (as defined by the mode), including pair chars and emacs lisp list
prefixes."
  (interactive)
  (let ((prefix-chars "@#`'"))
    (unless (zerop (skip-chars-backward prefix-chars (point-at-bol)))
      (setq mark-active nil))
    (skip-chars-forward prefix-chars (point-at-eol))
    (er/mark-outside-pairs)
    (when mark-active
      (skip-chars-backward prefix-chars (point-at-bol)))))

(defun mfa-lisp//add-emacs-lisp-mode-expansions ()
  "Add expansions for buffers in `emacs-lisp-mode'."
  (set (make-local-variable 'er/try-expand-list)
       (append er/try-expand-list '(mfa-lisp/mark-emacs-lisp-list-with-prefix))))
