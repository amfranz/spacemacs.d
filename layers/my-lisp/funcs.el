;; -*- lexical-binding: t -*-

(defun my-lisp/insert-at-beginning-of-sexp ()
  "Go to the character following the previous opening parenthesis and enter
insert state."
  (interactive)
  (lisp-state-prev-opening-paren)
  (forward-char)
  (evil-insert-state))

(defun my-lisp/insert-at-end-of-sexp ()
  "Go to the next closing parenthesis and enter insert state."
  (interactive)
  (lisp-state-next-closing-paren)
  (evil-insert-state))

(defun my-lisp/mark-emacs-lisp-list-with-prefix ()
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

(defun my-lisp//add-emacs-lisp-mode-expansions ()
  "Add expansions for buffers in `emacs-lisp-mode'."
  (set (make-local-variable 'er/try-expand-list)
       (append er/try-expand-list '(my-lisp/mark-emacs-lisp-list-with-prefix))))
