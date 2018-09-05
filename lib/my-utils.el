;; -*- lexical-binding: t -*-
;;
;; This file contains functions that are not needed during the the startup
;; sequence. Loading of this file should be able to be delayed until the user
;; initiates a functionality implemented here.
;;

;;;###autoload
(defun insert-date (arg)
  "Inserts todays date into the current buffer.
With PREFIX time will be included as well."
  (interactive "P")
  (insert (if arg
              (format-time-string "%Y-%m-%d %H:%M:%S")
            (format-time-string "%Y-%m-%d"))))

;;;###autoload
(defun kill-eshell ()
  "Forces eshell to quit if it gets stuck with 'text is read-only'."
  (interactive)
  (let ((inhibit-read-only t))
    (kill-this-buffer)))

;;;###autoload
(defun lisp-sandbox ()
  "Create a scratch buffer in lisp interaction mode"
  (interactive)
  (switch-to-buffer (get-buffer-create "*lisp-sandbox*"))
  (lisp-interaction-mode))

;;;###autoload
(defun magit-diff-this-file ()
  (interactive)
  (let ((file-name (buffer-file-name)))
    (if file-name
        (magit-diff "HEAD" nil (list buffer-file-name))
      (error "Buffer not visiting a file"))))

;;;###autoload
(defun projectile-copy-directory-path ()
  "Show and copy the full path to the current project directory in the minibuffer."
  (interactive)
  (let ((project-root (projectile-project-root)))
    (message "%s" (kill-new project-root))))

;; http://stackoverflow.com/questions/30697523/how-to-get-emacs-to-sort-lines-by-length
;;;###autoload
(defun sort-lines-by-length (reverse beg end)
  "Sort lines by length."
  (interactive "P\nr")
  (save-excursion
    (save-restriction
      (narrow-to-region beg end)
      (goto-char (point-min))
      (let ;; To make `end-of-line' and etc. to ignore fields.
          ((inhibit-field-text-motion t))
        (sort-subr reverse 'forward-line 'end-of-line nil nil
                   (lambda (l1 l2)
                     (apply #'< (mapcar (lambda (range) (- (cdr range) (car range)))
                                        (list l1 l2)))))))))

;;;###autoload
(defun open-terminal ()
  (interactive)
  (let ((process-environment (cons "EMACS_SOCKET_NAME" initial-environment)))
    (call-process "konsole" nil 0 nil "--workdir" (expand-file-name default-directory))))

;;;###autoload
(defun projectile-open-terminal ()
  (interactive)
  (let ((default-directory (projectile-project-root)))
    (open-terminal)))

;;;###autoload
(defun open-file-manager ()
  (interactive)
  (let ((process-environment (cons "EMACS_SOCKET_NAME" initial-environment)))
    (call-process "xdg-open" nil 0 nil (expand-file-name default-directory))))

(provide 'my-utils)
