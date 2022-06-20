;; -*- lexical-binding: t -*-
;;
;; This file contains functions that are not needed during the the startup
;; sequence. Loading of this file should be able to be delayed until the user
;; initiates a functionality implemented here.
;;

(eval-when-compile
  (require 'subr-x))

;;;###autoload
(defun my-find-custom-file ()
  "Edit the `custom-file', in the current window."
  (interactive)
  (find-file-existing custom-file))

;;;###autoload
(defun my-insert-date-or-time ()
  "Inserts the current time and/or date into the current buffer.

The timestamp will be formatted by `format-time-string'. The user will be asked
which format string to use."
  (interactive)
  (barf-if-buffer-read-only)
  (insert (format-time-string (completing-read "Format string: "
                                               '("%m/%d/%y"
                                                 "%Y-%m-%d"
                                                 "%Y-%m-%d %H:%M:%S")))))

(defconst lisp-sandbox-buffer-name "*lisp-sandbox*")

;;;###autoload
(defun lisp-sandbox ()
  "Switch to a scratch buffer with major mode `lisp-interaction-mode.'"
  (interactive)
  (let ((buffer (get-buffer lisp-sandbox-buffer-name)))
    (unless buffer
      (setq buffer (get-buffer-create lisp-sandbox-buffer-name))
      (with-current-buffer buffer
        (let ((buffer-undo-list t))
          (insert ";; -*- lexical-binding: t -*-\n"))
        (lisp-interaction-mode)
        (setq lexical-binding t)))
    (switch-to-buffer buffer)))

;;;###autoload
(defun projectile-copy-project-root ()
  "Show and copy the full path to the current project directory in the
minibuffer."
  (interactive)
  (if-let (project-root (projectile-project-root))
      (message "%s" (kill-new project-root))
    (message "WARNING: Current buffer is not part of a project!")))

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
(defun sort-lines-insert (value)
  (interactive "sInsert: ")
  (let* ((prefix (buffer-substring-no-properties (point-at-bol)
                                                 (point)))
         (prefix-len (length prefix)))
    ;; Walk lines backwards to find the first line with this prefix.
    (let ((beginning-of-list (point-at-bol)))
      (forward-line -1)
      (while (and (not (bobp))
                  (let ((line (buffer-substring-no-properties (point)
                                                              (point-at-eol))))
                    (string-prefix-p prefix line)))
        (setq beginning-of-list (point))
        (forward-line -1))
      (goto-char beginning-of-list))
    ;; Walk lines forwards to find the first line that is not less than the value.
    (while (and (not (eobp))
                (let ((line (buffer-substring-no-properties (point)
                                                            (point-at-eol))))
                  (and (string-prefix-p prefix line)
                       (let ((suffix (substring-no-properties line prefix-len)))
                         (string-lessp suffix value)))))
      (forward-line))
    ;; If we are not at the beginning of a line, then that means `forward-line'
    ;; encountered the last line of the buffer and it was missing a newline. The
    ;; point is now at the end of the last line of the buffer. We need to add
    ;; the missing newline for the following insertion to behave sanely.
    (unless (bolp)
      (insert "\n"))
    (insert prefix value "\n")
    (forward-line -1)
    (forward-char prefix-len)))

;;;###autoload
(defun open-terminal ()
  (interactive)
  (let ((process-environment (cons "EMACS_SOCKET_NAME" initial-environment)))
    (call-process "konsole" nil 0 nil
                  "--workdir" (expand-file-name default-directory))))

;;;###autoload
(defun projectile-open-terminal ()
  (interactive)
  (let ((default-directory (projectile-project-root)))
    (open-terminal)))

;;;###autoload
(defun projectile-open-shell ()
  (interactive)
  (projectile-with-default-dir (projectile-project-root)
    (call-interactively 'spacemacs/default-pop-shell)))

;;;###autoload
(defun open-file-manager ()
  (interactive)
  (let ((process-environment (cons "EMACS_SOCKET_NAME" initial-environment)))
    (call-process "xdg-open" nil 0 nil (expand-file-name default-directory))))

;;;###autoload
(defun dedup-safe-local-variables ()
  (let ((dedup (copy-list safe-local-variable-values)))
    (delete-dups dedup)
    (unless (eq (length dedup) (length safe-local-variable-values))
      (customize-save-variable 'safe-local-variable-values dedup))))

;;;###autoload
(defun renumber-list (start end)
  "Renumber the list items in the current region."
  (interactive "*r")
  (save-excursion
    (goto-char start)
    (when (re-search-forward "[0-9]+" end t)
      (let ((guide (match-string-no-properties 0))
            (end-mark (copy-marker end)))
        (unwind-protect
            (let ((num (string-to-number guide))
                  (fmt (format "%%0%dd" (length guide))))
              (while (and (re-search-forward "^" end-mark t)
                          (re-search-forward "[0-9]+" end-mark t))
                (setq num (1+ num))
                (replace-match (format fmt num))))
          (set-marker end-mark nil))))))

;;;###autoload
(defun read-string-with-history (prompt history)
  (let ((reply (read-string prompt
                            (car (symbol-value history))
                            (cons history 1))))
    (when (string-empty-p reply)
      (add-to-history history reply nil t))
    reply))

(provide 'my-utils)
