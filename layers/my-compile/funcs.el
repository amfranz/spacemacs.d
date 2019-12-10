;; -*- lexical-binding: t -*-

(defun first-error-no-select (&optional n)
  "Move point to the first error in the `next-error' buffer and highlight match.
With prefix arg N, visit the Nth error.
Finds and highlights the source line like \\[first-error], but does not
select the source buffer."
  (interactive "p")
  (let ((next-error-highlight next-error-highlight-no-select))
    (next-error n t))
  (pop-to-buffer next-error-last-buffer))

(spacemacs|define-transient-state goto-error
  :title "Goto Error Transient State"
  :doc "
 [_f_] first error [_n_/_j_] next error [_N_/_p_/_k_] previous error [_q_] quit"
  :on-enter
  (pop-to-buffer next-error-last-buffer)
  :bindings
  ("f" first-error-no-select)
  ("n" next-error-no-select)
  ("j" next-error-no-select)
  ("N" previous-error-no-select)
  ("p" previous-error-no-select)
  ("k" previous-error-no-select)
  ("q" nil :exit t))

(defun compilation-auto-quit-window-finish-function (buffer status)
  "Quit the *compilation* window if it went well."
  (let ((window (get-buffer-window buffer)))
    (when (and (equal status "finished\n")
               (compilation-no-warnings-or-errors-p))
      (run-with-timer
       (or compilation-auto-quit-window-delay 0) nil
       (lambda nil
         (when (and (window-live-p window)
                    (eq (window-buffer window)
                        buffer)
                    (not (eq (selected-window)
                             window)))
           (save-selected-window
             (quit-window nil window))))))))

(define-minor-mode compilation-auto-quit-window
  "Automatically close the *compilation* window if it went well."
  :global t
  (cond (compilation-auto-quit-window
         (add-hook 'compilation-finish-functions
                   'compilation-auto-quit-window-finish-function))
        (t
         (remove-hook 'compilation-finish-functions
                      'compilation-auto-quit-window-finish-function))))

(defun compilation-no-warnings-or-errors-p (&optional buffer)
  "Return t, if no gotoable output appeared."
  (with-current-buffer (or buffer (current-buffer))
    (save-excursion
      (goto-char (point-min))
      (let ((compilation-skip-threshold 1))
        (not (ignore-errors
               (compilation-next-error 1)
               t))))))

(defun display-compilation-buffer ()
  (interactive)
  (when compilation-last-buffer
    (pop-to-buffer compilation-last-buffer)))
