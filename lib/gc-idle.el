;; -*- lexical-binding: t -*-

;; Garbage collect only during idle times.
(defvar gc-idle-repeat-timer nil
  "Timer for `gc-idle--collect' to reschedule itself.")

(defun gc-idle--collect ()
  "Collect garbage when the user is idle."
  (when (timerp gc-idle-repeat-timer)
    (cancel-timer gc-idle-repeat-timer)
    (setq gc-idle-repeat-timer nil))
  (garbage-collect)
  (setq gc-idle-repeat-timer
        (run-with-idle-timer (time-add (current-idle-time) 300)
                             nil #'gc-idle--collect)))

(defun gc-idle-enable ()
  "Configure garbage collection to occur when the user is idle."
  (run-with-idle-timer 2 t #'gc-idle--collect))

(defun enable-gc-around-advice (orig-fun &rest args)
  "Around advice that turns garbage collection back on while the adviced
function is active."
  (let ((gc-cons-threshold 16777216) ; 16 MB
        (gc-cons-percentage 0.1))
    (apply orig-fun args)))

(defun gc-idle-exempt (fun)
  (advice-add fun :around #'enable-gc-around-advice))

(provide 'gc-idle)
