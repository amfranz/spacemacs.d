(defun mfa-string-inflection/auto-cycle ()
  (interactive)
  (cond
   ((eq major-mode 'emacs-lisp-mode)
    (string-inflection-all-cycle))
   ((eq major-mode 'python-mode)
    (string-inflection-python-style-cycle))
   ((eq major-mode 'java-mode)
    (string-inflection-java-style-cycle))
   ((eq major-mode 'ruby-mode)
    (string-inflection-ruby-style-cycle))
   (t
    (string-inflection-all-cycle))))
