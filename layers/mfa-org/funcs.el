;; -*- lexical-binding: t -*-

(defun mfa-org/org-agenda (&optional prefix)
  "Open the main org agenda file.
Giving the command a PREFIX arg will open the file in another window."
  (interactive "P")
  (let ((agenda (concat org-directory "agenda/agenda.org")))
    (if prefix
        (find-file-other-window agenda)
      (find-file agenda))))

(defun mfa-org/org-backlog (&optional prefix)
  "Open the main org backlog file.
Giving the command a PREFIX arg will open the file in another window."
  (interactive "P")
  (let ((backlog (concat org-directory "agenda/backlog.org")))
    (if prefix
        (find-file-other-window backlog)
      (find-file backlog))))

(defun mfa-org/org-inbox (&optional prefix)
  "Open the org agenda inbox file.
Giving the command a PREFIX arg will open the file in another window."
  (interactive "P")
  (let ((inbox (concat org-directory "agenda/inbox.org")))
    (if prefix
        (find-file-other-window inbox)
      (find-file inbox))))

(defun mfa-org/org-index (&optional prefix)
  "Open the org index.
Giving the command a PREFIX arg will open the index in another window."
  (interactive "P")
  (let ((index (concat org-directory "index.org")))
    (if prefix
        (find-file-other-window index)
      (find-file index))))

(defun mfa-org//custom-theme-set-variables ()
  ;; (when (eq 'zenburn spacemacs--cur-theme)
  (when (memq 'zenburn custom-enabled-themes)
    (zenburn-with-color-variables
      (custom-theme-set-variables
       'zenburn
       `(org-priority-faces '((?A . (:foreground ,zenburn-magenta))
                              (?B . (:foreground ,zenburn-yellow))
                              (?C . (:foreground ,zenburn-cyan))))))))

(defun mfa-org//org-show-entry (&rest ignore)
  (when (derived-mode-p 'org-mode)
    (org-show-entry)))

(defun mfa-org//ad-preserve-outline-visibility (orig-fun &rest args)
  (if (derived-mode-p 'org-mode)
      (org-save-outline-visibility nil
        (apply orig-fun args))
    (apply orig-fun args)))
