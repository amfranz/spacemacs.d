;; -*- lexical-binding: t -*-

(defun my-org/org-agenda (&optional prefix)
  "Open the main org agenda file.
Giving the command a PREFIX arg will open the file in another window."
  (interactive "P")
  (let ((agenda (concat org-directory "agenda/agenda.org")))
    (if prefix
        (find-file-other-window agenda)
      (find-file agenda))))

(defun my-org/org-backlog (&optional prefix)
  "Open the main org backlog file.
Giving the command a PREFIX arg will open the file in another window."
  (interactive "P")
  (let ((backlog (concat org-directory "agenda/backlog.org")))
    (if prefix
        (find-file-other-window backlog)
      (find-file backlog))))

(defun my-org/org-inbox (&optional prefix)
  "Open the org agenda inbox file.
Giving the command a PREFIX arg will open the file in another window."
  (interactive "P")
  (let ((inbox (concat org-directory "agenda/inbox.org")))
    (if prefix
        (find-file-other-window inbox)
      (find-file inbox))))

(defun my-org/org-index (&optional prefix)
  "Open the org index file.
Giving the command a PREFIX arg will open the file in another window."
  (interactive "P")
  (let ((index (concat org-directory "index.org")))
    (if prefix
        (find-file-other-window index)
      (find-file index))))

(defun my-org/projectile-notes (&optional arg)
  "Open the notes file of the current project.
Giving the command a PREFIX arg will open the file in another window."
  (interactive "P")
  (if-let (project-path (projectile-project-root))
      (let ((notes-org (concat project-path "notes.org")))
        (if arg
            (find-file-other-window notes-org)
          (find-file notes-org)))
    (message "WARNING: Current buffer is not part of a project!")))

(defun my-org//adjust-org-priority-faces ()
  (when (memq 'zenburn custom-enabled-themes)
    (zenburn-with-color-variables
      (custom-theme-set-variables
       'zenburn
       `(org-priority-faces '((?A . (:foreground ,zenburn-magenta))
                              (?B . (:foreground ,zenburn-yellow))
                              (?C . (:foreground ,zenburn-cyan))))))))

(defun my-org//ad-preserve-outline-visibility (orig-fun &rest args)
  (if (derived-mode-p 'org-mode)
      (org-save-outline-visibility nil
        (apply orig-fun args))
    (apply orig-fun args)))

(defvar my-org--org-display-inline-images-active nil
  "Tracks when `org-display-inline-images' is active.")

(defun my-org//mark-org-display-inline-images (orig-fun &rest args)
  "Track when `org-display-inline-images' is active so that the background color
to use for transparent areas is applied only to `org-mode' inline images."
  (let ((my-org--org-display-inline-images-active t))
    (apply orig-fun args)))

(defun my-org//create-image-with-background-color (args)
  "Pass background color to use for transparent areas of `org-mode' inline
images to `create-image'."
  (if (and my-org--org-display-inline-images-active
           (not (plist-member (cdddr args) :background)))
      (append args (list :background org-inline-image-background))
    args))
