;; -*- lexical-binding: t -*-

(defconst mfa-journal-packages '(org-journal))

(defun mfa-journal/post-init-org-journal ()
  ;; Share my journal to all my devices via Dropbox.
  (setq org-journal-dir "~/Dropbox/Workspace/org/journal/")

  ;; This is based on suggestions from the Spacemacs manual.
  (setq org-journal-date-prefix "# -*- mode: org-journal -*-\n#+TITLE: "
        org-journal-file-format "%Y-%m-%d.org"
        org-journal-time-format ""
        org-journal-time-prefix "* ")

  ;; When carrying over todo items from a previous journal file, save the new
  ;; journal file immediately so the todo items are sure to not get lost.
  (advice-add 'org-journal-carryover
              :after #'mfa-journal//save-carryover)

  ;; Avoid the `auto-mode-alist' being populated with two mappings for
  ;; `org-journal-mode', and then again every time `org-mode-hook' activates.
  ;; This is a hack. The proper fix would be not to update `auto-mode-alist' by
  ;; the defcustom set handler for the custom variables `org-journal-dir' and
  ;; `org-journal-file-format'.
  ;; We actually remove all `auto-mode-alist' mappings for `org-journal-mode'
  ;; here. We instead configure `org-journal' to use the `.org' extension, and
  ;; customize the major mode to `org-journal-mode' with a `mode' variable in
  ;; the first line of each journal file.
  (remove-hook 'org-mode-hook 'org-journal-update-auto-mode-alist)
  (with-eval-after-load 'org-journal
    (setq auto-mode-alist (rassq-delete-all 'org-journal-mode auto-mode-alist))
    (remove-hook 'org-mode-hook 'org-journal-update-auto-mode-alist)))
