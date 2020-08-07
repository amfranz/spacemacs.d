;; -*- lexical-binding: t -*-

(defconst my-journal-packages '(org-journal))

(defun my-journal/post-init-org-journal ()
  ;; Share my journal to all my devices via Syncthing.
  (setq org-journal-dir "~/Sync/org/journal/")

  ;; This is based on suggestions from: https://so.nwalsh.com/2020/02/29/dot-emacs
  ;; Thanks for Planet Emacs for the reference.
  (setq org-journal-file-type 'weekly
        org-journal-file-header #'my-journal//file-header
        org-journal-file-format "%Y-%m-%d.org"
        org-journal-date-prefix "* "
        org-journal-date-format "%A, %x"
        org-journal-time-prefix "** "
        org-journal-time-format "")

  ;; Set the initial visibility of journal files to show all headlines.
  (add-hook 'org-journal-mode-hook #'my-journal//set-org-journal-startup-folded)

  ;; TODO: improve comment
  ;; extra keybindings for org functionality.
  (spacemacs/safe-set-leader-keys "ooj" #'my-journal/org-journal-today)

  ;; When carrying over todo items from a previous journal file, save the new
  ;; journal file immediately so the todo items are sure to not get lost.
  (advice-add 'org-journal-carryover
              :after #'my-journal//save-after-carryover)

  ;; TODO: carryover breaks when `org-journal-carryover-items' calls
  ;; `outline-end-of-subtree', which fails if the file has no headings yet (eg.
  ;; when `org-journal-file-type' is 'daily and the `org-journal-date-prefix'
  ;; has been changed so it does not create a heading). A possible fix, which
  ;; needs to be tested, is to patch it so it does (if
  ;; (org-journal-org-heading-p) (outline-end-of-subtree) (goto-char
  ;; (point-max)))

  ;; Avoid the `auto-mode-alist' being populated with two mappings for
  ;; `org-journal-mode', and then again every time `org-mode-hook' activates.
  ;; This is a hack. The proper fix would be not to update `auto-mode-alist' by
  ;; the defcustom set handler for the custom variables `org-journal-dir' and
  ;; `org-journal-file-format'.
  ;; TODO: this stopped working, fix the following blurb
  ;; We actually remove all `auto-mode-alist' mappings for `org-journal-mode'
  ;; here. We instead configure `org-journal' to use the `.org' extension, and
  ;; customize the major mode to `org-journal-mode' with a `mode' variable in
  ;; the first line of each journal file.
  (when (featurep 'org-journal)
    (message "WARNING: org-journal was loaded too early, auto-mode-alist will likely be incorrect"))
  (remove-hook 'org-mode-hook 'org-journal-update-auto-mode-alist)
  (with-eval-after-load 'org-journal
    (remove-hook 'org-mode-hook 'org-journal-update-auto-mode-alist)))
