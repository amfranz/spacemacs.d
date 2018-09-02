;; -*- lexical-binding: t -*-

(defconst mfa-journal-packages '(org-journal))

(defun mfa-journal/post-init-org-journal ()
  (setq org-journal-dir "~/Dropbox/Workspace/org/journal/"
        org-journal-file-format "%Y-%m-%d.org")

  ;; When carrying over todo items from a previous journal file, save the new
  ;; journal file immediately so the todo items are sure to not get lost.
  (advice-add 'org-journal-carryover :after #'save-buffer)

  (with-eval-after-load 'org-journal
    (setq org-journal-file-pattern (org-journal-format-string->regex
                                    org-journal-file-format))))

    ;; (setq spacemacs-org-journal-mode-map (copy-keymap spacemacs-org-mode-map))
    ;; (spacemacs//init-leader-mode-map 'org-journal-mode 'spacemacs-org-journal-mode-map)

    ;; (spacemacs/set-leader-keys-for-major-mode 'org-journal-mode
    ;;   "jn" #'org-journal-open-next-entry
    ;;   "jp" #'org-journal-open-previous-entry
    ;;   "jj" #'org-journal-new-entry
    ;;   "js" #'org-journal-search)))
