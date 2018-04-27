;; -*- lexical-binding: t -*-

(defconst mfa-journal-packages '(calendar org-journal))

(defun mfa-journal/init-org-journal ()
  (use-package org-journal
    :defer t
    :init
    (progn
      (spacemacs/declare-prefix "aj" "journal")
      (spacemacs/set-leader-keys
        "ajj" #'org-journal-new-entry
        "ajv" #'view-journal
        "ajs" #'org-journal-search
        "ajS" #'search-all-journals))
    :config
    (progn
      (setq org-journal-dir "~/Dropbox/Workspace/org/journal/"
            org-journal-file-format "%Y-%m-%d.org")

      (setq org-journal-file-pattern (org-journal-format-string->regex
                                      org-journal-file-format))

      (setq spacemacs-org-journal-mode-map (copy-keymap spacemacs-org-mode-map))
      (spacemacs//init-leader-mode-map 'org-journal-mode 'spacemacs-org-journal-mode-map)

      (spacemacs/set-leader-keys-for-major-mode 'org-journal-mode
        "jn" #'org-journal-open-next-entry
        "jp" #'org-journal-open-previous-entry
        "jj" #'org-journal-new-entry
        "js" #'org-journal-search))))

(defun mfa-journal/init-calendar ()
  (use-package calendar
    :defer t
    :init
    (spacemacs/set-leader-keys "oc" #'calendar)
    :config
    (progn
      (define-key calendar-mode-map "Ji" #'org-journal-new-date-entry)
      (define-key calendar-mode-map "Jv" #'org-journal-read-entry)
      (define-key calendar-mode-map "JV" #'org-journal-display-entry)
      (define-key calendar-mode-map "Jn" #'org-journal-next-entry)
      (define-key calendar-mode-map "Jp" #'org-journal-previous-entry)
      (define-key calendar-mode-map "Jf" #'org-journal-search-forever)
      (define-key calendar-mode-map "JF" #'org-journal-search-future)
      (define-key calendar-mode-map "Jw" #'org-journal-search-calendar-week)
      (define-key calendar-mode-map "Jm" #'org-journal-search-calendar-month)
      (define-key calendar-mode-map "Jy" #'org-journal-search-calendar-year))))
