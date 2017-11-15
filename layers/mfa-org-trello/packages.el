(defconst mfa-org-trello-packages '(org org-trello))

(defun mfa-org//enable-trello-mode ()
  (when (and (string-prefix-p user-home-directory buffer-file-name)
             (string-match-p "/trello/" buffer-file-name))
    (org-trello-mode)))

(defun mfa-org-trello/post-init-org ()
  (with-eval-after-load 'org
    (add-hook 'org-mode-hook #'mfa-org//enable-trello-mode)))

(defun mfa-org-trello/init-org-trello ()
  (use-package org-trello
    :defer t
    :config
    (setq org-trello-input-completion-mechanism 'helm
          orgtrello-log-level orgtrello-log-warn)))
