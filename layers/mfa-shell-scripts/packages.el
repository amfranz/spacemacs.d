;; -*- lexical-binding: t -*-

(defconst mfa-shell-scripts-packages '(flycheck-bashate
                                       flycheck-checkbashisms
                                       (sh-script :location built-in)))

(defun mfa-shell-scripts/post-init-flycheck-bashate ()
  ;; Spacemacs adds this hook to lazy load the package. The problem is that
  ;; `flycheck-bashate-setup' adds bashate at the beginning of the list. We want
  ;; shellcheck to be the preferred checker, so we want to add bashate to the
  ;; end of the checker list instead.
  (remove-hook 'sh-mode-hook #'flycheck-bashate-setup)
  (with-eval-after-load 'sh-script
    (with-eval-after-load 'flycheck
      (require 'flycheck-bashate)
      (add-to-list 'flycheck-checkers 'bashate 'append)
      (flycheck-add-next-checker 'sh-shellcheck '(warning . bashate) 'append)))

  ;; Redefine the bashate checker to allow for customizable arguments.
  ;; The only addition is the introduction of `flycheck-bashate-args'.
  (with-eval-after-load 'flycheck-bashate
    (flycheck-def-args-var flycheck-bashate-args bashate)
    (flycheck-define-checker bashate
      "A checker using bashate.

See `https://github.com/alexmurray/bashate/'."
      :command ("bashate" (eval flycheck-bashate-args) source)
      :error-patterns ((error line-start "[E] "(message (minimal-match (one-or-more not-newline))) ": '" (one-or-more not-newline) "'\n"
                              " - " (file-name) " : L" line line-end)
                       (warning line-start "[W] "(message (minimal-match (one-or-more not-newline))) ": '" (one-or-more not-newline) "'\n"
                                " - " (file-name) " : L" line line-end))
      :modes sh-mode))

  ;; Be more lenient in regards to intendation by ignoring these:
  ;; - E002: ensure that indents are only spaces, and not hard tabs
  ;; - E003: ensure all indents are a multiple of 4 spaces
  (setq flycheck-bashate-args '("-i" "E002,E003")))

(defun mfa-shell-scripts/init-flycheck-checkbashisms ()
  (use-package flycheck-checkbashisms
    :if (configuration-layer/package-usedp 'flycheck)
    :after (sh-script flycheck)
    :config (flycheck-checkbashisms-setup)))

(defun mfa-shell-scripts/post-init-sh-script ()
  ;; This defun would be added via the default value of `sh-mode-hook', but
  ;; since the package is lazy-loaded the hook variable gets customized before
  ;; the default value takes effect.
  (with-eval-after-load 'sh-script
    (add-hook 'sh-mode-hook #'sh-electric-here-document-mode))

  ;; Adjust `tab-width' to the same value as `sh-basic-offset', and enable
  ;; indentation with tabs. These are only defaults, they can be customized on a
  ;; case per case basis with editorconfig, directory or file-local variables.
  (add-hook 'sh-mode-hook #'mfa-shell-scripts//adjust-indentation))
