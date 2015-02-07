(defconst mfa-diff-packages '(diffview))

(defun mfa-diff/init-diffview ()
  (use-package diffview
    :defer t
    :init
    (spacemacs/set-leader-keys-for-major-mode 'diff-mode
      "v" #'diffview-current)))
