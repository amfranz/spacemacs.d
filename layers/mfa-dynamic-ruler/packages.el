(defconst mfa-dynamic-ruler-packages '(dynamic-ruler))

(defun mfa-dynamic-ruler/init-dynamic-ruler ()
  (use-package dynamic-ruler
    :defer t
    :init
    (progn
      (defvar dynamic-ruler--active nil)

      (defun evil-dynamic-ruler ()
        "Temporarily display a horizontal ruler at `point'."
        (interactive)
        (let ((dynamic-ruler--active t))
          (dynamic-ruler)))

      (defun evil-dynamic-ruler-vertical ()
        "Temporarily display a vertical ruler at `point'."
        (interactive)
        (let ((dynamic-ruler--active t))
          (dynamic-ruler-vertical)))

      (spacemacs/declare-prefix "bg" "gauge/ruler")
      (spacemacs/set-leader-keys "bgh" #'evil-dynamic-ruler)
      (spacemacs/set-leader-keys "bgv" #'evil-dynamic-ruler-vertical))
    :config
    (progn
      (defun dynamic-ruler--map-hjkl (key)
        (if (and dynamic-ruler--active (vectorp key))
            (let ((k (elt key 0)))
              (cond ((eq k ?j) [?n])
                    ((eq k ?k) [?p])
                    ((eq k ?l) [?f])
                    ((eq k ?h) [?b])
                    (t key)))
          key))

      (advice-add 'momentary-string-display
                  :filter-return #'dynamic-ruler--map-hjkl)
      (advice-add 'dynamic-ruler-momentary-column
                  :filter-return #'dynamic-ruler--map-hjkl))))
