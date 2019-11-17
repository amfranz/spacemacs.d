;; -*- lexical-binding: t -*-

(defun my-company//adjust-company-box-theme-faces ()
  "Make `company-box' candidates adhere to the color theme instead of using a
hardcoded bright white text color."
  (when (memq 'zenburn custom-enabled-themes)
    (custom-theme-set-faces
     'zenburn
     '(company-box-candidate ((t (:inherit default)))))))
