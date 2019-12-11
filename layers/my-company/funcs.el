;; -*- lexical-binding: t -*-

(defun my-company//adjust-company-box-theme-faces ()
  "Make `company-box' candidates adhere to the color theme instead of using a
hardcoded bright white text color."
  (when (memq 'zenburn custom-enabled-themes)
    (custom-theme-set-faces
     'zenburn
     '(company-box-candidate ((t (:inherit default)))))))

;; Sometimes the `company-box' frame dies, which causes an error about a dead
;; frame whenever the popup would get displayed. This change checks for that
;; condition and pretends that no such frame was created yet if the current
;; frame is dead. This not only avoids the errors but also allows the package to
;; recover from the condition as it will create a new frame.
(with-eval-after-load 'el-patch
  (el-patch-feature company-box)
  (with-eval-after-load 'company-box
    (el-patch-defun company-box--get-frame ()
      "Return the child frame."
      (el-patch-swap
        (frame-parameter nil 'company-box-frame)
        (let ((frame (frame-parameter nil 'company-box-frame)))
          (when (frame-live-p frame) frame))))))
