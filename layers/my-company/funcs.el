;; -*- lexical-binding: t -*-

;; Sometimes the `company-box' frame dies, which causes an error about a dead
;; frame whenever the popup would get displayed. This change checks for that
;; condition and pretends that no such frame was created yet if the current
;; frame is dead. This not only avoids the errors but also allows the package to
;; recover from the condition as it will create a new frame.
(with-eval-after-load 'el-patch
  (el-patch-feature company-box)
  (with-eval-after-load 'company-box
    (el-patch-defun company-box--get-frame (&optional frame)
      "Return the company-box child frame on FRAME."
      (el-patch-swap
        (frame-local-getq company-box-frame frame)
        (let ((frame (frame-local-getq company-box-frame frame)))
          (when (frame-live-p frame) frame))))))
