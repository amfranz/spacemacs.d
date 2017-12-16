(defconst mfa-el-patch-packages '(el-patch))

(defun mfa-el-patch/init-el-patch ()
  (use-package el-patch
    :defer t))
