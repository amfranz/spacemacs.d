(defconst my-el-patch-packages '(el-patch))

(defun my-el-patch/init-el-patch ()
  (use-package el-patch
    :defer t))
