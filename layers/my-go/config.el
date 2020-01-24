;; -*- lexical-binding: t -*-

;; Automatic re-indentation after paste in `go-mode' may trigger a call to
;; goimports which will remove unused imports. That makes it frustrating to
;; paste source code containing new package imports, as they will immediately be
;; removed again if the buffer doesn't yet contain any logic using the new
;; package.
;;
;; To avoid this issue we will disable automatic re-indentation after paste. The
;; task to re-indent the source code will be left to an `after-save-hook' which
;; calls goimports, gofmt or equivalent.
;;
(add-to-list 'spacemacs-indent-sensitive-modes 'go-mode)
