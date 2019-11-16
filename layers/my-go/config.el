;; -*- lexical-binding: t -*-

;; Auto-indent after paste in Go triggers goimports, that makes pasting imports
;; difficult. Let's let gofmt triggered by file save handle the indentation.
(add-to-list 'spacemacs-indent-sensitive-modes 'go-mode)
