;; -*- lexical-binding: t -*-

(defun my-python//treemacs-ignored-file-predicates (file _)
  "Hides python bytecode files in filetree."
  (or (string-suffix-p ".pyc" file)
      (string-suffix-p ".pyo" file)))
