;; -*- lexical-binding: t -*-

(defun term-send-esc ()
  "Send ESC in term mode."
  (interactive)
  (term-send-raw-string "\e"))

(defun my-shell//term-customize-faces ()
  (when (memq 'zenburn custom-enabled-themes)
    (custom-theme-set-faces
     'zenburn
     '(term-color-default ((t (:foreground "#DBDBCB" :background "#3F3F3F"))))
     '(term-color-black   ((t (:foreground "#3F3F3F" :background "#708F80"))))
     '(term-color-red     ((t (:foreground "#A95050" :background "#DBA2A2"))))
     '(term-color-green   ((t (:foreground "#60B389" :background "#72D4A2"))))
     '(term-color-yellow  ((t (:foreground "#DEAE8E" :background "#EFDEAE"))))
     '(term-color-blue    ((t (:foreground "#99B7D6" :background "#93BEF2"))))
     '(term-color-magenta ((t (:foreground "#DB8BC2" :background "#EB92D2"))))
     '(term-color-cyan    ((t (:foreground "#8BCFD2" :background "#92DFE2"))))
     '(term-color-white   ((t (:foreground "#DBDBCB" :background "#FEFEFE")))))))

(defun my-shell//helm-kill-ring-action-yank-1 (orig-fun str)
  (if (derived-mode-p 'vterm-mode)
      (vterm-send-string str t)
    (funcall orig-fun str)))
