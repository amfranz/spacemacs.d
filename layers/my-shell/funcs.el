;; -*- lexical-binding: t -*-

(defun term-send-esc ()
  "Send ESC in term mode."
  (interactive)
  (term-send-raw-string "\e"))

(defun my-shell//vterm-customize-faces ()
  (when (memq 'zenburn custom-enabled-themes)
    (custom-theme-set-faces
     'zenburn
     '(vterm-color-default ((t (:foreground "#DBDBCB" :background "#3F3F3F"))))
     '(vterm-color-black   ((t (:foreground "#3F3F3F" :background "#708F80"))))
     '(vterm-color-red     ((t (:foreground "#A95050" :background "#DBA2A2"))))
     '(vterm-color-green   ((t (:foreground "#60B389" :background "#72D4A2"))))
     '(vterm-color-yellow  ((t (:foreground "#DEAE8E" :background "#EFDEAE"))))
     '(vterm-color-blue    ((t (:foreground "#99B7D6" :background "#93BEF2"))))
     '(vterm-color-magenta ((t (:foreground "#DB8BC2" :background "#EB92D2"))))
     '(vterm-color-cyan    ((t (:foreground "#8BCFD2" :background "#92DFE2"))))
     '(vterm-color-white   ((t (:foreground "#DBDBCB" :background "#FEFEFE")))))))
