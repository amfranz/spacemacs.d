;; -*- lexical-binding: t -*-

;;;###autoload
(defun display-scaling-factor ()
  "Reads the display scaling factor from the Cinnamon dconf database.
This will return 2 on Hi-DPI displays, 1 otherwise."
  (pcase (getenv "DESKTOP_SESSION")
    ("plasma"
     (round (string-to-number
             (shell-command-to-string
              "xrdb -query | awk -F :\\\\t 'BEGIN { dpi = 96 } $1 == \"Xft.dpi\" { dpi = $2 } END { print dpi }'"))
            96))
    ("cinnamon"
     (string-to-number
      (shell-command-to-string
       "dconf read /org/cinnamon/active-display-scale")))
    ("gnome"
     (string-to-number
      (shell-command-to-string
       "dconf read /org/gnome/desktop/interface/scaling-factor | awk '{ print $2 }'")))
    (_ 1)))

;;;###autoload
(defun display-adjusted-font-size ()
  "Returns my preferred font size of 14 point multiplied by the screen scaling
factor set by the desktop environment."
  (* 14 (display-scaling-factor)))

(provide 'display)
