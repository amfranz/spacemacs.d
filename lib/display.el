;; -*- lexical-binding: t -*-

;;;###autoload
(defun display-assume-graphic-p ()
  "My Emacs instances are dedicated to either the graphical environment or the
terminal. My customizations differ slightly between the two. This functions
returns whether this Emacs instance is dedicated to the graphical environment."
  (or (display-graphic-p)
      (string-equal (getenv "EMACS_SOCKET_NAME") "server-x")))

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

(defun display--pixels-per-inch ()
  "Calculates the DPI of the primary display."
  (round (display-pixel-height) (/ (display-mm-height) 25.4)))

;;;###autoload
(defun display-adjusted-font-size ()
  "Calculates a recommended size in pixels for the default font based on the DPI
of the monitor."
  ;; 7.38 characters per vertical inch, rounded to the closest integral number of pixels.
  ;; The guideline is to return 13 on 96 DPI displays and 14 on 101 DPI displays.
  (* (round (display--pixels-per-inch) 7.38)
     (display-scaling-factor)))

(provide 'display)
