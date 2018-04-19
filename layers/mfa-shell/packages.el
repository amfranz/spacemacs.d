(defconst mfa-shell-packages '(eterm-256color term))

(defun mfa-shell/init-eterm-256color ()
  (use-package eterm-256color
    :hook (term-mode . eterm-256color-mode)
    :init
    (setq xterm-color-names
          ["#3F3F3F"    ; black
           "#A95050"    ; red
           "#60B389"    ; green
           "#DEAE8E"    ; yellow
           "#99B7D6"    ; blue
           "#DB8BC2"    ; magenta
           "#8BCFD2"    ; cyan
           "#DBDBCB"]   ; white
          xterm-color-names-bright
          ["#708F80"    ; black
           "#DBA2A2"    ; red
           "#72D4A2"    ; green
           "#EFDEAE"    ; yellow
           "#93BEF2"    ; blue
           "#EB92D2"    ; magenta
           "#92DFE2"    ; cyan
           "#FEFEFE"]   ; white
          eterm-256color-disable-bold nil)))

(defun mfa-shell/post-init-term ()
  (with-eval-after-load 'term
    (evil-set-initial-state 'term-mode 'emacs)
    (define-key term-raw-map (kbd "s-v") #'term-paste)))
