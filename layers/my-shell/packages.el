;; -*- lexical-binding: t -*-

(defconst my-shell-packages
  '((eterm-256color
     :location (recipe
                :fetcher github
                :repo "dieggsy/eterm-256color"
                ;; the latest version in melpa is currently broken,
                ;; this is the last good revision known.
                :commit "dab96af559deb443c4c9c00e23389926e1607192"
                :files (:defaults
                        "eterm-256color.ti")))
    eshell
    shell-pop
    term
    vterm))

(defun my-shell/post-init-eshell ()
  (with-eval-after-load 'eshell
    (spacemacs/declare-prefix-for-mode 'eshell-mode "mi" "insert")
    (spacemacs/set-leader-keys-for-major-mode 'eshell-mode
      "ib" #'eshell-insert-buffer-name
      "ie" #'eshell-insert-envvar)))

(defun my-shell/init-eterm-256color ()
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

(defun my-shell/post-init-shell-pop ()
  ;; Do not kill shell buffers when the words 'finished' or 'exited' appear in
  ;; the output of a process (who thought this is a good idea?).
  (remove-hook 'term-mode-hook 'ansi-term-handle-close))

(defun my-shell/post-init-term ()
  (with-eval-after-load 'term
    (evil-collection-init 'term)
    (evil-define-key 'insert 'term-raw-map (kbd "C-c C-e") #'term-send-esc)
    (dolist (mode '(term-mode-map term-raw-map))
      (evil-define-key 'insert term-mode-map (kbd "s-v") #'term-paste))))

(defun my-shell/init-vterm ()
  (use-package vterm
    :defer t
    :init
    (spacemacs/safe-set-leader-keys "asv" #'vterm)
    :config
    (progn
      (add-hook 'spacemacs-post-theme-change-hook
                #'my-shell//vterm-customize-faces)
      (my-shell//vterm-customize-faces)
      (evil-collection-init 'vterm))))
