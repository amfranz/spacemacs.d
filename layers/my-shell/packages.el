;; -*- lexical-binding: t -*-

(defconst my-shell-packages '(eterm-256color
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
    (add-hook 'spacemacs-post-theme-change-hook
              #'my-shell//term-customize-faces)
    (my-shell//term-customize-faces)
    (evil-collection-init 'term)
    (evil-define-key 'insert 'term-raw-map (kbd "C-c C-e") #'term-send-esc)
    (dolist (mode '(term-mode-map term-raw-map))
      (evil-define-key 'insert term-mode-map (kbd "s-v") #'term-paste))))

(defun my-shell/post-init-vterm ()
  (with-eval-after-load 'vterm
    (when (configuration-layer/package-used-p 'eterm-256color)
      (setq vterm-term-environment-variable "eterm-256color"))

    (when dotspacemacs-distinguish-gui-tab
      (define-key vterm-mode-map (kbd "<C-i>") #'vterm-send-tab))

    (evil-define-key 'insert vterm-mode-map
      (kbd "C-a") #'vterm--self-insert
      (kbd "C-d") #'vterm--self-insert
      (kbd "C-e") #'vterm--self-insert
      (kbd "C-h") #'vterm--self-insert
      (kbd "C-k") #'vterm--self-insert
      (kbd "C-r") #'vterm--self-insert
      (kbd "C-u") #'vterm--self-insert
      (kbd "C-p") #'vterm--self-insert
      (kbd "C-n") #'vterm--self-insert
      (kbd "s-v") #'vterm-yank)

    (add-to-list 'spacemacs-indent-sensitive-modes 'vterm-mode)
    (advice-add 'helm-kill-ring-action-yank-1
                :around #'my-shell//helm-kill-ring-action-yank-1)

    (add-hook 'vterm-mode-hook #'my-shell//vterm-mode-hook)))
