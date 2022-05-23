;; -*- lexical-binding: t -*-

(defconst my-shell-packages '((ansi-color :location built-in)
                              (eshell :location built-in)
                              eterm-256color
                              shell-pop
                              (term :location built-in)
                              xterm-color
                              vterm))

(defun my-shell/init-ansi-color ()
  (with-eval-after-load 'ansi-color
    (spacemacs/after-load-theme 'zenburn
      (custom-theme-alter-faces
       'zenburn
       '(ansi-color-black          ((t :foreground "#3F3F3F" :background "#3F3F3F")))
       '(ansi-color-red            ((t :foreground "#A95050" :background "#A95050")))
       '(ansi-color-green          ((t :foreground "#60B389" :background "#60B389")))
       '(ansi-color-yellow         ((t :foreground "#DEAE8E" :background "#DEAE8E")))
       '(ansi-color-blue           ((t :foreground "#99B7D6" :background "#99B7D6")))
       '(ansi-color-magenta        ((t :foreground "#DB8BC2" :background "#DB8BC2")))
       '(ansi-color-cyan           ((t :foreground "#8BCFD2" :background "#8BCFD2")))
       '(ansi-color-white          ((t :foreground "#DBDBCB" :background "#DBDBCB")))
       '(ansi-color-bright-black   ((t :foreground "#708F80" :background "#708F80")))
       '(ansi-color-bright-red     ((t :foreground "#DBA2A2" :background "#DBA2A2")))
       '(ansi-color-bright-green   ((t :foreground "#72D4A2" :background "#72D4A2")))
       '(ansi-color-bright-yellow  ((t :foreground "#EFDEAE" :background "#EFDEAE")))
       '(ansi-color-bright-blue    ((t :foreground "#93BEF2" :background "#93BEF2")))
       '(ansi-color-bright-magenta ((t :foreground "#EB92D2" :background "#EB92D2")))
       '(ansi-color-bright-cyan    ((t :foreground "#92DFE2" :background "#92DFE2")))
       '(ansi-color-bright-white   ((t :foreground "#FEFEFE" :background "#FEFEFE")))))))

(defun my-shell/post-init-eshell ()
  (with-eval-after-load 'eshell
    (spacemacs/declare-prefix-for-mode 'eshell-mode "mi" "insert")
    (spacemacs/set-leader-keys-for-major-mode 'eshell-mode
      "ib" #'eshell-insert-buffer-name
      "ie" #'eshell-insert-envvar)))

(defun my-shell/init-eterm-256color ()
  (use-package eterm-256color
    :hook (term-mode . eterm-256color-mode)))

(defun my-shell/post-init-shell-pop ()
  ;; Do not kill shell buffers when the words 'finished' or 'exited' appear in
  ;; the output of a process (who thought this is a good idea?).
  (remove-hook 'term-mode-hook 'ansi-term-handle-close))

(defun my-shell/post-init-term ()
  (with-eval-after-load 'term

    (spacemacs/after-load-theme 'zenburn
      (custom-theme-alter-faces
       'zenburn
       '(term                      ((t :foreground "#DBDBCB" :inherit default)))
       '(term-color-black          ((t :foreground "#3F3F3F" :background "#3F3F3F")))
       '(term-color-red            ((t :foreground "#A95050" :background "#A95050")))
       '(term-color-green          ((t :foreground "#60B389" :background "#60B389")))
       '(term-color-yellow         ((t :foreground "#DEAE8E" :background "#DEAE8E")))
       '(term-color-blue           ((t :foreground "#99B7D6" :background "#99B7D6")))
       '(term-color-magenta        ((t :foreground "#DB8BC2" :background "#DB8BC2")))
       '(term-color-cyan           ((t :foreground "#8BCFD2" :background "#8BCFD2")))
       '(term-color-white          ((t :foreground "#DBDBCB" :background "#DBDBCB")))
       '(term-color-bright-black   ((t :foreground "#708F80" :background "#708F80")))
       '(term-color-bright-red     ((t :foreground "#DBA2A2" :background "#DBA2A2")))
       '(term-color-bright-green   ((t :foreground "#72D4A2" :background "#72D4A2")))
       '(term-color-bright-yellow  ((t :foreground "#EFDEAE" :background "#EFDEAE")))
       '(term-color-bright-blue    ((t :foreground "#93BEF2" :background "#93BEF2")))
       '(term-color-bright-magenta ((t :foreground "#EB92D2" :background "#EB92D2")))
       '(term-color-bright-cyan    ((t :foreground "#92DFE2" :background "#92DFE2")))
       '(term-color-bright-white   ((t :foreground "#FEFEFE" :background "#FEFEFE")))))

    (evil-collection-init 'term)
    (evil-define-key 'insert 'term-raw-map (kbd "C-c C-e") #'term-send-esc)
    (dolist (mode '(term-mode-map term-raw-map))
      (evil-define-key 'insert term-mode-map (kbd "s-v") #'term-paste))))

(defun my-shell/post-init-xterm-color ()
  (with-eval-after-load 'xterm-color
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
          )))

(defun my-shell/post-init-vterm ()
  ;; My answer will always be yes anyway, just do it.
  (setq vterm-always-compile-module t)

  (with-eval-after-load 'vterm
    ;; GUI Emacs can be configured to distinguish between TAB and C-i. This
    ;; restores expected terminal behavior of both bindings acting the same.
    (when dotspacemacs-distinguish-gui-tab
      (define-key vterm-mode-map (kbd "<C-i>") #'vterm-send-tab))

    ;; Make paste via Super-v work. I configured my terminal program with the
    ;; same key binding. This makes Super-v a universal paste, which I hope will
    ;; lead less mental overhead for such a common operation.
    (evil-define-key 'normal vterm-mode-map
      (kbd "s-v") #'vterm-yank)
    (evil-define-key 'insert vterm-mode-map
      (kbd "s-v") #'vterm-yank)

    (evil-define-key 'insert vterm-mode-map
      (kbd "C-a") #'vterm--self-insert
      (kbd "C-d") #'vterm--self-insert
      (kbd "C-e") #'vterm--self-insert
      (kbd "C-h") #'vterm--self-insert
      (kbd "C-k") #'vterm--self-insert
      (kbd "C-n") #'vterm--self-insert
      (kbd "C-p") #'vterm--self-insert
      (kbd "C-r") #'vterm--self-insert
      (kbd "C-u") #'vterm--self-insert
      (kbd "C-x") #'vterm--self-insert)

    (add-to-list 'spacemacs-indent-sensitive-modes 'vterm-mode)

    ;; Fixes yank by `helm-show-kill-ring'.
    (advice-add 'insert-for-yank :around #'my-shell//vterm-insert-for-yank)))
