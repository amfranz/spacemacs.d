;; -*- lexical-binding: t -*-

(defconst my-json-packages '(json-mode))

(defun my-json/post-init-json-mode ()
  ;; Highlight numbers mode makes a mess out of JSONs syntax highlighting.
  (add-hook 'json-mode-hook #'my-json//disable-highlight-numbers-mode)

  ;; C-i and TAB can be distinguished in the GUI version of Emacs, in the
  ;; terminal they are the same keycode. Packages typically bind functionality
  ;; to the keycode of TAB with the assumption that C-i is the same keycode. If
  ;; they keys can be distinguished, we rebind functionality that makes more
  ;; sense to be bound to C-i instead of TAB, to C-i.
  (when dotspacemacs-distinguish-gui-tab
    (with-eval-after-load 'json-mode
      (define-key json-mode-map (kbd "C-c TAB") nil)
      (define-key json-mode-map (kbd "C-c <C-i>") #'json-increment-number-at-point))))
