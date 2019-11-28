;; -*- lexical-binding: t -*-

(defconst my-tmux-packages '(turnip))

(defun my-tmux/init-turnip ()
  (use-package turnip
    :defer t
    :init
    (progn
      (spacemacs/declare-prefix "at" "tmux")
      (spacemacs/set-leader-keys "ata" #'turnip-attach)
      (spacemacs/set-leader-keys "atc" #'turnip-command)
      (spacemacs/set-leader-keys "atp" #'turnip-choose-pane)
      (spacemacs/set-leader-keys "atr" #'turnip-send-region))))
