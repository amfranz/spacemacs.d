;; -*- lexical-binding: t -*-

(defun my-bookmarks//load-repository ()
  (require 'bm))

(defun my-bookmarks//customize-transient-state ()
  (defhydra+ spacemacs/bm-transient-state ()
    "\n [_a_] annotate [_h_] helm [_n_/_p_] next/previous [_s_] show [_t_] toggle [_q_] quit"
    ("a" #'bm-bookmark-annotate)
    ("h" #'helm-bm :exit t)
    ("s" #'bm-show-all :exit t)
    ("p" #'bm-previous)))
