;; -*- lexical-binding: t -*-

(defconst my-restclient-packages '(polymode
                                   restclient))

(defun my-restclient/init-polymode ()
  (use-package polymode
    :defer t))

(defun my-restclient/post-init-restclient ()
  (with-eval-after-load 'restclient
    (define-hostmode pm/restclient-hostmode
      :mode 'restclient-mode)
    (define-innermode pm/restclient-innermode
      :mode 'emacs-lisp-mode
      :head-mode 'host
      :tail-mode 'host)
    (define-innermode pm/restclient-single-innermode pm/restclient-innermode
      :head-matcher "^:[^ ]+ := "
      :tail-matcher "\n")
    (define-innermode pm/restclient-multi-innermode pm/restclient-innermode
      :head-matcher "^:[^ ]+ := <<\n"
      :tail-matcher "^#\n")
    (define-polymode pm/restclient-mode
      :hostmode 'pm/restclient-hostmode
      :innermodes '(pm/restclient-single-innermode
                    pm/restclient-multi-innermode))))
