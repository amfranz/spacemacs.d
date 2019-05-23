;; -*- lexical-binding: t -*-

(defconst mfa-unbound-packages '((unbound :location (recipe :fetcher url
                                                            :url "https://www.emacswiki.org/emacs/download/unbound.el"))))

(defun mfa-unbound/init-unbound ()
  (use-package unbound
    :defer t))
