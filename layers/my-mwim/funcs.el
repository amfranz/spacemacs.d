;; -*- lexical-binding: t -*-

;; The reason this is here and not in `my-mwim/post-init-mwim' is because there
;; it would be evaluated too late. This change to the map will only take effect
;; if evaluated before the first invocation of `evilified-state-evilify-map' and
;; `evilified-state-evilify'.
(when (configuration-layer/package-used-p 'mwim)
  (with-eval-after-load 'evil-evilified-state
    (dolist (map (list evil-evilified-state-map
                       evil-evilified-state-map-original))
      (define-key map (kbd "C-e") nil))))
