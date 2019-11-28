;; -*- lexical-binding: t -*-

;; The reason this is here and not in a `post-init-eyebrowse' is because there
;; it would be evaluated too late. This change to the map will only take effect
;; if evaluated before the first invocation of `evilified-state-evilify-map' and
;; `evilified-state-evilify'.
(when (configuration-layer/package-used-p 'eyebrowse)
  (with-eval-after-load 'evil-evilified-state
    (dolist (map (list evil-evilified-state-map
                       evil-evilified-state-map-original))
      (define-key map "gt" #'eyebrowse-next-window-config)
      (define-key map "gT" #'eyebrowse-prev-window-config))))
