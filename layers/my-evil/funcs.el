;; -*- lexical-binding: t -*-

(with-eval-after-load 'evil-commands
  ;; This mirrors the properties of `evil-force-normal-state',
  ;; specifically we don't want this command to be recorded.
  (evil-define-command evil-force-normal-state-nohighlight ()
    "Switch to normal state and clear search highlight without recording current
command."
    :repeat abort
    :suppress-operator t
    ;; We need to call the original command instead of re-implementing it
    ;; because `evil-snipe' has an advice on it we do want called.
    (evil-force-normal-state)
    (evil-ex-nohighlight)))
