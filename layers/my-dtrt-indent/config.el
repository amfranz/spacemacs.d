;; -*- lexical-binding: t -*-

(defvar dtrt-indent-mode-whitelist '(prog-mode text-mode)
  "A list of major modes in which to enable `dtrt-indent' mode.

Also see `dtrt-indent-mode-blacklist'.")

(defvar dtrt-indent-mode-blacklist '()
  "A list of major modes in which not to enable the `dtrt-indent' minor mode.

The blacklist takes precedence over the whitelist. `dtrt-indent' mode will be
enabled in all modes listed in `dtrt-indent-mode-whitelist' as well as modes
derived from them, except in modes listed in `dtrt-indent-mode-blacklist' or
modes derived from them.")
