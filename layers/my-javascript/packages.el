;; -*- lexical-binding: t -*-

(defconst my-javascript-packages '(angular-snippets
                                   coffee-mode
                                   compile
                                   flycheck
                                   nvm
                                   js-mode
                                   js2-mode
                                   projectile))

(defun my-javascript/init-angular-snippets ()
  (use-package angular-snippets
    :defer t))

(defun my-javascript/pre-init-coffee-mode ()
  (setq iced-coffee-cs-keywords '("async" "await" "defer")))

(defun my-javascript/post-init-coffee-mode ()
  (with-eval-after-load 'flycheck
    ;; TODO: use flycheck-def-executable-var
    (setq flycheck-coffee-executable "nvpm-exec-coffee"
          flycheck-coffee-coffeelint-executable "nvpm-exec-coffeelint"))

  ;;
  ;; Integration with do-the-right-thing indent.
  ;;

  (with-eval-after-load 'dtrt-indent
    (push '(coffee ("\"\"\"" 0 "\"\"\"" nil)
                   ("\""     0 "\""     nil "\\.")
                   ("'''"    0 "'''"    nil)
                   ("'"      0 "'"      nil "\\.")
                   ("///"    0 "///"    nil)
                   ("/"      0 "/"      nil "\\.")
                   ("###"    0 "###"    nil)
                   ("#"      0 "$"      nil)
                   ("\\["    0 "\\]"    t))
          dtrt-indent-language-syntax-table)
    (push '(coffee-mode coffee coffee-tab-width)
          dtrt-indent-hook-mapping-list))

  (with-eval-after-load 'coffee-mode
    ;; Smartparens is bad at triple quoted strings, electric pair mode handles them better.
    ;; (add-hook 'coffee-mode-hook (lambda ()
    ;;   (smartparens-mode -1)
    ;;   (electric-pair-mode t)))

    (setq coffee-tab-width 2
          coffee-args-compile '("-c" "--bare"))

    (add-hook 'coffee-mode-hook (lambda ()
                                  (subword-mode t)
                                  (indent-guide-mode t)))

    ;;
    ;; More colors, more fun (edit: this fun is distracting...)
    ;;
                                        ;(add-hook 'coffee-mode-hook (lambda ()
                                        ;  (if (fboundp 'rainbow-identifiers-mode)
                                        ;    (rainbow-identifiers-mode 1))))

    ;;
    ;; Fixed Spacemacs integration.
    ;;

    (defun javascript/coffee-indent ()
      (if (coffee-line-wants-indent)
          (coffee--indent-insert-spaces (+ (coffee-previous-indent) coffee-tab-width))
        (coffee--indent-insert-spaces (coffee-previous-indent))))

    ;;
    ;; More sophisticated indentation rules.
    ;;

    (push "loop" coffee-indenters-bol)

    (dolist (char '( ?= ?+ ?- ?* ?/ ?% ?| ?& ?< ?: ))
      (push char coffee-indenters-eol))

    (defun coffee-indenters-bol-regexp-ignore-assignment (orig-fun &rest args)
      "Adds the ability to ignore assignments at the beginning of a bol indent keyword."
      (concat "\\(?:\\<\\w+\\>[ \t]*=[ \t]*\\)?" (apply orig-fun args)))

    (advice-add 'coffee-indenters-bol-regexp :around
                'coffee-indenters-bol-regexp-ignore-assignment)

    (defun coffee-goto-prev-line ()
      (beginning-of-line)
      (not (zerop (skip-chars-backward "\r\n\t "))))

    (defun coffee-line-last-char ()
      (end-of-line)
      (char-before))

    (defun coffee-line-nesting-delta ()
      (let ((delta 0))
        (dolist (char (append (thing-at-point 'line) nil))
          (cond
           ((memq char '( ?\( ?\[ ?{ ))
            (setq delta (+ delta 1)))
           ((memq char '( ?\) ?\] ?} ))
            (setq delta (- delta 1)))))
        delta))

    (defun coffee-line-maybe-wants-indent (orig-fun &rest args)
      "Lines increasing the nesting level want intend, as well as lines ending
with a comma or dot want indent, but not if the line prior also ends with a
comma or dot."
      (let ((wants-indent nil))
        (if (save-excursion
              (or
               (not (coffee-goto-prev-line))
               (when (> (coffee-line-nesting-delta) 0)
                 (setq wants-indent t)
                 t)
               (let ((last-char (coffee-line-last-char)))
                 (when (memq last-char '( ?. ?, ))
                   (setq wants-indent
                         (not (and (coffee-goto-prev-line)
                                   (eq (coffee-line-last-char) last-char))))
                   t))))
            wants-indent
          (apply orig-fun args))))

    (advice-add 'coffee-line-wants-indent :around
                'coffee-line-maybe-wants-indent)

    ;;
    ;; Key bindings.
    ;;

    (evil-leader/set-key-for-mode 'coffee-mode "mcc" 'coffee-compile-file)
    (evil-leader/set-key-for-mode 'coffee-mode "mcb" 'coffee-compile-buffer)
    (evil-leader/set-key-for-mode 'coffee-mode "mcr" 'coffee-compile-region)
    (evil-leader/set-key-for-mode 'coffee-mode "mct" 'coffee-cos-mode)
    (evil-leader/set-key-for-mode 'coffee-mode "mcj" 'coffee-js2coffee-replace-region)
    (evil-leader/set-key-for-mode 'coffee-mode "mrr" 'coffee-repl)
    (evil-leader/set-key-for-mode 'coffee-mode "mrl" 'coffee-send-line)
    (evil-leader/set-key-for-mode 'coffee-mode "mrv" 'coffee-send-region)
    (evil-leader/set-key-for-mode 'coffee-mode "mrb" 'coffee-send-buffer)))

(defun my-javascript/post-init-compile ()
  (with-eval-after-load 'compile
    ;; Teaches Emacs how to colorize nodejs stack traces.
    (push '(nodejs-stack "^ +at \\(?:\\(?:\\[object \\|new \\)?[^ \n]+ \\(?:\\[as [_a-zA-Z0-9]+\\] \\)?(\\)?\\(\\([^:)\n]*\\):\\([0-9]+\\):\\([0-9]+\\)\\))?$" 2 3 4 0 1) compilation-error-regexp-alist-alist)
    ;; Order is significant, needs to be pushed before mocha-assert.
    (push 'nodejs-stack compilation-error-regexp-alist)
    ;; Teaches Emacs how to colorize mocha test failures.
    (push '(mocha-assert "^ \\{2\\}[0-9]+) \\(.*\\):\n\\(?:.*\n\\)+? +at \\(?:\\(?:\\[object \\|new \\)?[^ \n]+ \\(?:\\[as [_a-zA-Z0-9]+\\] \\)?(\\)?\\([^:)\n]*\\):\\([0-9]+\\):\\([0-9]+\\))?$" 2 3 4 nil 1) compilation-error-regexp-alist-alist)
    ;; Order is significant, needs to be pushed after nodejs-stack.
    (push 'mocha-assert compilation-error-regexp-alist)))

(defun my-javascript/post-init-flycheck ()
  ;; This is a copy of Flychecks built-in gjslint checker - but this one
  ;; supports versions of gjslint that prefix error codes with "New Error ",
  ;; as well as negative error codes.
  (when nil  ;; FIXME: causes issues in SPC e v
    (with-eval-after-load 'flycheck
      (flycheck-define-checker javascript-gjslint
        "A Javascript syntax and style checker using Closure Linter.

See URL `https://developers.google.com/closure/utilities'."
        :command ("gjslint" "--unix_mode"
                  (config-file "--flagfile" flycheck-gjslintrc)
                  source)
        :error-patterns ((warning
                          line-start (file-name) ":" line ":(" (optional "New Error ")
                          (id (optional "-") (one-or-more digit)) ") " (message) line-end))
        :modes (js-mode js2-mode js3-mode)
        :next-checkers ((warning . javascript-jscs))))))

(defun my-javascript/init-nvm ()
  (use-package nvm
    :defer t))

(defun my-javascript/post-init-js-mode ()
  (with-eval-after-load 'js-mode
    (add-hook 'js-mode-hook (lambda ()
                              (setq evil-shift-width js-indent-level)))))

(defun my-javascript/post-init-js2-mode ()
  (with-eval-after-load 'js2-mode
    (setq js2-mode-show-parse-errors nil
          js2-mode-show-strict-warnings nil)
    (add-hook 'js2-mode-hook (lambda ()
                               (kill-local-variable 'next-error-function)))
    (add-hook 'js2-mode-hook (lambda ()
                               (setq evil-shift-width js2-basic-offset)))))

(defun my-javascript/post-init-projectile ()
  (with-eval-after-load 'projectile
    (projectile-register-project-type
     'nodejs-gulp-mocha '("package.json" "gulpfile.*" "test/mocha.opts")
     :compile "nvpm-exec-gulp"
     :test "nvpm-exec-mocha")
    (defun projectile-test-suffix--javascript (orig-fun project-type)
      (cond
       ((member project-type '(nodejs-gulp-mocha)) "_test")
       (t (funcall orig-fun project-type))))
    (advice-add 'projectile-test-suffix :around
                'projectile-test-suffix--javascript)))
