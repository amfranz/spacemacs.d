;; -*- lexical-binding: t -*-

(defun spacemacs/get-theme-package-name-if-available (theme)
  (if (memq theme emacs-built-in-themes)
      (intern (format "%S-theme" theme))
    (let ((package (or (alist-get theme spacemacs-theme-name-to-package)
                       (intern (format "%S-theme" theme)))))
      (if (configuration-layer/package-used-p package)
          package))))

(defmacro spacemacs/after-load-theme (theme &rest body)
  "Execute BODY after THEME is loaded."
  (declare (indent 1) (debug t))
  (let ((package-var (make-symbol "package")))
    `(if-let ((,package-var (spacemacs/get-theme-package-name-if-available ,theme)))
         (eval-after-load ,package-var (lambda () ,@body)))))

(defun custom-theme-alter-variables (theme &rest args)
  (custom-check-theme theme)

  ;; Process all the needed autoloads before anything else, so that the
  ;; subsequent code has all the info it needs (e.g. which var corresponds
  ;; to a minor mode), regardless of the ordering of the variables.
  (dolist (entry args)
    (let* ((symbol (indirect-variable (nth 0 entry))))
      (unless (or (get symbol 'standard-value)
                  (memq (get symbol 'custom-autoload) '(nil noset)))
        ;; This symbol needs to be autoloaded, even just for a `set'.
        (custom-load-symbol symbol))))
  (setq args (custom--sort-vars args))

  (let ((theme-settings (get theme 'theme-settings)))
    (dolist (entry args)
      (unless (listp entry)
        (error "Incompatible Custom theme spec"))

      (let ((symbol (indirect-variable (nth 0 entry)))
            (value (nth 1 entry)))
        (let ((setting theme-settings))
          (while (and setting
                      (not (and
                            (eq (caar setting) 'theme-value)
                            (eq (cadar setting) symbol))))
            (setq setting (cdr setting)))
          (if setting
              (setcar (cdddar setting) value)
            (put theme 'theme-settings
                 (nconc theme-settings
                        (list (list 'theme-value symbol theme value))))))

        (when (memq theme custom-enabled-themes)
          (let ((theme-value (get symbol 'theme-value)))
            (if-let ((setting (assq theme theme-value)))
                (setcar (cdr setting) value)
              (let (new-theme-value)
                (dolist (prec (cons 'user custom-enabled-themes))
                  (let ((setting (assq prec theme-value)))
                    (if (eq prec theme)
                        (push (list theme value) new-theme-value)
                      (if setting
                          (push setting new-theme-value)))))
                (put symbol 'theme-value new-theme-value))))
          ;; Ignore `custom-enabled-themes' and `custom-safe-themes'.
          (unless (memq symbol '(custom-enabled-themes custom-safe-themes))
            (custom-theme-recalc-variable symbol)))))))

(defun custom-theme-alter-faces (theme &rest face-specs)
  (custom-check-theme theme)
  (let ((theme-settings (get theme 'theme-settings)))
    (dolist (entry face-specs)
      (unless (listp entry)
        (error "Incompatible Custom theme spec"))
      (let ((face (car entry))
            (spec (nth 1 entry)))
        (when (get face 'face-alias)
          (setq face (get face 'face-alias)))
        (let ((setting theme-settings))
          (while (and setting
                      (not (and
                            (eq (caar setting) 'theme-face)
                            (eq (cadar setting) face))))
            (setq setting (cdr setting)))
          (if setting
              (setcar (cdddar setting) spec)
            (put theme 'theme-settings
                 (nconc theme-settings
                        (list (list 'theme-face face theme spec))))))
        (when (memq theme custom-enabled-themes)
          (let ((theme-face (get face 'theme-face)))
            (if-let ((setting (assq theme theme-face)))
                (setcar (cdr setting) spec)
              (let (new-theme-face)
                (dolist (prec (cons 'user custom-enabled-themes))
                  (let ((setting (assq prec theme-face)))
                    (if (eq prec theme)
                        (push (list theme spec) new-theme-face)
                      (if setting
                          (push setting new-theme-face)))))
                (put face 'theme-face new-theme-face))))
          (when (facep face)
            (dolist (frame (frame-list))
              (face-spec-recalc face frame))))))))

(provide 'my-theming)
