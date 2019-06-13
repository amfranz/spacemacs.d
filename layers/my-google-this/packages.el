(defconst my-google-this-packages '(google-this))

(defun my-google-this/init-google-this ()
  (use-package google-this
    :defer t
    :init
    (progn
      (spacemacs/declare-prefix "og" "google")
      (spacemacs/set-leader-keys
        "oga" #'google-this-ray
        "ogc" #'google-this-cpp-reference
        "oge" #'google-this-error
        "ogf" #'google-this-lucky-search
        "ogg" #'google-this-noconfirm
        "ogi" #'google-this-lucky-and-insert-url
        "ogl" #'google-this-line
        "ogr" #'google-this-region
        "ogs" #'google-this-symbol
        "ogt" #'google-this
        "ogw" #'google-this-word))
    :config
    (setq google-this-wrap-in-quotes t)))
