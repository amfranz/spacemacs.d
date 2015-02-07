(defconst mfa-string-inflection-packages '(string-inflection))

(defun mfa-string-inflection/init-string-inflection ()
  (use-package string-inflection
    :defer t
    :init
    (progn
      (spacemacs/declare-prefix "xi" "string-inflection")
      (spacemacs/set-leader-keys "xic" #'string-inflection-camelcase)
      (spacemacs/set-leader-keys "xie" #'string-inflection-lisp)
      (spacemacs/set-leader-keys "xii" #'string-inflection-all-cycle)
      (spacemacs/set-leader-keys "xij" #'string-inflection-java-style-cycle)
      (spacemacs/set-leader-keys "xil" #'string-inflection-lower-camelcase)
      (spacemacs/set-leader-keys "xir" #'string-inflection-ruby-style-cycle)
      (spacemacs/set-leader-keys "xis" #'string-inflection-underscore)
      (spacemacs/set-leader-keys "xit" #'string-inflection-toggle)
      (spacemacs/set-leader-keys "xiu" #'string-inflection-upcase))))
