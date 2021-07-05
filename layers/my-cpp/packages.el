;; -*- lexical-binding: t -*-

(defconst my-cpp-packages '(cc-mode
                            cmake-font-lock
                            (cmake-format :location (recipe
                                                     :fetcher github
                                                     :repo "simonfxr/cmake-format.el"))
                            cmake-mode
                            eldoc-cmake
                            evil-surround
                            highlight-doxygen
                            lsp-mode
                            modern-cpp-font-lock
                            (nxml-mode :location built-in)
                            qt-pro-mode
                            yaml-mode
                            zeal-at-point))

;; NOTE: The package is missing a require for either cl or cl-lib (not sure), so
;;       the compilation might result in invalid byte code for the call to incf.
;;       If that happens, as a workaround, delete cmake-format.elc.
(defun my-cpp/init-cmake-format ()
  (use-package cmake-format
    :init (add-hook 'cmake-mode-hook #'cmake-format-mode)))

(defun my-cpp/post-init-cc-mode ()
  (spacemacs/add-to-hooks #'my-cpp//disable-escape-quotes-after-insert
                          c-c++-mode-hooks)
  (spacemacs/add-to-hooks #'my-cpp//lsp-ui-flycheck-configure
                          c-c++-mode-hooks 'append)
  (with-eval-after-load 'cc-mode
    (setf (alist-get 'other c-default-style) "linux"))

  (let (docs-at-point docs-at-point-set-docset)
    (when (configuration-layer/package-used-p 'dash-at-point)
      (setq docs-at-point #'dash-at-point
            docs-at-point-set-docset #'dash-at-point-set-docset))
    (when (configuration-layer/package-used-p 'zeal-at-point)
      (setq docs-at-point #'zeal-at-point
            docs-at-point-set-docset #'zeal-at-point-set-docset))
    (when docs-at-point
      (with-eval-after-load 'cc-mode
        (dolist (mode c-c++-modes)
          (spacemacs/safe-set-leader-keys-for-major-mode mode
            "hd" docs-at-point
            "hD" docs-at-point-set-docset)))))

  ;; It is deliberate that this hook is installed here, in cc-modes post-init;
  ;; `my-cpp//ccls-use-project-build-directory' needs to be invoked before
  ;; `spacemacs//c-c++-setup-backend'.
  (let ((local-vars-hooks '(c-mode-local-vars-hook c++-mode-local-vars-hook)))
    (when (configuration-layer/package-used-p 'lsp-mode)
      (spacemacs/add-to-hooks #'my-cpp//clangd-use-project-build-directory
                              local-vars-hooks))
    (when (configuration-layer/package-used-p 'ccls)
      (spacemacs/add-to-hooks #'my-cpp//ccls-use-project-build-directory
                              local-vars-hooks))))

(defun my-cpp/init-eldoc-cmake ()
  (use-package eldoc-cmake
    :hook (cmake-mode . eldoc-cmake-enable)))

(defun my-cpp/post-init-evil-surround ()
  ;; In `c++-mode' the HTML tag surround pair is pretty much useless. It is far
  ;; more useful to have angle bracket surround pairs.
  (with-eval-after-load 'evil-surround
    (defun my-evil-surround-pairs-c++-mode ()
      (push '(?< . ("< " . " >")) evil-surround-pairs-alist))
    (add-lazy-hook 'c++-mode #'my-evil-surround-pairs-c++-mode)))

(defun my-cpp/init-highlight-doxygen ()
  (use-package highlight-doxygen
    :commands (highlight-doxygen-mode)
    :init (spacemacs/add-to-hooks #'highlight-doxygen-mode
                                  c-c++-mode-hooks)))

(defun my-cpp/init-cmake-font-lock ()
  (use-package cmake-font-lock
    :init (advice-add 'cmake-font-lock-activate :override
                      #'my-cpp//cmake-font-lock-activate)
    :defer t))

(defun my-cpp/init-cmake-mode ()
  (use-package cmake-mode
    :defer t))

(defun my-cpp/post-init-lsp-mode ()
  (with-eval-after-load 'lsp-mode
    (setq ccls-executable (concat dotspacemacs-directory "bin/ccls-ulimit"))
    (add-to-list 'lsp-file-watch-ignored "[/\\\\]\\.ccls-cache\\'")
    (add-to-list 'lsp-file-watch-ignored "[/\\\\]build\\'")
    (add-to-list 'lsp-file-watch-ignored "[/\\\\]vendor\\'"))
  (advice-add 'lsp-register-client :filter-args
              #'my-lsp//ccls-add-library-folders-fn))

(defun my-cpp/init-modern-cpp-font-lock ()
  (use-package modern-cpp-font-lock
    :hook (c++-mode . modern-c++-font-lock-mode)
    :diminish modern-c++-font-lock-mode))

(defun my-cpp/post-init-nxml-mode ()
  ;; QMake C++ project UI designer files.
  (add-to-list 'auto-mode-alist '("\\.ui\\'" . nxml-mode)))

(defun my-cpp/init-qt-pro-mode ()
  (use-package qt-pro-mode
    :mode ("\\.pro\\'" "\\.pri\\'")))

(defun my-cpp/post-init-yaml-mode ()
  (add-to-list 'auto-mode-alist '("\\.clang-format\\'" . yaml-mode)))

(defun my-cpp/post-init-zeal-at-point ()
  (with-eval-after-load 'zeal-at-point
    (add-to-list 'zeal-at-point-docsets "qt5")))
