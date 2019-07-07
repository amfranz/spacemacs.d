;; -*- lexical-binding: t -*-

(defun my-dired//require-dired+ ()
  (require 'dired+))

(defun dired-home ()
  "Open a dired buffer in `user-home-directory'."
  (interactive)
  (dired user-home-directory))

(defun my-dired//diff-files (file1 file2)
  (if (and (file-directory-p file1)
           (file-directory-p file2))
      (ztree-diff file1 file2)
    (ediff-files file1 file2)))

;; This is not my creation, it was sourced from the accepted answer of this Stackoverflow question:
;; https://stackoverflow.com/questions/18121808/emacs-ediff-marked-files-in-different-dired-buffers
(defun my-dired/ediff-marked-pair ()
  "Run ediff-files on a pair of files marked in dired buffer"
  (interactive)
  (let* ((marked-files (dired-get-marked-files nil nil))
         (other-win (get-window-with-predicate
                     (lambda (window)
                       (with-current-buffer (window-buffer window)
                         (and (not (eq window (selected-window)))
                              (eq major-mode 'dired-mode))))))
         (other-marked-files (and other-win
                                  (with-current-buffer (window-buffer other-win)
                                    (dired-get-marked-files nil)))))
    (cond ((= (length marked-files) 2)
           (my-dired//diff-files (nth 0 marked-files)
                                 (nth 1 marked-files)))
          ((and (= (length marked-files) 1)
                (= (length other-marked-files) 1))
           (my-dired//diff-files (nth 0 marked-files)
                                 (nth 0 other-marked-files)))
          (t (error "mark exactly 2 files, at least 1 locally")))))

;; TODO: submit this upstream
;; An altered version of `all-the-icons-dired--display' that doesn't add an icon
;; to files that already have one. This makes it possible to call it multiple
;; times on the same dired buffer, which eases integration with `dired-subtree'.
(defun my-dired//all-the-icons-dired--display ()
  "Display the icons of files in a dired buffer."
  (when dired-subdir-alist
    (let ((inhibit-read-only t)
          (remote-p (and (fboundp 'tramp-tramp-file-p)
                         (tramp-tramp-file-p default-directory))))
      (save-excursion
        (goto-char (point-min))
        (while (not (eobp))
          (when (dired-move-to-filename nil)
            (let ((file (dired-get-filename 'verbatim t)))
              (unless (member file '("." ".."))
                (let ((filename (dired-get-filename nil t)))
                  (unless (get-text-property (- (point) 2) 'icon)
                    (insert (concat (propertize
                                     (if (file-directory-p filename)
                                         (let* ((matcher (all-the-icons-match-to-alist file all-the-icons-dir-icon-alist)))
                                           (cond
                                            (remote-p
                                             (all-the-icons-octicon "file-directory" :v-adjust all-the-icons-dired-v-adjust :face 'all-the-icons-dired-dir-face))
                                            ((file-symlink-p filename)
                                             (all-the-icons-octicon "file-symlink-directory" :v-adjust all-the-icons-dired-v-adjust :face 'all-the-icons-dired-dir-face))
                                            ((all-the-icons-dir-is-submodule filename)
                                             (all-the-icons-octicon "file-submodule" :v-adjust all-the-icons-dired-v-adjust :face 'all-the-icons-dired-dir-face))
                                            ((file-exists-p (format "%s/.git" filename))
                                             (all-the-icons-octicon "repo" :v-adjust all-the-icons-dired-v-adjust :face 'all-the-icons-dired-dir-face))
                                            (t (apply (car matcher) (list (cadr matcher) :face 'all-the-icons-dired-dir-face :v-adjust all-the-icons-dired-v-adjust)))))
                                       (all-the-icons-icon-for-file file :v-adjust all-the-icons-dired-v-adjust))
                                     'icon t)
                                    " ")))))))
          (forward-line 1))))))

(defun my-dired//dired-subtree--insert-all-the-icons ()
  "Registers a hook to add icons to the files inserted by `dired-subtree'.

The hook is added or removed, depending on whether `all-the-icons-dired-mode' is
currently enabled or not."
  (if (bound-and-true-p all-the-icons-dired-mode)
      (add-hook 'dired-subtree-after-insert-hook #'all-the-icons-dired--display t t)
    (remove-hook 'dired-subtree-after-insert-hook #'all-the-icons-dired--display t)))

(defun my-dired/find-file-horizontal-split (&optional arg)
  "Open the file at point by horizontally splitting `next-window'.
Stay in current window with a prefix argument ARG."
  (interactive "P")
  (let ((file (dired-get-file-for-visit))
        (window (split-window-horizontally)))
    (with-selected-window window
      (without-purpose
        (find-file-existing file)))
    (unless arg
      (select-window window))))

(defun my-dired/find-file-vertical-split (&optional arg)
  "Open the file at point by vertically splitting `next-window'.
Stay in current window with a prefix argument ARG."
  (interactive "P")
  (let ((file (dired-get-file-for-visit))
        (window (split-window-vertically)))
    (with-selected-window window
      (without-purpose
        (find-file-existing file)))
    (unless arg
      (select-window window))))

(defun my-dired/find-file-ace (&optional arg)
  "Open current file or tag in window selected by `ace-window'.
Stay in current window with a prefix argument ARG."
  (interactive "P")
  (let ((file (dired-get-file-for-visit)))
    (require 'ace-window)
    (aw-select "Select window" (lambda (window)
                                 (with-selected-window window
                                   (without-purpose
                                     (find-file-existing file)))
                                 (unless arg
                                   (select-window window))))))

(defun my-dired/find-file-ace-horizontal-split (&optional arg)
  "Open current file by horizontally splitting window selected by `ace-window'.
Stay in current window with a prefix argument ARG."
  (interactive "P")
  (let ((file (dired-get-file-for-visit)))
    (require 'ace-window)
    (aw-select "Select window" (lambda (old-window)
                                 (let (new-window)
                                   (with-selected-window old-window
                                     (setq new-window (split-window-horizontally)))
                                   (with-selected-window new-window
                                     (without-purpose
                                       (find-file-existing file)))
                                   (unless arg
                                     (select-window new-window)))))))

(defun my-dired/find-file-ace-vertical-split (&optional arg)
  "Open current file by vertically splitting window selected by `ace-window'.
Stay in current window with a prefix argument ARG."
  (interactive "P")
  (let ((file (dired-get-file-for-visit)))
    (require 'ace-window)
    (aw-select "Select window" (lambda (old-window)
                                 (let (new-window)
                                   (with-selected-window old-window
                                     (setq new-window (split-window-vertically)))
                                   (with-selected-window new-window
                                     (without-purpose
                                       (find-file-existing file)))
                                   (unless arg
                                     (select-window new-window)))))))
