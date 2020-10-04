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

;; NOTE: This is "subtree" and not "subdir". They are different functionalities.
(defun treemacs-icons-dired--display-icons-for-subtree ()
  "Display the icons of files in a subtree of the dired buffer."
  (when (bound-and-true-p treemacs-icons-dired-mode)
    (treemacs-with-writable-buffer
     (save-excursion
       (dired-subtree-with-subtree
        (unless (get-text-property (1- (point)) 'icon)
          (when-let ((file (dired-get-filename nil t))
                     (icon (if (file-directory-p file)
                               treemacs-icon-dir-closed
                             (treemacs-icon-for-file file))))
            (insert (propertize icon 'icon t)))))))))

(defun my-dired//dired-subtree--insert-treemacs-icons ()
  "Registers a hook to add icons to the files inserted by `dired-subtree'.

The hook is added or removed, depending on whether `treemacs-icons-dired-mode' is
currently enabled or not."
  (if treemacs-icons-dired-mode
      (add-hook 'dired-subtree-after-insert-hook #'treemacs-icons-dired--display-icons-for-subtree t)
    (remove-hook 'dired-subtree-after-insert-hook #'treemacs-icons-dired--display-icons-for-subtree)))

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

(defun my-dired/kill-subdir (&optional arg)
  "Remove all lines of current subdirectory.
Lower levels are unaffected. Moves point to the previous subdir."
  (interactive)
  (dired-kill-subdir)
  (diredp-next-subdir -1))

(defun my-dired//maybe-dired-filter-mode ()
  "Enables `dired-omit-mode', except in dired buffers in auto save directories."
  (unless (and auto-save-list-file-prefix
               (string-prefix-p (expand-file-name auto-save-list-file-prefix)
                                (expand-file-name default-directory)))
    (dired-omit-mode)))

(defun my-dired//dired-auto-revert-hooks ()
  (if global-auto-revert-mode
      (add-hook 'dired-mode-hook #'auto-revert--global-adopt-current-buffer)
    (remove-hook 'dired-mode-hook #'auto-revert--global-adopt-current-buffer)))
