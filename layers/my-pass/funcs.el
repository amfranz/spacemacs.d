;; -*- lexical-binding: t -*-

(with-eval-after-load 'auth-source-pass
  ;; FIXME: this is hacky - find a sensible way to reorganize the password wallet
  (defun auth-source-pass--find-match-with-port (host port user)
    (if (not (string-equal port "sudo"))
        (auth-source-pass--find-match host user)
      (if (string-equal host "localhost")
          (auth-source-pass--find-match "localhost" nil)
        (auth-source-pass--find-match "updox" nil))))

  ;; FIXME: rewrite this to use el-patch
  ;; A change is that "port" is passed to the find-match defun.
  (defun auth-source-pass--build-result (host port user)
    "Build auth-source-pass entry matching HOST, PORT and USER."
    (let ((entry (auth-source-pass--find-match-with-port host port user)))
      (when entry
        (let ((retval (list
                       :host host
                       :port (or (auth-source-pass-get "port" entry) port)
                       :user (or (auth-source-pass-get "user" entry) user)
                       :secret (auth-source-pass-get 'secret entry)
                       ;; TRAMP does not understand the lambda
                       ;; :secret (lambda () (auth-source-pass-get 'secret entry))
                       )))
          (auth-source-pass--do-debug "return %s as final result (plus hidden password)"
                                      (seq-subseq retval 0 -2)) ;; remove password
          retval)))))

(defun my-pass//no-copy-to-clipboard (orig-fun &rest args)
  "Prevents passwords intended for the kill ring from being shared with the
clipboard."
  (let ((interprogram-cut-function nil))
    (apply orig-fun args)))
