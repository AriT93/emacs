;;; Sacha Chua's remember.el configuration

;; You can get remember.el from
;; http://sacha.free.net.ph/notebook/emacs/dev/remember

(require 'remember)
(require 'planner)
(require 'planner-rss)
(require 'remember-planner)
;(require 'remember-bibl)

(setq remember-save-after-remembering t)
(setq remember-handler-functions '(remember-planner-append))
(setq remember-annotation-functions planner-annotation-functions)

(defvaralias 'remember-annotation-functions 'planner-annotation-functions)

(defun ajk/my-cleanup-then-save-buffers-kill-emacs (&optional arg)
  "Clean up before saving buffers and killing Emacs."
  (interactive "P")
  ;; stop here if there's a *Remember* buffer
  (if (get-buffer remember-buffer)
      (remember)
    ;; clean up Gnus
    (and
     (fboundp 'gnus-alive-p)
     (gnus-alive-p)
     (let ((gnus-interactive-exit nil))
       (gnus-group-exit)))
    (save-buffers-kill-emacs arg)))
(defalias 'sacha/save-buffers-kill-emacs
          'ajk/my-cleanup-then-save-buffers-kill-emacs)

(defun sacha/remember-to-notes ()
  "Start remember for notes."
  (interactive)
  (remember)
  (set (make-variable-buffer-local 'remember-handler-functions)
       '(remember-append-to-file)))

(global-set-key (kbd "<f9> n SPC") 'sacha/remember-to-notes)

(provide 'remember-config)