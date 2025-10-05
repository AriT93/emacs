;;; early-init.el --- Early initialization -*- lexical-binding: t; -*-

;; Prevent double package initialization (manual init in emacs-config.org)
(setq package-enable-at-startup nil)

(setq load-prefer-newer t)
(setq native-comp-jit-compilation t)
;; Improve performance during startup
(setq read-process-output-max (* 4 1024 1024))  ; 4MB for better LSP performance
(setq native-comp-speed 3)  ; Maximum optimization level
(setq native-comp-async-report-warnings-errors 'silent)

(menu-bar-mode -1)
(tool-bar-mode -1)
(setq gc-cons-threshold (* 80 1000 1000))

(defun efs/display-startup-time ()
  (message "Emacs loaded in %s with %d garbage collections."
           (format "%.2f seconds"
                   (float-time
                     (time-subtract after-init-time before-init-time)))
           gcs-done))

(add-hook 'emacs-startup-hook #'efs/display-startup-time)

;; Reset GC threshold after startup for better runtime performance
(add-hook 'emacs-startup-hook
          (lambda () (setq gc-cons-threshold (* 2 1000 1000))))

(add-to-list 'load-path (expand-file-name "~/emacs/config/"))
(setq native-comp-deferred-compilation t)
(provide 'early-init)
;;;early-init ends here