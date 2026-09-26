;;; early-init.el --- Early initialization -*- lexical-binding: t; -*-
;;; Commentary:
;; Tracked copy: ~/emacs/config/early-init.el.  Install on a machine with
;;   ln -sf ~/emacs/config/early-init.el ~/.emacs.d/early-init.el
;; (or copy it).  GC, native-comp and process-output settings live ONLY
;; here -- the 2026-09 review found an untracked copy that had drifted and
;; was silently overriding the main config.
;;; Code:

;; Packages are initialized manually in emacs-config.org
(setq package-enable-at-startup nil)
(setq load-prefer-newer t)

;; Native compilation: JIT on, capped at 2 parallel jobs (unlimited was
;; spawning too many processes).  Speed 2 is the default; 3 can miscompile.
(setq native-comp-jit-compilation t
      native-comp-async-jobs-number 2
      native-comp-speed 2
      native-comp-async-report-warnings-errors 'silent)

;; Larger reads from subprocesses (LSP servers)
(setq read-process-output-max (* 4 1024 1024))

(menu-bar-mode -1)
(tool-bar-mode -1)

;; GC: effectively off during startup, 50MB afterwards
(setq gc-cons-threshold (* 100 1024 1024)
      gc-cons-percentage 0.6)
(add-hook 'emacs-startup-hook
          (lambda ()
            (setq gc-cons-threshold (* 50 1024 1024)
                  gc-cons-percentage 0.1)))

(defun efs/display-startup-time ()
  (message "Emacs loaded in %s with %d garbage collections."
           (format "%.2f seconds"
                   (float-time
                    (time-subtract after-init-time before-init-time)))
           gcs-done))
(add-hook 'emacs-startup-hook #'efs/display-startup-time)

(add-to-list 'load-path (expand-file-name "~/emacs/config/"))

(provide 'early-init)
;;; early-init.el ends here
