;; -*- lexical-binding: t; -*-
(message "loading load-path-config")

;; System-wide paths - check both Homebrew locations (Intel and Apple Silicon)
(dolist (sys-path '("/usr/local/share/emacs/site-lisp"     ; Intel Mac Homebrew
                    "/opt/homebrew/share/emacs/site-lisp"  ; Apple Silicon Mac Homebrew
                    "/usr/share/emacs/site-lisp"))         ; Linux
  (when (file-exists-p sys-path)
    (add-to-list 'load-path sys-path)))

;; Local site packages - check each one exists
(dolist (site-path '("~/emacs/site/lisp"
                     "~/emacs/site/nano-elfeed"
                     "~/emacs/site/relative-date"
                     "~/emacs/site/ruby-block"
                     "~/emacs/site/blog"))
  (let ((expanded-path (expand-file-name site-path)))
    (when (file-exists-p expanded-path)
      (add-to-list 'load-path expanded-path))))

;; Local checkout (not on MELPA); straight builds it from ~/dev/git
(use-package org-block-capf
  :straight (:local-repo "~/dev/git/org-block-capf"
             :host github :repo "xenodium/org-block-capf")
  :hook (org-mode . org-block-capf-add-to-completion-at-point-functions))

(provide 'load-path-config-new)
