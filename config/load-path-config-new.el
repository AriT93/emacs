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
                     "~/emacs/site/ruby-block"
                     "~/emacs/site/blog"))
  (let ((expanded-path (expand-file-name site-path)))
    (when (file-exists-p expanded-path)
      (add-to-list 'load-path expanded-path))))

;; Development packages - check each one exists
(dolist (dev-path '("~/dev/git/lsp-bridge/"
                    "~/dev/git/flyover/"
                    "~/dev/git/org-block-capf"))
  (let ((expanded-path (expand-file-name dev-path)))
    (when (file-exists-p expanded-path)
      (add-to-list 'load-path expanded-path))))

;; Only require org-block-capf if it's available
(when (locate-library "org-block-capf")
  (require 'org-block-capf)
  (add-hook 'org-mode-hook #'org-block-capf-add-to-completion-at-point-functions))

(load "ps-print")

(autoload 'python-mode "python-mode" "Python editing mode." t)

(provide 'load-path-config-new)
