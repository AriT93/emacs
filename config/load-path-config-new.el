;; -*- lexical-binding: t; -*-
(message "loading load-path-config")
(add-to-list 'load-path "/usr/local/share/emacs/site-lisp")
(add-to-list 'load-path (expand-file-name "~/emacs/site/color-theme/themes"))
(add-to-list 'load-path (expand-file-name "~/emacs/site/lisp"))
(add-to-list 'load-path (expand-file-name "~/emacs/site/ruby-block"))
(add-to-list 'load-path (expand-file-name "~/emacs/site/blog"))
(add-to-list 'load-path "~/dev/git/lsp-bridge/")
(add-to-list 'load-path "~/dev/git/flyover/")
(add-to-list 'load-path "~/dev/git/org-block-capf")
(require 'org-block-capf)
(add-hook 'org-mode-hook #'org-block-capf-add-to-completion-at-point-functions)

(load "ps-print")

(autoload 'python-mode "python-mode" "Python editing mode." t)

(provide 'load-path-config-new)
