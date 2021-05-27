(message "loading load-path-config")
(add-to-list 'load-path "/usr/local/share/emacs/site-lisp")
(add-to-list 'load-path (expand-file-name "~/emacs/site/color-theme/themes"))
(add-to-list 'load-path (expand-file-name "~/emacs/site/lisp"))
(add-to-list 'load-path (expand-file-name "~/emacs/site/blog"))
(add-to-list 'load-path (expand-file-name "~/emacs/site/rcodeools"))
(add-to-list 'load-path (expand-file-name "~/emacs/site/rdebug"))

(load "ps-print")
;;(load-file "~/emacs/site/cedet/common/cedet.elc")

(autoload 'python-mode "python-mode" "Python editing mode." t)

(provide 'load-path-config-new)
