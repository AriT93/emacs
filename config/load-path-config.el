(message "loading load-path-config")
(add-to-list 'load-path (expand-file-name "~/emacs/site/emacs-w3m"))
(add-to-list 'load-path "/usr/local/share/emacs/site-lisp")
(add-to-list 'load-path (expand-file-name "~/emacs/site/emacs-w3m/attic"))
(add-to-list 'load-path (expand-file-name "~/emacs/site/color-theme/themes"))
(add-to-list 'load-path (expand-file-name "~/emacs/site/lisp"))
;;(add-to-list 'load-path (expand-file-name "~/emacs/site/yasnippet-0.7.0"))
(add-to-list 'load-path (expand-file-name "~/emacs/site/yasnippet"))
(add-to-list 'load-path (expand-file-name "~/emacs/site/yasnippets-rails"))
(add-to-list 'load-path (expand-file-name "~/emacs/site/mailcrypt"))
(add-to-list 'load-path (expand-file-name "~/emacs/site/win32"))
(add-to-list 'load-path (expand-file-name "~/emacs/site/bhl"))
;;(add-to-list 'load-path (expand-file-name "~/emacs/site/icicles.del"))
(add-to-list 'load-path (expand-file-name "~/emacs/site/bbdb/lisp"))
(add-to-list 'load-path (expand-file-name "~/emacs/site/jde/lisp"))
;;(add-to-list 'load-path (expand-file-name "c:/temp/jdee/lisp"))
(add-to-list 'load-path (expand-file-name "~/emacs/site/cedet/semantic/"))
(add-to-list 'load-path (expand-file-name "~/emacs/site/cedet/speedbar"))
(add-to-list 'load-path (expand-file-name "~/emacs/site/elib"))
(add-to-list 'load-path (expand-file-name "~/emacs/site/cedet/eieio"))
(add-to-list 'load-path (expand-file-name "~/emacs/site/auctex"))
(add-to-list 'load-path (expand-file-name "~/emacs/site/edb"))
(add-to-list 'load-path (expand-file-name "~/emacs/site/ecb"))
(add-to-list 'load-path (expand-file-name "~/emacs/site/xslide"))
;;(add-to-list 'load-path (expand-file-name "~/emacs/site/slime"))
(add-to-list 'load-path (expand-file-name "~/emacs/site/xslt-process/lisp"))
(add-to-list 'load-path (expand-file-name "~/emacs/site/other"))
(add-to-list 'load-path (expand-file-name "~/emacs/site/multi-mode"))
(add-to-list 'load-path (expand-file-name "~/emacs/site/other/css-mode"))
(add-to-list 'load-path (expand-file-name "~/emacs/site/muse/lisp"))
(add-to-list 'load-path (expand-file-name "~/emacs/site/cedet"))
(add-to-list 'load-path (expand-file-name "~/emacs/site/css-mode"))
(add-to-list 'load-path (expand-file-name "~/emacs/site/cedet/common"))
(add-to-list 'load-path (expand-file-name "~/emacs/site/remember"))
(add-to-list 'load-path (expand-file-name "~/emacs/site/blog"))
;;(add-to-list 'load-path (expand-file-name "~/emacs/site/vm"))
(add-to-list 'load-path (expand-file-name "~/tmp/vm-8.1.0-devo-571/lisp"))
(add-to-list 'load-path (expand-file-name "~/emacs/site/oneonone"))
(add-to-list 'load-path (expand-file-name "~/emacs/site/w3/lisp/"))
(add-to-list 'load-path (expand-file-name "~/emacs/site/html"))
(add-to-list 'load-path (expand-file-name "~/emacs/site/dotnet"))
(add-to-list 'load-path (expand-file-name "~/emacs/site/python"))
(add-to-list 'load-path (expand-file-name "~/emacs/site/color-theme"))
(add-to-list 'load-path (expand-file-name "~/emacs/site/color-theme/themes"))
(add-to-list 'load-path (expand-file-name "~/emacs/site/java-tools"))
(add-to-list 'load-path (expand-file-name "~/emacs/site/mmm-mode"))
(add-to-list 'load-path (expand-file-name "~/emacs/site/php-mode"))
(add-to-list 'load-path (expand-file-name "~/emacs/site/js2-mode"))
(add-to-list 'load-path (expand-file-name "~/emacs/site/ruby"))
(add-to-list 'load-path (expand-file-name "~/emacs/site/auto-complete"))
;;(add-to-list 'load-path (expand-file-name "~/.emacs.d/emacs-rails"))
(add-to-list 'load-path (expand-file-name "~/emacs/site/yaml-mode"))
(add-to-list 'load-path (expand-file-name "~/emacs/site/ruby-block"))
(add-to-list 'load-path (expand-file-name "~/emacs/site/rinari"))
(add-to-list 'load-path (expand-file-name "~/emacs/site/rdebug"))

(load-file "~/emacs/site/python/python-mode.el")
(load "ps-print")
(load-file "~/emacs/site/cedet/common/cedet.elc")

(autoload 'python-mode "python-mode" "Python editing mode." t)

(provide 'load-path-config)
