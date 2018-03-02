(if (window-system)
    (tool-bar-mode -1))
(menu-bar-mode -1)
(show-paren-mode 1)
(setq uniquify-buffer-name-style t)
(setq uniquify-buffer-name-style (quote post-forward))
(setq uniquify-min-dir-content 1)
(electric-pair-mode 1)
(setq cal-tex-diary t)
(setq blog-root "/ssh:abturet@turetzky.org:~/blog/")
(add-hook 'diary-display-hook 'fancy-diary-display)
(add-hook 'text-mode-hook ' turn-on-auto-fill)
(add-hook 'before-save-hook 'time-stamp)
(setq dired-omit-files-p t)
(add-hook 'dired-load-hook '(lambda () (require 'dired-x)))
(setq tramp-auto-save-directory "~/tmp")
(setq backup-directory-alist
      '((".*" . "~/tmp/")))
(setq auto-save-file-name-transforms
      '((".*" "~/tmp")))
(setq message-log-max 1000)
(set-default-font "Monaco-13")
(add-to-list 'default-frame-alist '(font . "Monaco-13"))
(setq help-at-pt-display-when-idle t)
(setq help-at-pt-timer-delay 0.1)
(help-at-pt-set-timer)
(setq mode-line-in-non-selected-windows nil)

(global-set-key "\C-cy" 'popup-kill-ring)
                ;; '(lambda ()
                ;;            (interactive)
                ;;            (popup-menu 'yank-menu)))

(require 'smtpmail)

(setq send-mail-function 'smtpmail-send-it
      message-send-mail-function 'smtpmail-send-it
      smtpmail-auth-credentials (expand-file-name "~/.authinfo.gpg")
      smtp-smtp-server "https://outlook.office365.com/EWS/Exchange.asmx"
      smtpmail-stream-type 'starttls
      smtpmail-smtp-service 587)
(setq starttls-use-gnutls t)
(setq user-full-name "Ari Turetzky")
(setq user-mail-address "aturetzky@quantcast.com")

(setq TeX-command-list
      (quote (
              ("TeX" "tex \\\\nonstopmode\\\\input %t" TeX-run-TeX nil t)
              ("LaTeX" "%l -shell-escape \\\\nonstopmode\\\\input{%t}" TeX-run-LaTeX nil t)
              ("LaTeX PDF" "pdflatex -shell-escape \\\\nonstopmode\\\\input{%t}" TeX-run-LaTeX nil t)
              ("View" "%v" TeX-run-discard nil nil)
              ("Print" "gsview32 %f" TeX-run-command t nil)
              ("File" "dvips %d -o %f " TeX-run-command t nil)
              ("BibTeX" "bibtex %s"</FONT> TeX-run-BibTeX nil nil)
              ("Index" "makeindex %s" TeX-run-command nil t)
              ("Check" "lacheck %s" TeX-run-compile nil t)
              ("Other" "" TeX-run-command t t))))

(require 'package)

(add-to-list 'package-archives
             '("marmalade" . "https://marmalade-repo.org/packages/"))
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/"))
(add-to-list 'package-archives
             '("elpa" . "https://elpa.gnu.org/packages/"))
(add-to-list 'package-archives
             '("org" . "https://orgmode.org/elpa/"))

(package-initialize)

(require 'use-package)

(require 'load-path-config-new)

(use-package js-comint
  :ensure t
  :init
  (setq inferior-js-program-command "node"))
(use-package js2-mode
  :ensure t
  :bind (:map js2-mode-map
              ("\C-x\C-e" . js-send-last-sexp)
              ("\C-\M-x"  . js-send-last-sexp-and-go)
              ("\C-cb"    . js-send-buffer)
              ("\C-c\C-b" . js-send-buffer-and-go)
              ("\C-cl"    . js-load-file-and-go))
  )

(use-package highline
  :ensure t
  :init
  (highline-mode t)
  (setq highlin-face '((:background "thistle4")))
  (setq highline-vertical-face (quote ((t (:background "lemonChiffon2"))))))

(use-package ivy
  :ensure t
  :init
  (ivy-mode 1)
  (diminish 'ivy-mode "  " )
  (setq ivy-use-virtual-buffers t)
  (setq enable-recursive-minibuffers t)
  (define-key minibuffer-local-map (kbd "C-r") 'counsel-minibuffer-history)
  :bind
  (
   ("\C-s" . 'swiper)
   ("C-x C-f" . 'counsel-find-file)
   ("C-c j" . 'counsel-git-grep)
   ("C-c k" . 'counsel-ag)
   ("C-c l" . 'counsel-locate)))

(use-package ace-window
  :ensure t
  :config
  (ace-window-display-mode)
  :bind
  ("\M-o" . ace-window))

(use-package git-gutter
  :ensure t
  :init
  (global-git-gutter-mode))

(use-package git-timemachine
  :ensure t
  )

;; Notes in *scratch* v. 0.2
;; Copyright (c) 2006 by Michal Nazarewicz (mina86/AT/mina86.com)
;; Released under GNU GPL

(defconst scratch-file (expand-file-name "~/.emacs.d/scratch")
  "File where content of *scratch* buffer will be read from and saved to.")
(defconst scratch-file-autosave (concat scratch-file ".autosave")
  "File where to autosave content of *scratch* buffer.")

(save-excursion
  (set-buffer (get-buffer-create "*scratch*"))
  (if (file-readable-p scratch-file)
      (if (and (file-readable-p scratch-file-autosave)
               (file-newer-than-file-p scratch-file-autosave scratch-file)t)
          (insert-file-contents scratch-file-autosave nil nil nil t)
        (insert-file-contents scratch-file nil nil nil t)
        (set-buffer-modified-p nil)))
  (auto-save-mode 1)
  (setq buffer-auto-save-file-name scratch-file-autosave)
                                        ; (setq revert-buffer-function 'scratch-revert)
  (fundamental-mode))
(add-hook 'kill-buffer-query-functions 'kill-scratch-buffer)
(add-hook 'kill-emacs-hook 'kill-emacs-scratch-save)

(defun scratch-revert (ignore-auto noconfirm)
  (when (file-readable-p scratch-file)
    (insert-file-contents scratch-file nil nil nil t)
    (set-buffer-modified-p nil)))

(defun kill-scratch-buffer ()
  (not (when (string-equal (buffer-name (current-buffer)) "*scratch*")
         (delete-region (point-min) (point-max))
         (set-buffer-modified-p nil)
         (next-buffer)
         t)))

(defun kill-emacs-scratch-save ()
  (let ((buffer (get-buffer-create "*scratch*")))
    (if buffer
        (save-excursion
          (set-buffer buffer)
          (write-region nil nil scratch-file)
          (unless (string-equal scratch-file buffer-auto-save-file-name)
            (delete-auto-save-file-if-necessary t))))))

(use-package flycheck
  :diminish flycheck-mode
  :ensure t
  :init
  (setq flycheck-emacs-lisp-initialize-packages 1)
  (setq flycheck-emacs-lisp-load-path 'inherit)
  )

(server-start)

(use-package diminish
  :ensure t
  :init
  (diminish 'org-mode  "")
  )
(use-package org
  :ensure t
  :diminish  "")
(require 'ox-twbs)
(require 'ox-twbs)
(use-package org-mime
  :ensure t)
(setq org-ellipsis " ⤵")
(setq org-src-fontify-natively t)
(setq org-src-tab-acts-natively t)
(setq org-src-window-setup 'current-window)

(require 'org-bullets)
(add-hook 'org-mode-hook (lambda() (org-bullets-mode 1)))
(setq org-startup-with-inline-images t)
(add-hook 'org-babel-after-execute-hook 'org-redisplay-inline-images)
;;***********remember + Org config*************
(setq org-remember-templates
      '(("Tasks" ?t "* TODO %?\n %i\n %a" "H://todo.org")
        ("Appointments" ?a "* Appointment: %?\n%^T\n%i\n %a" "H://todo.org")))
(setq remember-annotation-functions '(org-remember-annotation))
(setq remember-handler-functions '(org-remember-handler))
(add-hook 'remember-mode-hook 'org-remember-apply-template)
(global-set-key (kbd "C-c r") 'remember)

(setq org-todo-keywords '((type "TODO" "STARTED" "WAITING" "DONE")))
(setq org-todo-keywords-1 '((type "TODO" "STARTED" "WAITING" "DONE")))
(setq org-agenda-include-diary t)
(setq org-agenda-include-all-todo t)
(org-babel-do-load-languages
 'org-babel-load-languages
 '((shell  . t)
   (js  . t)
   (emacs-lisp . t)
   (python . t)
   (ruby . t)
   (css . t )
   (plantuml . t)
   (java . t)))
(setq org-confirm-babel-evaluate nil)

(use-package virtualenvwrapper
  :ensure t
  :init
  (venv-initialize-interactive-shells)
  (venv-initialize-eshell)
  (setq venv-location "~/.virtualenvs")
  )
(setq org-plantuml-jar-path "/usr/local/Cellar/plantuml/1.2018.1/libexec/plantuml.jar")
(setq plantuml-jar-path "/usr/local/Cellar/plantuml/1.2018.1/libexec/plantuml.jar")


(setq org-mime-export-options '(:section-numbers nil
:with-author nil
:with-toc nil))

;;(require 'javascript-mode)
;;(require 'js2-mode)
(use-package ag
  :ensure t)
(require 'highline)
(require 'dired-details)
(dired-details-install)
(require 'uniquify)
;;(require 'sawzall)
(use-package boxquote
  :ensure t)
;;(require 'rs-info)
(require 'tex-site)
(require 'tramp)
(use-package gist
  :ensure t)
(use-package web-mode
  :ensure t)
;;(require 'xslt-process)
(require 'ls-lisp)
;;(require 'cmd-mode)
(use-package puppet-mode
  :ensure t)
(require 'blog)
;;(require 'erc)
;;(require 'erc-menu)
(use-package htmlize
  :ensure t)
;;(require 'jdee-config)
(require 'cl)
;;(require 'misc)
;;  (require 'remember)
;;(require 'skeleton-conf)
(require 'keys-config-new)
;;(require 'html-config)
;;(require 'vb-config)
;;(require 'xml-config)
;;(require 'sql-config)
;;(require 'mail-config)
;;(require 'erc-config)
;;(require 'gnus-config)
;;(require 'css-mode)
(require 'ari-custom-new)
;;(require 'csharp-config)
;;(require 'php-mode)
;;(require 'vc-svn)
;; (use-package  color-theme
;;   :ensure t
;;   :init
;;    (setq color-theme-directory "~/emacs/site/color-theme/themes"))
(use-package all-the-icons
  :ensure t)
(use-package powerline
  :ensure t
  :init
  (setq powerline-image-apple-rgb t)
  (setq powerline-height 28)
  )
(use-package hc-zenburn-theme
  :ensure t
  :init
  (powerline-default-theme)
  (load-theme 'hc-zenburn t)
  (hc-zenburn-with-color-variables
    (custom-theme-set-faces
     'hc-zenburn
     `(ac-candidate-face ((t (:background ,hc-zenburn-bg+3 :foreground ,hc-zenburn-green+4))))
     `(ac-selection-face ((t (:background ,hc-zenburn-cyan  :foreground ,hc-zenburn-blue-4))))
     `(popup-isearch-match ((t (:background ,hc-zenburn-cyan :foreground ,"Blue"))))))
  )
;;     (use-package moe-theme
;;       :ensure t
;;       :config
;;       (load-theme 'moe-dark t)
;;       (moe-dark)
;;       (powerline-moe-theme))
;;     (require 'moe-dark)
;;     (moe-dark)

;; (use-package zenburn-theme
;;   :ensure t
;;   ;;  :init
;;                                         ;  (load-theme 'zenburn t)
;;   )
(use-package exec-path-from-shell
  :ensure t
  :init
  (when (memq window-system '(mac ns x))
    (exec-path-from-shell-initialize))
  (setq exec-path-from-shell-check-startup-files nil))
(use-package inf-ruby
  :ensure t)
(require 'ruby-mode)
(use-package  ruby-electric
  :ensure t)
(use-package coffee-mode
  :ensure t)
(use-package feature-mode
  :ensure t)
(require 'rcodetools)
(use-package yasnippet
  :ensure t)
(yas-global-mode t)
(yas-global-mode)
(use-package auto-complete
  :diminish "  "
  :ensure t
  :init
  (setq ac-use-menu-map t)
  (setq ac-use-fuzzy t))
(require 'auto-complete-config)
;;  (ac-config-default)
;;    (require 'auto-complete-yasnippet)
(use-package haml-mode
  :ensure t)
(use-package rvm
  :ensure t
  :hook
  (ruby-mode . rvm-activate-corresponding-ruby))
(rvm-use-default)
(require 'ruby-config-new)

(require 'eclim)
;;  (global-eclim-mode)
(require 'eclimd)
(use-package ac-emacs-eclim
  :ensure t)
(require 'ac-emacs-eclim)
;;(ac-emacs-eclim-config)
;;  (setq eclim-eclipse-dirs '("~/eclipse/java-oxygen-tar/"))
(setq eclim-executable "~/eclipse/java-oxygen-tar/Eclipse.app/Contents/Eclipse/eclim")
(setq eclimd-executable "~/eclipse/java-oxygen-tar/Eclipse.app/Contents/Eclipse/eclimd")

(use-package projectile
  :ensure t
  :init
  (projectile-global-mode)
  (setq projectile-switch-project-action 'projectile-dired)
  (setq projectile-require-project-root nil))

(global-auto-complete-mode t)           ;enable global-mode
(setq ac-auto-start t)                  ;automatically start
(setq ac-dwim 3)                        ;Do what i mean
(setq ac-override-local-map nil)        ;don't override local map
(define-key ac-complete-mode-map "\t" 'ac-expand)
(define-key ac-complete-mode-map "\r" 'ac-complete)
(define-key ac-complete-mode-map "\M-n" 'ac-next)
(define-key ac-complete-mode-map "\M-p" 'ac-previous)
(set-default 'ac-sources '(ac-source-words-in-buffer ac-source-yasnippet ac-source-abbrev ac-source-words-in-buffer ac-source-dictionary ac-source-files-in-current-dir))

(setq ac-modes
      (append ac-modes
              '(eshell-mode
                                        ;org-mode
                )))
                                        ;(add-to-list 'ac-trigger-commands 'org-self-insert-command)

(add-hook 'emacs-lisp-mode-hook
          (lambda ()
            (setq ac-sources '(ac-source-yasnippet ac-source-abbrev ac-source-words-in-buffer ac-source-symbols))))

(add-hook 'eshell-mode-hook
          (lambda ()
            (setq ac-sources '(ac-source-yasnippet ac-source-abbrev ac-source-files-in-current-dir ac-source-words-in-buffer))))
(add-hook 'web-mode-hook
          (lambda ()
            (setq ac-sources '(ac-source-yasnippet ac-source-abbrev ac-source-files-in-current-dir ac-source-words-in-buffer))))
(add-hook 'yaml-mode-hook
          (lambda ()
            (setq ac-sources '(ac-source-yasnippet ac-source-abbrev ac-source-semantic ac-source-files-in-current-dir ac-source-words-in-buffer ac-source-words-in-same-mode-buffers ))))
(add-hook 'js2-mode-hook
          (lambda ()
            (add-to-list 'ac-sources '(ac-source-files-in-current-dir ac-source-symbols ac-source-abbrev ac-source-yasnippet ac-source-words-in-same-mode-buffers ac-source-variables)(auto-complete-mode))))
(setq ac-modes
      (append ac-modes
              '(sql-mode
                sqlplus-mode
                js2-mode
                coffee-mode
                JavaSript-IDE-mode
                text-mode
                css-mode
                web-mode
                perl-mode
                ruby-mode
                scala-mode
                java-mode
                yaml-mode
                )))

;;  (use-package color-theme
;;    :ensure t
;;    :init
;;    (color-theme-initialize)
;;    (color-theme-zenburn))

(add-to-list 'auto-mode-alist
             (cons
              (concat "\\." (regexp-opt '("xml" "xsd" "svg" "rss" "rng" "build" "config") t) "\\'" )'nxml-mode))

;;
;; What files to invoke the new html-mode for?
(add-to-list 'auto-mode-alist '("\\.inc\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.php\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.[sj]?html?\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.jsp\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.t\\'" . perl-mode))
(add-to-list 'auto-mode-alist '("\\.pp\\'" . puppet-mode))
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
;;


(add-hook 'html-mode-hook 'abbrev-mode)
(add-hook 'web-mode-hook 'abbrev-mode)

(autoload 'dash-at-point "dash-at-point"
  "Search the word at point with Dash." t nil)

(autoload 'markdown-mode' "markdown-mode" "Major Mode for editing Markdown" t)
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))

(autoload 'ruby-mode "ruby-mode"
  "Mode for editing ruby source files" t)
(setq auto-mode-alist
      (append '(("\\.rb$" . ruby-mode)) auto-mode-alist))
(setq interpreter-mode-alist (append '(("ruby" . ruby-mode))
                                     interpreter-mode-alist))
;;   (autoload 'run-ruby "inf-ruby"
;;     "Run an inferior Ruby process")
;;   (autoload 'inf-ruby-keys "inf-ruby"
;;     "Set local key defs for inf-ruby in ruby-mode")
;;   (add-hook 'ruby-mode-hook
;;         '(lambda ()
;;            (inf-ruby-keys)
;;   ))
(load-library "rdebug")
(setq ri-ruby-script (expand-file-name "~/emacs/site/lisp/ri-emacs.rb"))
(autoload 'ri (expand-file-name "~/emacs/site/lisp/ri-ruby.el") nil t)
(load  (expand-file-name "~/emacs/site/lisp/ri-ruby.el"))
(setq ri-ruby-script (expand-file-name "~/emacs/site/lisp/ri-emacs.rb"))
(autoload 'ri (expand-file-name "~/emacs/site/lisp/ri-ruby.el") nil t)
(load  (expand-file-name "~/emacs/site/lisp/ri-ruby.el"))
(setq rct-debug nil)

(require 'dired-x)
(setq dired-omit-files
      (rx(or(seq bol(? ".") "#")
            (seq bol"."(not(any".")))
            (seq "~" eol)
            (seq bol "CVS" eol)
            (seq bol "svn" eol))))

(setq dired-omit-extensions
      (append dired-latex-unclean-extensions
              dired-bibtex-unclean-extensions
              dired-texinfo-unclean-extensions))


(add-hook 'dired-mode-hook (lambda () (dired-omit-mode 1)))

(setq-default indent-tabs-mode nil)
(setq-default c-basic-offset 4)

;;(require 'multiple-cursors)

;;(require 'whitespace)
;;(autoload 'nuke-trailing-whitespace "whitespace" nil t)
;;(add-hook 'write-file-hooks 'nuke-trailing-whitespace)

;;(require 'start-opt)
;; (defadvice whitespace-cleanup (around whitespace-cleanup-indent-tab
;;                                       activate)
;;   "Fix whitespace-cleanup indent-tabs-mode bug"
;;   (let ((whitespace-indent-tabs-mode indent-tabs-mode)
;;         (whitespace-tab-width tab-width))
;;     ad-do-it))
;; (add-to-list 'nuke-trailing-whitespace-always-major-modes 'csharp-mode)

;;(autoload 'hideshowvis-enable "hideshowvis" "Highlight foldable regions")

;; (dolist (hook (list 'emacs-lisp-mode-hook
;;                     'c++-mode-hook
;;                     'ruby-mode-hook
;;                     'c-sharp-mode-hook
;;                     'java-mode-hook
;;                     ))
;;   (add-hook hook 'hideshowvis-enable))

(add-hook 'sql-mode-hook 'my-sql-mode-hook)
(defun my-sql-mode-hook()
  (message "SQL mode hook executed")
  (define-key sql-mode-map [f5] 'sql-send-buffer))

(setq sql-db2-program "db2cmd")
(setq sql-db2-options '("-c" "-i" "-w" "db2" "-v" ))


;;(setq sql-db2-program "db2cmd db2clp.bat db2.exe")
(setq sql-ms-program "osql")
(setq sql-mysql-program "c:/cygwin/usr/local/bin/mysql")
(setq sql-pop-to-buffer-after-send-region nil)
(setq sql-product (quote ms))

;;(require 'semantic-ia)
;;(if window-system
;;    (progn
;;      (setq semantic-load-turn-everything-on t)
;;      (semantic-load-enable-gaudy-code-helpers)))

(provide 'emacs-config-new)
