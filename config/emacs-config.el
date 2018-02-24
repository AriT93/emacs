;;; package -- summary emacs config
;;; Commentary:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; emacs-config.el                                                           ;;
;; This file will hold specific setting I like for Emacs out side of         ;;
;; customize.  Mostly requires and such but a few setq's and such as well    ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Code:
(if (window-system)
    (tool-bar-mode -1))
(menu-bar-mode -1)
(message "loading emacs-config")
(show-paren-mode 1)
(electric-pair-mode 1)
;;(setq w32-use-full-screen-buffer nil)
(setq uniquify-buffer-name-style t)
(setq uniquify-buffer-name-style (quote post-forward))
(setq uniquify-min-dir-content 1)
(setq cal-tex-diary t)
(setq blog-root "/scp:abturet@turetzky.org:~/blog/")
(add-hook 'diary-display-hook 'fancy-diary-display)
(add-hook 'text-mode-hook ' turn-on-auto-fill)
(add-hook 'before-save-hook 'time-stamp)
(setq dired-omit-files-p t)
(add-hook 'dired-load-hook '(lambda () (require 'dired-x)))
;;(setq TeX-command-list (quote (("TeX" "tex \\\\nonstopmode\\\\input %t" TeX-run-TeX nil t) ("LaTeX" "%l -shell-escape \\\\nonstopmode\\\\input{%t}" TeX-run-LaTeX nil t) ("LaTeX PDF" "pdflatex -shell-escape \\\\nonstopmode\\\\input{%t}" TeX-run-LaTeX nil t) ("View" "%v" TeX-run-discard nil nil) ("Print" "gsview32 %f" TeX-run-command t nil) ("File" "dvips %d -o %f " TeX-run-command t nil) ("BibTeX" "bibtex %s"</FONT> TeX-run-BibTeX nil nil) ("Index" "makeindex %s" TeX-run-command nil t) ("Check" "lacheck %s" TeX-run-compile nil t) ("Other" "" TeX-run-command t t))))

(require 'package)
(add-to-list 'package-archives
             '("marmalade" .
               "http://marmalade-repo.org/packages/") t)
(add-to-list 'package-archives
             '("melpa" .
               "http://melpa.org/packages/") t )
(add-to-list 'package-archives
             '("org" .
               "http://orgmode.org/elpa/") t )
(package-initialize)
(require 'use-package)
(cond
 ((string="w32" window-system)
  (set-face-attribute 'mode-line nil :family "Century Gothic" :height 1.0 :weight 'ultra-light ))
 ((string="x" window-system)
  (set-face-attribute 'mode-line nil :family "Liberation Mono:bold" :height 1.0 ))
((string="ns" window-system)
 (set-face-attribute 'mode-line nil :family "Monaco" :height 1.0)))

(setq-default compile-command "nmake")
(setq tramp-auto-save-directory "~/tmp")
(setq message-log-max 250)
;;(require 'multiple-cursors)
(require 'load-path-config)
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
(require 'js-comint)
(setq inferior-js-program-command "rhino")
(add-hook 'js2-mode-hook '(lambda ()
                (local-set-key "\C-x\C-e" 'js-send-last-sexp)
                (local-set-key "\C-\M-x" 'js-send-last-sexp-and-go)
                (local-set-key "\C-cb" 'js-send-buffer)
                (local-set-key "\C-c\C-b" 'js-send-buffer-and-go)
                (local-set-key "\C-cl" 'js-load-file-and-go)
                ))

;;decide settings for specific environs
;; (cond
;;  ((string="w32" window-system)
;;   (require 'cygwin-mount)
;;   (setq cygwin-mount-cygwin-bin-directory "c:/cygwin/bin/")
;;   (require 'setup-cygwin)
;;   (set-shell-bash)))


;;(setq default-frame-alist (quote ((foreground-color . "gray") (background-color . "black") (font . "-outline-Liberation Mono-bold-r-normal-normal-14x-97-96-96-c-*-iso8859-1"))))
(cond
 ((string="w32" window-system)
(setq default-frame-alist (quote ((foreground-color . "gray") (background-color . "black") (font . "Consolas-11")))))
 ((string="x" window-system)
(setq default-frame-alist (quote ((foreground-color . "gray") (background-color . "black") (font . "Liberation Mono:bold")))))
((string="ns" window-system)
(setq default-frame-alist (quote ((foreground-color . "gray") (background-color . "black") (font . "Monaco-13:bold"))))))


(setq muse-project-alist
      '(("WikiNew"
         ("H:/Wiki/WikiNew"
          :default "WelcomePage"
          :force-publish ("WelcomePage")
          :major-mode muse-mode)
         (:base "xhtml"
                :path "C:\\Documents and Settings\\All Users\\webdocs\\wwwroot\\WikiNew"
                :style-sheet "<link rel=\"stylesheet\" type=\"text/css\" href=\"http://localhost/WikiNew/style.css\"/>"))))
(setq muse-wiki-interwiki-alist
      '(("EmacsWiki" . "http://www.emavcswiki.org/cgi-bin/wiki/")
        ("WikiPlanner" . "http://localhost/Plans")
        ("WikiNew" . "http://localhost/WikiNew")))
(server-start)



(require 'org)

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





(defun insert-time()
  (interactive)
  (insert (format-time-string "%y-%m-%d-%R")))


;;(require 'semantic-config)
;;#(load-library "javascript")
;;(require 'javascript-mode)
(require 'js2-mode)
;;(if (not (string-equal system-type "windows-nt"))(require 'magit))
(require 'highline)
(require 'dired-details)
(dired-details-install)
(require 'uniquify)
(require 'sawzall)
;;(require 'vm)
(require 'boxquote)
(require 'rs-info)
(require 'terminal)
(require 'tex-site)
(require 'tramp)
(require 'gist)
;;(require 'ecb)
(require 'speedbar)
(require 'web-mode)
;;(require 'xslt-process)
;;(require 'semantic-load)
(require 'ls-lisp)
(require 'blog)
(require 'erc)
(require 'erc-menu)
(require 'htmlize)
;;(require 'jdee-config)
(require 'cl)
;;(require 'semantic-util-modes)
(require 'misc)
(require 'remember)
(require 'skeleton-conf)
(require 'keys-config)
(require 'html-config)
(require 'vb-config)
(require 'xml-config)
(require 'sql-config)
;;(require 'mail-config)
(require 'erc-config)
(require 'gnus-config)
;;(require 'jde)
;;(require 'css-mode)
(require 'ari-custom)
(require 'csharp-config)
;;(require 'muse)
;;(require 'muse-xml)
;;(require 'muse-mode)
;;(require 'muse-html)
;;(require 'muse-wiki)
;;(require 'muse-colors)
;;(require 'muse-latex)
;;(require 'muse-colors)
(require 'php-mode)
(require 'vc-svn)
;;(require 'psvn)
(require 'color-theme)
(require 'ruby-mode)
(require 'ruby-electric)
(require 'org)
(require 'coffee-mode)
(require 'feature-mode)
;;(require 'rcodetools)
;;(require 'popup)
(require 'yasnippet)
;;(yas-global-mode t)
;;(yas-global-mode)
(require 'auto-complete)
(require 'auto-complete-config)
(require 'auto-complete-yasnippet)
(require 'haml-mode)
(require 'rvm)
(rvm-use-default)
;;(require 'rails)
;;(require 'yasnippet)
;;(require 'zenburn
;;(require 'vivid-chalk)
(add-to-list 'load-path "~/emacs/site/emacs-eclim")
(require 'eclim)
(global-eclim-mode)
(require 'eclimd)
(require 'ac-emacs-eclim-source)
(ac-emacs-eclim-config)
(setq eclim-executable "/Applications/eclipse/eclim")
(setq eclim-eclipse-dirs '("~/eclipse/java-oxygen-tar/"))
(setq eclim-executable "~/eclipse/java-oxygen-tar/Eclipse.app/Contents/Eclipse/plugins/org.eclim_2.7.0/bin/eclim")
(setq eclimd-executable "~/eclipse/java-oxygen-tar/Eclipse.app/Contents/Eclipse/plugins/org.eclim_2.7.0/bin/eclimd")
(setq help-at-pt-display-when-idle t)
(setq help-at-pt-timer-delay 0.1)
(help-at-pt-set-timer)

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
(require 'ruby-config)



(if (window-system)
    (progn (require 'color-theme)
           (color-theme-initialize)
           (color-theme-zenburn))
;;           (load-theme 'org-beautify))

           ;;(color-theme-simple-2)
           ;;(color-theme-calm-forest)
;;           (load-file "~/emacs/site/color-theme/themes/LazyCatTheme.el")
;;           (load-file "~/emacs/site/color-theme/themes/vivid-chalk.el")
;;           (color-theme-vivid-chalk))
;;    (zenburn)
           ;;(color-theme-hash)
       )

;;(add-to-list 'vc-handled-backends 'SVN)
 (highline-mode t)
;;(require 'bbdb)
;; (if (window-system)
;;     (require 'oneonone-config))
;;(bbdb-initialize 'w3)
;;(load-library "mailcrypt")

;;(mc-setversion "gpg")
;;(setq mc-gpg-user-id "arit93@yahoo.com")

;;(setq mc-temp-directory "~/tmp/")
;;(setq mc-gpg-user-id user-mail-address)

;;ensure correct handling of xml files by nxml-mode
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

;;font lock faces
(setq highline-face '((:background "thistle4")))
(setq highline-vertical-face (quote ((t (:background "lemonChiffon2")))))

;;;; ------------------------------------------------------------------------
;;;; --- Frame max toggle - From: "rgb" <rbielaws@...> / gnu.emacs.help / 18 Mar 2005 16:30:32 -0800
;;;; ------------------------------------------------------------------------
;;(make-variable-frame-local 'my-frame-state)

(defun my-frame-maximize ()
  "Maximize Emacs window in win32"
  (interactive)

  (modify-frame-parameters nil '((my-frame-state . t)))
  (w32-send-sys-command ?\xf030))

 (defun my-frame-restore ()
   "Restore Emacs window in win32"
   (interactive)

   (modify-frame-parameters nil '((my-frame-state . nil)))
   (w32-send-sys-command ?\xF120))

(defun my-frame-toggle ()
  "Maximize/Restore Emacs frame based on `my-frame-state'"
  (interactive)
  (if my-frame-state
          (my-frame-restore)
        (my-frame-maximize)))

(global-set-key (kbd "M-m") 'my-frame-toggle)

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
   ;; (autoload 'run-ruby "inf-ruby"
   ;;   "Run an inferior Ruby process")
   ;; (autoload 'inf-ruby-keys "inf-ruby"
   ;;   "Set local key defs for inf-ruby in ruby-mode")
   ;; (add-hook 'ruby-mode-hook
   ;;       '(lambda ()
   ;;          (inf-ruby-keys)
;; ))
(autoload 'inf-ruby-minor-mode "inf-ruby" "Run an inferior Ruby process" t)
(add-hook 'ruby-mode-hook 'inf-ruby-minor-mode)
(load-library "rdebug")


(setenv "PATH" (concat (getenv "PATH") "/Users/abturet/.rvm/bin:/usr/bin:/bin:/usr/sbin:/sbin:/Library/TeX/texbin/:/usr/local/bin:/usr/texbin:/usr/X11/bin:/usr/local/share/npm/bin;c:/CYGWIN/bin;"))
(setenv "CVSROOT" ":ext:mrpy@cctech:/opt/CC/cvs/cvsroot")
(setenv "CVS_RSH" "plink")
(setenv "PLINK_PROTOCOL" "ssh")


;;transparency
(set-frame-parameter (selected-frame) 'alpha '(85 50))
(add-to-list 'default-frame-alist '(alpha 85 50))

(setq ri-ruby-script (expand-file-name "~/emacs/site/lisp/ri-emacs.rb"))
(autoload 'ri (expand-file-name "~/emacs/site/lisp/ri-ruby.el") nil t)
(load  (expand-file-name "~/emacs/site/lisp/ri-ruby.el"))

(setq rct-debug nil)


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

(require 'cmd-mode)
(require 'ls-lisp)
(require 'puppet-mode)

;; (dolist (hook (list 'emacs-lisp-mode-hook
;;                     'c++-mode-hook))
;;   (add-hook hook 'hideshowvis-enable))
(add-to-list 'write-file-functions 'time-stamp)

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
(ivy-mode 1)
(setq ivy-use-virtual-buffers t)
(setq enable-recursive-minibuffers t)
(global-set-key (kbd "C-x C-f") 'counsel-find-file)
(global-set-key (kbd "C-c j") 'counsel-find-file)
(global-set-key (kbd "C-c k") 'counsel-ag)
(global-set-key (kbd "C-x l") 'counsel-locate)
(define-key minibuffer-local-map (kbd "C-r") 'counsel-minibuffer-history)

(global-set-key "\C-cy" '(lambda ()
                           (interactive)
                           (popup-menu 'yank-menu)))

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

(require 'org-bullets)
(add-hook 'org-mode-hook (lambda() (org-bullets-mode 1)))
(setq org-startup-with-inline-images t)
;;(require 'org-plus-contrib)
(require 'org-mime)
;; (use-package org-plus-contrib
;;   :ensure t
;;   :init (require 'org-mime))

(org-babel-do-load-languages
 'org-babel-load-languages '(
                             (C . t)
                             (shell . t)
                             (ruby . t)
                             (js . t)
                             (python . t)))

(setq org-confirm-babel-evaluate nil)

(add-hook 'org-babel-after-execute-hook 'org-redisplay-inline-images)

(setq mode-line-in-non-selected-windows nil)
;;(color-theme-hash)
;;(zenburn)
;;(vivid-chalk)

(provide 'emacs-config)
