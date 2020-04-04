(if (window-system)
    (tool-bar-mode -1))
(menu-bar-mode -1)
(show-paren-mode 1)
(setq inhibit-startup-screen t)
(setq inhibit-splash-screen t)
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
      '((".*" "~/tmp/" t)))
(setq message-log-max 1000)
;;     (set-default-font "Monaco-11")
;;(set-face-attribute 'default nil :family "Monaco" :height 110 :weight 'normal)
(set-face-attribute 'default nil :family "JetBrains Mono" :height 110 :weight 'normal)
;;     (add-to-list 'default-frame-alist '(font . "Monaco-18"))
(setq help-at-pt-display-when-idle t)
(setq help-at-pt-timer-delay 0.1)
(help-at-pt-set-timer)
(setq show-paren-style 'mixed)
(setq mode-line-in-non-selected-windows nil)
(fset 'yes-or-no-p 'y-or-n-p)

(use-package popup
  :ensure t)
(use-package pos-tip
  :ensure t)
(use-package popup-kill-ring
  :ensure t)

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

;; ;;mu4e
;; (add-to-list 'load-path "/usr/local/Cellar/mu/1.0/share/emacs/site-lisp/mu/mu4e/")
;; (require 'mu4e)

;; (setq mu4e-maildir (expand-file-name "~/mail/qc/"))
;; (setq mu4e-drafts-folder "/Drafts")
;; (setq mu4e-sent-folder "/Sent")
;; (setq mu4e-trash-folder "/Trash")

;; (setq mu4e-get-mail-command "mbsync qc"
;;       mu4e-html2text-command "w3m -T text/html"
;;       mu4e-update-interval 120
;;       mu4e-headers-auto-update t
;;       mu4e-compose-signature-auto-include nil)

;; (setq mu4e-view-show-images t)

;; (when (fboundp 'imagemagick-register-types)
;;   (imagemagick-register-types))
;; (setq mu4e-mu-binary "/usr/local/bin/mu")
;; (setq mu4e-compose-reply-to-address "aturetzky@quantcast.com")

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



(unless (package-installed-p 'use-package)
(package-refresh-contents)
(package-install 'use-package))
(eval-when-compile
  (require 'use-package))

(require 'load-path-config-new)

(use-package nvm
  :ensure t)
(use-package js-comint
  :ensure t
  :config
  (require 'nvm)
  (js-do-use-nvm))

(use-package js2-mode
  :ensure t
  :bind (:map js2-mode-map
              ("\C-x\C-e" . js-send-last-sexp)
              ("\C-\M-x"  . js-send-last-sexp-and-go)
              ("\C-cb"    . js-send-buffer)
              ("\C-c\C-b" . js-send-buffer-and-go)
              ("\C-cl"    . js-load-file-and-go))
  :config
  (setq js2-strict-missing-semi-warning nil)
  (setq js2-missing-semi-one-line-override nil)
  )

(use-package swiper
  :ensure t)
(use-package counsel
  :ensure t)
(use-package ivy
  :ensure t
  :init
  (ivy-mode 1)
  (setq ivy-use-virtual-buffers t)
  (setq ivy-use-selectable-prompt t)
  (setq enable-recursive-minibuffers t)
  (define-key minibuffer-local-map (kbd "C-r") 'counsel-minibuffer-history)
  :bind
  (
   ("\C-s" . 'swiper)
   ("C-x C-f" . 'counsel-find-file)
   ("C-c j" . 'counsel-git-grep)
   ("C-c k" . 'counsel-ag)
   ("C-c l" . 'counsel-locate)
   ("M-x" . 'counsel-M-x)))
(use-package ivy-rich
  :init
  (ivy-rich-mode 1)
  :config
  (setq ivy-format-function #'ivy-format-function-line))

(use-package ace-window
  :ensure t
  :config
  (ace-window-display-mode)
  (set-face-attribute 'aw-leading-char-face nil :height 3.0)
  (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))
  :bind
  ("M-o" . 'ace-window))

(use-package magit
  :ensure t)
(require 'magit)
(use-package git-gutter-fringe+
  :ensure t
  :diminish
  :init
  (global-git-gutter+-mode))

(use-package git-timemachine
  :ensure t
  :diminish
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

(use-package treemacs-projectile
  :after treemacs projectile
  :ensure t)
(use-package treemacs-magit
  :after treemacs magit
  :ensure t)
(use-package treemacs
  :ensure t
  :defer t
  :config
  (treemacs-follow-mode t)
  (treemacs-filewatch-mode t)
  (treemacs-fringe-indicator-mode t)
  (doom-themes-treemacs-config)
  (setq doom-themes-treemacs-theme "doom-colors")
  (global-set-key (kbd "M-0") 'treemacs-select-window))

(setq sh-basic-offset 2)
(setq sh-indentation 2)
(setq smie-indent-basic 2)

;; (use-package composite
;;   :defer t
;;   :init
;;   (defvar composition-ligature-table (make-char-table nil))
;;   :hook
;;   (((prog-mode conf-mode nxml-mode markdown-mode help-mode rjsx-mode)
;;     . (lambda () (setq-local composition-function-table composition-ligature-table))))
;;   :config
;;   ;; support ligatures, some toned down to prevent hang
;;   (when (version<= "27.0" emacs-version)
;;     (let ((alist
;;            '((33 . ".\\(?:\\(==\\|[!=]\\)[!=]?\\)")
;;              (35 . ".\\(?:\\(###?\\|_(\\|[(:=?[_{]\\)[#(:=?[_{]?\\)")
;;              (36 . ".\\(?:\\(>\\)>?\\)")
;;              (37 . ".\\(?:\\(%\\)%?\\)")
;;              (38 . ".\\(?:\\(&\\)&?\\)")
;;              (42 . ".\\(?:\\(\\*\\*\\|[*>]\\)[*>]?\\)")
;;              ;; (42 . ".\\(?:\\(\\*\\*\\|[*/>]\\).?\\)")
;;              (43 . ".\\(?:\\([>]\\)>?\\)")
;;              ;; (43 . ".\\(?:\\(\\+\\+\\|[+>]\\).?\\)")
;;              ;; (45 . ".\\(?:\\(-[->]\\|<<\\|>>\\|[-<>|~]\\)[-<>|~]?\\)")
;;              ;; (46 . ".\\(?:\\(\\.[.<]\\|[-.=]\\)[-.<=]?\\)")
;;              ;; (46 . ".\\(?:\\(\\.<\\|[-=]\\)[-<=]?\\)")
;;              ;; (47 . ".\\(?:\\(//\\|==\\|[=>]\\)[/=>]?\\)")
;;              ;; (47 . ".\\(?:\\(//\\|==\\|[*/=>]\\).?\\)")
;;              (48 . ".\\(?:\\(x[a-fA-F0-9]\\).?\\)")
;;              (58 . ".\\(?:\\(::\\|[:<=>]\\)[:<=>]?\\)")
;;              (59 . ".\\(?:\\(;\\);?\\)")
;;              (60 . ".\\(?:\\(!--\\|\\$>\\|\\*>\\|\\+>\\|-[-<>|]\\|/>\\|<[-<=]\\|=[<>|]\\|==>?\\||>\\||||?\\|~[>~]\\|[$*+/:<=>|~-]\\)[$*+/:<=>|~-]?\\)")
;;              (61 . ".\\(?:\\(!=\\|/=\\|:=\\|<<\\|=[=>]\\|>>\\|[=>]\\)[=<>]?\\)")
;;              (62 . ".\\(?:\\(->\\|=>\\|>[-=>]\\|[-:=>]\\)[-:=>]?\\)")
;;              ;; t(63 . ".\\(?:\\([.:=?]\\)[.:=?]?\\)")
;;              (91 . ".\\(?:\\(|\\)[]|]?\\)")
;;              ;; (92 . ".\\(?:\\([\\n]\\)[\\]?\\)")
;;              (94 . ".\\(?:\\(=\\)=?\\)")
;;              (95 . ".\\(?:\\(|_\\|[_]\\)_?\\)")
;;              (119 . ".\\(?:\\(ww\\)w?\\)")
;;              (123 . ".\\(?:\\(|\\)[|}]?\\)")
;;              (124 . ".\\(?:\\(->\\|=>\\||[-=>]\\||||*>\\|[]=>|}-]\\).?\\)")
;;              (126 . ".\\(?:\\(~>\\|[-=>@~]\\)[-=>@~]?\\)"))))
;;       (dolist (char-regexp alist)
;;         (set-char-table-range composition-ligature-table (car char-regexp)
;;                               `([,(cdr char-regexp) 0 font-shape-gstring]))))
;;     (set-char-table-parent composition-ligature-table composition-function-table))
;;   )

(use-package flycheck-pos-tip
  :after flycheck
  :config
  (flycheck-pos-tip-mode)
  )
(use-package flycheck
  :diminish flycheck-mode
  :ensure t
  :init
  (setq flycheck-emacs-lisp-initialize-packages 1)
  (setq flycheck-emacs-lisp-load-path 'inherit)
  :config
  (flycheck-add-mode 'javascript-eslint 'rjsx-mode)
  (flycheck-add-mode 'javascript-jshint 'rjsx-mode)
  )

(server-start)

(use-package diminish
  :ensure t)

(diminish 'org-mode  "")
(diminish 'auto-revert-mode)
(diminish 'yas-minor-mode)
(diminish 'eldoc-mode)
(diminish 'org-src-mode)
(diminish 'abbrev-mode)
(diminish 'ivy-mode)
(diminish 'global-highline-mode)
(diminish 'ruby-block-mode)
(diminish 'ruby-electric-mode)
(diminish "seeing-is-believing")
(diminish 'hs-minor-mode)
(diminish 'ruby-block-mode)
(diminish 'global-highline-mode)

(use-package org
  :ensure t
  :diminish  "")
(use-package ox-twbs
  :ensure t)
(use-package ox-jira
  :ensure t)
(require 'org-tempo)
(use-package org-mime
  :ensure t)
(setq org-ellipsis " ⤵")
(setq org-src-fontify-natively t)
(setq org-src-tab-acts-natively t)
(setq org-src-window-setup 'current-window)
(use-package plantuml-mode
  :ensure t)
(use-package org-bullets
  :ensure t)
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

(setq org-todo-keywords '((sequence "TODO(t)" "STARTED(s)" "WAITING(w)" "|" "DONE(d)" "CANCELLED(c)")))
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
   (sql . t)
   (java . t)
   (dot . t)))
(setq org-confirm-babel-evaluate nil)

(use-package virtualenvwrapper
  :ensure t
  :init
  (venv-initialize-interactive-shells)
  (venv-initialize-eshell)
  (setq venv-location "~/.virtualenvs")
  )
(setq org-plantuml-jar-path "/usr/local/Cellar/plantuml/1.2018.12/libexec/plantuml.jar")
(setq plantuml-jar-path "/usr/local/Cellar/plantuml/1.2018.12/libexec/plantuml.jar")


(setq org-mime-export-options '(:section-numbers nil
                                                 :with-author nil
                                                 :with-toc nil))

(use-package ag
  :ensure t)
(require 'dired-details)
(dired-details-install)
(require 'uniquify)
(use-package boxquote
  :ensure t)
;;     (require 'tex-site)
(require 'tramp)
(use-package gist
  :ensure t)
(use-package web-mode
  :ensure t)
(require 'ls-lisp)
(use-package puppet-mode
  :ensure t)
(require 'blog)
(use-package htmlize
  :ensure t)
(require 'cl)
(require 'keys-config-new)
(use-package yaml-mode
  :ensure t)
(require 'ari-custom-new)
(use-package ruby-block
  :ensure t)
(use-package popup
  :ensure t)
(use-package popup-kill-ring
  :ensure t)
(use-package all-the-icons
  :ensure t)
(use-package all-the-icons-dired
  :ensure t)
(use-package all-the-icons-ivy
  :ensure t
  :config
  (all-the-icons-ivy-setup))
(use-package powerline
  :ensure t
  :init
  (setq powerline-image-apple-rgb t)
  (setq powerline-height 28)
  )
;; (use-package panda-theme
;;   :ensure t
;;   :config
;;   ;;(load-theme 'panda t)
;;   )
(use-package spaceline
    :ensure t
    :config
    (require 'spaceline-config)
    (spaceline-emacs-theme)
    (load-theme 'spacemacs-dark t))

;; (use-package doom-modeline
;; :ensure t
;;   :config
;;   (doom-modeline-init)
;;   :init
;;   ;;(load-theme 'doom-Iosvkem t)
;;   (doom-themes-org-config)
;;   )

;; ;; (use-package hc-zenburn-theme
;; ;;   :
;;  ensure t
;;   :init
;;   (powerline-default-theme)
;;   (load-theme 'hc-zenburn t)
;;   (hc-zenburn-with-color-variables
;;     (custom-theme-set-faces
;;      'hc-zenburn
;;      `(ac-candidate-face ((t (:background ,hc-zenburn-bg+3 :foreground ,hc-zenburn-green+4))))
;;      `(ac-selection-face ((t (:background ,hc-zenburn-cyan  :foreground ,hc-zenburn-blue-4))))
;;      `(popup-isearch-match ((t (:background ,hc-zenburn-cyan :foreground ,"Blue"))))))
;;   )
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
;; (use-package auto-complete
;;   :diminish "  "
;;   :ensure t
;;   :init
;;   (setq ac-use-menu-map t)
;;   (setq ac-use-fuzzy t))
;; (require 'auto-complete-config)
;; (ac-config-default)
;; (require 'auto-complete-yasnippet)
(use-package haml-mode
  :ensure t)
;; (use-package rvm
;;   :ensure t
;;   :hook
;;   (ruby-mode . rvm-activate-corresponding-ruby))
;; (rvm-use-default)
(use-package beacon
  :ensure t
  :init
  (beacon-mode))
(use-package rainbow-mode
  :ensure t)
(use-package rainbow-delimiters
  :ensure t
  :config
  (add-hook 'prog-mode-hook #'rainbow-delimiters-mode))
(require 'ruby-config-new)

(use-package highline
  :ensure t
  :config
  (global-highline-mode t)
  (setq highline-face '((:background "gray32")))
  (set-face-attribute 'region nil :background "DarkOliveGreen")
  (setq highline-vertical-face (quote ((t (:background "lemonChiffon2"))))))
(set-face-attribute 'show-paren-match nil :foreground "CadetBlue")

(use-package hlinum
  :ensure t)
(use-package linum-relative
  :ensure t)

  (global-linum-mode)
  (hlinum-activate)

(use-package company
            :ensure t
            :defer 2
            :diminish
            :custom
            (company-minimum-prefix-length 1)
            (company-idle-begin 0.0)
            (company-show-numbers t)
            (company-tooltip-align-annotations 't)
            (global-company-mode t))

          (require 'company)
          (add-hook  'after-init-hook 'global-company-mode)
          (use-package company-quickhelp
            :config
            :init
            (company-quickhelp-mode))
          (use-package lsp-mode
            :commands lsp
            :hook ((ruby-mode . lsp))
            :custom          (lsp-auto-configure t)
                              (lsp-prefer-flymake nil)
                              (lsp-inhibit-message t)
                              (lsp-eldoc-render-all nil)

            :ensure t)
(use-package lsp-ui
  :commands lsp-ui-mode
  :after lsp-mode
  :config
  (define-key lsp-ui-mode-map "\C-ca" 'lsp-execute-code-action)
  (define-key lsp-ui-mode-map [remap xref-find-definitions] #'lsp-ui-peek-find-definitions)
  (define-key lsp-ui-mode-map [remap xref-find-references] #'lsp-ui-peek-find-references)
  (define-key lsp-ui-mode-map (kbd "<f5>") #'lsp-ui-find-workspace-symbol)
  )

(use-package lsp-treemacs
  :after lsp-mode
  :config
  (lsp-treemacs-sync-mode t)
  )

          ;;(require 'lsp)
          ;;(require 'lsp-mode)
          (require 'lsp-clients)
          (require 'lsp-ui-flycheck)
          (require 'lsp-solargraph)
          ;;     (require 'lsp-mode)
          (setq lsp-inhibit-message t)
          (setq lsp-prefer-flymake nil)
          (use-package company-lsp
            :commands company-lsp
            :ensure t)
          (require 'company-lsp)
          (push 'company-lsp company-backends)
                                                  ;  ; (setq lsp-eldoc-render-all nil)
          (setq lsp-eldoc-render-all nil)

          ;;      (setq lsp-highlight-symbol-at-point t)
          ;; (setq  lsp-java--workspace-folders (list "/Users/aturetzky/dev/git/permission-center/api"))
          ;; (setq lsp-java-format-settings-profile "Quantcast")
          ;; (setq lsp-java-format-settings-url "~/Users/aturetzky/eclipse-java-google-style.xml")
          ;; (require 'lsp-java)
          ;; (add-hook 'java-mode-hook #'lsp-java-enable)
          ;; (add-hook 'java-mode-hook 'flycheck-mode)
          ;; (add-hook 'java-mode-hook 'company-mode)
          ;; (add-hook 'java-mode-hook (lambda ()(lsp-ui-flycheck-enable t)))
          ;; (add-hook 'java-mode-hook 'lsp-ui-mode)
          ;; (add-hook 'java-mode-hook 'lsp-ui-sideline-mode)
;;          (require 'lsp-ui)
;;          (require 'lsp-ui-flycheck)
;;          (setq lsp-prefer-flymake nil)
          ;;     (setq lsp-ui-doc-enable-eldoc nil)
          ;; (setq lsp-ui-sideline-enable t)
          ;;       (setq lsp-ui-sideline-show-symbol nil)
          ;;       (setq lsp-ui-sideline-show-hover nil)
          ;;       (setq lsp-ui-sideline-show-code-actions t)
          ;;       (setq lsp-ui-sideline-update-mode 'point)
          ;;      (setq lsp-ui-flycheck-live-reporting t)
          ;;      (setq lsp-ui-flycheck-enable t)
          ;;      (setq lsp-ui-sideline-enable nil)
          ;;      (lsp-ui-sideline-mode t)

          ;; (setq lsp-java-import-maven-enabled nil);
          ;; (setq lsp-java-import-gradle-enabled t)
          ;; (setq lsp-java-progress-report t)
          ;; (setq lsp-java-auto-build t)
          ;;      (setq lsp-ui-doc-mode nil)
          ;;      (setq lsp-ui-doc-enable t)


          (setq lsp-message-project-root-warning t)
          (setq lsp-auto-guess-root t)

          (define-key company-active-map (kbd "C-n") 'company-select-next-or-abort)
          (define-key company-active-map (kbd "C-p") 'company-select-previous-or-abort)
          ;; (use-package company-box
          ;;   :after company
          ;;   :ensure t
          ;;   :diminish
          ;;   :hook
          ;;   (company-mode . company-box-mode)
          ;;   :custom (company-box-icons-alist 'company-box-icons-all-the-icons))

;;     (require 'eclim)
;;     (require 'eclimd)
;;     (use-package ac-emacs-eclim
;;       :ensure t)
;;     (require 'ac-emacs-eclim)
;;     (ac-emacs-eclim-java-setup)
;;     (setq eclim-executable "~/eclipse/java-oxygen-tar/Eclipse.app/Contents/Eclipse/eclim")
;;     (setq eclimd-executable "~/eclipse/java-oxygen-tar/Eclipse.app/Contents/Eclipse/eclimd")

(use-package projectile
  :ensure t
  :init
  (projectile-global-mode)
  (setq projectile-switch-project-action #'projectile-dired)
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
  (setq projectile-require-project-root nil)
  (setq projectile-indexing-method 'alien))

(use-package counsel-projectile
  :ensure t
  :init
  (counsel-projectile-mode))

;;    (global-auto-complete-mode t)           ;enable global-mode
;;    (setq ac-auto-start t)                  ;automatically start
;;    (setq ac-dwim 3)                        ;Do what i mean
;;    (setq ac-override-local-map nil)        ;don't override local map
;;    (define-key ac-complete-mode-map "\t" 'ac-expand)
;;    (define-key ac-complete-mode-map "\r" 'ac-complete)
;;    (define-key ac-complete-mode-map "\M-n" 'ac-next)
;;    (define-key ac-complete-mode-map "\M-p" 'ac-previous)
;;    (set-default 'ac-sources '(ac-source-words-in-buffer ac-source-yasnippet ac-source-abbrev ac-source-words-in-buffer ac-source-dictionary ac-source-files-in-current-dir))

;;    (setq ac-modes
;;          (append ac-modes
;;                  '(eshell-mode
;;                                            ;org-mode
;;                    )))
;;                                            ;(add-to-list 'ac-trigger-commands 'org-self-insert-command)

;;    (add-hook 'emacs-lisp-mode-hook
;;              (lambda ()
;;                (setq ac-sources '(ac-source-yasnippet ac-source-abbrev ac-source-words-in-buffer ac-source-symbols))))

;;    (add-hook 'eshell-mode-hook
;;              (lambda ()
;;                (setq ac-sources '(ac-source-yasnippet ac-source-abbrev ac-source-files-in-current-dir ac-source-words-in-buffer))))
;;    (add-hook 'web-mode-hook
;;              (lambda ()
;;                (setq ac-sources '(ac-source-yasnippet ac-source-abbrev ac-source-files-in-current-dir ac-source-words-in-buffer))))
;;    (add-hook 'yaml-mode-hook
;;              (lambda ()
;;                (setq ac-sources '(ac-source-yasnippet ac-source-abbrev ac-source-semantic ac-source-files-in-current-dir ac-source-words-in-buffer ac-source-words-in-same-mode-buffers ))))
;;    (add-hook 'js2-mode-hook
;;              (lambda ()
;;                (add-to-list 'ac-sources '(ac-source-files-in-current-dir ac-source-symbols ac-source-abbrev ac-source-yasnippet ac-source-words-in-same-mode-buffers ac-source-variables)(auto-complete-mode))))
;;    (setq ac-modes
;;          (append ac-modes
;;                  '(sql-mode
;;                    sqlplus-mode
;;                    js2-mode
;;                    coffee-mode
;;                    JavaSript-IDE-mode
;;                    text-mode
;;                    css-mode
;;                    web-mode
;;                    perl-mode
;;                    ruby-mode
;;                    scala-mode
;; ;;                   java-mode
;;                    yaml-mode
;;                    )))

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
;; (setq ri-ruby-script (expand-file-name "~/emacs/site/lisp/ri-emacs.rb"))
;; (autoload 'ri (expand-file-name "~/emacs/site/lisp/ri-ruby.el") nil t)
;; (load  (expand-file-name "~/emacs/site/lisp/ri-ruby.el"))
;; (setq ri-ruby-script (expand-file-name "~/emacs/site/lisp/ri-emacs.rb"))
;; (autoload 'ri (expand-file-name "~/emacs/site/lisp/ri-ruby.el") nil t)
;; (load  (expand-file-name "~/emacs/site/lisp/ri-ruby.el"))

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
(require 'sql)
(setq sql-mysql-program "mysql")
(setq sql-pop-to-buffer-after-send-region nil)
(setq sql-product (quote ms))
(setq sql-mysql-login-params (append sql-mysql-login-params '(port)))

(exec-path-from-shell-initialize)
(use-package rjsx-mode
  :ensure t)
(add-hook 'js2-mode-hook 'lsp)
(add-hook 'rjsx-mode-hook 'lsp)
(add-hook 'rjsx-mode-hook 'emmet-mode)

(use-package prettier-js
  :config
  (add-hook 'js2-mode-hook 'prettier-js-mode)
  (add-hook 'rjsx-mode-hook 'prettier-js-mode)
  )

(setq emmet-expand-jsx-className? t)

;;(require 'semantic-ia)
;;(if window-system
;;    (progn
;;      (setq semantic-load-turn-everything-on t)
;;      (semantic-load-enable-gaudy-code-helpers)))

(use-package cypher-mode
  :ensure t)
(setq n4js-cli-program "~/Downloads/cypher-shell/cypher-shell")
(setq n4js-cli-arguments '("-u" "neo4j"))
(setq n4js-pop-to-buffer t)
(setq n4js-font-lock-keywords cypher-font-lock-keywords)

(provide 'emacs-config-new)
