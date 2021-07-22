(require 'package)

(setq package-archives '(
                                 ("melpa"  . "https://melpa.org/packages/")
                                 ("elpa"   . "https://elpa.gnu.org/packages/")
                                 ("org"    . "https://orgmode.org/elpa/")
                                 ("nongnu" . "https://elpa.nongnu.org/nongnu/")
                                 ))
(package-initialize)

(unless (package-installed-p 'use-package)
(package-refresh-contents)
(package-install 'use-package))
(require 'use-package)

(require 'load-path-config-new)

(show-paren-mode 1)
(recentf-mode 1)
(setq recentf-max-menu-items 25)
(setq recentf-max-saved-items 25)
(setq undo-limit 8000000)
(setq undo-strong-limit 12000000)
(setq undo-outer-limit 12000000)
(setq read-process-output-max (* 2048 2048))
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
(setq tramp-auto-save-directory "~/tmp")
(setq backup-directory-alist
      '((".*" . "~/tmp/")))
(setq message-log-max 1000)
(set-face-attribute 'default nil :family "JetBrainsMono Nerd Font" :height 120 :weight 'normal)
(setq help-at-pt-display-when-idle t)
(setq help-at-pt-timer-delay 0.1)
(help-at-pt-set-timer)
(setq show-paren-style 'mixed)
(setq mode-line-in-non-selected-windows nil)
(fset 'yes-or-no-p 'y-or-n-p)
(setq browse-url-browser-function 'eww-browse-url)
(add-hook 'eww-after-render-hook 'eww-readable)
(add-hook 'eww-after-render-hook 'visual-line-mode)
(setq native-comp-speed 2)
(setq package-native-compile t)
(require 'xwidget)
   ;;; follow links in xwidgets
(use-package xwwp-follow-link
 :custom
 (xwwp-follow-link-completion-backend 'ivy)
  :bind (:map xwidget-webkit-mode-map
              ("v" . xwwp-follow-link)))
(use-package string-inflection
  :ensure t)

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
   ("\C-s" . 'swiper-isearch)
   ("C-x C-f" . 'counsel-find-file)
   ("C-c j" . 'counsel-git-grep)
   ("C-c k" . 'counsel-ag)
   ("C-x L" . 'counsel-locate)
   ("M-x" . 'counsel-M-x))
  :config
  (setq swiper-use-visual-line nil)
  (setq swiper-use-visual-line-p (lambda (a) nil)))
(use-package ivy-rich
  :init
  (ivy-rich-mode 1)
  :config
  (setq ivy-format-function #'ivy-format-function-line))
;; (use-package ivy-posframe
;;   :ensure t
;;   :after ivy
;;   :init
;;   (setq ivy-posframe-hide-minibuffer t)
;;   (setq ivy-posframe-min-width nil)
;;   (setq ivy-posframe-width nil)
;;   (setq ivy-posframe-border-width 2)
;;   (setq ivy-posframe-parameters
;;         '((left-fringe . 8)
;;           (right-fringe .8)))
;;   (ivy-posframe-mode t)
;;   )
(use-package all-the-icons-ivy-rich
  :defer 2
  :ensure t
  :init(all-the-icons-ivy-rich-mode 1))
(use-package all-the-icons-ivy
  :defer 2
  :init (add-hook 'after-init-hook 'all-the-icons-ivy-setup))
(use-package marginalia
  :defer 2
  :ensure t
  :init
  (marginalia-mode)
  :bind
  (:map minibuffer-local-map
        ("M-A" . marginalia-cycle)))

(global-set-key "\C-cy" 'counsel-yank-pop)

(use-package no-littering
  :ensure t)

(setq auto-save-file-name-transforms
 `((".*" ,(no-littering-expand-var-file-name "auto-save/") t)))

(use-package pos-tip
  :defer 2
  :ensure t)

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

(use-package nvm
  :defer 2
  :ensure t)
(use-package js-comint
  :ensure t
  :defer 2
  :config
  (require 'nvm)
  (js-do-use-nvm))

(use-package js2-mode
  :ensure t
  :defer 2
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

(use-package marginalia
  :defer 2
  :ensure t
  :init
  (marginalia-mode)
  :bind
  (:map minibuffer-local-map
        ("M-A" . marginalia-cycle))
  :custom
 (marginalia-annotators '(marginalia-annotators-heavy marginalia-annotators-light nil)))

(use-package ace-window
  :ensure t
  :config
  (set-face-attribute 'aw-leading-char-face nil :height 3.0 :foreground "dodgerblue")
  (ace-window-display-mode)
  (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))
  :bind
  ("M-o" . 'ace-window))

(use-package magit
  :defer 2
  :ensure t)
(require 'magit)
(use-package git-gutter-fringe+
     :defer 2
     :after magit
  :ensure t
  :diminish
  :init
  (global-git-gutter+-mode))

(use-package git-timemachine
     :defer 2
  :ensure t
  :diminish
  )

(use-package persistent-scratch
      :ensure t
      :config
      (persistent-scratch-setup-default))

(use-package treemacs-projectile
  :after treemacs projectile
  :ensure t)
(use-package treemacs-magit
  :after treemacs magit
  :ensure t)
(use-package treemacs
  :ensure t
  :config
  (setq treemacs-space-between-root-nodes nil)
  (treemacs-follow-mode t)
  (treemacs-filewatch-mode t)
  (treemacs-fringe-indicator-mode t)
  (doom-themes-treemacs-config)
  (setq doom-themes-treemacs-theme "doom-colors")
  (global-set-key (kbd "M-0") 'treemacs-select-window))

(require 'doom-themes)
(setq doom-themes-enable-bold t)
(setq doom-themes-enable-italic t)
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes")

;; (load-theme 'tron-legacy t)
;; (load-theme 'doom-zenburn t)
;; (load-theme 'doom-dark+ t)
;; (powerline-default-theme)

(use-package spaceline
     :defer 2
     :ensure t)
   (use-package spaceline-all-the-icons
     :defer 2
     :ensure t
     :after spaceline
     :config
     (setq spaceline-all-the-icons-separator-type 'arrow)
     (spaceline-all-the-icons-theme)
     )
   (require 'spaceline-config)
;; (use-package doom-modeline
;;   :ensure t
;;   :init (doom-modeline-mode 1))
  (setq starttls-use-gnutls t)
  (require 'gnutls)

(use-package ligature
       :load-path "~/dev/git/ligature.el"
       :config
       ;; Enable the "www" ligature in every possible major mode
       (ligature-set-ligatures 't '("www"))
       ;; Enable traditional ligature support in eww-mode, if the
       ;; `variable-pitch' face supports it
;;       (ligature-set-ligatures 'eww-mode '("ff" "fi" "ffi"))
       ;; Enable all Cascadia Code ligatures in programming modes
       (ligature-set-ligatures 'prog-mode '("|||>" "<|||" "<==>" "<!--" "####" "~~>" "***" "||=" "||>"
                                            ":::" "::=" "=:=" "===" "==>" "=!=" "=>>" "=<<" "=/=" "!=="
                                            "!!." ">=>" ">>=" ">>>" ">>-" ">->" "->>" "-->" "---" "-<<"
                                            "<~~" "<~>" "<*>" "<||" "<|>" "<$>" "<==" "<=>" "<=<" "<->"
                                            "<--" "<-<" "<<=" "<<-" "<<<" "<+>" "</>" "###" "#_(" "..<"
                                            "..." "+++" "/==" "///" "_|_" "www" "&&" "^=" "~~" "~@" "~="
                                            "~>" "~-" "**" "*>" "*/" "||" "|}" "|]" "|=" "|>" "|-" "{|"
                                            "[|" "]#" "::" ":=" ":>" ":<" "$>" "==" "=>" "!=" "!!" ">:"
                                            ">=" ">>" ">-" "-~" "-|" "->" "--" "-<" "<~" "<*" "<|" "<:"
                                            "<$" "<=" "<>" "<-" "<<" "<+" "</" "#{" "#[" "#:" "#=" "#!"
                                            "##" "#(" "#?" "#_" "%%" ".=" ".-" ".." ".?" "+>" "++" "?:"
                                            "?=" "?." "??" ";;" "/*" "/=" "/>" "//" "__" "~~" "(*" "*)"
                                            "\\\\" "://"))
       ;; Enables ligature checks globally in all buffers. You can also do it
       ;; per mode with `ligature-mode'.
       (global-ligature-mode t))

(use-package flycheck-pos-tip
  :defer 2
  :after flycheck
  :config
  (flycheck-pos-tip-mode)
  )
(use-package flycheck
  :defer 2
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
       :ensure org-plus-contrib
       :ensure t
       :diminish  ""
       :config
       (setq org-default-notes-file "~/Documents/notes/notes.org")
       (require 'org-capture)
       (setq org-capture-templates
             '(("t" "Todo" entry (file+headline "~/Documents/notes/todo.org" "Tasks")
                "* TODO %?\n  %i\n  %a")
               ("j" "Journal" entry (file+datetree "~/Documents/notes/notes.org")
                "* %?\nEntered on %U\n  %i\n  %a")
               ("w" "Tweet" entry (file+datetree "~/Documents/notes/tweets.org")
                "* %?\nEntered on %U\n  %i\n  %a")))
       (require 'org-habit)
       (setq org-habit-show-all-today t)
       (setq org-habit-show-habits t)
       (setq org-startup-indented t)
       (setq org-variable-pitch-mode 1)
       (visual-line-mode 1)
       (org-indent-mode)
       (require 'ox-gfm)
       (require 'ox-md)
       (require 'ox-confluence)
       (require 'ox-jira)
       )


     ;; This is needed as of Org 9.2
     (require 'org-tempo)

     (add-to-list 'org-structure-template-alist '("sh" . "src shell"))
     (add-to-list 'org-structure-template-alist '("el" . "src elisp"))
     (add-to-list 'org-structure-template-alist '("py" . "src python"))
     (add-to-list 'org-structure-template-alist '("ru" . "src ruby"))
     (add-to-list 'org-structure-template-alist '("sc" . "src scheme"))

     ;; Automatically tangle our Emacs.org config file when we save it
     (defun efs/org-babel-tangle-config ()
       (when (string-equal (buffer-file-name)
                           (expand-file-name "~/emacs/config/emacs-config.org"))
         ;; Dynamic scoping to the rescue
         (let ((org-confirm-babel-evaluate nil))
           (org-babel-tangle))))

     (add-hook 'org-mode-hook (lambda () (add-hook 'after-save-hook #'efs/org-babel-tangle-config)))

(defun ek/babel-ansi ()
  (when-let ((beg (org-babel-where-is-src-block-result nil nil)))
    (save-excursion
      (goto-char beg)
      (when (looking-at org-babel-result-regexp)
        (let ((end (org-babel-result-end))
              (ansi-color-context-region nil))
          (ansi-color-apply-on-region beg end))))))
(add-hook 'org-babel-after-execute-hook 'ek/babel-ansi)

     (fset 'capture-tweet
           (kmacro-lambda-form [?U ?\C-  ?j ?\M-x ?o ?r ?g ?- ?c ?a ?p ?t ?u ?r ?e return ?w ?\C-y] 0 "%d"))
     (use-package ox-twbs
       :ensure t)
     (use-package ox-gfm
       :ensure t)

     (use-package ox-jira
       :ensure t)
     (require 'org-tempo)
     (use-package org-mime
       :ensure t)
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
           '(("Tasks" ?t "* TODO %?\n %i\n %a" "~/Documents/notes/todo.org")
             ("Appointments" ?a "* Appointment: %?\n%^T\n%i\n %a" "~/Documents/notes/todo.org")))
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
        (cypher . t)
        (sql . t)
        (scheme . t)
        (java . t)
        (dot . t)))
     (setq org-confirm-babel-evaluate nil)

     (use-package geiser
       :defer 2
       :ensure t
       :config
       (setq geiser-active-implementations '(mit))
       (setq geiser-default-implementation 'mit)
       (setq scheme-program-name "scheme")
       (setq geiser-mit-binary "/usr/local/bin/scheme")
     )

     (use-package ox-pandoc
       :defer 2
       :ensure t
       :config
       (setq org-pandoc-options '((standalone . t))))

     (use-package org-variable-pitch
       :defer 2
       :after org
       :ensure t
       :config
       (add-hook 'org-mode-hook 'org-variable-pitch-minor-mode)
       (add-hook 'after-init-hook #'org-variable-pitch-setup))

     (use-package olivetti
       :after org
       :ensure t
       :config
       (setq olivetti-minimum-body-width 120))

     (use-package virtualenvwrapper
       :defer 2
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

     (use-package zenburn-theme
       :defer 2
       :after ace-window
       :ensure t
       :init
       (setq zenburn-override-colors-alist '(
                                             ("zenburn-bg" . "gray16")
                                             ("zenburn-bg-1" . "#5F7F5F")))

       :config
       (setq zenburn-use-variable-pitch t)
       (setq zenburn-scale-org-headlines t)
       (setq zenburn-scale-outline-headlines t))

     ;; (use-package vscode-dark-plus-theme
     ;;   :ensure t
     ;;   :after ace-window
     ;;   :init
     ;;   (load-theme 'vscode-dark-plus t))

     (use-package modus-themes
       :ensure t
       :after ace-window
       :init
       (setq modus-themes-org-blocks 'gray-background)
       (modus-themes-load-themes)
       :config
       (modus-themes-load-operandi))

(use-package exec-path-from-shell
  :ensure t
  :config
  (setq exec-path-from-shell-check-startup-files t)
  (setq exec-path-from-shell-variables `("PATH" "ARTIFACTORY_PASSWORD" "ARTIFACTORY_USER"))
  (setq exec-path-from-shell-arguments '("-l" "-i"))
         (when (memq window-system '(mac ns x))
    (exec-path-from-shell-initialize)))

(use-package inf-ruby
  :defer 2
  :ensure t)
(require 'ruby-mode)
(use-package  ruby-electric
  :ensure t)
(use-package coffee-mode
  :defer 2
  :ensure t)
(use-package feature-mode
  :defer 2
  :ensure t
  :config
  (setq feature-use-docker-compose nil)
  (setq feature-rake-command "cucumber --format progress {OPTIONS} {feature}"))
;;     (require 'rcodetools)
(use-package yasnippet
  :defer 2
  :ensure t
  :config
  (yas-global-mode t)
  (yas-global-mode))
(use-package yasnippet-snippets
  :defer 2
  :ensure t)
(use-package tree-mode
  :defer 2
  :ensure t)
(use-package rake
  :defer 2
  :ensure t)
(use-package inflections
  :defer 2
  :ensure t)
(use-package graphql
  :defer 2
  :ensure t)
(require 'org-protocol)
(use-package haml-mode
  :defer 2
  :ensure t)
(use-package beacon
  :defer 2
  :ensure t
  :init
  (beacon-mode))
(use-package rainbow-mode
  :defer 2
  :ensure t)
(use-package rainbow-delimiters
  :ensure t
  :config
  (add-hook 'prog-mode-hook #'rainbow-delimiters-mode))
(require 'ruby-config-new)
(require 'keys-config-new)
(require 'ari-custom-new)
(require 'erc-config)
(require 'gnus-config)
(require 'mail-config)
(require 'gnus-config)

(use-package highline
        :ensure t
     :defer 2
     :config
       (global-highline-mode t))
     ;;   (setq highline-face '((:background "gray40")))
     ;;   (set-face-attribute 'region nil :background "DarkOliveGreen")
     ;;   (setq highline-vertical-face (quote ((t (:background "lemonChiffon2"))))))
     ;; (set-face-attribute 'show-paren-match nil :foreground "CadetBlue")


(column-number-mode)
(global-display-line-numbers-mode t)

;; Disable line numbers for some modes
(dolist (mode '(org-mode-hook
                erc-mode-hook
                term-mode-hook
                eshell-mode-hook
                vterm-mode-hook
                treemacs-mode-hook
                gnus-mode-hook
                mu4e-view-mode-hook
                gnus-article-mode-hook
                dashboard-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

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
  :ensure t
  :config
  :after company
  :init
  (company-quickhelp-mode))
(use-package terraform-mode
  :defer 2
  :ensure t)
(use-package lsp-mode
  :commands (lsp lsp-deferred)
  :hook ((ruby-mode . lsp-deferred) (python-mode . lsp-deferred)(lsp-mode . lsp-enable-which-key-integration))
  :custom
  (lsp-auto-configure t)
  (lsp-prefer-flymake nil)
  (lsp-inhibit-message t)
  (lsp-eldoc-render-all nil)
  :config
  (lsp-enable-which-key-integration t)
  (setq lsp-enable-symbol-highlighting t)
  (setq lsp-modeline-code-actions-enable t)
  (define-key lsp-mode-map (kbd "C-c l") lsp-command-map)
  :ensure t)

(use-package lsp-ivy
  :defer 2
  :ensure t)

(use-package lsp-ui
  :defer 2
  :commands lsp-ui-mode
  :after lsp-mode
  :config
  (define-key lsp-ui-mode-map "\C-ca" 'lsp-execute-code-action)
  (define-key lsp-ui-mode-map [remap xref-find-definitions] #'lsp-ui-peek-find-definitions)
  (define-key lsp-ui-mode-map [remap xref-find-references] #'lsp-ui-peek-find-references)
  (define-key lsp-ui-mode-map (kbd "<f5>") #'lsp-ui-find-workspace-symbol)
  (setq lsp-ui-sideline-enable nil)
  (setq lsp-lens-enable t)
  )

(use-package lsp-treemacs
  :defer 2
  :after lsp
  :config
  (lsp-treemacs-sync-mode t)
  )
(require 'lsp-ui-flycheck)
(setq lsp-inhibit-message t)
(setq lsp-prefer-flymake nil)
(setq lsp-eldoc-render-all nil)

(setq lsp-auto-guess-root nil)

(define-key company-active-map (kbd "C-n") 'company-select-next-or-abort)
(define-key company-active-map (kbd "C-p") 'company-select-previous-or-abort)
(use-package company-box
  :after company
  :ensure t
  :diminish
  :hook
  (company-mode . company-box-mode)
  :custom (company-box-icons-alist 'company-box-icons-all-the-icons))

(use-package projectile
  :ensure t
  :init
  (projectile-global-mode)
  (setq projectile-switch-project-action #'projectile-dired)
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
  (setq projectile-require-project-root nil)
  (setq projectile-indexing-method 'alien)
  :custom
  ((projectile-completion-system 'ivy)))

(use-package counsel-projectile
  :ensure t
  :init
  (counsel-projectile-mode))

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

;;     (autoload 'dash-at-point "dash-at-point"
;;       "Search the word at point with Dash." t nil)

(autoload 'markdown-mode' "markdown-mode" "Major Mode for editing Markdown" t)
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))

(autoload 'ruby-mode "ruby-mode"
  "Mode for editing ruby source files" t)
(setq auto-mode-alist
      (append '(("\\.rb$" . ruby-mode)) auto-mode-alist))
(setq interpreter-mode-alist (append '(("ruby" . ruby-mode))
                                     interpreter-mode-alist))

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

(add-hook 'sql-mode-hook 'my-sql-mode-hook)
(defun my-sql-mode-hook()
  (message "SQL mode hook executed")
  (define-key sql-mode-map [f5] 'sql-send-buffer))

(setq sql-ms-program "osql")
(require 'sql)
(setq sql-mysql-program "mysql")
(setq sql-pop-to-buffer-after-send-region nil)
(setq sql-product (quote ms))
(setq sql-mysql-login-params (append sql-mysql-login-params '(port)))

(use-package rjsx-mode
   :defer 2
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

(use-package deft
  :ensure t
  :config
  (setq deft-extensions'("org" "txt" "md"))
  (setq deft-default-extension "org")
  (setq deft-recursive t)
  (setq deft-directory "~/Documents/notes")
  (setq deft-use-filename-as-title nil)
  (setq deft-use-filter-string-for-filename t)
  (setq deft-auto-save-interval 0)
  (setq deft-file-naming-rules '((noslash . "-")
                                  (nospace . "-")
                                  (case-fn . downcase)))
  (setq deft-text-mode 'org-mode)
  (global-set-key (kbd "<f8>") 'deft)
  )

(use-package cypher-mode
  :ensure t)
(setq n4js-cli-program "~/Downloads/cypher-shell/cypher-shell")
(setq n4js-cli-arguments '("-u" "neo4j"))
(setq n4js-pop-to-buffer t)
(setq n4js-font-lock-keywords cypher-font-lock-keywords)

(use-package which-key
  :ensure t
  :init
  (which-key-mode)
  :diminish which-key-mode
  :config
  (setq which-key-idle-delay 1))

(use-package helpful
  :ensure t)

(use-package prescient
  :ensure t
  :config
  (prescient-persist-mode 1))

(use-package ivy-prescient
  :ensure t
  :after counsel
  :config
  (ivy-prescient-mode 1)
  (setq  prescient-sort-length-enable nil)
  (setq ivy-re-builders-alist
 '((counsel-ag . ivy--regex)
   (t . ivy-prescient-re-builder))))

(use-package company-prescient
  :ensure t
  :after company
  :config
  (company-prescient-mode 1))

(use-package general
  :ensure t
  :config
  (general-create-definer my-leader-def
    :prefix "C-c")
  (my-leader-def
    "t" 'projectile-find-file
    "a" 'ace-jump-mode
    "g" '(:ignore t :which-key "rspec")
    "gp" '(inf-ruby-switch-from-compilation :which-key "enter debugger")
    "ga" '(rspec-verify-all :which-key "run all specs")
    "gs" '(rspec-verify-single :which-key "run single spec")
    "gr" '(rspec-rerun :which-key "rerun spec")
    "gf" '(rspec-run-last-failed :which-key "rerun last failed")
    "f" '(:ignore t :which-key "cucumber")
    "ff" '(feature-verify-all-scenarios-in-project :which-key "run all cukes")
    "fs" '(feature-verify-scenario-at-pos :whick-key "run cuke at point")
    "fv" '(feature-verify-all-scenarios-in-buffer :which-key "run all cukes in buffer")
    "fg" '(feature-goto-step-definition :which-key "goto step definition")
    "fr" '(feature-register-verify-redo :which-key "repeat last cuke")
    "m" 'mu4e
    "b" '(:ignore t :which-key "eww")
    "bf" '(eww-follow-link :which-key "eww-follow-link")))

(provide 'emacs-config-new)
