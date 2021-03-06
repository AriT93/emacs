(if (window-system)
         (tool-bar-mode -1))
     (menu-bar-mode -1)
     (show-paren-mode 1)
     (setq gc-cons-threshold 100000000)
     (setq undo-limit 8000000)
     (setq undo-strong-limit 12000000)
     (setq undo-outer-limit 12000000)
     (setq read-process-output-max (* 1024 1024))
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
     (set-face-attribute 'default nil :family "JetBrainsMono Nerd Font" :height 120 :weight 'normal)
     (setq help-at-pt-display-when-idle t)
     (setq help-at-pt-timer-delay 0.1)
     (help-at-pt-set-timer)
     (setq show-paren-style 'mixed)
     (setq mode-line-in-non-selected-windows nil)
     (fset 'yes-or-no-p 'y-or-n-p)
;;     (add-hook 'eww-after-render-hook 'eww-readable)
     (setq comp-speed 3)
     (setq package-native-compile t)

(use-package pos-tip
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

(require 'package)

(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/"))
(add-to-list 'package-archives
             '("elpa" . "https://elpa.gnu.org/packages/"))
(add-to-list 'package-archives
             '("org" . "https://orgmode.org/elpa/"))
     (add-to-list 'package-archives
             '("nongnu" . "https://elpa.nongnu.org/nongnu/"))



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
  :ensure t
  :init(all-the-icons-ivy-rich-mode 1))
(use-package all-the-icons-ivy
  :init (add-hook 'after-init-hook 'all-the-icons-ivy-setup))
(use-package quelpa
  :ensure t)
(use-package quelpa-use-package
  :ensure t)
(use-package consult :quelpa (consult :fetcher github :repo "minad/consult")
  :after projectile
  :ensure t
  :init
  (setq register-preview-delay 0
        register-preview-function #'consult-register-format)
  :config
  (setq consult-project-root-function #'projectile-project-root)
  (setq consult-narrow-key "<")
  )
(global-set-key "\C-cy" 'counsel-yank-pop)
(use-package marginalia
  :ensure t
  :init
  (marginalia-mode)
  :bind
  (:map minibuffer-local-map
        ("M-A" . marginalia-cycle)))

(use-package ace-window
  :ensure t
  :after (zenburn-theme)
  :config
  (set-face-attribute 'aw-leading-char-face nil :height 3.0
  :foreground "dodgerblue")
  (ace-window-display-mode)
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
     :ensure t)
   (use-package spaceline-all-the-icons
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
       :load-path "/Users/ari.turetzky/dev/git/ligature.el"
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
                                            "\\" "://"))
       ;; Enables ligature checks globally in all buffers. You can also do it
       ;; per mode with `ligature-mode'.
       (global-ligature-mode t))

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
       :ensure t
       :config
       (setq geiser-active-implementations '(mit))
       (setq geiser-default-implementation 'mit)
       (setq scheme-program-name "scheme")
       (setq geiser-mit-binary "/usr/local/bin/scheme")
     )

     (use-package ox-pandoc
       :ensure t
       :config
       (setq org-pandoc-options '((standalone . t))))

     (use-package org-variable-pitch
       :ensure t
       :config
       (add-hook 'org-mode-hook 'org-variable-pitch-minor-mode)
       (add-hook 'after-init-hook #'org-variable-pitch-setup))

     (use-package olivetti
       :ensure t
       :config
       (setq olivetti-minimum-body-width 120)
       (add-hook 'org-mode-hook 'olivetti-mode))
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

     (use-package zenburn-theme
       :ensure t
       :init
       (setq zenburn-override-colors-alist '(
                                             ("zenburn-bg" . "gray16")
                                             ("zenburn-bg-1" . "#5F7F5F")))
     ;;  (load-theme 'zenburn t)
       :config
       (setq zenburn-use-variable-pitch t)
       (setq zenburn-scale-org-headlines t)
       (setq zenburn-scale-outline-headlines t)
       (set-face-attribute 'aw-leading-char-face nil :height 3.0 :foreground "dodgerblue")
       (set-face-attribute 'ivy-current-match nil :height 1.1 :foreground "wheat" :background "#5f7f5f" :underline nil))

     (use-package vscode-dark-plus-theme
       :ensure t
       :init
       (load-theme 'vscode-dark-plus t)
       :config
       (set-face-attribute 'aw-leading-char-face nil :height 3.0 :foreground "dodgerblue")
       (set-face-attribute 'ivy-current-match nil :height 1.1 :foreground "wheat" :background "#5f7f5f" :underline nil)
       )
     ;;( use-package hc-zenburn-theme
     ;;  :ensure t
     ;; :init
     ;; (powerline-default-theme)
     ;; (load-theme 'hc-zenburn t)
     ;; (hc-zenburn-with-color-variables
     ;;   (custom-theme-set-faces
     ;;    'hc-zenburn
     ;;    `(company-tooltip-common ((t (:background ,hc-zenburn-bg+3 :foreground ,hc-zenburn-green+4))))
     ;;    `(company-tooltip-selection ((t (:background ,"gray40" :foreground ,"LightBlue3"))))
     ;;    `(popup-isearch-match ((t (:background ,hc-zenburn-cyan :foreground ,"Blue"))))))
     ;; )

(use-package exec-path-from-shell
       :ensure t
       :config
       (when (memq window-system '(mac ns x))
         (exec-path-from-shell-initialize))
       (setq exec-path-from-shell-check-startup-files t)
       (setq exec-path-from-shell-variables `("PATH" "ARTIFACTORY_PASSWORD" "ARTIFACTORY_USER")
       ))
     (use-package inf-ruby
       :ensure t)
     (require 'ruby-mode)
     (use-package  ruby-electric
       :ensure t)
     (use-package coffee-mode
       :ensure t)
     (use-package feature-mode
       :ensure t
       :config
       (setq feature-use-docker-compose nil)
       (setq feature-rake-command "cucumber --format progress {feature}"))
;;     (require 'rcodetools)
     (use-package yasnippet
       :ensure t
       :config
       (yas-global-mode t)
       (yas-global-mode))
     (use-package yasnippet-snippets
       :ensure t)
     (use-package tree-mode
       :ensure t)
     (use-package rake
       :ensure t)
     (use-package inflections
       :ensure t)
     (use-package graphql
       :ensure t)
     (require 'org-protocol)
     (use-package haml-mode
       :ensure t)
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
     (require 'keys-config-new)
     (require 'erc-config)
     (require 'mail-config)

(use-package highline
       :ensure t
       :config
       (global-highline-mode t)
       (setq highline-face '((:background "gray40")))
       (set-face-attribute 'region nil :background "DarkOliveGreen")
       (setq highline-vertical-face (quote ((t (:background "lemonChiffon2"))))))
     (set-face-attribute 'show-paren-match nil :foreground "CadetBlue")

     (use-package hlinum
       :ensure t)
     (use-package linum-relative
       :ensure t)

       (hlinum-activate)


(column-number-mode)
(global-display-line-numbers-mode t)

;; Disable line numbers for some modes
(dolist (mode '(org-mode-hook
                erc-mode-hook
                term-mode-hook
                eshell-mode-hook
                vterm-mode-hook
                neotree-mode-hook
                telega-chat-mode-hook
                telega-root-mode-hook
                telega-webpage-mode-hook
                treemacs-mode-hook
                dashboard-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

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

(use-package elfeed
                            :ensure t)
                          (use-package elfeed-org
                            :ensure t
                            :after elfeed
                            :init
                            (elfeed-org))
                          ;; (use-package elfeed-goodies
                          ;;   :after elfeed
                          ;;   :ensure t
                          ;;   :init
                          ;;   (elfeed-goodies/setup))

                       (use-package visual-fill
                         :ensure t)
                       (use-package visual-fill-column
                         :ensure t)
                       (add-hook 'elfeed-show-mode-hook (lambda()
                                                          (set-face-attribute 'variable-pitch (selected-frame) :font (font-spec :family "Helvetica" :size 14))
                                                          (setq fill-column 100)
                                                          (visual-fill-mode t)
                                                          (adaptive-wrap-prefix-mode t)
                                                          (toggle-word-wrap)
                                                          (visual-fill-column-mode)))


   (use-package twittering-mode
     :ensure t
     :config
     (defface my-twit-face
       '((t :family "Helvetica"
            :height 160
            ))
       "face for twitter")
(defalias 'epa--decode-coding-string 'decode-coding-string)
     (setq twittering-use-master-password t)
     (setq twittering-icon-mode t)
     (setq twittering-use-icon-storage t)

     (setq twittering-status-format "%RT{%FACE[my-twit-face]{RT}}%i %S (%s),  %@:
   %FOLD[  ]{%FACE[my-twit-face]{%FILL[ ]{%T}} %QT{
   +----
   %FOLD[|]{%i %S (%s),  %@:
   %FOLD[  ]{%FILL[]{%FACE[my-twit-face]{%T}} }}
   +----}}
   "))

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
