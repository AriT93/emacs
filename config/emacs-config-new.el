(require 'package)

(setq package-archives '(
                         ("melpa"  . "https://melpa.org/packages/")
                         ("elpa"   . "https://elpa.gnu.org/packages/")
                         ("nongnu" . "https://elpa.nongnu.org/nongnu/")
                         ("melpa-stable" . "https://stable.melpa.org/packages/")
                         ))
(package-initialize)

(setq treesit-extra-load-path nil)
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(require 'use-package)

(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name
        "straight/repos/straight.el/bootstrap.el"
        (or (bound-and-true-p straight-base-dir)
            user-emacs-directory)))
      (bootstrap-version 7))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(require 'load-path-config-new)

(set-face-attribute 'default nil
                    :inherit nil
                    :height 160
                    :weight 'Regular
                    :foundry "microsoft"
                    :family "Cascadia Code")
(set-face-attribute 'region nil :background "DarkOliveGreen")
(set-face-attribute 'highlight nil :background "DarkSeaGreen4")
(set-face-attribute 'fringe nil :background "#070018")
(set-face-attribute 'header-line nil :box '(:line-width 4 :color "#1d1a26" :style nil))
(set-face-attribute 'header-line-highlight nil :box '(:color "#d0d0d0"))
(set-face-attribute 'line-number-current-line nil :foreground "PaleGreen2" :italic t)
(set-face-attribute 'tab-bar-tab nil :box '(:line-width 4 :color "#070019" :style nil))
(set-face-attribute 'tab-bar-tab-inactive nil :box '(:line-width 4 :color "#4a4759" :style nil))
(set-face-attribute 'variable-pitch nil :weight 'regular :height 160 :family "Helvetica")
(set-face-attribute 'show-paren-match nil :foreground "CadetBlue")

(show-paren-mode 1)
  (recentf-mode 1)
  (fringe-mode 10)
  (tool-bar-mode -1)
(menu-bar-mode -1)
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
  (setq uniquify-min-dir-content 0)
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
  (setq help-at-pt-display-when-idle t)
  (setq help-at-pt-timer-delay 0.1)
  (help-at-pt-set-timer)
  (setq show-paren-style 'mixed)
  (setq mode-line-in-non-selected-windows nil)
  (fset 'yes-or-no-p 'y-or-n-p)
  (setq browse-url-browser-function 'browse-url-default-browser)
  (add-hook 'eww-after-render-hook 'eww-readable)
  (add-hook 'eww-after-render-hook 'visual-line-mode)
  (setq native-comp-speed 2)
  (setq package-native-compile t)
  (require 'xwidget)
  (setq alert-default-style 'notifier)

;;; follow links in xwidgets
(use-package xwwp-follow-link
  :custom
  (xwwp-follow-link-completion-backend 'ivy)
  :bind (:map xwidget-webkit-mode-map
              ("v" . xwwp-follow-link)))
(use-package string-inflection
  :ensure t)
(use-package font-lock
  :ensure nil
  :custom-face
      (font-lock-comment-face ((t (:foreground "PaleGreen4" :italic t)))))

(use-package vterm
  :ensure t
  :init
  (setq vterm-max-scrollback 1000000)
  )

(use-package fzf
  :bind
  ;; Don't forget to set keybinds!
  :config
  (setq fzf/args "-x --color bw --print-query --margin=1,0 --no-hscroll"
        fzf/preview-command "bat --style=numbers,changes --color=always --line-range :40 {}"
        fzf/args-for-preview "bat --style=numbers,changes --color=always --line-range :40 {}"
        fzf/executable "fzf"
        fzf/git-grep-args "-i --line-number %s"
        ;; command used for `fzf-grep-*` functions
        ;; example usage for ripgrep:
        ;; fzf/grep-command "rg --no-heading -nH"
        fzf/grep-command "grep -nrH"
        ;; If nil, the fzf buffer will appear at the top of the window
        fzf/position-bottom t
        fzf/window-height 15))

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
  (("\C-s" . 'swiper-isearch)
   ("C-x C-f" . 'fzf-find-file)
   ("C-c j" . 'fzf-git-grep)
   ("C-c k" . 'counsel-ag)
   ("C-x L" . 'counsel-locate)
   ("M-x" . 'counsel-M-x))
  :custom-face
  (ivy-minibuffer-match-face-2 ((t (:height 160 :family "Cascadia Code" :underline t :backgound "DarkSeagreen4"))))
  (ivy-current-match (( t ( :background "DarkSeaGreen4" :height 160 :family "Cascadia Code"))))
  :config
  (setq swiper-use-visual-line nil)
  (setq swiper-use-visual-line-p (lambda (a) nil)))

(use-package ivy-rich
  :init
  (ivy-rich-mode 1)
  :config
  (setq ivy-format-function #'ivy-format-function-line))
(use-package nerd-icons-ivy-rich
  :ensure t
  :init
  (nerd-icons-ivy-rich-mode 1))

(use-package ivy-posframe
  :ensure t
  :after ivy
  :init
  (setq ivy-posframe-hide-minibuffer t)
  (setq ivy-posframe-min-width nil)
  (setq ivy-posframe-width nil)
  (setq ivy-posframe-border-width 10)
  (setq ivy-posframe-parameters
        '((left-fringe . 8)
          (right-fringe .8)))
  )

(defun my-ivy-posframe-get-size ()
  "Set the ivy-posframe size according to the current frame."
  (let ((height (or ivy-posframe-height (or ivy-height 10)))
        (width (min (or ivy-posframe-width 200) (round (* 1 (frame-width))))))
    (list :height height :width width :min-height height :min-width width)))

(setq ivy-posframe-size-function 'my-ivy-posframe-get-size)

(use-package nerd-icons-completion
  :ensure t
  :after marginalia
  :config
  (nerd-icons-completion-mode)
  (add-hook 'marginalia-mode-hook #'nerd-icons-completion-marginalia-setup))

(global-set-key "\C-cy" 'counsel-yank-pop)

(use-package no-littering
  :ensure t)

(setq auto-save-file-name-transforms
      `((".*" ,(no-littering-expand-var-file-name "auto-save/") t)))

(use-package pos-tip
  :defer 2
  :ensure t)

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
  (ace-window-display-mode)
  (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))
  :bind
  ("M-o" . 'ace-window)
  :custom-face
  (aw-leading-char-face ((t (:height 3.0 :foreground "dodgerblue")))))

(use-package magit
  :ensure t)
(require 'magit)

(use-package git-timemachine
  :defer 2
  :ensure t
  :diminish
  )
(use-package git-gutter
  :ensure t
  :hook (prog-mode . git-gutter-mode)
  :config
  (setq git-gutter:update-interval 0.02)
   (defun rpo/git-gutter-mode ()
  "Enable git-gutter mode if current buffer's file is under version control."
  (if (and (buffer-file-name)
      (vc-backend (buffer-file-name))
          (not (cl-some (lambda (suffix) (string-suffix-p suffix (buffer-file-name)))
                      '(".pdf" ".svg" ".png"))))
      (git-gutter-mode 1)))
   (add-hook 'find-file-hook #'rpo/git-gutter-mode)
  )

(use-package git-gutter-fringe
  :ensure t
  :init
  (with-eval-after-load 'git-gutter (require 'git-gutter-fringe))
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

(use-package doom-themes
  :ensure t
  :config
  (setq doom-themes-enable-bold t)
  (setq doom-themes-enable-italic t)
  (add-to-list 'custom-theme-load-path "~/.emacs.d/themes")
  (doom-themes-org-config)
  (require 'doom-themes-ext-org))
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes")
(use-package hc-zenburn-theme
  :ensure t)
(load-theme 'hc-zenburn t)

(use-package nerd-icons
  :ensure t
  )
(use-package doom-modeline
  :ensure t
  :config
  (setq doom-modeline-buffer-file-name-style 'buffer-name)
  (setq doom-modeline-env-enable-ruby nil)
  (setq doom-modeline-vcs-icon t)
  (setq doom-modeline-vcs-max-length 40)
  (setq doom-modeline-battery nil)
  (doom-modeline-mode 1))
(require 'gnutls)
(setq starttls-use-gnutls t)
(setq auto-revert-check-vc-info t)

(use-package ligature
  :load-path "~/dev/git/ligature.el"
  :config
  ;; Enable the "www" ligature in every possible major mode
  (ligature-set-ligatures 't '("www"))
  ;; Enable traditional ligature support in eww-mode, if the
  ;; `variable-pitch' face supports it
  (ligature-set-ligatures 'eww-mode '("ff" "fi" "ffi"))
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
  (global-flycheck-mode)
  :config
  (flycheck-add-mode 'javascript-eslint 'rjsx-mode)
  (flycheck-add-mode 'javascript-jshint 'rjsx-mode)
  (flycheck-add-mode 'javascript-eslint 'jtsx-jsx-mode)
  (flycheck-add-mode 'javascript-jshint 'jrsx-jsx-mode)
  (flycheck-add-mode 'ruby-rubocop 'ruby-mode)
  )

(server-start)

(use-package diminish
  :ensure t
  :config

  (diminish 'org-mode  "")
  (diminish 'auto-revert-mode)
  (diminish 'yas-minor-mode)
  (diminish 'emmet-mode)
  (diminish 'rjsx-minor-mode)
  (diminish 'eldoc-mode)
  (diminish 'org-src-mode)
  (diminish 'abbrev-mode)
  (diminish 'ivy-mode)
  (diminish 'global-highline-mode)
  (diminish 'ruby-block-mode)
  (diminish 'ruby-electric-mode)
  (diminish 'buffer-face-mode)
  (diminish 'auto-fill-function)
  (diminish "seeing-is-believing")
  (diminish 'hs-minor-mode)
  (diminish 'ruby-block-mode)
  (diminish 'global-highline-mode))

(require 'ox-latex)
(use-package org
  :pin nongnu
  :ensure t
  :custom-face
  (org-block ((t :inherit default
                 :extend t
                 :background "gray15"
                 :height 160 :family "Cascadia Code")))
  (org-block-begin-line ((t (:family "Cascadia Code" :italic t))))
  (org-variable-pitch-fixed-face ((t (:inherit 'org-block :extend t :family "Cascadia Code"))))
  :config
  (setq org-default-notes-file "~/Documents/notes/notes.org")
  (add-to-list 'org-latex-classes
               '("novel" "\\documentclass{novel}"
                 (
                  "\\begin{ChapterStart}\\ChapterTitle{{%s} \\the\\value{novelcn}\\stepcounter{novelcn}}\\end{ChapterStart}"  "\\newline")               (
                  "\\QuickChapter[3em]{%s}"  "\\newline"
                  "\\begin{ChapterStart}\\ChapterTitle{%s}\\end{ChapterStart}"  "\\newline"
                  "\\begin{ChapterStart}\\ChapterTitle{%s}\\end{ChapterStart}"  "\\newline")))
  (setq org-latex-pdf-process
        '("latexmk -f -pdf -%latex  -shell-escape -interaction=nonstopmode -output-directory=%o %f")))

(require 'org-capture)
(setq org-capture-templates
      '(
        ("t" "Todo" entry (file+headline "~/Documents/notes/todo.org" "Tasks")
         "* TODO %?\n  %i\n  %a")
        ("j" "Journal" entry (file+datetree "~/Documents/notes/notes.org")
         "* %?\nEntered on %U\n  %i\n  %a")
        ("i" "Jira Issue" entry (file+headline "~/Documents/notes/work.org" "Issues")
         "* TODO %^{JiraIssueKey}"
         :jump-to-captured t
         :immediate-finish t
         :empty-lines-after 1)))

(use-package ox-jira
  :ensure t)
(require 'org-habit)
(setq org-habit-show-all-today t)
(setq org-habit-show-habits t)
(setq org-startup-indented nil)
(visual-line-mode 1)
(require 'ox-gfm)
(use-package org-modern
  :ensure t
  :init
  (with-eval-after-load 'org (global-org-modern-mode)))
(require 'org-modern)
(require 'ox-md)
(require 'ox-confluence)
(require 'ox-jira)
(add-hook 'org-modern-mode-hook 'org-variable-pitch-minor-mode)
(add-hook 'org-mode-hook 'org-variable-pitch-minor-mode)

(add-hook 'org-agenda-finalize-hook #'org-modern-agenda)

(use-package biblio
  :ensure t)
(use-package org-ref
  :ensure t
  :after (biblio)
  :defer nil
  :config
  (setq org-ref-bibliography-notes "~/Documents/notes/bibnotes.org"
        org-ref-default-bibliography '("~/Documents/references.bib")
        org-ref-pdf-directory "~/Documents/pdf/"
        reftex-default-bibliography '("~/Documents/references.bib")
        org-ref-completion-library 'org-ref-ivy-cite
        org-cite-csl-styles-dir "~/Zotero/styles")
)

(setq org-latex-listings 'minted)
(add-to-list 'org-latex-packages-alist '("" "minted" t))

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

(use-package jiralib2
  :ensure t
  :config
  (setq
   jiralib2-auth 'cookie
   jiralib2-url "https://jira2.workday.com"
   )
  (add-hook 'org-roam-capture-new-node-hook #'fg/jira-update-heading)
  (add-hook 'org-capture-before-finalize-hook #'fg/jira-update-heading)
  )
(use-package emacsql
  :ensure t)

(use-package org-roam
  :after org
  :ensure t
  :init
  (setq org-roam-v2-ack t)
  :custom
  (org-roam-directory "~/Documents/org-roam" )
  :config
  (org-roam-db-autosync-enable)
  (setq org-roam-database-connector 'sqlite-builtin))

(setq org-roam-capture-templates '(("d" "default" plain "%?" :if-new
                                    (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n")
                                    :unnarrowed t)
                                   ("c" "region" plain "%i" :if-new
                                    (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n")
                                    :unnarrowed t)
                                   ("i" "Jira Issue" entry "* TODO ${title}\n:PROPERTIES:\n:JiraIssueKey: ${title}\n:END:\n"
                                    :if-new
                                    (file+head "%<%Y%m%d%H%M%S>-${slug}.org"
                                               "#+title: ${title}\n\n" )

                                    :unnarrowed t)
                                   ))
(setq org-roam-capture-ref-templates '(("r" "ref" plain "%a %i"
                                        :target (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n#+date: %t\n\n")
                                        :jump-to-captured t
                                        :unnarrowed t)))
(setq org-roam-node-display-template
      (concat "${title:30} "
              (propertize "${tags:*}" 'face 'org-tag)))

(setq org-roam-dailies-directory "daily/")
(setq org-roam-completion-everywhere t)
(setq org-roam-dailies-capture-templates
      '(("d" "default" entry
         "* %?"
         :if-new (file+head "%<%Y-%m-%d>.org"
                            "#+title: %<%Y-%m-%d>\n#+OPTIONS: ^:nil num:nil whn:nil toc:nil H:0 date:nil author:nil title:nil\n\n
   "))
        ("c" "region" entry
         "* %? %i"
         :if-new (file+head "%<%Y-%m-%d>.org"
                            "#+title: %<%Y-%m-%d>\n#+OPTIONS: ^:nil num:nil whn:nil toc:nil H:0 date:nil author:nil title:nil\n\n
   "))
        ("l" "link" entry
         "* %? \n%i"
         :target (file+olp "%<%Y-%m-%d>.org"
                           ("Links"))
         :unnarrowed t
         )))

(defun ek/babel-ansi ()
  (when-let ((beg (org-babel-where-is-src-block-result nil nil)))
    (save-excursion
      (goto-char beg)
      (when (looking-at org-babel-result-regexp)
        (let ((end (org-babel-result-end))
              (ansi-color-context-region nil))
          (ansi-color-apply-on-region beg end))))))
(add-hook 'org-babel-after-execute-hook 'ek/babel-ansi)
(use-package ox-twbs
  :ensure t)
(use-package ox-gfm
  :ensure t)

(use-package org-mime
  :ensure t)
(setq org-src-fontify-natively t)
(setq org-src-tab-acts-natively t)
(setq org-src-window-setup 'current-window)
(use-package plantuml-mode
  :ensure t)
(setq org-startup-with-inline-images t)
(add-hook 'org-babel-after-execute-hook 'org-redisplay-inline-images)

(setq org-todo-keywords
      '((
         sequence "TODO(t)" "STARTED(s)" "WAITING(w)" "|" "DONE(d)" "CANCELLED(c)")))
(setq org-agenda-include-diary t)
(setq org-agenda-include-all-todo t)

(with-eval-after-load 'org
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
     (dot . t))))
(setq org-confirm-babel-evaluate nil)

(use-package ox-pandoc
  :defer 2
  :ensure t
  :config
  (setq org-pandoc-options '((standalone . t)))
  (setq org-pandoc-command (substring (shell-command-to-string "which pandoc") 0 -1)))

 (use-package org-variable-pitch
   :after org
   :ensure t
   )

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
(setq org-plantuml-jar-path "/opt/homebrew/Cellar/plantuml/1.2024.8/libexec/plantuml.jar")
(setq plantuml-jar-path "/opt/homebrew/Cellar/plantuml/1.2024.8/libexec/plantuml.jar")


(setq org-mime-export-options '(:section-numbers nil
                                                 :with-author nil
                                                 :with-toc nil))

;; (use-package zenburn-theme
;;   :defer 2
;;   :after (:all ace-window)
;;   :ensure t
;;   :init
;;   (setq zenburn-override-colors-alist '(
;;                                         ("zenburn-bg" . "gray16")
;;                                         ("zenburn-bg-1" . "#5F7F5F")))


;;        (load-theme 'zenburn t)
;;   :config
;;   (setq zenburn-use-variable-pitch t)
;;   (setq zenburn-scale-org-headlines t)
;;   (setq zenburn-scale-outline-headlines t)
;;   )

;; (use-package vscode-dark-plus-theme
;;   :ensure t
;;   :after ace-window
;;   :init
;;   (load-theme 'vscode-dark-plus t))

(use-package exec-path-from-shell
  :ensure t
  :config
  (setq exec-path-from-shell-check-startup-files t)
  (setq exec-path-from-shell-variables `("PATH" "ARTIFACTORY_PASSWORD" "ARTIFACTORY_USER"))
  (setq exec-path-from-shell-arguments '("-l" "-i"))
  (when (memq window-system '(mac ns x))
    (exec-path-from-shell-initialize)))



(require 'org-crypt)
(org-crypt-use-before-save-magic)
(setq org-tags-exclude-from-inheritance (quote("crypt")))
(if (memq window-system '(mac ns x))
    (let* ((gpg-command "gpg --list-secret-key --keyid-format short")
           (grep-sec "grep sec")
           (grep-key "ggrep -o -P '(?<=/)[A-Z0-9]{8}'")
           (head-command "head -1")
           (full-command (format "%s | %s | %s | %s" gpg-command grep-sec grep-key head-command))
           (key (substring (shell-command-to-string full-command) 0 -1)))
      (setq org-crypt-key key))
  (let* ((gpg-command "gpg --list-secret-key --keyid-format short")
         (grep-sec "grep sec")
         (grep-key "grep -o -P '(?<=/)[A-Z0-9]{8}'")
         (head-command "head -1")
         (full-command (format "%s | %s | %s | %s" gpg-command grep-sec grep-key head-command))
         (key (substring (shell-command-to-string full-command) 0 -1)))
    (setq org-crypt-key key)))

;; yaml
(require 'yaml-mode)
(add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))
(add-to-list 'auto-mode-alist '("\\.yaml$" . yaml-mode))

(use-package inf-ruby
  :defer 2
  :ensure t)
(require 'ruby-mode)
(use-package  ruby-electric
  :ensure t)
(use-package feature-mode
  :defer 2
  :ensure t
  :config
  (setq feature-use-docker-compose nil)
  (setq feature-rake-command "cucumber --format progress {OPTIONS} {feature}"))

(use-package yasnippet
  :defer 2
  :ensure t
  :config
  (yas-global-mode t))
(use-package yasnippet-snippets
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
(require 'org-roam-protocol)
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
(require 'blog)

(use-package highline
  :ensure t
  :defer 2
  :config
  (global-highline-mode t)
  (setq highline-face '((:background "gray40")))
  (setq highline-vertical-face '(( :background "lemonChiffon2"))))


(column-number-mode)
(global-display-line-numbers-mode t)

;; Disable line numbers for some modes
(dolist (mode '(org-mode-hook
                org-modern-mode
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
    :ensure t
    :pin melpa
    :commands (lsp lsp-deferred)
    :hook ((go-mode . lsp-deferred)(go-ts-mode . lsp-deferred)(ruby-mode . lsp-deferred) (java-mode . lsp-deferred) (python-mode . lsp-deferred)(jtsx-jsx-mode . lsp-deferred)(lsp-mode . lsp-enable-which-key-integration))
    :custom
    (lsp-auto-configure t)
    (lsp-prefer-flymake nil)
    (lsp-inhibit-message t)
    (lsp-eldoc-render-all t)
    :config
    (setq lsp-enable-which-key-integration t)
    (setq lsp-enable-symbol-highlighting t)
    (setq lsp-modeline-code-actions-enable t)
    (setq lsp-diagnostics-provider :auto)
    (setq lsp-diagnostics-mode nil)
    (setq lsp-semantic-tokens-enable t)
    (define-key lsp-mode-map (kbd "C-c l") lsp-command-map)
    (setq lsp-idle-delay 0.500)
    (setq lsp-log-io nil)
    (setq lsp-completion-provider :capf)
    (setq lsp-enable-file-watchers nil)
    )


(use-package lsp-bridge
  :straight '(lsp-bridge :type git :host github :repo "manateelazycat/lsp-bridge"
            :files (:defaults "*.el" "*.py" "acm" "core" "langserver" "multiserver" "resources")
            :build (:not compile))
:hook
  (prog-mode . lsp-bridge-mode))


  (use-package lsp-java
    :ensure t
    :config (add-hook 'java-mode-hook #'lsp))

  (setenv "JAVA_HOME" "/opt/homebrew/Cellar/openjdk/22.0.2/")
  (setq lsp-java-java-path "/opt/homebrew/Cellar/openjdk/22.0.2/bin/java")
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
    (setq lsp-ui-sideline-enable t)
    (setq lsp-lens-enable t)
    (setq lsp-ui-sideline-enable t
          lsp-ui-sideline-show-symbol t
          lsp-ui-sideline-show-hover t
          lsp-ui-sideline-show-flycheck t
          lsp-ui-sideline-show-code-actions t
          lsp-ui-sideline-show-diagnostics t)

    (setq lsp-ui-doc-enable nil)
    (setq lsp-ui-imenu-enable nil)
    (setq lsp-ui-peek-enable t)       )

  (use-package lsp-treemacs
    :defer 2
    :after lsp
    :config
    (lsp-treemacs-sync-mode t)
    )
  (require 'lsp-ui-flycheck)
  (setq lsp-inhibit-message t)
  (setq lsp-prefer-flymake nil)
  (setq lsp-eldoc-render-all t)

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

(autoload 'markdown-mode' "markdown-mode" "Major Mode for editing Markdown" t)
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))
(use-package markdown-mode
:hook
(markdown-mode . nb/markdown-unhighlight)
:config
(defvar nb/current-line '(0 . 0)
  "(start . end) of current line in current buffer")
(make-variable-buffer-local 'nb/current-line)

(defun nb/unhide-current-line (limit)
  "Font-lock function"
  (let ((start (max (point) (car nb/current-line)))
        (end (min limit (cdr nb/current-line))))
    (when (< start end)
      (remove-text-properties start end
                              '(invisible t display "" composition ""))
      (goto-char limit)
      t)))

(defun nb/refontify-on-linemove ()
  "Post-command-hook"
  (let* ((start (line-beginning-position))
         (end (line-beginning-position 2))
         (needs-update (not (equal start (car nb/current-line)))))
    (setq nb/current-line (cons start end))
    (when needs-update
      (font-lock-fontify-block 3))))

(defun nb/markdown-unhighlight ()
  "Enable markdown concealling"
  (interactive)
  (markdown-toggle-markup-hiding 'toggle)
  (font-lock-add-keywords nil '((nb/unhide-current-line)) t)
  (add-hook 'post-command-hook #'nb/refontify-on-linemove nil t))
:custom-face
(markdown-header-delimiter-face ((t (:foreground "#616161" :height 0.9))))
(markdown-header-face-1 ((t (:height 1.6  :foreground "#A3BE8C" :weight extra-bold :inherit markdown-header-face))))
(markdown-header-face-2 ((t (:height 1.4  :foreground "#EBCB8B" :weight extra-bold :inherit markdown-header-face))))
(markdown-header-face-3 ((t (:height 1.2  :foreground "#D08770" :weight extra-bold :inherit markdown-header-face))))
(markdown-header-face-4 ((t (:height 1.15 :foreground "#BF616A" :weight bold :inherit markdown-header-face))))
(markdown-header-face-5 ((t (:height 1.1  :foreground "#b48ead" :weight bold :inherit markdown-header-face))))
(markdown-header-face-6 ((t (:height 1.05 :foreground "#5e81ac" :weight semi-bold :inherit markdown-header-face))))
:hook
(markdown-mode . abbrev-mode))

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
  (add-hook 'js-mode-hook 'lsp)
  (add-hook 'rjsx-mode-hook 'lsp)
  (add-hook 'rjsx-mode-hook 'emmet-mode)

  (use-package jtsx
    :ensure t
    :hook((jtsx-jsx-mode . lsp-deferred)(jtsx-jsx-mode . emmet-mode)(jtsx-jsx-mode . prettier-js-mode))
    )
  (use-package prettier-js
    :config
    (add-hook 'js2-mode-hook 'prettier-js-mode)
    (add-hook 'rjsx-mode-hook 'prettier-js-mode)
    (add-hook 'jtsx-jsx-mode 'prettier-js-mode)
    )

(setq emmet-expand-jsx-className? t)

(use-package emmet-mode
  :ensure t
  :config
  (add-to-list 'emmet-jsx-major-modes 'jtsx-jsx-mode))

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

(add-to-list 'load-path "~/dev/git/notdeft/")
(add-to-list 'load-path "~/dev/git/notdeft/extras")
(setq notdeft-directory "~/Documents/org-roam/")
(setq notdeft-directories '("~/Documents/org-roam/"))
(setq notdeft-xapian-program (expand-file-name"~/dev/git/notdeft/xapian/notdeft-xapian"))
(require 'notdeft-autoloads)
(global-set-key (kbd "<f9>") 'notdeft)

(use-package cypher-mode
  :ensure t)
;;     (setq n4js-cli-program "~/Downloads/cypher-shell/cypher-shell")
(setq n4js-cli-program "/opt/homebrew/bin/cypher-shell")
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
  :ensure t
  :init
  (defun helpful--autoloaded-p (sym buf)
    "Return non-nil if function SYM is autoloaded."
    (-when-let (file-name (buffer-file-name buf))
      (setq file-name (s-chop-suffix ".gz" file-name))
      (help-fns--autoloaded-p sym)))

  (defun helpful--skip-advice (docstring)
    "Remove mentions of advice from DOCSTRING."
    (let* ((lines (s-lines docstring))
           (relevant-lines
            (--take-while
             (not (or (s-starts-with-p ":around advice:" it)
                      (s-starts-with-p "This function has :around advice:" it)))
             lines)))
      (s-trim (s-join "\n" relevant-lines)))))

(use-package elfeed
  :ensure t
  :config
  ;;
  ;; linking and capturing
  ;;
  (defun elfeed-link-title (entry)
    "Copy the entry title and URL as org link to the clipboard."
    (interactive)
    (let* ((link (elfeed-entry-link entry))
           (title (elfeed-entry-title entry))
           (titlelink (concat "[[" link "][" title "]]")))
      (when titlelink
        (kill-new titlelink)
        (x-set-selection 'PRIMARY titlelink)
        (message "Yanked: %s" titlelink))))
  ;; show mode
  (defun elfeed-show-link-title ()
    "Copy the current entry title and URL as org link to the clipboard."
    (interactive)
    (elfeed-link-title elfeed-show-entry))
  (defun elfeed-show-quick-url-note ()
    "Fastest way to capture entry link to org agenda from elfeed show mode"
    (interactive)
    (elfeed-link-title elfeed-show-entry)
    (org-roam-dailies-capture-today nil "l")
    (yank)
    (org-capture-finalize))
  (bind-keys :map elfeed-show-mode-map
             ("l" . elfeed-show-link-title)
             ("v" . elfeed-show-quick-url-note))
  )

(use-package elfeed-org
  :ensure t
  :after elfeed
  :config
  (setq rmh-elfeed-org-files (list "~/.emacs.d/elfeed.org"))
  (elfeed-org))

;; (use-package visual-fill
;;   :ensure t)

 (defun elfeed-olivetti (buff)
  (with-current-buffer buff
    (setq fill-column 100)
    (setq buffer-read-only nil)
    (goto-char (point-min))
    (re-search-forward "\n\n")
    (fill-individual-paragraphs (point-min) (point-max))
    (setq buffer-read-only t))
  (switch-to-buffer buff)
  ;;       (olivetti-mode)
  (visual-fill-column-mode)
  (elfeed-show-refresh)
  )

(add-hook 'elfeed-show-mode-hook (lambda()
                                   (setq fill-column 100)
                                   ;;(visual-fill-mode t)
                                   (adaptive-wrap-prefix-mode t)
                                   (toggle-word-wrap)
                                   (setq elfeed-show-entry-switch 'elfeed-olivetti)
                                   ))

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
  (setq ivy-prescient-retain-classic-highlighting t)
  (setq ivy-prescient-enable-filtering nil)
  (setq ivy-prescient-enable-sorting t)
  (setq ivy-re-builders-alist
        '(
          (counsel-M-x . ivy--regex-plus)
          (ivy-switch-buffer . ivy--regex-plus)
          (ivy-switch-buffer-other-window . ivy--regex-plus)
          (counsel-ag . ivy--regex-plus)
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
    "t" 'fzf-projectile
    "a" 'ace-jump-mode
    "g" '(:ignore t :which-key "rspec")
    "gp" '(inf-ruby-switch-from-compilation :which-key "enter debugger")
    "ga" '(rspec-verify-all :which-key "run all specs")
    "gs" '(rspec-verify-single :which-key "run single spec")
    "gr" '(rspec-rerun :which-key "rerun spec")
    "gf" '(rspec-run-last-failed :which-key "rerun last failed")
    "i"  '(:ignore t :which-key "inf-ruby")
    "ib" '(ruby-send-buffer :which-key "ruby-send-buffer")
    "v"  '(:ignore t :which-key "avy")
    "va" '(avy-goto-word-1 :which-key "avy-goto-word-1")
    "vl" '(avy-goto-line :which-key "avy-goto-line")
    "vs" '(avy-goto-char-timer :which-key "avy-goto-char-timer")
    "vc" '(avy-goto-char :which-key "avy-goto-char")
    "f" '(:ignore t :which-key "cucumber")
    "ff" '(feature-verify-all-scenarios-in-project :which-key "run all cukes")
    "fs" '(feature-verify-scenario-at-pos :which-key "run cuke at point")
    "fv" '(feature-verify-all-scenarios-in-buffer :which-key "run all cukes in buffer")
    "fg" '(feature-goto-step-definition :which-key "goto step definition")
    "fr" '(feature-register-verify-redo :which-key "repeat last cuke")
    "m" 'mu4e
    "o" 'find-file
    "b" '(:ignore t :which-key "eww")
    "bf" '(eww-follow-link :which-key "eww-follow-link")
    "z" '(:ignore t :which-key "roam")
    "zd" '(:ignore t :which-key "dailies")
    "zdc" '(org-roam-dailies-capture-today :which-key "capture today")
    "zdt" '(org-roam-dailies-goto-today :which-key "goto today")
    "zdd" '(org-roam-dailies-goto-tomorrow :which-key "goto tomorrow")
    "zf" '(org-roam-node-find :which-key "org-roam-node-find")
    "zi" '(org-roam-node-insert :which-key "org-roam-node-insert")
    "zv" '(org-roam-node-visit :which-key "org-roam-node-visit")
    "zo" '(org-roam-node-open :which-key "org-roam-node-open")
    "zt" '(:ignore t :which-key "roam-tag")
    "zta" '(org-roam-tag-add :which-key "roam-tag-add")
    "ztr" '(org-roam-tag-add :which-key "roam-tag-remove")
    "zr"  '(:ignore t :which-key "roam-ref")
    "zra" '(org-roam-ref-add :which-key "roam-ref-add")
    "zrr" '(org-roam-ref-remove :which-key "roam-ref-remove")
    "zb"  '(org-roam-buffer-toggle :which-key "roam-buffer-toggle")
    "q" '(:ignore t :which-key "copilot")
    "qa" '(copilot-accept-completion :which-key "copilot-accept-completion")
    "qd" '(copilot-diagnose :which-key "copilot-diagnose")
    "ql" '(copilot-accept-completion-by-line :which-key "copilot-accept-completion-by-line")
    "qw" '(copilot-accept-completion-by-word :which-key "copilot-accept-completion-by-word")
    "qp" '(copilot-previous-completion :which-key "copilot-previous-completion")
    "qn" '(copilot-next-completion :which-key "copilot-next-completion")))

(use-package quelpa-use-package
    :ensure t)
  (use-package copilot
    :quelpa (copilot :fetcher github
                     :repo "copilot-emacs/copilot.el"
                     :branch "main"
                     :files ("*.el")))

(use-package gptel-aibo
  :quelpa (gptel-aibo :fetcher github
                      :repo "dolmens/gptel-aibo"
                      :branch "main")
  :after(gptel flycheck))
;; ;; you can utilize :map :hook and :config to customize copilot
  (define-key copilot-completion-map (kbd "<tab>") 'copilot-accept-completion)

  (use-package chatgpt-shell
    :ensure t)
  (straight-use-package 'gptel
    :ensure t)
(with-eval-after-load 'gptel (add-hook 'gptel-post-stream-hook  'gptel-auto-scroll))


  (use-package copilot-chat
    :ensure t
    :config
    (setq copilot-chat-frontend 'org))

  (let ((model-config '((:version . "gpt-4o-mini") (:short-version)
                         (:label . "ChatGPT") (:provider . "OpenAI")
                         (:path . "/v1/chat/completions") (:token-width . 3)
                         (:context-window . 128000)
                         (:handler . chatgpt-shell-openai--handle-chatgpt-command)
                         (:filter . chatgpt-shell-openai--filter-output)
                         (:payload . chatgpt-shell-openai--make-payload)
                         (:headers . chatgpt-shell-openai--make-headers)
                         (:url . chatgpt-shell-openai--make-url)
                         (:key . chatgpt-shell-openai-key)
                         (:url-base . chatgpt-shell-api-url-base)
                         (:validate-command . chatgpt-shell-openai--validate-command))))
    (add-to-list 'chatgpt-shell-models model-config))

  (gptel-make-ollama "Ollama"             ;Any name of your choosing
  :host "localhost:11434"               ;Where it's running
  :stream t                             ;Stream responses
  :models '(mistral:latest))             ;List of models

  ;; :key can be a function that returns the API key.
(gptel-make-gemini "Gemini"
  :key (gptel-api-key-from-auth-source "generativelanguage.googleapis.com")
  :stream t)

  (use-package ob-chatgpt-shell
    :ensure t)
  (require 'ob-chatgpt-shell)
  (ob-chatgpt-shell-setup)

(use-package magit-delta
  :ensure t
  :hook
  (magit-mode . magit-delta-mode))

(use-package popper
  :ensure t ; or :straight t
  :bind (("C-`"   . popper-toggle-latest)
         ("M-`"   . popper-cycle)
         ("C-M-`" . popper-toggle))
  :init
  (setq popper-reference-buffers
        '("\\*Messages\\*"
          "Output\\*$"
          "\\*Async Shell Command\\*"
          help-mode
          compilation-mode))
  (popper-mode +1)
  (popper-echo-mode +1))                ; For echo area hints

(use-package blamer
  :commands (blamer-mode)
  :config
  (setq blamer-view 'overlay-right
        blamer-type 'visual
        blamer-max-commit-message-length 180
        blamer-author-formatter " ✎ [%s] - "
        blamer-commit-formatter "● %s ● "
        blamer-smart-background-p nil)
  :custom
  (blamer-idle-time 1.0)
  (blamer-min-offset 10)
  :custom-face
  (blamer-face ((t :foreground "PaleGreen2"
                   :height 120
                   :italic t
                   :family "Helvetica"
                   :background "gray40"))))
(global-blamer-mode)

(use-package svg-tag-mode
  :hook ((prog-mode . svg-tag-mode))
  :config
  (setq svg-tag-tags
        '(
          ("\\W?DONE\\b" . ((lambda (tag) (svg-tag-make "DONE" :face 'org-done :margin 0))))
          ("FIXME\\b" . ((lambda (tag) (svg-tag-make "FIXME" :face 'org-todo :inverse t :margin 0))))
          ("\\/\\/\\W?MARK\\b:\\|MARK\\b:" . ((lambda (tag) (svg-tag-make "MARK" :face 'font-lock-doc-face :inverse t :margin 0 :crop-right t))))
          ("MARK\\b:\\(.*\\)" . ((lambda (tag) (svg-tag-make tag :face 'font-lock-doc-face :crop-left t))))

          ("\\/\\/\\W?TODO\\b\\|TODO\\b" . ((lambda (tag) (svg-tag-make "TODO" :face 'org-todo :inverse t :margin 0 :crop-right t))))
          ("TODO\\b\\(.*\\)" . ((lambda (tag) (svg-tag-make tag :face 'org-todo :crop-left t))))
          )))

(use-package tree-sitter-langs
  :ensure t )
(use-package tree-sitter
  :ensure t
  :config
  (require 'tree-sitter-langs)
  (global-tree-sitter-mode))

(use-package pdf-tools
  :ensure t
  :config (pdf-tools-install :no-query)
  (setq-default pdf-view-display-size 'fit-page)
  (add-hook 'pdf-view-mode-hook (lambda() (display-line-numbers-mode -1))))

(use-package discover
  :ensure t)

(use-package mastodon
  :ensure  t
  :config
  (setq mastodon-active-user "AriT93")
  (setq mastodon-instance-url "https://mastodon.social")
  (mastodon-discover))

(use-package auctex
  :ensure t)
(add-to-list 'load-path "~/dev/git/procress")
(use-package procress
  :commands procress-auctex-mode
  :init
  (add-hook 'LaTeX-mode-hook #'procress-auctex-mode)
  (add-hook 'LaTeX/P-mode-hook #'procress-auctex-mode)

  :config
  (setq TeX-command-extra-options "-shell-escape")
  (procress-load-default-svg-images))

(set-face-attribute 'default nil
                    :inherit nil
                    :height 160
                    :weight 'Regular
                    :foundry "microsoft"
                    :family "Cascadia Code")
(set-face-attribute 'region nil :background "DarkOliveGreen")
(set-face-attribute 'highlight nil :background "DarkSeaGreen4")
(set-face-attribute 'fringe nil :background "#070018")
(set-face-attribute 'header-line nil :box '(:line-width 4 :color "#1d1a26" :style nil))
(set-face-attribute 'header-line-highlight nil :box '(:color "#d0d0d0"))
(set-face-attribute 'line-number-current-line nil :foreground "PaleGreen2" :italic t)
(set-face-attribute 'tab-bar-tab nil :box '(:line-width 4 :color "#070019" :style nil))
(set-face-attribute 'tab-bar-tab-inactive nil :box '(:line-width 4 :color "#4a4759" :style nil))
(set-face-attribute 'variable-pitch nil :weight 'regular :height 160 :family "Helvetica")
(set-face-attribute 'show-paren-match nil :foreground "CadetBlue")
(provide 'emacs-config-new)
