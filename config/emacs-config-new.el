;; -*- lexical-binding: t; -*-
;; Don't configure package.el yet - it will be set up AFTER straight.el loads
;; to avoid triggering package.el autoload before straight.el

(setq treesit-extra-load-path nil)

;; Suppress lexical binding warnings during startup for better performance
(setq warning-suppress-log-types '((files) (package reinitialization)))

;; Suppress verbose messages during startup
(setq flyover-debug nil)              ; Disable flyover debug messages

;; Suppress org-mode verbose messages
(setq org-element-cache-persistent nil)  ; Disable org cache messages
(setq org-startup-folded nil)           ; Disable org startup messages
(setq org-agenda-start-with-log-mode nil) ; Disable org agenda log messages

;; Suppress general verbose messages
(setq message-log-max 50)              ; Reduce (not eliminate) message log during startup
(setq inhibit-startup-echo-area-message t) ; Already set, but ensure it's on

;; Suppress byte-compilation warnings during startup
(setq native-comp-async-report-warnings-errors nil)  ; Silence native-comp warnings
(setq byte-compile-warnings '(not free-vars unresolved noruntime lexical make-local))

;; Optimize file loading for better startup performance
(setq load-prefer-newer t)  ; Prefer .elc over .el when .elc is newer
;; Removed load-source-file-function setting - it conflicts with load-prefer-newer
;; and causes unnecessary decompression of .el.gz files during startup

;; package.el is not used at all (straight.el only); early-init.el sets
;; package-enable-at-startup nil so it never initializes.

;; Reduce startup overhead
(setq inhibit-startup-screen t)
(setq inhibit-startup-message t)
(setq inhibit-startup-echo-area-message t)

;; Set early to avoid race with doom-themes-ext-org loading after first org file opens.
;; org-heading-keyword-regexp-format has an optional group 2; the done-headline
;; fontification keyword references it unconditionally, causing font-lock errors.
(setq org-fontify-done-headline nil)

;; Optimize garbage collection during startup
;; GC thresholds, native-comp and read-process-output-max: see early-init.el

;; Startup timing diagnostics
(defvar ari/startup-times (make-hash-table))
(defun ari/startup-timer (label)
  (puthash label (float-time) ari/startup-times)
  (message "Startup checkpoint: %s at %.2fs" label (float-time)))

(ari/startup-timer "package-init")

(setq warning-suppress-log-types
      (append warning-suppress-log-types
              '((files missing-lexbind-cookie))))

;; Configure straight.el settings BEFORE loading
(setq straight-check-for-modifications '(check-on-save find-when-checking))
(setq straight-use-package-by-default t)

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

;; The lockfile is tracked in this repo. straight only reads lockfiles from
;; its own straight/versions/ dir, so link default.el to the tracked copy
;; (created on any machine that doesn't have the link yet).
(let ((tracked (expand-file-name "~/emacs/config/straight-lockfile.el"))
      (link (straight--versions-file "default.el")))
  (when (and (file-exists-p tracked) (not (file-symlink-p link)))
    (make-directory (file-name-directory link) t)
    (when (file-exists-p link) (rename-file link (concat link ".bak") t))
    (make-symbolic-link tracked link)))

;; Claim org via straight IMMEDIATELY after bootstrap, before any other
;; package can load the built-in org and cause a version mismatch warning.
;; Tracks the `bugfix' branch: org's stable maintenance line (9.8.x
;; releases + fixes). `main' is 10.0 development, which is where the
;; 2026-07 `org-export-dispatch' PDF regression came from. The exact
;; commit is pinned by the lockfile.
(straight-use-package
 '(org :type git :host github :repo "emacs-straight/org-mode"
       :branch "bugfix"
       :depth full
       :pre-build (straight-recipes-org-elpa--build)
       :build (:not autoloads)
       :files (:defaults "lisp/*.el" ("etc/styles/" "etc/styles/*"))))

;; use-package ships with Emacs 29+
(require 'use-package)
(ari/startup-timer "use-package-loaded")

(require 'load-path-config-new)
(ari/startup-timer "load-path-loaded")

;; gpg-agent auto-starts via its socket; no need to launch it manually.
(defun ensure-gpg-agent-running ()
  "Ping gpg-agent to ensure it is ready."
  (call-process "gpg-connect-agent" nil nil nil "updatestartuptty" "/bye"))

(add-hook 'emacs-startup-hook 'ensure-gpg-agent-running)

(setq epg-gpg-program "gpg2")
(setq auth-sources '("~/.authinfo.gpg"))
(setq auth-source-cache-expiry 3600)
(setq auth-source-debug nil)
(setq auth-source-do-cache t)

(defun ari/headless-linux-p ()
  "Non-nil on a Linux machine with no display server.
This matches the writer-deck (Ubuntu Server + kmscon, no X11/Wayland),
where gpg-agent has no GNOME Keyring / pinentry-mac integration to
fall back on and must use Emacs loopback pinentry instead."
  (and (eq system-type 'gnu/linux)
       (not (getenv "DISPLAY"))
       (not (getenv "WAYLAND_DISPLAY"))))

;; On Mac / desktop Linux, gpg-agent uses the system pinentry
;; (pinentry-mac + login keychain, or pinentry-gnome3 + GNOME Keyring),
;; so Emacs should stay out of the way. Only the headless writer-deck
;; needs loopback pinentry so Emacs can prompt in the minibuffer.
(when (ari/headless-linux-p)
  (setq epa-pinentry-mode 'loopback)
  (use-package pinentry
    :config
    (pinentry-start)))

;; Network security configuration
(setq gnutls-verify-error t)           ; Fail on TLS verification errors
(setq gnutls-min-prime-bits 2048)      ; Minimum encryption strength
(setq network-security-level 'high)    ; High security level for network connections

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

;; Emacs 31 / modern built-in features
(pixel-scroll-precision-mode 1)      ; Smooth scrolling
(save-place-mode 1)                  ; Remember cursor position in files
(winner-mode 1)                      ; Undo window config with C-c <left>/<right>
(repeat-mode 1)                      ; Repeat commands without re-pressing prefix
(context-menu-mode 1)                ; Right-click context menus
(setq ielm-history-file (locate-user-emacs-file "ielm-history"))  ; IELM history persistence

;; Emacs 31 window layout commands
(global-set-key (kbd "C-x w t") 'window-layout-transpose)
(global-set-key (kbd "C-x w r") 'window-layout-rotate-clockwise)
(global-set-key (kbd "C-x w f") 'window-layout-flip-leftright)

;; Built-in "batteries included" features (from karthinks.com)
(setq dictionary-server "dict.org")                   ; Use dict.org, skip localhost prompt
(add-hook 'text-mode-hook #'dictionary-tooltip-mode)  ; Hover for definitions
(add-hook 'prog-mode-hook #'subword-mode)             ; CamelCase navigation
(undelete-frame-mode 1)                               ; Recover deleted frames
(global-set-key (kbd "C-c D") 'duplicate-dwim)        ; Duplicate line/region
(global-set-key (kbd "C-c w") 'compare-windows)       ; Quick diff two windows
(global-set-key (kbd "C-c u") 'ffap-menu)             ; List all URLs in buffer

;; Speedbar docked in side window (Emacs 31 feature)
(setq speedbar-use-images t)
(defun speedbar-toggle-side-window ()
  "Toggle speedbar in a side window instead of separate frame."
  (interactive)
  (let ((sb-buf (get-buffer " SPEEDBAR")))
    (if (and sb-buf (get-buffer-window sb-buf))
        (delete-window (get-buffer-window sb-buf))
      (progn
        (unless sb-buf
          (speedbar-frame-mode 1)
          (speedbar-frame-mode -1)
          (setq sb-buf (get-buffer " SPEEDBAR")))
        (when sb-buf
          (display-buffer-in-side-window sb-buf
            '((side . left) (window-width . 35))))))))
(global-set-key (kbd "C-c s b") 'speedbar-toggle-side-window)
;; (menu-bar-mode t) ; Removed - conflicts with early-init.el which disables for performance
(setq recentf-max-menu-items 25)
(setq recentf-max-saved-items 25)
(setq undo-limit 8000000)
(setq undo-strong-limit 12000000)
(setq undo-outer-limit 12000000)
(setq inhibit-startup-screen t)
(setq inhibit-splash-screen t)
(setq uniquify-buffer-name-style (quote post-forward))
(setq uniquify-min-dir-content 0)
(electric-pair-mode 1)
(setq cal-tex-diary t)
(setq blog-root "/ssh:abturet@turetzky.org:~/blog/")
(add-hook 'diary-display-hook 'fancy-diary-display)
(add-hook 'text-mode-hook 'turn-on-auto-fill)
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
(setq use-short-answers t)  ; Modern replacement for (fset 'yes-or-no-p 'y-or-n-p) - Emacs 28+
(setq browse-url-browser-function 'browse-url-default-browser)
(add-hook 'eww-after-render-hook 'eww-readable)
(add-hook 'eww-after-render-hook 'visual-line-mode)
(setq package-native-compile t)
;; xwidget will autoload when needed
(setq alert-default-style 'notifier)

;;; follow links in xwidgets
(use-package xwwp
  :defer t)
(use-package string-inflection
  :defer t)
(use-package font-lock
  :straight nil
  :custom-face
      (font-lock-comment-face ((t (:foreground "PaleGreen4" :italic t)))))

(use-package vterm
  :straight t
  :init
  (setq vterm-max-scrollback 1000000)
  )

(use-package vertico
  :init
  (vertico-mode)
  :custom
  (vertico-cycle t)
  (vertico-count 15)
  (vertico-resize t)
  :config
  ;; Add prompt indicator to `completing-read-multiple'
  (defun crm-indicator (args)
    (cons (format "[CRM%s] %s"
                  (replace-regexp-in-string
                   "\\`\\[.*?]\\*\\|\\[.*?]\\*\\'" ""
                   crm-separator)
                  (car args))
          (cdr args)))
  (advice-add #'completing-read-multiple :filter-args #'crm-indicator))

;; Persist history over Emacs restarts
(use-package savehist
  :straight nil
  :init
  (savehist-mode))

;; Consult commands with enhanced search and navigation
(use-package consult
  :bind (;; C-c bindings in `mode-specific-map'
         ("C-c M-x" . consult-mode-command)
         ("C-c h" . consult-history)
         ("C-c k" . consult-kmacro)
         ("C-c m" . consult-man)
         ("C-c i" . consult-info)
         ;; C-x bindings in `ctl-x-map'
         ("C-x M-:" . consult-complex-command)
         ("C-x b" . consult-buffer)
         ("C-x 4 b" . consult-buffer-other-window)
         ("C-x 5 b" . consult-buffer-other-frame)
         ("C-x r b" . consult-bookmark)
         ("C-x p b" . consult-project-buffer)
         ;; Custom M-# bindings for fast register access
         ("M-#" . consult-register-load)
         ("M-'" . consult-register-store)
         ("C-M-#" . consult-register)
         ;; Other custom bindings
         ("M-y" . consult-yank-pop)
         ;; Replace search commands
         ("C-s" . consult-line)
         ("M-s g" . consult-grep)
         ("M-s G" . consult-git-grep)
         ("M-s r" . consult-ripgrep)
         ;; Replace isearch bindings
         ("M-s s" . consult-isearch-history)
         :map isearch-mode-map
         ("M-e" . consult-isearch-history)
         ("M-s e" . consult-isearch-history)
         ("M-s l" . consult-line))
  :init
  ;; Optionally configure preview. Note that the preview-key can also be
  ;; configured on a per-command basis via `consult-config'.
  (setq consult-preview-key "M-.")
  ;; For some commands and buffer sources, use a different key that is defined here
  (setq consult-narrow-key "<") ;; "C-+"
  :config
  ;; Configure file preview
  (setq consult-fontify-preserve nil)
  (consult-customize
   consult-theme :preview-key '(:debounce 0.2 any)
   consult-ripgrep consult-git-grep consult-grep
   consult-bookmark consult-recent-file consult-xref
   :preview-key '(:debounce 0.4 any))
   ;; Note: Removed consult--source-* references (internal API, causes warnings)

  ;; Note: consult-buffer-sources customization removed - the defaults
  ;; (buffer, recent-file, project-recent-file) are exactly what we want,
  ;; and referencing the internal consult--source-* variables causes errors
  ;; with newer consult versions where they're not defined until first use
  )

;; Flexible text matching - replacement for ivy-prescient
(use-package orderless
  :custom
  (completion-styles '(orderless basic partial-completion flex))
  ;; initialism: "ornf" matches org-roam-node-find (kept from prescient filtering)
  (orderless-matching-styles '(orderless-literal orderless-regexp orderless-initialism))
  (completion-category-overrides '((file (styles partial-completion))
                                    (org-roam-node (styles basic orderless))))
  (tab-always-indent 'complete)  ;; TAB indents first, then completes
  (completion-cycle-threshold 3))  ;; Cycle through 3 or fewer candidates

;; Contextual actions - similar to ivy actions but more powerful
(use-package embark

  ;; Basic keybindings
  :bind
  (("C-." . embark-act)         ;; Main command to show actions
   ("C-;" . embark-dwim)        ;; Most likely action (Do What I Mean)
   ("C-h B" . embark-bindings)  ;; Alternative to describe-bindings
   :map minibuffer-local-map
   ("M-o" . embark-export)      ;; Export candidates to buffer
   ("C-c C-o" . embark-collect))  ;; Collect candidates in buffer

  :init
  ;; Replace the key help with Embark's helpful indicator
  (setq prefix-help-command #'embark-prefix-help-command)

  :config
  ;; Hide the mode line of the Embark live/completions buffers
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none))))


  ;; Custom actions for specific categories
  (defun embark-insert-relative-path (file)
    "Insert relative path to FILE."
    (interactive "fFile: ")
    (let ((path (file-relative-name file)))
      (insert path)))
      
  (defun embark-save-relative-path (file)
    "Save relative path to FILE in kill ring."
    (interactive "fFile: ")
    (kill-new (file-relative-name file)))
      
  ;; Add custom actions for file targets
  (define-key embark-file-map (kbd "r") #'embark-insert-relative-path)
  (define-key embark-file-map (kbd "R") #'embark-save-relative-path))

;; Enhanced Embark+Consult integration
(use-package embark-consult
  :after (embark consult)
  :hook
  (embark-collect-mode . consult-preview-at-point-mode)
  :config
  ;; Make embark-export work with consult-* commands
  (defun embark-export-consult-grep (lines)
    "Create a grep mode buffer with LINES."
    (let ((buffer (generate-new-buffer "*Embark Export Grep*")))
      (with-current-buffer buffer
        (insert (string-join lines "\n"))
        (grep-mode))
      (pop-to-buffer buffer)))

  ;; Register the consult-grep exporter for ripgrep, grep, git-grep, etc.
  (setf (alist-get 'consult-grep embark-exporters-alist) #'embark-export-consult-grep)
  (setf (alist-get 'consult-git-grep embark-exporters-alist) #'embark-export-consult-grep)
  (setf (alist-get 'consult-ripgrep embark-exporters-alist) #'embark-export-consult-grep))

;; Add nerd-icons to marginalia
(use-package nerd-icons-completion
  :after marginalia
  :hook (marginalia-mode . nerd-icons-completion-marginalia-setup)
  :config
  (nerd-icons-completion-mode))

;; Add a posframe-like UI for Vertico (if desired)
(use-package vertico-posframe
  :after vertico
  :config
  (setq vertico-posframe-parameters
        '((left-fringe . 8)
          (right-fringe . 8)))
  (setq vertico-posframe-poshandler #'posframe-poshandler-frame-center)
  (vertico-posframe-mode 1))

;; Add visual directory navigation
(use-package vertico-directory
  :after vertico
  :straight nil
  :bind (:map vertico-map
              ("RET" . vertico-directory-enter)
              ("DEL" . vertico-directory-delete-char)
              ("M-DEL" . vertico-directory-delete-word))
  :hook (rfn-eshadow-update-overlay . vertico-directory-tidy))

(global-set-key "\C-cy" 'consult-yank-pop)


(use-package pos-tip
  :defer 2)


(use-package nvm
  :defer 2)
(use-package js-comint
  :defer 2
  :config
  (require 'nvm)
  (js-do-use-nvm))

(use-package js2-mode
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
  :init
  (marginalia-mode)
  :bind
  (:map minibuffer-local-map
        ("M-A" . marginalia-cycle)))

(use-package ace-window
  :defer t  ; Lazy-load ace-window - only load when invoked
  :commands (ace-window)
  :bind
  ("M-o" . ace-window)
  :config
  (ace-window-display-mode)
  (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))
  :custom-face
  (aw-leading-char-face ((t (:height 3.0 :foreground "dodgerblue")))))

(use-package magit
  :defer t  ; Lazy-load magit - only load when git commands are used
  :commands (magit-status magit-dispatch magit-file-dispatch))

;; The console-next repo's prepare-commit-msg hook prefixes every commit
;; message with "BRANCH-NAME " (JIRA key + trailing space) before the
;; buffer is opened. git-commit-mode's own setup always lands point at
;; the very start of the buffer regardless of what's already on line 1,
;; so without this, point sits before the branch prefix instead of right
;; after it. Appended (not prepended) so it runs after git-commit-mode's
;; own setup rather than being immediately overridden by it.
(with-eval-after-load 'git-commit
  (add-hook 'git-commit-setup-hook
            (lambda ()
              (goto-char (line-end-position 1)))
            t))

(use-package git-timemachine
  :defer 2
  )
(use-package git-gutter
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
  :init
  (with-eval-after-load 'git-gutter (require 'git-gutter-fringe))
  )


(use-package persistent-scratch
  :config
  (persistent-scratch-setup-default))

(use-package treemacs-projectile
  :after treemacs projectile)
(use-package treemacs-magit
  :after treemacs magit)
(use-package treemacs
  :defer t  ; Lazy-load treemacs - only load when explicitly invoked
  :commands (treemacs treemacs-select-window)
  :config
  (setq treemacs-space-between-root-nodes nil)
  (treemacs-follow-mode t)
  (treemacs-filewatch-mode t)
  (treemacs-fringe-indicator-mode t)
  (doom-themes-treemacs-config)
  (setq doom-themes-treemacs-theme "doom-colors"))
;; M-0 stays text-scale-adjust (keys-config); use M-x treemacs-select-window

(use-package doom-themes
  :defer t  ; Lazy-load doom-themes - load after startup
  :init
  ;; Set theme early to avoid visual flashing
  (setq doom-themes-enable-bold t)
  (setq doom-themes-enable-italic t)
  :config
  ;; Disable done-headline fontification: doom-themes-ext-org uses match group 2
  ;; which is optional in current org's heading regexp, causing "No match 2" errors.
  (setq org-fontify-done-headline nil)
  (doom-themes-org-config)
  (require 'doom-themes-ext-org))
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes")
(add-to-list 'custom-theme-load-path "~/emacs/site")
(use-package hc-zenburn-theme
  :custom-face
  (region ((t (:background "DarkOliveGreen"))))
  (highlight ((t (:background "DarkSeaGreen4"))))
  (consult-highlight-match ((t (:background "DarkSeaGreen4"))))
  (consult-highlight-mark ((t (:background "DarkSeaGreen4"))))
  (lazy-highlight ((t (:background "DarkSeaGreen4")))))
(use-package solarized-theme
  :custom-face
  (region ((t (:background "DarkOliveGreen"))))
  (highlight ((t (:background "DarkSeaGreen4"))))
  (consult-highlight-match ((t (:background "DarkSeaGreen4"))))
  (consult-highlight-mark ((t (:background "DarkSeaGreen4"))))
  (lazy-highlight ((t (:background "DarkSeaGreen4")))));; (load-theme 'hc-zenburn t)
(load-theme 'solarized-wombat-dark t)

(use-package nerd-icons
  )
(use-package doom-modeline
  :config
  (setq doom-modeline-buffer-file-name-style 'buffer-name)
  (setq doom-modeline-env-enable-ruby nil)
  (setq doom-modeline-vcs-icon t)
  (setq doom-modeline-vcs-max-length 40)
  (setq doom-modeline-battery nil)
  (doom-modeline-mode 1))

;; gnutls loads automatically when needed
(setq starttls-use-gnutls t)
(setq auto-revert-check-vc-info nil)

(use-package ligature
  :straight (:local-repo "~/dev/git/ligature.el"
             :host github :repo "mickeynp/ligature.el")
  :defer 2  ; Defer ligature loading - not critical for startup
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


(use-package flymake
  :straight nil
  :hook ((prog-mode . flymake-mode)
         (text-mode . flymake-mode))
  :bind (:map flymake-mode-map
         ("M-n" . flymake-goto-next-error)
         ("M-p" . flymake-goto-prev-error)
         ("C-c ! l" . flymake-show-buffer-diagnostics)
         ("C-c ! n" . flymake-goto-next-error)
         ("C-c ! p" . flymake-goto-prev-error))
  :custom
  (flymake-no-changes-timeout 0.5)
  (flymake-start-on-save-buffer t)
  :config
  ;; Disable flymake in org-mode (no need for org-lint overhead)
  (add-hook 'org-mode-hook (lambda () (flymake-mode -1))))

;; ESLint support for JS/TS modes
(use-package flymake-eslint
  :hook ((js-mode js-ts-mode typescript-ts-mode jtsx-jsx-mode rjsx-mode)
         . flymake-eslint-enable))

;; Ruby: rubocop backend is built-in to Emacs 29+, eglot provides LSP diagnostics

(require 'server)
(unless (server-running-p) (server-start))

;; diminish removed: doom-modeline doesn't display minor modes

;; Defer ox-latex loading - only needed when exporting
(with-eval-after-load 'org (require 'ox-latex))
(use-package org
  :straight
  :demand t
  :custom-face
  (org-block ((t :inherit default
                 :extend t
                 :background "gray15"
                 :height 160 :family "Cascadia Code")))
  (org-block-begin-line ((t (:family "Cascadia Code" :italic t))))
  (org-variable-pitch-fixed-face ((t (:inherit 'org-block :extend t :family "Cascadia Code"))))
  :config
  (setq org-agenda-files  '("~/Documents/notes/todo.org" "~/Documents/org-roam/daily"))
  (setq org-startup-indented nil)
  (setq org-hide-emphasis-markers t)
  (setq org-default-notes-file "~/Documents/notes/notes.org")
  ;; Tag vocabulary for org-roam Zettelkasten workflow
  (setq org-tag-alist
        '(;; Content types (mutually exclusive)
          (:startgroup)
          ("permanent-note" . ?p)
          ("literature-note" . ?l)
          ("reference" . ?r)
          ("structure-note" . ?s)
          ("work-item" . ?w)
          ("meeting" . ?m)
          ("daily" . ?d)
          (:endgroup)
          ;; Domain tags
          ("console" . ?c)
          ("jenkins" . ?j)
          ("security" . ?S)
          ("governance" . ?g)
          ("infrastructure" . ?i)
          ("devx" . ?x)
          ("platform" . ?P)
          ;; Project tags
          ("agr" . ?a)
          ("slsa" . ?L)
          ("cortex" . ?C)
          ("backstage" . ?b)
          ("dora" . ?D)))
  (setq org-tag-persistent-alist org-tag-alist)
  (setq org-complete-tags-always-offer-all-agenda-files t)
  (add-to-list 'org-latex-classes
               '("novel" "\\documentclass{novel}"
                 (
                  "\\begin{ChapterStart}\\ChapterTitle{{%s} \\the\\value{novelcn}\\stepcounter{novelcn}}\\end{ChapterStart}"  "\\newline")               (
                  "\\QuickChapter[3em]{%s}"  "\\newline"
                  "\\begin{ChapterStart}\\ChapterTitle{%s}\\end{ChapterStart}"  "\\newline"
                  "\\begin{ChapterStart}\\ChapterTitle{%s}\\end{ChapterStart}"  "\\newline")))
  (setq org-latex-pdf-process
        '("latexmk -f -pdf -%latex  -shell-escape -interaction=nonstopmode -output-directory=%o %f")))

;; Defer org-capture loading - only needed when capturing
(with-eval-after-load 'org (require 'org-capture))
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

  (use-package ox-jira)
  ;; Defer org-habit loading
  (with-eval-after-load 'org (require 'org-habit))
  (setq org-habit-show-all-today t)
  (setq org-habit-show-habits t)
;; Buffer-local minor modes: hook them to org-mode. Calling them inside
;; `with-eval-after-load' only enabled them in whatever buffer was current
;; when org loaded (*scratch*), where org-appear's post-command handler
;; then ran the org parser on elisp ("rx '**' range error").
(use-package org-appear
  :hook (org-mode . org-appear-mode))
(use-package org-superstar
  :hook (org-mode . org-superstar-mode))
  (use-package org-modern
    :init
    (with-eval-after-load 'org (global-org-modern-mode)))
  (with-eval-after-load 'org
    (require 'org-modern)
    (require 'ox-md)
;;    (require 'ox-confluence)
    (require 'ox-jira))
  ;; org-variable-pitch removed — it fires face-checks on every fontification
  ;; and errors 300+ times per session when org-indent-mode is not active
  (add-hook 'org-agenda-finalize-hook #'org-modern-agenda)

(use-package biblio)
(use-package org-ref
  :after (biblio)
  :defer t  ; Changed from nil to t - lazy-load org-ref
  :config
  (setq org-ref-bibliography-notes "~/Documents/notes/bibnotes.org"
        org-ref-default-bibliography '("~/Documents/references.bib")
        org-ref-pdf-directory "~/Documents/pdf/"
        reftex-default-bibliography '("~/Documents/references.bib")
        org-ref-completion-library 'org-ref-capf
        org-cite-csl-styles-dir "~/Zotero/styles")
)


(setq org-latex-listings 'minted)
(add-to-list 'org-latex-packages-alist '("" "minted" t))

;; This is needed as of Org 9.2
(with-eval-after-load 'org (require 'org-tempo))

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
  :config
  (setq
   jiralib2-auth 'cookie
   jiralib2-url "https://jira2.workday.com"
   )
  (add-hook 'org-roam-capture-new-node-hook #'fg/jira-update-heading)
  (add-hook 'org-capture-before-finalize-hook #'fg/jira-update-heading)
  )
(use-package emacsql)

(use-package org-roam
  :after org
  :demand t
  :init
  (setq org-roam-v2-ack t)
  :custom
  (org-roam-directory "~/Documents/org-roam")
  (org-roam-file-exclude-regexp "#[^#]*#\\|\\.#")
  :config
  ;; Defer initial DB sync until Emacs is idle — avoids blocking startup
  (add-hook 'emacs-startup-hook
            (lambda () (run-with-idle-timer 4 nil #'org-roam-db-autosync-mode +1)))
  (setq org-roam-database-connector 'sqlite-builtin)

  ;; SQLite performance tuning for org-roam
  (defun my/org-roam-db-apply-pragmas ()
    "Apply performance pragmas to org-roam SQLite connection."
    (when (and (fboundp 'org-roam-db) (org-roam-db))
      (emacsql (org-roam-db) "PRAGMA journal_mode=WAL")
      (emacsql (org-roam-db) "PRAGMA synchronous=normal")
      (emacsql (org-roam-db) "PRAGMA cache_size=-32000")
      (emacsql (org-roam-db) "PRAGMA temp_store=memory")))

  (add-hook 'org-roam-db-autosync-mode-hook #'my/org-roam-db-apply-pragmas)

  ;; Batch DB updates instead of per-save
  (setq org-roam-db-update-on-save nil)

  (defvar my/org-roam-pending-files nil)

  (defun my/org-roam-queue-update ()
    (when (and (fboundp 'org-roam-file-p) (org-roam-file-p))
      (cl-pushnew (buffer-file-name) my/org-roam-pending-files :test #'equal)))

  (defun my/org-roam-flush-pending ()
    (when my/org-roam-pending-files
      (dolist (f my/org-roam-pending-files)
        (when (file-exists-p f)
          (org-roam-db-update-file f)))
      (setq my/org-roam-pending-files nil)))

  (add-hook 'after-save-hook #'my/org-roam-queue-update)
  (run-with-idle-timer 10 t #'my/org-roam-flush-pending)

  (setq org-roam-capture-templates
        '(("d" "default" plain "%?" :if-new
           (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n")
           :unnarrowed t)
          ("c" "region" plain "%i" :if-new
           (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n")
           :unnarrowed t)
          ("i" "Jira Issue" entry "* TODO ${title}\n:PROPERTIES:\n:JiraIssueKey: ${title}\n:END:\n"
           :if-new
           (file+head "%<%Y%m%d%H%M%S>-${slug}.org"
                      "#+title: ${title}\n#+filetags: :work-item:\n\n")
           :unnarrowed t)
          ;; Permanent Note (processed knowledge)
          ("p" "permanent note" plain
           "#+filetags: :permanent-note:\n\n* Summary\n%?\n\n* Key Insights\n\n* Connections\n- Related to:\n\n* Source\n%i"
           :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org"
                              "#+title: ${title}\n#+date: %t\n")
           :unnarrowed t)
          ;; Literature Note (from a specific source)
          ("l" "literature note" plain
           "#+filetags: :literature-note:\n\n* Source\n%^{Source URL or Reference}\n\n* Key Points\n- %?\n\n* My Interpretation\n\n* Actionable Items\n- [ ] "
           :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org"
                              "#+title: ${title}\n#+date: %t\n")
           :unnarrowed t)
          ;; Structure Note (MOC/topic index)
          ("s" "structure note" plain
           "#+filetags: :structure-note:\n\n* Overview\n%?\n\n* Key Concepts\n\n* Related Notes\n\n* Open Questions\n"
           :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org"
                              "#+title: ${title}\n#+date: %t\n")
           :unnarrowed t)
          ;; Meeting Note
          ("m" "meeting note" plain
           "#+filetags: :meeting:\n\n* Attendees\n%^{Attendees}\n\n* Agenda\n%?\n\n* Discussion\n\n* Action Items\n- [ ]\n\n* Decisions Made\n"
           :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org"
                              "#+title: Meeting: ${title}\n#+date: %t\n")
           :unnarrowed t)
          ;; Quick Reference (URL capture with minimal friction)
          ("q" "quick reference" plain
           "#+filetags: :reference:\n%?"
           :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org"
                              "#+title: ${title}\n#+date: %t\n")
           :unnarrowed t)
          ))
  (setq org-roam-capture-ref-templates
        '(("r" "ref" plain "#+filetags: :reference:\n%?"
           :target (file+head "%<%Y%m%d%H%M%S>-${slug}.org"
                              "#+title: ${title}\n#+date: %t\n")
           :jump-to-captured t
           :unnarrowed t)
          ;; Annotated reference (when you want to add notes immediately)
          ("a" "annotated ref" plain
           "#+filetags: :reference:\n\n* Summary\n%?\n\n* Key Takeaways\n-\n\n* Related To\n"
           :target (file+head "%<%Y%m%d%H%M%S>-${slug}.org"
                              "#+title: ${title}\n#+date: %t\n")
           :jump-to-captured t
           :unnarrowed t)))
  (setq org-roam-node-display-template
        (concat "${title:30} "
                (propertize "${tags:*}" 'face 'org-tag)))

  (setq org-roam-dailies-directory "daily/")
  (setq org-roam-completion-everywhere t)
  (setq org-roam-dailies-capture-templates
        '(("d" "default" entry
           "* %<%H:%M> %?"
           :target (file+head "%<%Y-%m-%d>.org"
                              "#+title: %<%Y-%m-%d>\n#+filetags: :daily:\n#+OPTIONS: ^:nil num:nil whn:nil toc:nil H:0 date:nil author:nil title:nil\n\n"))
          ("c" "region" entry
           "* %<%H:%M> %? %i"
           :target (file+head "%<%Y-%m-%d>.org"
                              "#+title: %<%Y-%m-%d>\n#+filetags: :daily:\n#+OPTIONS: ^:nil num:nil whn:nil toc:nil H:0 date:nil author:nil title:nil\n\n"))
          ("l" "link" entry
           "* %<%H:%M> [[%^{URL}][%^{Title}]]\n  %?"
           :target (file+head "%<%Y-%m-%d>.org"
                              "#+title: %<%Y-%m-%d>\n#+filetags: :daily:\n#+OPTIONS: ^:nil num:nil whn:nil toc:nil H:0 date:nil author:nil title:nil\n\n"))
          ;; Meeting in daily - ID on meeting headline for org-roam indexing
          ("m" "meeting" entry
           "* %<%H:%M> Meeting: %^{Meeting Title}\n:PROPERTIES:\n:ID:        %(org-id-new)\n:MEETING_WITH: %^{With}\n:END:\n** Discussed\n%?\n** Action Items\n- [ ] "
           :target (file+head "%<%Y-%m-%d>.org"
                              "#+title: %<%Y-%m-%d>\n#+filetags: :daily:\n#+OPTIONS: ^:nil num:nil whn:nil toc:nil H:0 date:nil author:nil title:nil\n\n"))
          ;; TODO/Task
          ("t" "task" entry
           "* TODO %?"
           :target (file+head "%<%Y-%m-%d>.org"
                              "#+title: %<%Y-%m-%d>\n#+filetags: :daily:\n#+OPTIONS: ^:nil num:nil whn:nil toc:nil H:0 date:nil author:nil title:nil\n\n"))
          ;; Insight to process later
          ("i" "insight" entry
           "* %<%H:%M> IDEA %?\n:PROPERTIES:\n:PROCESS: t\n:END:"
           :target (file+head "%<%Y-%m-%d>.org"
                              "#+title: %<%Y-%m-%d>\n#+filetags: :daily:\n#+OPTIONS: ^:nil num:nil whn:nil toc:nil H:0 date:nil author:nil title:nil\n\n"))
          ))

  ;;
;;CLI helper function for creating org-roam nodes from command line
  ;; Used by research-investigator agent to programmatically create nodes
  (defun create-org-roam-node-cli (title tags content)
    "Create org-roam node non-interactively from CLI.
TITLE is the node title, TAGS is a string like \":tag1:tag2:\", CONTENT is the body text."
    (let* ((slug (org-roam-node-slug
                  (org-roam-node-create :title title)))
           (timestamp (format-time-string "%Y%m%d%H%M%S"))
           (filename (expand-file-name
                      (format "%s-%s.org" timestamp slug)
                      org-roam-directory))
           (org-id-overriding-file-name filename)
           id)
      (with-temp-buffer
        (insert ":PROPERTIES:\n:ID:        \n:END:\n")
        (insert (format "#+title: %s\n" title))
        (when tags (insert (format "#+filetags: %s\n" tags)))
        (insert "\n" content)
        (goto-char 25)
        (setq id (org-id-get-create))
        (write-file filename)
        (org-roam-db-update-file filename))
      filename)))

;; org-remark: in-buffer highlighting/annotation, stored as a companion
;; notes org file next to the source (named via
;; `ari/org-remark-notes-file-name' in ari-custom.org: the source file's
;; name, snake_cased, plus "_notes.org", so several source files in the
;; same directory/session each get their own notes file instead of
;; sharing one). Keybindings follow the org-remark documentation's
;; suggested "C-c n" prefix (wired below via the `general' leader in the
;; General section).
(use-package org-remark
  :custom
  (org-remark-notes-file-name #'ari/org-remark-notes-file-name)
  :init
  (org-remark-global-tracking-mode +1))

;; org-wc: on-demand word counts as overlays next to headings, summed
;; over sub-headings. Not live -- re-run org-wc-display to refresh.
(use-package org-wc
  :defer t
  :commands (org-wc-display org-wc-remove-overlays
             org-word-count org-wc-count-subtrees))

(use-package org-ql
  :defer t)

;; Move org-roam CAPF to end so cheaper completions run first
(defun my/org-roam-capf-to-back ()
  "Reorder so org-roam-complete-everywhere runs last."
  (when (memq 'org-roam-complete-everywhere completion-at-point-functions)
    (setq-local completion-at-point-functions
                (append (remq 'org-roam-complete-everywhere
                              completion-at-point-functions)
                        '(org-roam-complete-everywhere)))))

(add-hook 'org-mode-hook #'my/org-roam-capf-to-back)

(defun ek/babel-ansi ()
  (when-let* ((beg (org-babel-where-is-src-block-result nil nil)))
    (save-excursion
      (goto-char beg)
      (when (looking-at org-babel-result-regexp)
        (let ((end (org-babel-result-end))
              (ansi-color-context-region nil))
          (ansi-color-apply-on-region beg end))))))
(add-hook 'org-babel-after-execute-hook 'ek/babel-ansi)
(use-package ox-twbs)
(use-package ox-gfm
  :after org)


(use-package org-mime)
(add-to-list 'org-src-lang-modes '("typescript" . javascript))
(setq org-src-fontify-natively t)
(setq org-src-tab-acts-natively t)
(setq org-src-window-setup 'current-window)
;; Use the `plantuml' executable (Homebrew / apt) rather than hunting for the jar.
(use-package plantuml-mode
  :mode ("\\.puml\\'" "\\.plantuml\\'")
  :custom
  (plantuml-default-exec-mode 'executable))
(setq org-plantuml-exec-mode 'plantuml)
(setq org-startup-with-inline-images t)
(add-hook 'org-babel-after-execute-hook 'org-redisplay-inline-images)

(setq org-todo-keywords
      '((
         sequence "TODO(t)" "STARTED(s)" "WAITING(w)" "|" "DONE(d)" "CANCELLED(c)")))
(setq org-agenda-include-diary t)
(setq org-agenda-include-all-todo t)

;; babel backends must be installed before org-babel-do-load-languages below
(use-package ob-typescript)
(use-package ob-cypher)
(use-package ob-aider :defer t)
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
     (dot . t)
     (typescript . t))))
(setq org-confirm-babel-evaluate nil)

(use-package ox-pandoc
  :defer 2
  :config
  (setq org-pandoc-options '((standalone . t)))
  (setq org-pandoc-command (or (executable-find "pandoc") "pandoc")))

 (use-package org-variable-pitch
   :after org
   )

(use-package olivetti
  :after org
  :config
  (setq olivetti-minimum-body-width 120))

(use-package virtualenvwrapper
  :defer 2
  :config
  (venv-initialize-interactive-shells)
  (venv-initialize-eshell)
  (setq venv-location "~/.virtualenvs"))


;; Final startup timing
(ari/startup-timer "config-complete")

;; ari/display-startup-timing defined in ari-custom.el

;; Reset settings after startup for better runtime performance
(add-hook 'emacs-startup-hook
          (lambda ()
            ;; Re-enable message log for runtime (GC reset lives in early-init.el)
            (setq message-log-max 1000)))

(setq org-mime-export-options '(:section-numbers nil
                                                 :with-author nil
                                                 :with-toc nil))






(use-package exec-path-from-shell
  :config
  (setq exec-path-from-shell-check-startup-files nil)
  (setq exec-path-from-shell-variables '("PATH" "ANTHROPIC_API_KEY" "ARTIFACTORY_PASSWORD" "ARTIFACTORY_USER"))
  (setq exec-path-from-shell-arguments '("-l"))  ; drop -i: interactive shell loads full .zshrc (~5s)
  (when (memq window-system '(mac ns x))
    (exec-path-from-shell-initialize)))

;; Homebrew on Apple Silicon: make sure /opt/homebrew/bin is searched first,
;; even when the login shell's PATH (imported above) doesn't include it.
(when (file-directory-p "/opt/homebrew/bin")
  (add-to-list 'exec-path "/opt/homebrew/bin"))



(with-eval-after-load 'org (require 'org-crypt))
(org-crypt-use-before-save-magic)
(setq org-tags-exclude-from-inheritance (quote("crypt")))
;; First secret key's short ID (what the old gpg|grep|ggrep pipeline found),
;; looked up in-process via epg and off the startup path.
(defun ari/org-crypt-default-key ()
  "Short (8 hex) ID of the first secret GPG key, or nil."
  (require 'epg)
  (when-let* ((key (car (epg-list-keys (epg-make-context 'OpenPGP) nil t)))
              (sub (car (epg-key-sub-key-list key))))
    (substring (epg-sub-key-id sub) -8)))
(run-with-idle-timer 3 nil (lambda () (setq org-crypt-key (ari/org-crypt-default-key))))

(use-package yaml-mode
:mode "\\.ya?ml\\'")

(use-package org-roam-ui :after org-roam :defer t)   ; C-c z u
(use-package org-contrib :defer t)
(use-package orgit :after magit :defer t)
(use-package consult-project-extra :defer t)
(use-package terraform-mode :defer t)
(use-package eglot-java :defer t)
(use-package visual-fill :defer t)
(use-package ghostel :defer t)                        ; see ari-custom fix



(use-package inf-ruby
  :defer 2)
(use-package ruby-electric
  :defer t)
(use-package feature-mode
  :defer 2
  :config
  (setq feature-use-docker-compose nil)
  (setq feature-rake-command "cucumber --format progress {OPTIONS} {feature}"))

(use-package yasnippet
  :defer 2
  :config
  (yas-global-mode t))
(use-package yasnippet-snippets
  :defer 2)
(use-package rake
  :defer 2)
(use-package inflections
  :defer 2)
(use-package graphql
  :defer 2)
;; Defer org-protocol and org-roam-protocol
(with-eval-after-load 'org (require 'org-protocol))
(with-eval-after-load 'org-roam (require 'org-roam-protocol))
(use-package haml-mode
  :defer 2)
;; pulse.el (built-in) replaces beacon - flash line on jumps
(require 'pulse)
(setq pulse-iterations 10)
(setq pulse-delay 0.04)
(defun pulse-line (&rest _)
  "Pulse the current line."
  (pulse-momentary-highlight-one-line (point)))
(dolist (cmd '(scroll-up-command scroll-down-command
               recenter-top-bottom other-window
               ace-window windmove-up windmove-down
               windmove-left windmove-right))
  (advice-add cmd :after #'pulse-line))
(use-package rainbow-mode
  :defer 2)
(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

;; ari/validate-config-files defined in ari-custom.el

;; Load essential configs immediately
(require 'keys-config-new)
(require 'ari-custom-new)

;; Defer non-essential configs to after startup
(run-with-idle-timer 2 nil (lambda ()
  (require 'ruby-config-new)
  (require 'erc-config)
  (require 'gnus-config)
  (require 'mail-config-new)
  (require 'blog)))



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

(use-package corfu
  :custom
  (corfu-auto t)
  (corfu-auto-delay 0.3)
  (corfu-auto-prefix 2)
  (corfu-min-width 80)
  (corfu-max-width corfu-min-width)
  (corfu-count 10)
  (corfu-scroll-margin 5)
  (corfu-cycle t)
  :bind
  (:map corfu-map
        ("C-n" . corfu-next)
        ("C-p" . corfu-previous)
        ("<tab>" . corfu-insert)
        ("TAB" . corfu-insert))
  :init
  (global-corfu-mode)
  (defun corfu-enable-in-minibuffer ()
    "Enable Corfu in the minibuffer if `completion-at-point' is bound."
    (when (where-is-internal #'completion-at-point (list (current-local-map)))
      (corfu-mode 1)))
  (add-hook 'minibuffer-setup-hook #'corfu-enable-in-minibuffer)
  )

(use-package corfu-popupinfo
  :after corfu
  :straight nil
  :hook (corfu-mode . corfu-popupinfo-mode)
  :custom
  (corfu-popupinfo-delay '(0.5 . 0.2))
  :bind (:map corfu-map
              ("M-p" . corfu-popupinfo-scroll-down)
              ("M-n" . corfu-popupinfo-scroll-up)
              ("C-M-d" . corfu-popupinfo-toggle)))

;; Icons for corfu - nerd-icons-corfu uses nerd font glyphs consistent with
;; doom-modeline and nerd-icons-completion (replaces kind-icon/SVG approach)
(use-package nerd-icons-corfu
  :after corfu
  :config
  (add-to-list 'corfu-margin-formatters #'nerd-icons-corfu-formatter))

;; orderless configured in the vertico/consult section above

;; Enhance completion at point via cape
(use-package cape
  :init
  ;; cape-file globally (fast, path-based, no buffer scanning)
  (add-to-list 'completion-at-point-functions #'cape-file)
  ;; cape-dabbrev and cape-keyword scoped to prog-mode only
  (add-hook 'prog-mode-hook
            (lambda ()
              (add-to-list 'completion-at-point-functions #'cape-dabbrev)
              (add-to-list 'completion-at-point-functions #'cape-keyword))))

;; dabbrev scans only current buffer — with 250+ org-roam buffers open,
;; scanning all buffers on every completion trigger causes multi-second lag
(setq dabbrev-check-all-buffers nil)
(setq dabbrev-check-other-buffers nil)










;;   (setq lsp-auto-guess-root nil)


(use-package project
  :straight nil  ;; Built into Emacs
  :config
  ;; Project switching should open dired, like your projectile config
  (setq project-switch-commands 'project-dired)
  
  ;; Customize project.el
  (setq project-switch-use-entire-map t)
  
  ;; Add project root detection for common VCS beyond Git
  (add-to-list 'project-find-functions #'project-try-vc)
  
  ;; Project completion uses the same system as your other completions
  (setq project-read-file-name-function #'project--read-file-cpd-relative)
  
  ;; Global keybinding for project commands (similar to your C-c p for projectile)
  :bind-keymap
  ("C-c p" . project-prefix-map))

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

(use-package web-mode
  :defer t)

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






(add-hook 'sql-mode-hook 'my-sql-mode-hook)
(defun my-sql-mode-hook()
  (message "SQL mode hook executed")
  (define-key sql-mode-map [f5] 'sql-send-buffer))

;; Defer SQL - only load when needed
(autoload 'sql-mode "sql" "SQL editing mode" t)

;; SQL configuration - deferred until sql is loaded
(with-eval-after-load 'sql
  (setq sql-ms-program "osql")
  (setq sql-mysql-program "mysql")
  (setq sql-pop-to-buffer-after-send-region nil)
  (setq sql-product (quote ms))
  (setq sql-mysql-login-params (append sql-mysql-login-params '(port))))


  (use-package rjsx-mode
    :defer 2)
  ;; NOTE: eglot-ensure hooks moved to main eglot configuration (line ~2189)
  ;; to avoid duplicate hook registration which can cause font-locking issues

  (use-package jtsx
    :hook((jtsx-jsx-mode . emmet-mode)(jtsx-jsx-mode . prettier-js-mode))
    )
  (use-package prettier-js
    :hook ((js2-mode rjsx-mode jtsx-jsx-mode) . prettier-js-mode))

(setq emmet-expand-jsx-className? t)

(use-package emmet-mode
  :defer t
  :config
  (add-to-list 'emmet-jsx-major-modes 'jtsx-jsx-mode))

(use-package deft
  :bind ("<f8>" . deft)
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
  (setq deft-text-mode 'org-mode))

;; Loaded at startup on purpose (with org-roam, the only eager note tools).
;; The xapian backend binary is built in the checkout (see notdeft docs).
(use-package notdeft
  :straight (:local-repo "~/dev/git/notdeft"
             :host github :repo "hasu/notdeft")
  :demand t
  :bind ("<f9>" . notdeft)
  :init
  (setq notdeft-directory "~/Documents/org-roam/"
        notdeft-directories '("~/Documents/org-roam/")
        notdeft-xapian-program
        (expand-file-name "~/dev/git/notdeft/xapian/notdeft-xapian")))

(use-package cypher-mode
  :defer t)

;; Use executable-find for cross-platform compatibility
(when-let* ((cypher-shell-path (executable-find "cypher-shell")))
  (setq n4js-cli-program cypher-shell-path))
(setq n4js-cli-arguments '("-u" "neo4j"))
(setq n4js-pop-to-buffer t)
(with-eval-after-load 'n4js
  (require 'cypher-mode)
  (setq n4js-font-lock-keywords cypher-font-lock-keywords))

(use-package which-key
  :straight nil
  :init
  (which-key-mode)
  :config
  (setq which-key-idle-delay 1))


(use-package helpful
  :defer t  ; Lazy-load helpful - only load when help commands are used
  :commands (helpful-callable helpful-variable helpful-key helpful-at-point)
  :config
  (setq help-enable-symbol-autoload t)     ;; Show symbols that can be autoloaded
  (setq help-enable-completion-autoload t));; Enable completion for autoloadable symbols

(use-package elfeed
  :commands elfeed
  :config
  ;; Org-link functions (keep for org-roam integration)
  (defun elfeed-link-title (entry)
    "Copy the entry title and URL as org link to the clipboard."
    (interactive)
    (let* ((link (elfeed-entry-link entry))
           (title (elfeed-entry-title entry))
           (titlelink (concat "[[" link "][" title "]]")))
      (when titlelink
        (kill-new titlelink)
        (gui-set-selection 'PRIMARY titlelink)
        (message "Yanked: %s" titlelink))))

  (defun elfeed-show-link-title ()
    "Copy the current entry title and URL as org link to the clipboard."
    (interactive)
    (elfeed-link-title elfeed-show-entry))

  (defun elfeed-show-quick-url-note ()
    "Capture entry link to org-roam daily."
    (interactive)
    (elfeed-link-title elfeed-show-entry)
    (org-roam-dailies-capture-today nil "l")
    (yank)
    (org-capture-finalize)))

(use-package elfeed-org
  :after elfeed
  :config
  (setq rmh-elfeed-org-files (list "~/.emacs.d/elfeed.org"))
  (elfeed-org))



;; Cleaner article view: no line numbers, sans font
(add-hook 'elfeed-show-mode-hook
          (lambda ()
            (display-line-numbers-mode -1)
            (face-remap-add-relative 'default '(:family "Sans Serif"))))

;; Restore keybindings after nano-elfeed sets its own
(with-eval-after-load 'elfeed
  (bind-keys :map elfeed-show-mode-map
             ("l" . elfeed-show-link-title)
             ("v" . elfeed-show-quick-url-note)
             ("r" . ari/elfeed-capture-to-roam)))

(use-package prescient
  :config
  (prescient-persist-mode 1))

;; prescient only sorts (recency/frequency); orderless does the matching,
;; so the minibuffer and corfu use the same rules and orderless's
;; !exclude / &annotation / =literal prefixes work.
(use-package vertico-prescient
  :after vertico
  :custom
  (vertico-prescient-enable-filtering nil)
  :config
  (vertico-prescient-mode 1))



(use-package general
  :config
  (general-create-definer my-leader-def
    :prefix "C-c")
  (my-leader-def
    "t" 'project-find-file  ;; Was 'fzf-projectile
    "g" '(:ignore t :which-key "rspec")
    "gp" '(inf-ruby-switch-from-compilation :which-key "enter debugger")
    "ga" '(rspec-verify-all :which-key "run all specs")
    "gs" '(rspec-verify-single :which-key "run single spec")
    "gr" '(rspec-rerun :which-key "rerun spec")
    "gf" '(rspec-run-last-failed :which-key "rerun last failed")
    "r"  '(:ignore t :which-key "inf-ruby")
    "rb" '(ruby-send-buffer :which-key "ruby-send-buffer")
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
    "ztr" '(org-roam-tag-remove :which-key "roam-tag-remove")
    "zr"  '(:ignore t :which-key "roam-ref")
    "zra" '(org-roam-ref-add :which-key "roam-ref-add")
    "zrr" '(org-roam-ref-remove :which-key "roam-ref-remove")
    "zb"  '(org-roam-buffer-toggle :which-key "roam-buffer-toggle")
    ;; New Zettelkasten workflow keybindings
    "zp"  '(ari/promote-reference-to-permanent :which-key "promote to permanent")
    "zu"  '(org-roam-ui-mode :which-key "roam-ui")
    "zw"  '(ari/find-unprocessed-insights :which-key "weekly review")
    "zO"  '(ari/find-orphan-references :which-key "orphan refs")
    "zc"  '(ari/find-connected-references :which-key "connected refs")
    "q" '(:ignore t :which-key "copilot")
    "qa" '(copilot-accept-completion :which-key "copilot-accept-completion")
    "qd" '(copilot-diagnose :which-key "copilot-diagnose")
    "ql" '(copilot-accept-completion-by-line :which-key "copilot-accept-completion-by-line")
    "qw" '(copilot-accept-completion-by-word :which-key "copilot-accept-completion-by-word")
    "qp" '(copilot-previous-completion :which-key "copilot-previous-completion")
    "qn" '(copilot-next-completion :which-key "copilot-next-completion")
    "e" '(:ignore t :which-key "eglot")
    "ef" '(eglot-format :which-key "format buffer")
    "ea" '(eglot-code-actions :which-key "code actions")
    "er" '(eglot-rename :which-key "rename")
    "ed" '(xref-find-definitions :which-key "find definition")
    "eR" '(eglot-reconnect :which-key "reconnect")
    "eq" '(eglot-shutdown :which-key "shutdown server")
    "s" '(:ignore t :which-key "search")
    "sl" '(consult-line :which-key "search line")
    "sL" '(consult-line-multi :which-key "search line in buffers")
    "so" '(consult-outline :which-key "search outline")
    "sm" '(consult-mark :which-key "search marks")
    "si" '(consult-imenu :which-key "search imenu")
    "sI" '(consult-imenu-multi :which-key "search imenu in buffers")
    "sr" '(consult-ripgrep :which-key "ripgrep")
    "sR" '(my/consult-ripgrep-at-point :which-key "ripgrep symbol")
    ;; org-remark: mirrors the "C-c n ..." bindings suggested in the
    ;; org-remark documentation (my-leader-def's prefix is already "C-c").
    "n" '(:ignore t :which-key "org-remark")
    "nm" '(org-remark-mark :which-key "mark")
    "no" '(org-remark-open :which-key "open")
    "n]" '(org-remark-view-next :which-key "view next")
    "n[" '(org-remark-view-prev :which-key "view prev")
    "nr" '(org-remark-remove :which-key "remove")
    "nc" '(org-remark-change :which-key "change")
    "nt" '(org-remark-toggle :which-key "toggle")
    "nv" '(org-remark-view :which-key "view")
    "nE" '(ari/org-remark-export-to-latex-with-margin-notes :which-key "export margin-note PDF")
    "nz" '(ari/strip-zero-width-spaces :which-key "strip zero-width spaces (buffer/region)")
    "nZ" '(ari/strip-zero-width-spaces-file :which-key "strip zero-width spaces (file...)")
    "N" '(:ignore t :which-key "org-noter")
    "Ne" '(ari/org-noter-export-annotated-pdf :which-key "export annotated PDF")
    ;; "w" is taken globally by compare-windows (C-c w), so word-count
    ;; lives under "c" instead.
    "c" '(:ignore t :which-key "word count")
    "cd" '(org-wc-display :which-key "display counts")
    "cr" '(org-wc-remove-overlays :which-key "remove overlays")
    "cc" '(org-word-count :which-key "count region/buffer")
    "cs" '(org-wc-count-subtrees :which-key "count subtrees (property)")))

    (use-package copilot
      :straight (:host github :repo "copilot-emacs/copilot.el"
                 :branch "main"
                :files ("*.el" (:exclude "copilot-chat.el")))
      :defer 10  ; Defer copilot loading
      :config
      ;; you can utilize :map :hook and :config to customize copilot
      (define-key copilot-completion-map (kbd "<tab>") 'copilot-accept-completion))

  (use-package gptel-aibo
    :straight (:host github :repo "dolmens/gptel-aibo"
               :branch "main")
    :after gptel
    :defer 15)  ; Defer gptel-aibo loading

  (use-package shell-maker
    :straight (:host github :repo "xenodium/shell-maker" :files ("*.el")))

  (use-package chatgpt-shell
    :straight (:host github :repo "xenodium/chatgpt-shell" :files ("*.el"))
    :custom
    ;; Functions, so ~/.authinfo.gpg is only decrypted on first use
    (chatgpt-shell-openai-key
     (lambda () (auth-source-pick-first-password :host "api.openai.com" :user "apikey")))
    (chatgpt-shell-anthropic-key
     (lambda () (auth-source-pick-first-password :host "api.anthropic.com" :user "apiKey")))
    :config
    (require 'chatgpt-shell)
    ;; Default to local Ollama (free); API models remain selectable.
    ;; Model list is queried from Ollama, so no-op if it isn't running.
    (ignore-errors (chatgpt-shell-ollama-load-models))
    (setq chatgpt-shell-model-version "qwen3.5-9b-mlx-64k")) ; chatgpt-shell drops ":latest"
(use-package acp
  :defer t)  ; required by agent-shell
(use-package agent-shell
  ;; Built from the local checkout (whatever branch is checked out there);
  ;; straight clones it to ~/dev/git on a machine that doesn't have it.
  :straight (:local-repo "~/dev/git/agent-shell"
             :host github :repo "xenodium/agent-shell")
  :defer t
  :config
  (setq agent-shell-anthropic-authentication
        (agent-shell-anthropic-make-authentication :login t))
  (setq agent-shell-anthropic-claude-acp-command
        (list (expand-file-name "~/n/bin/claude-agent-acp")))
  (setq agent-shell-session-strategy 'new)
  ;; n (node version manager) puts node in ~/n/bin, but Emacs's subprocess
  ;; PATH comes from a login shell that sources .zprofile, not .zshrc.
  ;; Inject n/bin so the claude-agent-acp shebang (#!/usr/bin/env node) resolves.
  (setq agent-shell-anthropic-claude-environment
        (list (format "PATH=%s:%s"
                      (expand-file-name "~/n/bin")
                      (getenv "PATH")))))

  (use-package gptel
    :defer t
    :config
    (add-hook 'gptel-post-stream-hook 'gptel-auto-scroll))

  (use-package copilot-chat
    :straight (:host github :repo "chep/copilot-chat.el"
               :branch "master"
               :files ("*.el"))
    :after copilot
    :defer 15
    :custom
    (copilot-chat-backend 'curl)
    (copilot-chat-frontend 'org)
    (copilot-chat-default-model "claude-sonnet-5")
    (copilot-chat-model-ignore-picker t))



  ;; gptel presets - named bundles of model + system prompt + settings
  ;; Apply with @preset-name in a prompt, or via the transient menu
  (with-eval-after-load 'gptel
    (gptel-make-ollama "Ollama"
      :host "localhost:11434"
      :stream t
      :models '(qwen3.5-9b-mlx-64k:latest    ; `ollama list'
                qwen3.5:9b-mlx
                gemma4-12b-64k:latest))
    ;; Key as a function: looked up (and authinfo decrypted) on first use
    (gptel-make-gemini "Gemini"
      :key #'gptel-api-key-from-auth-source
      :stream t)
    (gptel-make-preset 'coding
      :description "Code generation and review"
      :system "You are an expert programmer. Provide concise, correct code with brief explanations. Prefer idiomatic solutions. No unnecessary prose."
      :temperature 0.2)
    (gptel-make-preset 'writing
      :description "Writing and documentation"
      :system "You are a helpful writing assistant. Be clear and concise, maintaining the author's voice."
      :temperature 0.7)
    ;; Default: Claude via the GitHub Copilot subscription (no per-token
    ;; cost). First use runs `gptel-gh-login' (GitHub device-code flow).
    ;; Ollama and Gemini stay available from the gptel menu.
    (setq gptel-backend (gptel-make-gh-copilot "Copilot")
          gptel-model 'claude-sonnet-5))

    (use-package ob-chatgpt-shell
      :straight t)
    (require 'ob-chatgpt-shell)
    (ob-chatgpt-shell-setup)
  (use-package aidermacs
  :bind (("C-c a" . aidermacs-transient-menu))
  :custom
  ;; Local Ollama model (free). `aidermacs-use-architect-mode' is obsolete.
  (aidermacs-default-model "ollama_chat/qwen3.5-9b-mlx-64k:latest"))

  ;; aider removed - aidermacs above is the active aider integration

(use-package magit-delta
  :hook
  (magit-mode . magit-delta-mode))

(use-package popper
  :bind (("C-`"   . popper-toggle)
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
(add-hook 'prog-mode-hook #'blamer-mode)

(use-package svg-tag-mode
  :straight t
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

;; Tree-sitter packages removed - using built-in treesit in Emacs 29+
;; See Programming Languages section below for modern treesit configuration

(use-package pdf-tools
  :magic ("%PDF" . pdf-view-mode)
  :config
  (pdf-tools-install :no-query)
  (setq-default pdf-view-display-size 'fit-page)
  (setq pdf-annot-activate-created-annotations t)
  (add-hook 'pdf-view-mode-hook (lambda() (display-line-numbers-mode -1)))
  (defun ari/pdf-print-buffer ()
    "Save any pending annotations and print the current PDF."
    (interactive)
    (unless (derived-mode-p 'pdf-view-mode)
      (user-error "Not in a pdf-view buffer"))
    (when (buffer-modified-p)
      (pdf-view-save-buffer))
    (let ((file (buffer-file-name)))
      (unless file
        (user-error "Buffer has no associated file"))
      (start-process "pdf-print" nil "lpr" file)
      (message "Sent %s to the printer" file)))
  (define-key pdf-view-mode-map (kbd "C-c C-a p") #'ari/pdf-print-buffer))

  (defun ari/org-noter-notes-name-no-spaces (document-path)
    "Suggest a snake_case notes file name for DOCUMENT-PATH.
Uses the same `ari/snake-case-string' helper (ari-custom.org) that
org-remark's notes file naming uses, so both packages produce
consistently-named companion notes files."
    (concat (ari/snake-case-string (file-name-base document-path)) ".org"))

  (defun ari/org-noter--auto-answer-new-notes-prompts (orig-fn prompt collection &rest args)
    "Auto-answer org-noter's new-notes-file prompts instead of asking:
always pick the snake_case name, and always save next to the document
(the first, and after `org-noter-notes-search-path' is empty, only
directory org-noter itself offers)."
    (cond
     ((string-prefix-p "What name do you want the notes to have?" prompt)
      (or (seq-find (lambda (name) (string-match-p "\\`[a-z0-9]+\\(_[a-z0-9]+\\)*\\.org\\'" name))
                    collection)
          (apply orig-fn prompt collection args)))
     ((string-prefix-p "Where do you want to save it?" prompt)
      (if (consp collection) (car collection) (apply orig-fn prompt collection args)))
     (t (apply orig-fn prompt collection args))))

  (use-package org-noter
    :commands org-noter
    :hook (org-noter-find-additional-notes-functions . ari/org-noter-notes-name-no-spaces)
    :config
    (setq org-noter-notes-search-path nil
          org-noter-default-notes-file-names nil
          org-noter-auto-save-last-location t
          org-noter-always-create-frame nil
          org-noter-doc-split-fraction '(0.6 . 0.4)
          ;; Capture a HIGHLIGHT property (page + region coords) on any note
          ;; taken with an active selection, so the annotated-PDF export can
          ;; mark the passage - see below for why the actual PDF-mutating
          ;; half of this feature is turned back off.
          org-noter-highlight-selected-text t)
    ;; org-noter-pdf's own handler for `org-noter-highlight-selected-text'
    ;; calls `pdf-annot-add-highlight-markup-annotation', which writes a real
    ;; annotation into the *original* PDF - not wanted here (the original
    ;; must never be touched; see the pdf-tools section). Removing this
    ;; hook keeps the HIGHLIGHT property (still stored by
    ;; `org-noter-insert-note' itself, independent of this hook) without the
    ;; write; `ari/org-noter-export-annotated-pdf' draws the mark instead,
    ;; only in the exported copy.
    (remove-hook 'org-noter--add-highlight-hook #'org-noter-pdf--highlight-location)
    ;; Precise notes (region-anchored, real position on the page) are the
    ;; default for "i" - plain page-only notes carry no position an
    ;; annotated-PDF export could use (see `ari/org-noter-export-annotated-pdf'
    ;; in ari-custom.org), so swap the usual plain/precise assignment.
    (define-key org-noter-doc-mode-map (kbd "i") #'org-noter-insert-precise-note)
    (define-key org-noter-doc-mode-map (kbd "I") #'org-noter-insert-note)
    (advice-add 'completing-read :around #'ari/org-noter--auto-answer-new-notes-prompts))

(use-package discover
  :defer t)

(use-package mastodon
  :commands mastodon
  :config
  (setq mastodon-active-user "AriT93")
  (setq mastodon-instance-url "https://mastodon.social")
  (mastodon-discover))

(use-package auctex)
(use-package procress
  :commands procress-auctex-mode
  :init
  (add-hook 'LaTeX-mode-hook #'procress-auctex-mode)
  (add-hook 'LaTeX/P-mode-hook #'procress-auctex-mode)

  :config
  (setq TeX-command-extra-options "-shell-escape")
  (procress-load-default-svg-images))

(use-package eglot
  :straight nil
  :hook ((python-ts-mode . eglot-ensure)
         (java-mode . eglot-ensure)
         (go-ts-mode . eglot-ensure)
         (js-ts-mode . eglot-ensure)
         (typescript-ts-mode . eglot-ensure)
         (tsx-ts-mode . eglot-ensure)
         (rust-ts-mode . eglot-ensure)
         (c-ts-mode . eglot-ensure)
         (c++-ts-mode . eglot-ensure)
         ;; Keep legacy modes for compatibility
         (python-mode . eglot-ensure)
         (go-mode . eglot-ensure)
         (js2-mode . eglot-ensure)
         (js-mode . eglot-ensure)
         (rjsx-mode . eglot-ensure)
         (jtsx-jsx-mode . eglot-ensure))
         ;; NOTE: Removed (text-mode . eglot-ensure) - this was causing
         ;; eglot to start on ALL text files (org, markdown, etc.), which
         ;; would fail and interfere with font-locking
  :config
  ;; Performance settings
  (setq eglot-events-buffer-size 0) ;; Don't keep events buffer
  (setq eglot-sync-connect nil)     ;; Don't block when connecting
  (setq eglot-autoshutdown t)       ;; Shutdown unused servers
  (setq eglot-connect-timeout 10)   ;; Extend timeout for large projects

  ;; Better completion and diagnostics
  (setq eglot-send-changes-idle-time 0.5)
  (setq eglot-ignored-server-capabilities '(:inlayHintProvider))
  
  ;; Additional performance optimizations
  (setq eglot-extend-to-xref t)
  (setq eglot-prefer-plaintext t)
  
  ;; Better error handling and logging
  (setq eglot-confirm-server-initiated-edits nil)
  
  ;; Completion improvements
  (setq eglot-completion-at-point-function #'eglot-completion-at-point)

  ;; Semantic token highlighting (built-in in recent eglot, enable explicitly)
  (when (fboundp 'eglot-semantic-tokens-mode)
    (add-hook 'eglot-managed-mode-hook #'eglot-semantic-tokens-mode))

  ;; Key bindings
  :bind (:map eglot-mode-map
              ("C-c l f" . eglot-format)
              ("C-c l a" . eglot-code-actions)
              ("C-c l r" . eglot-rename)
              ("C-c l d" . xref-find-definitions)))

;; Add EGLOT for Ruby tree-sitter mode if available
(when (fboundp 'ruby-ts-mode)
  (add-hook 'ruby-ts-mode-hook #'eglot-ensure))

;; Fallback for traditional ruby-mode if tree-sitter not available
(unless (fboundp 'ruby-ts-mode)
  (add-hook 'ruby-mode-hook #'eglot-ensure))

;; flymake configured in main section; eglot hooks it automatically

;; Enhanced eldoc configuration
(setq eldoc-echo-area-use-multiline-p t)
(setq eldoc-echo-area-display-truncation-message nil)
(setq eldoc-echo-area-prefer-doc-buffer t)

;; Better EGLOT integration with consult and embark
(use-package consult-eglot
  :after (eglot consult)
  :config
  ;; Use consult for EGLOT commands
  (setq eglot-completion-at-point-function #'eglot-completion-at-point)
  
  ;; Better xref integration
  (setq xref-show-definitions-function #'xref-show-definitions-completing-read))

;; EGLOT code actions with embark (main embark block is in the completion section)
(with-eval-after-load 'embark
  (add-to-list 'embark-keymap-alist '(eglot-mode . embark-eglot-map)))

;; Better language server fallbacks and error handling
(defun eglot-safe-ensure ()
  "Safely ensure EGLOT is enabled with error handling."
  (interactive)
  (condition-case err
      (eglot-ensure)
    (error
     (message "EGLOT failed to start: %s" (error-message-string err))
     (message "Flymake will provide diagnostics instead"))))

;; Custom EGLOT hooks for better integration
(defun eglot-setup-completion ()
  "Set up completion for EGLOT buffers."
  (setq-local completion-styles '(basic partial-completion emacs22))
  (setq-local completion-category-defaults nil)
  (setq-local completion-category-overrides '((eglot (styles basic partial-completion)))))

(add-hook 'eglot-managed-mode-hook #'eglot-setup-completion)

;; Built-in Tree-sitter Configuration (Emacs 29+)
;; Maximum syntax highlighting detail
(setq treesit-font-lock-level 4)

;; Use treesit-auto for automatic grammar installation and mode management
(use-package treesit-auto
  :demand t
  :config
  ;; Detect ABI version and use appropriate grammar revisions
  ;; ABI 14 (tree-sitter 0.20.x) -> v0.20.x grammars
  ;; ABI 15+ (tree-sitter 0.22+) -> v0.23.3 grammars
  (let* ((abi-version (and (fboundp 'treesit-library-abi-version)
                           (treesit-library-abi-version nil)))
         (use-legacy (and abi-version (<= abi-version 14)))
         (grammar-rev (if use-legacy "v0.20.4" "v0.23.3"))
         (bash-rev (if use-legacy "v0.20.5" "v0.23.3"))
         (c-rev (if use-legacy "v0.20.6" "v0.23.3"))
         (cpp-rev (if use-legacy "v0.20.0" "v0.23.3"))
         (css-rev (if use-legacy "v0.19.0" "v0.23.3"))
         (go-rev (if use-legacy "v0.20.0" "v0.23.3"))
         (html-rev (if use-legacy "v0.20.0" "v0.23.3"))
         (java-rev (if use-legacy "v0.20.2" "v0.23.3"))
         (js-rev (if use-legacy "v0.20.1" "v0.23.3"))
         (json-rev (if use-legacy "v0.20.2" "v0.23.3"))
         (ruby-rev (if use-legacy "v0.20.0" "v0.23.3"))
         (rust-rev (if use-legacy "v0.20.4" "v0.23.3"))
         (ts-rev (if use-legacy "v0.20.3" "v0.23.3"))
         (yaml-rev (if use-legacy "v0.5.0" "v0.5.0")))

    (message "Tree-sitter ABI version: %s, using %s grammars"
             abi-version (if use-legacy "legacy (0.20.x)" "modern (0.23.3)"))

    (setq treesit-auto-recipe-list
          (list
           (make-treesit-auto-recipe
            :lang 'bash
            :ts-mode 'bash-ts-mode
            :remap 'sh-mode
            :url "https://github.com/tree-sitter/tree-sitter-bash"
            :revision bash-rev
            :ext "\\.sh\\'")
           (make-treesit-auto-recipe
            :lang 'c
            :ts-mode 'c-ts-mode
            :remap 'c-mode
            :url "https://github.com/tree-sitter/tree-sitter-c"
            :revision c-rev
            :ext "\\.c\\'")
           (make-treesit-auto-recipe
            :lang 'cpp
            :ts-mode 'c++-ts-mode
            :remap 'c++-mode
            :url "https://github.com/tree-sitter/tree-sitter-cpp"
            :revision cpp-rev
            :ext "\\.\\(cpp\\|cc\\|cxx\\|hpp\\|hh\\)\\'")
           (make-treesit-auto-recipe
            :lang 'css
            :ts-mode 'css-ts-mode
            :remap 'css-mode
            :url "https://github.com/tree-sitter/tree-sitter-css"
            :revision css-rev
            :ext "\\.css\\'")
           (make-treesit-auto-recipe
            :lang 'go
            :ts-mode 'go-ts-mode
            :remap 'go-mode
            :url "https://github.com/tree-sitter/tree-sitter-go"
            :revision go-rev
            :ext "\\.go\\'")
           (make-treesit-auto-recipe
            :lang 'html
            :ts-mode 'html-ts-mode
            :remap 'html-mode
            :url "https://github.com/tree-sitter/tree-sitter-html"
            :revision html-rev
            :ext "\\.html\\'")
           (make-treesit-auto-recipe
            :lang 'java
            :ts-mode 'java-ts-mode
            :remap 'java-mode
            :url "https://github.com/tree-sitter/tree-sitter-java"
            :revision java-rev
            :ext "\\.java\\'")
           (make-treesit-auto-recipe
            :lang 'javascript
            :ts-mode 'js-ts-mode
            :remap 'js-mode
            :url "https://github.com/tree-sitter/tree-sitter-javascript"
            :revision js-rev
            :ext "\\.js\\'")
           (make-treesit-auto-recipe
            :lang 'json
            :ts-mode 'json-ts-mode
            :remap 'json-mode
            :url "https://github.com/tree-sitter/tree-sitter-json"
            :revision json-rev
            :ext "\\.json\\'")
           (make-treesit-auto-recipe
            :lang 'python
            :ts-mode 'python-ts-mode
            :remap 'python-mode
            :url "https://github.com/tree-sitter/tree-sitter-python"
            :revision grammar-rev
            :ext "\\.py\\'")
           (make-treesit-auto-recipe
            :lang 'ruby
            :ts-mode 'ruby-ts-mode
            :remap 'ruby-mode
            :url "https://github.com/tree-sitter/tree-sitter-ruby"
            :revision ruby-rev
            :ext "\\.rb\\'")
           (make-treesit-auto-recipe
            :lang 'rust
            :ts-mode 'rust-ts-mode
            :remap 'rust-mode
            :url "https://github.com/tree-sitter/tree-sitter-rust"
            :revision rust-rev
            :ext "\\.rs\\'")
           (make-treesit-auto-recipe
            :lang 'tsx
            :ts-mode 'tsx-ts-mode
            :remap 'typescript-mode
            :url "https://github.com/tree-sitter/tree-sitter-typescript"
            :revision ts-rev
            :ext "\\.tsx\\'"
            :source-dir "tsx/src")
           (make-treesit-auto-recipe
            :lang 'typescript
            :ts-mode 'typescript-ts-mode
            :remap 'typescript-mode
            :url "https://github.com/tree-sitter/tree-sitter-typescript"
            :revision ts-rev
            :ext "\\.ts\\'"
            :source-dir "typescript/src")
           (make-treesit-auto-recipe
            :lang 'yaml
            :ts-mode 'yaml-ts-mode
            :remap 'yaml-mode
            :url "https://github.com/ikatyang/tree-sitter-yaml"
            :revision yaml-rev
            :ext "\\.\\(yaml\\|yml\\)\\'"))))

  ;; Enable global auto mode
  (global-treesit-auto-mode))

;; Configure indentation for tree-sitter modes
(setq js-ts-mode-indent-offset 2)
(setq typescript-ts-mode-indent-offset 2)
(setq tsx-ts-mode-indent-offset 2)
(setq python-ts-mode-indent-offset 4)
(setq java-ts-mode-indent-offset 4)
(setq go-ts-mode-indent-offset 4)
(setq rust-ts-mode-indent-offset 4)
(setq c-ts-mode-indent-offset 4)
(setq c++-ts-mode-indent-offset 4)

(use-package go-mode
  :hook (go-mode . eglot-ensure))

  ;; Defer flyover - load with flymake
  (use-package flyover
  :straight (:local-repo "~/dev/git/flyover"
             :host github :repo "konrad1977/flyover")
  :defer t)
(with-eval-after-load 'flymake (require 'flyover))
  (add-hook 'flymake-mode-hook #'flyover-mode)

  ;; Use theme colors for error/warning/info faces
  (setq flyover-use-theme-colors t)

  ;; Adjust background lightness (lower values = darker)
  (setq flyover-background-lightness 45)

  ;; Make icon background darker than foreground
  (setq flyover-percent-darker 40)

  (setq flyover-text-tint 'lighter) ;; or 'darker or nil
  ;; Enable wrapping of long error messages across multiple lines
  (setq flyover-wrap-messages t)

  ;; Maximum length of each line when wrapping messages
  (setq flyover-max-line-length 80)

  ;; "Percentage to lighten or darken the text when tinting is enabled."
  (setq flyover-text-tint-percent 50)
  (setq flyover-levels '(error warning info))

  (setq flyover-checkers '(flymake))
  (setq flyover-debug nil)  ; Disable flyover debug messages

  ;;; Hide checker name for a cleaner UI
  (setq flyover-hide-checker-name t) 

  ;;; show at end of the line instead.
  (setq flyover-show-at-eol t) 

  ;;; Hide overlay when cursor is at same line, good for show-at-eol.
  (setq flyover-hide-when-cursor-is-on-same-line t) 

  ;;; Show an arrow (or icon of your choice) before the error to highlight the error a bit more.
  (setq flyover-show-virtual-line t)

  ;;; Icons
  (setq flyover-info-icon "🛈")
  (setq flyover-warning-icon "⚠")
  (setq flyover-error-icon "✘")

  ;;; Icon padding
  ;;; You might want to adjust this setting if you icons are not centererd or if you more or less space.fs
  (setq flyover-icon-left-padding 0.9)
  (setq flyover-icon-right-padding 0.9)


;; eros - Evaluation Result OverlayS for Emacs Lisp
;; Shows eval results (C-x C-e, etc.) as inline overlays at cursor
(use-package eros
  :hook (emacs-lisp-mode . eros-mode)
  :custom
  (eros-eval-result-prefix "=> ")
  (eros-eval-result-duration 'command))  ; Overlay disappears on next keypress

;; quickrun - Run code snippets in various languages
;; Results can display as overlays or in a popup buffer
(use-package quickrun
  :defer t
  :bind (("C-c q q" . quickrun)
         ("C-c q r" . quickrun-region)
         ("C-c q s" . quickrun-shell))
  :custom
  (quickrun-timeout-seconds 30)          ; Allow longer running code
  (quickrun-focus-p nil))                ; Don't steal focus to output buffer

;; LanguageTool server jar path - find the installed version dynamically
(defvar ari/languagetool-server-jar
  (car (file-expand-wildcards
        (expand-file-name "~/emacs/site/languagetool/LanguageTool-*/languagetool-server.jar")))
  "Path to LanguageTool server jar.")

;; LanguageTool via flymake. The config standardized on flymake in May 2026
;; (eglot is flymake-native), so LanguageTool now rides the same pipeline as
;; everything else. Auto-enabled in plain text / markdown through the text-mode
;; flymake-mode hook. Org is intentionally ON-DEMAND via `ari/languagetool-check'
;; (C-c L) so org-roam buffers stay fast — flymake-mode is left off in org-mode
;; (see the Flymake block above).
(use-package flymake-languagetool
  :commands (flymake-languagetool-load flymake-languagetool-maybe-load)
  :hook (text-mode . flymake-languagetool-load)
  :init
  (setq flymake-languagetool-server-jar ari/languagetool-server-jar)
  (setq flymake-languagetool-server-port "8081")   ; must be a string, not an integer
  (setq flymake-languagetool-language "en-US")
  ;; "picky" level unlocks many stricter style/grammar rules — free, local, no
  ;; subscription. If any are too noisy for fiction, silence individual ones via
  ;; `flymake-languagetool-disabled-rules'.
  (setq flymake-languagetool-check-params '(("level" . "picky")))
  ;; n-gram language model: context-aware confusion-pair detection (their/there,
  ;; its/it's, …). Points at the PARENT dir that contains the per-language subdir
  ;; (here, an `en/'). Only passed if the data is actually present, so the server
  ;; still starts on machines without the ~13GB dataset downloaded.
  (let ((ngram-dir (expand-file-name "~/emacs/site/languagetool/ngrams")))
    (when (file-directory-p (expand-file-name "en" ngram-dir))
      (setq flymake-languagetool-server-args (list "--languageModel" ngram-dir))))
  :config
  ;; Skip org directive lines (#+TITLE:, #+OPTIONS:, #+LATEX_HEADER:, #+LATEX: …),
  ;; which all carry the `org-meta-line' face — otherwise LaTeX headers and
  ;; directives raise grammar false positives. Export-block bodies are already
  ;; covered by the default `org-block' entry in the ignore alist.
  (let ((cell (assq 'org-mode flymake-languagetool-ignore-faces-alist)))
    (when (and cell (not (memq 'org-meta-line (cdr cell))))
      (setcdr cell (cons 'org-meta-line (cdr cell))))))

;; writegood-mode for passive voice and weasel words (lightweight complement)
(use-package writegood-mode
  :defer t
  :bind ("C-c W" . writegood-mode))

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
