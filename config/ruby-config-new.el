;; -*- lexical-binding: t; -*-
;;; ruby-config --- Summary 
;;; Commentary:
;;; Code:
;;; coding: utf-8  -*- lexical-binding: t; -*-

(autoload 'ruby-mode "ruby-mode" "Ruby editing mode." t)
(setq auto-mode-alist (cons '("\\.rhtml$" . web-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.erb$" . web-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.html.erb$" . web-mode) auto-mode-alist))
(add-to-list 'auto-mode-alist '("\\.html?" . web-mode))

;; Use ruby-ts-mode for .rb files (inherits from prog-mode, so hooks work automatically)
(add-to-list 'auto-mode-alist '("\\.rb$" . ruby-ts-mode))

(use-package ruby-tools
   :ensure t
   :init
   (add-hook 'ruby-mode #'ruby-tools-mode)
   (add-hook 'ruby-ts-mode #'ruby-tools-mode)
   :diminish ruby-tools-mode)
(use-package rubocopfmt
  :ensure t
  :hook
  (ruby-mode . rubocopfmt-mode)
  (ruby-ts-mode . rubocopfmt-mode)
 )

(use-package seeing-is-believing
  :diminish seeing-is-believing
  :ensure t
  :config
  (setq seeing-is-believing-alignment 'chunk)
  (setq seeing-is-believing-max-length 150)
  (setq seeing-is-believing-max-results 15)
  :hook
  (ruby-mode . seeing-is-believing)
  (ruby-ts-mode . seeing-is-believing)
  )

(use-package rspec-mode
  :ensure t
  :config
  (setq rspec-use-spring-when-possible nil)
  (setq rspec-command-options "--format progress"))
  (add-hook 'after-init-hook  'inf-ruby-switch-setup)

  (require 'ruby-block)
  (require 'ruby-electric)
  
  ;; Common Ruby configuration function
  (defun setup-ruby-environment ()
    "Set up common Ruby environment settings."
    (set (make-local-variable 'indent-tabs-mode) 'nil)
    (set (make-local-variable 'tab-width) 2)
    (ruby-electric-mode t)
    (ruby-block-mode t)
    (local-set-key (kbd "<return>") 'newline-and-indent))
  
  ;; Hook for ruby-ts-mode (inherits from prog-mode, so other hooks work automatically)
  (add-hook 'ruby-ts-mode-hook #'setup-ruby-environment)

(use-package rbenv
  :ensure t
  :init
  (setq rbenv-show-active-ruby-in-modeline nil)
  :config
  (setq rbenv-installation-dir "/opt/homebrew/Cellar/rbenv/1.2.0")
  (setq rbenv-executable "/opt/homebrew/bin/rbenv")
  (global-rbenv-mode t))

(use-package autotest
  :ensure t
  )

;; Hideshow setup for Ruby
(defun setup-ruby-hideshow ()
  "Set up hideshow for Ruby modes."
  (hs-minor-mode))

;; Hook for ruby-ts-mode
(add-hook 'ruby-ts-mode-hook #'setup-ruby-hideshow)

(eval-after-load "hideshow"
  '(add-to-list 'hs-special-modes-alist
    `(ruby-ts-mode
      ,(rx (or "def" "class" "module" "do" "{" "[")) ; Block start
      ,(rx (or "}" "]" "end"))                       ; Block end
      ,(rx (or "#" "=begin"))                        ; Comment start
      ruby-forward-sexp nil)))

;; (global-set-key (kbd "C-c h") 'hs-hide-block)
;; (global-set-key (kbd "C-c s") 'hs-show-block)

(provide 'ruby-config-new)
;;; ruby-config-new ends here
