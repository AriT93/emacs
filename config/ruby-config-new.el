;; -*- lexical-binding: t; -*-
;;; ruby-config --- Summary 
;;; Commentary:
;;; Code:
;;; coding: utf-8  -*- lexical-binding: t; -*-

(autoload 'ruby-mode "ruby-mode" "Ruby editing mode." t)
(setq auto-mode-alist (cons '("\\.rb$" . ruby-mode)  auto-mode-alist))
(setq auto-mode-alist (cons '("\\.rhtml$" . web-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.erb$" . web-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.html.erb$" . web-mode) auto-mode-alist))
(add-to-list 'auto-mode-alist '("\\.html?" . web-mode))

;; Tree-sitter Ruby mode for Emacs 31
;; Use ruby-ts-mode when available, fallback to ruby-mode
(when (fboundp 'ruby-ts-mode)
  (add-to-list 'auto-mode-alist '("\\.rb$" . ruby-ts-mode))
  (setq ruby-ts-mode-indent-offset 2))

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
  
  ;; Hook for traditional ruby-mode
  (add-hook 'ruby-mode-hook #'setup-ruby-environment)
  
  ;; Hook for tree-sitter ruby-ts-mode
  (when (fboundp 'ruby-ts-mode)
    (add-hook 'ruby-ts-mode-hook #'setup-ruby-environment))



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

;; Common hideshow setup for Ruby modes
(defun setup-ruby-hideshow ()
  "Set up hideshow for Ruby modes."
  (hs-minor-mode))

;; Hook for traditional ruby-mode
(add-hook 'ruby-mode-hook #'setup-ruby-hideshow)

;; Hook for tree-sitter ruby-ts-mode
(when (fboundp 'ruby-ts-mode)
  (add-hook 'ruby-ts-mode-hook #'setup-ruby-hideshow))

(eval-after-load "hideshow"
  '(add-to-list 'hs-special-modes-alist
    `(ruby-mode
      ,(rx (or "def" "class" "module" "do" "{" "[")) ; Block start
      ,(rx (or "}" "]" "end"))                       ; Block end
      ,(rx (or "#" "=begin"))                        ; Comment start
      ruby-forward-sexp nil)))

;; Add tree-sitter mode to hideshow if available
(when (fboundp 'ruby-ts-mode)
  (eval-after-load "hideshow"
    '(add-to-list 'hs-special-modes-alist
      `(ruby-ts-mode
        ,(rx (or "def" "class" "module" "do" "{" "[")) ; Block start
        ,(rx (or "}" "]" "end"))                       ; Block end
        ,(rx (or "#" "=begin"))                        ; Comment start
        ruby-forward-sexp nil))))

;; (global-set-key (kbd "C-c h") 'hs-hide-block)
;; (global-set-key (kbd "C-c s") 'hs-show-block)

;; Tree-sitter specific Ruby features for Emacs 31
(when (fboundp 'ruby-ts-mode)
  ;; Enhanced indentation for tree-sitter Ruby
  (setq ruby-ts-mode-indent-offset 2)
  
  ;; Better syntax highlighting with tree-sitter
  (add-hook 'ruby-ts-mode-hook
            (lambda ()
              ;; Enable tree-sitter-based syntax highlighting
              (when (fboundp 'tree-sitter-hl-mode)
                (tree-sitter-hl-mode))
              
              ;; Configure tree-sitter indentation
              (setq-local treesit-indent-offset 2)
              
              ;; Enable tree-sitter-based code folding
              (when (fboundp 'tree-sitter-fold-mode)
                (tree-sitter-fold-mode))))
  
  ;; Tree-sitter specific key bindings for Ruby
  ;; Defer key binding until the mode is loaded
  (add-hook 'ruby-ts-mode-hook
            (lambda ()
              (when (boundp 'ruby-ts-mode-map)
                (define-key ruby-ts-mode-map (kbd "C-c t") 'tree-sitter-hl-mode)
                (define-key ruby-ts-mode-map (kbd "C-c f") 'tree-sitter-fold-mode)))))

(provide 'ruby-config-new)
;;; ruby-config-new ends here
