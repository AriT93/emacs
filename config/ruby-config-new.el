;;; ruby-config --- Summary
;;; Commentary:
;;; Code:

(autoload 'ruby-mode "ruby-mode" "Ruby editing mode." t)
(setq auto-mode-alist (cons '("\\.rb$" . ruby-mode)  auto-mode-alist))
(setq auto-mode-alist (cons '("\\.rhtml$" . web-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.erb$" . web-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.html.erb$" . web-mode) auto-mode-alist))

(yas-global-mode)
(global-flycheck-mode)

;; yaml
(require 'yaml-mode)
(add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))
(add-to-list 'auto-mode-alist '("\\.yaml$" . yaml-mode))

(use-package ruby-tools
   :ensure t
   :init
   (add-hook 'ruby-mode #'ruby-tools-mode)
   :diminish ruby-tools-mode)
(use-package rubocopfmt
  :ensure t
  :hook
  (ruby-mode . rubocopfmt-mode)
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
  )

(use-package rspec-mode
  :ensure t
  :config
  (setq rspec-use-spring-when-possible nil)
  (setq rspec-command-options "--format progress"))
  (add-hook 'after-init-hook  'inf-ruby-switch-setup)

  (require 'ruby-block)
  (diminish 'ruby-block-mode)
  (require 'ruby-electric)
  (diminish 'ruby-electric-mode)
  (add-hook 'ruby-mode-hook
            (lambda()
              (add-hook 'write-file-functions
                        #'(lambda()
                           (save-excursion
                             (untabify (point-min) (point-max))
                             (delete-trailing-whitespace)
                             )))
              (set (make-local-variable 'indent-tabs-mode) 'nil)
              (set (make-local-variable 'tab-width) 2)
              (ruby-electric-mode t)
              (ruby-block-mode t)
              (local-set-key (kbd "<return>") 'newline-and-indent)
              (diminish 'org-mode  "")
              (diminish 'auto-revert-mode)
              (diminish 'yas-minor-mode)
              (diminish 'eldoc-mode)
              (diminish 'org-src-mode)
              (diminish 'eclim-mode)
              (diminish 'abbrev-mode)
              (diminish 'ivy-mode)
              (diminish 'global-highline-mode)
              (diminish 'ruby-block-mode)
              (diminish 'ruby-electric-mode)
              (diminish "seeing-is-believing")
              (diminish 'hs-minor-mode)
              (diminish 'ruby-block-mode)
              (diminish 'global-highline-mode)
              ))

(add-to-list 'auto-mode-alist '("\\.html?" . web-mode))
(add-to-list 'auto-mode-alist '("\\.html\\.erb" . web-mode))

(use-package rbenv
  :ensure t
  :config
  (setq rbenv-installation-dir "/usr/local/opt/rbenv")
  (global-rbenv-mode t))

     (require 'lsp)
     (add-hook 'ruby-mode-hook #'lsp)

;; (setq ri-ruby-script (expand-file-name "~/emacs/site/lisp/ri-emacs.rb"))
  ;; (autoload 'ri "ri-ruby.el" nil t)
(use-package autotest
  :ensure t
  )

(add-hook 'ruby-mode-hook
  (lambda () (hs-minor-mode)))

(eval-after-load "hideshow"
  '(add-to-list 'hs-special-modes-alist
    `(ruby-mode
      ,(rx (or "def" "class" "module" "do" "{" "[")) ; Block start
      ,(rx (or "}" "]" "end"))                       ; Block end
      ,(rx (or "#" "=begin"))                        ; Comment start
      ruby-forward-sexp nil)))

(global-set-key (kbd "C-c h") 'hs-hide-block)
(global-set-key (kbd "C-c s") 'hs-show-block)

(provide 'ruby-config-new)
;;; ruby-config-new ends here
