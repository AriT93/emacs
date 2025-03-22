;;; ruby-config --- Summary
;;; Commentary:
;;; Code:

(autoload 'ruby-mode "ruby-mode" "Ruby editing mode." t)
(setq auto-mode-alist (cons '("\\.rb$" . ruby-mode)  auto-mode-alist))
(setq auto-mode-alist (cons '("\\.rhtml$" . web-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.erb$" . web-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.html.erb$" . web-mode) auto-mode-alist))
(add-to-list 'auto-mode-alist '("\\.html?" . web-mode))

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
    (require 'ruby-electric)
    (add-hook 'ruby-mode-hook
              (lambda()
                (set (make-local-variable 'indent-tabs-mode) 'nil)
                (set (make-local-variable 'tab-width) 2)
                (ruby-electric-mode t)
                (ruby-block-mode t)
                (local-set-key (kbd "<return>") 'newline-and-indent)
                ))
    (add-hook 'ruby-mode-hook #'eglot-ensure)
;;    (add-to-list 'eglot-server-programs '(ruby-mode . ("bundle" "exec" "solargraph" "stdio")))



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

(add-hook 'ruby-mode-hook
  (lambda () (hs-minor-mode)))

(eval-after-load "hideshow"
  '(add-to-list 'hs-special-modes-alist
    `(ruby-mode
      ,(rx (or "def" "class" "module" "do" "{" "[")) ; Block start
      ,(rx (or "}" "]" "end"))                       ; Block end
      ,(rx (or "#" "=begin"))                        ; Comment start
      ruby-forward-sexp nil)))

;; (global-set-key (kbd "C-c h") 'hs-hide-block)
;; (global-set-key (kbd "C-c s") 'hs-show-block)

(provide 'ruby-config-new)
;;; ruby-config-new ends here
