;;; ruby-config --- Summary
;;; Commentary:
;;; Code:

(autoload 'ruby-mode "ruby-mode" "Ruby editing mode." t)
(setq auto-mode-alist (cons '("\\.rb$" . ruby-mode)  auto-mode-alist))
(setq auto-mode-alist (cons '("\\.rhtml$" . web-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.erb$" . web-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.html.erb$" . web-mode) auto-mode-alist))

;;(yas-load-directory "~/emacs/site/yasnippet/snippets/")
;; (setq yas/snippet-dirs "~/emacs/site/yasnippet-0.7.0/snippets")
;; (yas/load-directory yas/snippet-dirs)
(use-package yasnippet-snippets
   :ensure t)
(yas-global-mode)
(global-flycheck-mode)
;;(require 'xslt-process)

;; (defun try-complete-abbrev (old)
;;   (if (expand-abbrev) t nil))

;; (setq hippie-expand-try-functions-list
;;       '(try-complete-abbrev
;;         try-complete-file-name
;;         try-expand-dabbrev))

;; yaml
(require 'yaml-mode)
(add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))
(add-to-list 'auto-mode-alist '("\\.yaml$" . yaml-mode))

;;   (use-package ruby-tools
;;     :ensure t
;;     :hook
;;     (ruby-mode . ruby-tools-mode)
;;     :diminish ruby-tools-mode)
 (use-package rubocopfmt
   :ensure t
   :hook
   (ruby-mode . rubocopfmt-mode)
   )

(use-package seeing-is-believing
  :diminish seeing-is-believing
  :ensure t
  :init
  (setq seeing-is-believing-alignment 'chunk)
  (setq seeing-is-believing-max-length 150)
  (setq seeing-is-believing-max-results 15)
  :bind (:map ruby-mode-map
              ("\C-c\C-c" . seeing-is-believing-run-as-xmpfilter))
  :hook
  (ruby-mode . seeing-is-believing)
  )

;; ruby-mode-hook
    ;; (use-package flymake-ruby
    ;;   :ensure t
    ;;   )
  ;; (add-hook 'ruby-mode-hook 'flymake-ruby-load)
  (use-package rspec-mode
    :ensure t
    :config
    (setq rspec-use-spring-when-possible nil)
    :after
    (rspec-install-snippets)
    )
    (require 'ruby-block)
    (diminish 'ruby-block-mode)
    (require 'ruby-electric)
    (diminish 'ruby-electric-mode)
    (add-hook 'ruby-mode-hook
              (lambda()
                (add-hook 'write-file-functions
                          '(lambda()
                             (save-excursion
                               (untabify (point-min) (point-max))
                               (delete-trailing-whitespace)
                               )))
                (set (make-local-variable 'indent-tabs-mode) 'nil)
                (set (make-local-variable 'tab-width) 2)
;;                (imenu-add-to-menubar "IMENU")
                (ruby-electric-mode t)
;;               (eldoc-mode -1)
    ;;           (global-eldoc-mode -1)
  ;;             (lsp-ui-doc-mode -1)
  ;;             (setq lsp-ui-doc-enable-eldoc nil)
                (ruby-block-mode t)
;;                (define-key ruby-mode-map "\M-\C-o" 'rct-complete-symbol)
                (local-set-key (kbd "<return>") 'newline-and-indent)
    ;;            (lsp-ui-sideline-mode t)
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
  :init
  (global-rbenv-mode))
(require 'lsp)
(add-hook 'ruby-mode-hook 'lsp)
 ;; (use-package lsp-ruby
 ;;   :ensure t )
 ;; (require 'lsp-ruby)
;;(add-hook 'ruby-mode-hook #'lsp-ruby-enable)

     ;; (add-hook 'java-mode-hook
     ;;           (lambda()
     ;;             (add-to-list 'ac-sources '(ac-emacs-eclim-source ac-source-eclim ac-source-symbols
     ;;                                                              ac-source-abbrev ac-source-yasnippet ac-source-words-in-same-mode-buffers ac-source-variables))
     ;;             (ac-emacs-eclim-java-setup))
     ;;           )
     ;; (add-hook 'ruby-mode-hook
     ;;           (lambda ()
     ;;             (add-to-list 'ac-sources 'ac-source-rcodetools)
     ;;             (delete 'ac-sources "ac-source-emacs-eclim")
     ;;             ))

(setq ri-ruby-script (expand-file-name "~/emacs/site/lisp/ri-emacs.rb"))


  (autoload 'ri "ri-ruby.el" nil t)
;;  (global-auto-complete-mode t)
  ;; (define-key ac-complete-mode-map "\C-n" 'ac-next)
  ;; (define-key ac-complete-mode-map "\C-p" 'ac-previous)
  ;; (setq ac-auto-start 3)
  ;; (define-key ac-complete-mode-map "\t" 'ac-complete)
  ;; (define-key ac-complete-mode-map "\r" nil)
  ;;(require 'unit-test)
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
