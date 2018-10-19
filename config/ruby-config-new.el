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
(yas/global-mode)
(global-flycheck-mode)
;;(require 'xslt-process)

;; yaml
(require 'yaml-mode)
(add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))
(add-to-list 'auto-mode-alist '("\\.yaml$" . yaml-mode))

(use-package ruby-tools
  :ensure t
  :hook
  (ruby-mode . ruby-tools-mode)
  :diminish ruby-tools-mode)

(use-package seeing-is-believing
  :diminish ""
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
  (add-hook 'ruby-mode-hook
            (lambda()
              (add-hook 'write-file-functions
                        '(lambda()
                           (save-excursion
                             (untabify (point-min) (point-max))
                             (delete-trailing-whitespace)
                             )))
             (lambda ()
               (add-to-list 'ac-sources '(ac-source-rcodetools ac-source-dictionary))
               (delete 'ac-sources "ac-source-emacs-eclim")
  )
                (set (make-local-variable 'indent-tabs-mode) 'nil)
                (set (make-local-variable 'tab-width) 2)
                (imenu-add-to-menubar "IMENU")
              (require 'ruby-electric)
              (ruby-electric-mode t)
              (require 'ruby-block)
                (ruby-block-mode t)
                ;;           (local-set-key 'f1 'ri)
                ;;           (local-set-key "\M-\C-i" 'ri-ruby-complete-symbol)
                ;;           (local-set-key 'f4 'ri-ruby-show-args)
                (define-key ruby-mode-map "\M-\C-o" 'rct-complete-symbol)
                (local-set-key (kbd "<return>") 'newline-and-indent)
;;                (add-to-list 'ac-sources 'ac-source-dictionary t)
                ))

(add-to-list 'auto-mode-alist '("\\.html?" . web-mode))
(add-to-list 'auto-mode-alist '("\\.html\\.erb" . web-mode))

;; (add-hook 'java-mode-hook
;;           (lambda()
;;             (add-to-list 'ac-sources '(ac-emacs-eclim-source ac-source-eclim ac-source-symbols
;;                                                              ac-source-abbrev ac-source-yasnippet ac-source-words-in-same-mode-buffers ac-source-variables))
;;             (ac-emacs-eclim-java-setup))
;;           )
;;(add-hook 'ruby-mode-hook
;;          (lambda ()
;;            (add-to-list 'ac-sources 'ac-source-rcodetools)
;;            (delete 'ac-sources "ac-source-emacs-eclim")
;;            ))

(setq ri-ruby-script (expand-file-name "~/emacs/site/lisp/ri-emacs.rb"))


  (autoload 'ri "ri-ruby.el" nil t)
;;  (global-auto-complete-mode t)
;;  (define-key ac-complete-mode-map "\C-n" 'ac-next)
;;  (define-key ac-complete-mode-map "\C-p" 'ac-previous)
;;  (setq ac-auto-start 3)
;;  (define-key ac-complete-mode-map "\t" 'ac-complete)
;;  (define-key ac-complete-mode-map "\r" nil)
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
