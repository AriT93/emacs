;;; ruby-config --- Summary
;;; Commentary:
;;; Code:
;;(require 'ecb-autoloads)
(autoload 'ruby-mode "ruby-mode" "Ruby editing mode." t)
(setq auto-mode-alist (cons '("\\.rb$" . ruby-mode)  auto-mode-alist))
(setq auto-mode-alist (cons '("\\.rhtml$" . web-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.erb$" . web-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.html.erb$" . web-mode) auto-mode-alist))

;;(require 'auto-complete)

;;(yas-load-directory "~/emacs/site/yasnippet/snippets/")


;; (setq yas/snippet-dirs "~/emacs/site/yasnippet-0.7.0/snippets")
;; (yas/load-directory yas/snippet-dirs)
(yas/global-mode)
(global-flycheck-mode)
;;(require 'xslt-process)

;;(require 'ruby-block)
;;(require 'rails)
;; (defvar ac-ruby-sources
;;   '(ac-source-rcodetools))

(defun try-complete-abbrev (old)
  (if (expand-abbrev) t nil))

(setq hippie-expand-try-functions-list
      '(try-complete-abbrev
        try-complete-file-name
        try-expand-dabbrev))

;; yaml
(require 'yaml-mode)
(add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))
(add-to-list 'auto-mode-alist '("\\.yaml$" . yaml-mode))

;; rdebug
(require 'rdebug)
(setq rdebug-short-key-mode t)

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
              (delete "ac-source-emacs-eclim" `ac-sources))
              (set (make-local-variable 'indent-tabs-mode) 'nil)
              (set (make-local-variable 'tab-width) 2)
              (imenu-add-to-menubar "IMENU")
            (require 'ruby-electric)
            (ruby-electric-mode t)
;;            (require 'ruby-block)
              (ruby-block-mode t)
              ;;           (local-set-key 'f1 'ri)
              ;;           (local-set-key "\M-\C-i" 'ri-ruby-complete-symbol)
              ;;           (local-set-key 'f4 'ri-ruby-show-args)
              (define-key ruby-mode-map "\M-\C-o" 'rct-complete-symbol)
              (local-set-key (kbd "<return>") 'newline-and-indent)
;;              (add-to-list 'ac-sources 'ac-source-dictionary t)
              ))

(add-to-list 'auto-mode-alist '("\\.html?" . web-mode))
(add-to-list 'auto-mode-alist '("\\.html\\.erb" . web-mode))



;;(load "~/emacs/site/yasnippets-rails/setup.el")

(require 'rinari)
(setq rinari-tags-file-name "TAGS")

;;(require 'auto-complete-ruby)
;;(require 'auto-complete-css)


(add-hook 'java-mode-hook
          (lambda()
            (add-to-list 'ac-sources '(ac-emacs-eclim-source ac-source-eclim ac-source-symbols ac-source-abbrev ac-source-yasnippet ac-source-words-in-same-mode-buffers ac-source-variables)))
)
   (add-hook 'ruby-mode-hook
             (lambda ()
               (add-to-list 'ac-sources 'ac-source-rcodetools)
               (delete "ac-source-emacs-eclim" `ac-sources)
))



(setq ri-ruby-script (expand-file-name "~/emacs/site/lisp/ri-emacs.rb"))


(autoload 'ri "ri-ruby.el" nil t)
(global-auto-complete-mode t)
;; (define-key ac-complete-mode-map "\C-n" 'ac-next)
;; (define-key ac-complete-mode-map "\C-p" 'ac-previous)
;; (setq ac-auto-start 3)
;; (define-key ac-complete-mode-map "\t" 'ac-complete)
;; (define-key ac-complete-mode-map "\r" nil)
(require 'unit-test)
(require 'autotest)

(provide 'ruby-config)
;;; ruby-config ends here
