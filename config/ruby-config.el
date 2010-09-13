(require 'ecb-autoloads)
(autoload 'ruby-mode "ruby-mode" "Ruby editing mode." t)
(setq auto-mode-alist (cons '("\\.rb$" . ruby-mode)  auto-mode-alist))
(setq auto-mode-alist (cons '("\\.rhtml$" . eruby-nxhtml-mumamo) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.erb$" . eruby-nxhtml-mumamo) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.html.erb$" . eruby-nxhtml-mumamo) auto-mode-alist))

(require 'auto-complete)
(require 'rcodetools)
(require 'haml-mode)
(require 'yasnippet)

(yas/initialize)
(yas/load-directory "~/emacs/site/yasnippet/snippets")


(require 'ruby-block)
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
           (set (make-local-variable 'indent-tabs-mode) 'nil)
           (set (make-local-variable 'tab-width) 2)
           (imenu-add-to-menubar "IMENU")
;           (require 'ruby-electric)
           (ruby-electric-mode t)
;           (require 'ruby-block)
           (ruby-block-mode t)
;           (local-set-key 'f1 'ri)
           (local-set-key "\M-\C-i" 'ri-ruby-complete-symbol)
;           (local-set-key 'f4 'ri-ruby-show-args)
           (define-key ruby-mode-map "\M-\C-o" 'rct-complete-symbol)
           (local-set-key (kbd "<return>") 'newline-and-indent)
           (add-to-list 'ac-sources 'ac-source-dictionary t)
))

;; nxhtml
(setq *nxhtml-autostart-file* (expand-file-name "~/emacs/site/nxhtml/autostart.el"))
(load *nxhtml-autostart-file*)
(setq
      nxhtml-global-minor-mode t
      mumamo-chunk-coloring 'submode-colored
      nxhtml-skip-welcome t
      indent-region-mode t
      rng-nxml-auto-validate-flag nil
      nxml-degraded t)
(add-to-list 'auto-mode-alist '("\\.html$" . nxhtml-mumamo-mode))
(add-to-list 'auto-mode-alist '("\\.html\\.erb" . eruby-html-mumamo-mode))



(load "~/emacs/site/yasnippets-rails/setup.el")

(require 'rinari)
(setq rinari-tags-file-name "TAGS")

(require 'auto-complete nil t)
(require 'auto-complete-config)
(require 'auto-complete-yasnippet)
;;(require 'auto-complete-ruby)
;;(require 'auto-complete-css)

(global-auto-complete-mode t)           ;enable global-mode
(setq ac-auto-start t)                  ;automatically start
(setq ac-dwim 3)                        ;Do what i mean
(setq ac-override-local-map nil)        ;don't override local map
(define-key ac-complete-mode-map "\t" 'ac-expand)
(define-key ac-complete-mode-map "\r" 'ac-complete)
(define-key ac-complete-mode-map "\M-n" 'ac-next)
(define-key ac-complete-mode-map "\M-p" 'ac-previous)
(set-default 'ac-sources '(ac-source-yasnippet ac-source-abbrev ac-source-words-in-buffer ac-source-dictionary ac-source-semantic ac-source-rcodetools))

   (setq ac-modes
         (append ac-modes
                 '(eshell-mode
                   ;org-mode
                   )))
   ;(add-to-list 'ac-trigger-commands 'org-self-insert-command)

   (add-hook 'emacs-lisp-mode-hook
             (lambda ()
               (setq ac-sources '(ac-source-yasnippet ac-source-abbrev ac-source-words-in-buffer ac-source-symbols))))

   (add-hook 'eshell-mode-hook
             (lambda ()
               (setq ac-sources '(ac-source-yasnippet ac-source-abbrev ac-source-files-in-current-dir ac-source-words-in-buffer))))

   (add-hook 'ruby-mode-hook
             (lambda ()
               (add-to-list 'ac-sources 'ac-source-rcodetools)
              ;;  (add-to-list  'ac-omni-completion-sources (cons "\\." '(ac-source-rcodetools)))
              ;; (add-to-list 'ac-omni-completion-sources  (cons "::" '(ac-source-rcodetools))))
))



(setq ri-ruby-script (expand-file-name "~/emacs/site/lisp/ri-emacs.rb"))

(add-hook 'ruby-mode-hook
          (lambda()
            (add-hook 'write-file-functions;;(add-hook 'local-write-file-hooks
                          '(lambda()
                             (save-excursion
                               (untabify (point-min) (point-max))
                               (delete-trailing-whitespace)
                               )))
          (set (make-local-variable 'indent-tabs-mode) 'nil)
          (set (make-local-variable 'tab-width) 2)
          (imenu-add-to-menubar "IMENU")
          (require 'ruby-electric)
          (ruby-electric-mode t)
          ))
(add-hook 'ruby-mode-hook (lambda ()
                            (local-set-key [f1] 'ri)
                            (local-set-key "\M-\C-i" 'ri-ruby-complete-symbol)
                            (local-set-key [f4] 'ri-ruby-show-args)))

(autoload 'ri "ri-ruby.el" nil t)
(load (expand-file-name "~/emacs/site/lisp/ri-ruby.el"))
(global-auto-complete-mode t)
(define-key ac-complete-mode-map "\C-n" 'ac-next)
(define-key ac-complete-mode-map "\C-p" 'ac-previous)
(setq ac-auto-start 3)
(define-key ac-complete-mode-map "\t" 'ac-complete)
(define-key ac-complete-mode-map "\r" nil)

(provide 'ruby-config)
