;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; emacs-config.el                                                           ;;
;;                                                                           ;;
;; This file will hold specific setting I like for emacs out side of         ;;
;; customize.  Mostly requires and such but a few setq's and such as well    ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tool-bar-mode nil)
(menu-bar-mode nil)
(message "loading emacs-config")
(setq w32-use-full-screen-buffer nil)
(setq uniquify-buffer-name-style t)
(setq uniquify-buffer-name-style (quote post-forward))
(setq uniquify-min-dir-content 1)
(setq cal-tex-diary t)
(add-hook 'diary-display-hook 'fancy-diary-display)
(add-hook 'text-mode-hook ' turn-on-auto-fill)
(autoload 'nuke-trailing-whitespace "whitespace" nil t)
(add-hook 'write-file-hooks 'nuke-trailing-whitespace)
(cond
 ((string="w32" window-system)
  (set-face-attribute 'mode-line nil :family "Liberation Mono" :height 1.0 ))
 ((string="x" window-system)
  (set-face-attribute 'mode-line nil :family "Liberation Mono" :height 1.0 )))

(setq-default compile-command "nmake")
(setq tramp-auto-save-directory "~/tmp")
(setq message-log-max 250)
(require 'load-path-config)
(require 'whitespace)
(require 'start-opt)

(require 'js-comint)
(setq inferior-js-program-command "rhino")
(add-hook 'js2-mode-hook '(lambda ()
                            (local-set-key "\C-x\C-e" 'js-send-last-sexp)
                            (local-set-key "\C-\M-x" 'js-send-last-sexp-and-go)
                            (local-set-key "\C-cb" 'js-send-buffer)
                            (local-set-key "\C-c\C-b" 'js-send-buffer-and-go)
                            (local-set-key "\C-cl" 'js-load-file-and-go)))

(add-to-list 'nuke-trailing-whitespace-always-major-modes 'csharp-mode)

;;decide settings for specific environs
(if( string-equal system-name "SYS286558")
    (progn
      (setq TeX-command-list (quote (("TeX" "tex \\\\nonstopmode\\\\input %t" TeX-run-TeX nil t) ("LaTeX" "%l \\\\nonstopmode\\\\input{%t}" TeX-run-LaTeX nil t) ("LaTeX PDF" "pdflatex \\\\nonstopmode\\\\input{%t}" TeX-run-LaTeX nil t) ("View" "%v" TeX-run-discard nil nil) ("Print" "gsview32 %f" TeX-run-command t nil) ("File" "dvips %d -o %f " TeX-run-command t nil) ("BibTeX" "bibtex %s" TeX-run-BibTeX nil nil) ("Index" "makeindex %s" TeX-run-command nil t) ("Check" "lacheck %s" TeX-run-compile nil t) ("Other" "" TeX-run-command t t))))
      (require 'latex)
      (require 'cygwin-mount)
      (setq cygwin-mount-cygwin-bin-directory "c:/cygwin/bin/")
      (require 'setup-cygwin)
      (message "Welcome to: %s"(system-name))
      (setq printer-name "JH135ADELL5100")
      (setq ps-header-line-pad 0.15)
      (setq ps-header-offset 12.346456692913385)
      (setq ps-printer-name "\\SYS286558\JH135ADELL5100C")
      (setq ps-top-margin 64.51968503937007)
      (setq default-frame-alist (quote ((foreground-color . "gray") (background-color . "black") (font . "Monaco-10"))))
      (setq tramp-default-method "plink")
      (setq ps-lpr-command "c:\\Program Files\\Ghostgum\\gsview\\gsprint.exe")

      ;; THis line causes ghostscript to query which printer to
      ;; use - which you may not need if, for example, you only
      ;; have one printer.
      (setq ps-lpr-switches '("-query"))

      (setq ps-printer-name t)
      (setq blog-root "/abturet@ariserve.dynup.net:~/blog/")
      (setq muse-project-alist
            '(("WikiPlanner"
               ("~/Plans"           ;; where your Planner pages are located
                :default "TaskPool" ;; use value of `planner-default-page'
                :force-publish ("WelcomePage")
                :major-mode planner-mode
                :visit-link planner-visit-link)
               ;; This next part is for specifying where Planner pages
               ;; should be published and what Muse publishing style to
               ;; use.  In this example, we will use the XHTML publishing
               ;; style.
               (:base "planner-xhtml"
                      ;; where files are published to
                      ;; (the value of `planner-publishing-directory', if
                      ;;  you have a configuration for an older version
                      ;;  of Planner)
                      :path "C:/InetPub/wwwroot/Wiki"
                      :header "c:/InetPub/wwwroot/Wiki/head.html"))
              ("WikiNew"
               ("~/WikiNew"
                :default "WelcomePage"
                :force-publish ("WelcomePage")
                :major-mode muse-mode)
               (:base "xhtml"
                      :path "C:\\InetPub\\wwwroot\\WikiNew"
                      :header "C:/InetPub/wwwroot/WikiNew/head.html")
               (:base "pdf"
                      :path "C:/InetPub/wwwroot/WikiNew/pdf"))
              ("CourseRegistration"
               ("~/Development/CourseRegistration/doc"
                :default "CourseRegistration"
                :force-publish ("CourseRegistration")
                :major-mode muse-mode)
               (:base "xhtml"
                      :path "//syssrv100/Development Web/CourseRegistration/"
                      :include "/*.html"
                      :header "~/Development/CourseRegistration/doc/head.html"
                      :footer "~/Development/CourseRegistration/doc/foot.html"))))
      (setq muse-wiki-interwiki-alist
            '(("EmacsWiki" . "http://www.emavcswiki.org/cgi-bin/wiki/")
              ("WikiPlanner" . "http://localhost/Plans")
              ("WikiNew" . "http://localhost/WikiNew")))
      (server-start)
      )
  (progn
    (message "Welcome to %s"(system-name))
    (setq vm-print-command-switches "-o media=Letter -o page-right=36 -o page-left=36 -o page-top=72 -o page-bottom=72")
    (setq  lpr-switches "-o media=Letter -o page-right=72 -o page-left=72 -o page-top=86 -o page-bottom=86")
    (setq blog-root "/abturet@ariserve.dynup.net:~/blog/")
    ;;  (setq planner-publishing-directory "~/public_html/Plans")
    (setq browse-url-browser-function (quote browse-url-default-macosx-browser))
    (setq browse-url-netscape-program "mozilla")
    (if( string-equal system-name "localhost")
;;;         (setq default-frame-alist (quote ((foreground-color . "white") (background-color . "black") (tool-bar-lines . 1) (menu-bar-lines . 1) (font . "-bitstream-bitstream vera sans mono-medium-r-normal-*-14-0-0-0-m-0-iso8859-1"))))
;;;       (setq default-frame-alist (quote ((foreground-color . "white") (background-color . "black") (tool-bar-lines . 1) (menu-bar-lines . 1) (font . "-bitstream-bitstream vera sans mono-medium-r-normal-*-16-0-0-0-m-0-iso8859-1")))))
        (setq default-frame-alist (quote ((foreground-color . "white") (background-color . "black") (font . "Liberation Mono-12:bold"))))
      (setq default-frame-alist (quote ((foreground-color . "white") (background-color . "black") (font . "Liberation Mono-12:bold")))))

    (setq planner-project "WikiPlanner")
    (setq muse-project-alist
          '(("WikiPlanner"
             ("~/Plans"           ;; where your Planner pages are located
              :default "TaskPool" ;; use value of `planner-default-page'
              :major-mode planner-mode
              :visit-link planner-visit-link)

             ;; This next part is for specifying where Planner pages
             ;; should be published and what Muse publishing style to
             ;; use.  In this example, we will use the XHTML publishing
             ;; style.

             (:base "planner-xhtml"
                    ;; where files are published to
                    ;; (the value of `planner-publishing-directory', if
                    ;;  you have a configuration for an older version
                    ;;  of Planner)
                    :path "~/public_html/Plans/"))
            ("WikiNew"
             ("~/WikiNew"
              :major-mode muse-mode)
             (:base "xhtml"
                    :path "~/public_html/WikiNew"
                    :header "~/public_html/WikiNew/head.html")
             (:base "pdf"
                    :path "~/public_html/WikiNew/pdf"))))
    (setq muse-wiki-interwiki-alist
          '(("EmacsWiki" . "http://www.emacswiki.org/cgi-bin/wiki/")
            ("WikiPlanner" . "http://localhost/Plans")
            ("WikiNew" . "http://localhost/WikiNew")))

    )
  )


(defun insert-time()
  (interactive)
  (insert (format-time-string "%y-%m-%d-%R")))

(require 'semantic-config)
;;#(load-library "javascript")
;;(require 'javascript-mode)
(require 'js2-mode)
(require 'magit)
(require 'highline)
(require 'uniquify)
;;(require 'vm)
(require 'boxquote)
(require 'rs-info)
(require 'terminal)
(require 'tex-site)
(require 'tramp)
(require 'gist)
(require 'ecb)
(require 'speedbar)
;; (require 'xslt-process)
(require 'semantic-load)
(require 'blog)
(require 'erc)
(require 'erc-menu)
(require 'htmlize)
(require 'jdee-config)
(require 'cl)
(require 'semantic-util-modes)
(require 'misc)
(require 'remember)
(require 'skeleton-conf)
(require 'keys-config)
(require 'html-config)
(require 'vb-config)
(require 'xml-config)
(require 'sql-config)
;;(require 'mail-config)
(require 'erc-config)
(require 'gnus-config)
(require 'jde)
(require 'css-mode)
(require 'ari-custom)
(require 'csharp-config)
(require 'muse)
(require 'muse-xml)
(require 'muse-mode)
(require 'muse-html)
(require 'muse-wiki)
(require 'muse-colors)
(require 'muse-latex)
(require 'muse-colors)
(require 'php-mode)
(require 'mmm-mode)
(require 'mmm-auto)
(require 'mmm-sample)
(require 'vc-svn)
(require 'psvn)
(require 'color-theme)
(require 'ruby-mode)
(require 'ruby-electric)
(require 'ruby-config)
;;(require 'rails)
;;(require 'yasnippet)
;;(require 'zenburn)

;;(load-file "~/emacs/site/color-theme/themes/LazyCatTheme.el")
(load-file "~/emacs/site/color-theme/themes/vivid-chalk.el")
(vivid-chalk)
(color-theme-initialize)
;;(color-theme-simple-2)
;;(require 'LazyCatFont)
;;(require 'LazyCatTheme)
;;(color-theme-calm-forest)
(add-to-list 'vc-handled-backends 'SVN)
;; (highline-mode t)
(require 'bbdb)
(if (window-system)
    (require 'oneonone-config))
;;(bbdb-initialize 'w3)
(load-library "mailcrypt")

(mc-setversion "gpg")
(setq mc-gpg-user-id "arit93@yahoo.com")

(setq mc-temp-directory "~/tmp/")
(setq mc-gpg-user-id user-mail-address)

;;ensure correct handling of xml files by nxml-mode
(add-to-list 'auto-mode-alist
             (cons
              (concat "\\." (regexp-opt '("xml" "xsd" "svg" "rss" "rng" "build" "config") t) "\\'" )'nxml-mode))

(setq mmm-global-mode 'maybe)


(mmm-add-group
 'fancy-html
 '(
   (html-php-tagged
    :submode php-mode
    :face mmm-code-submode-face
    :front "<[?]php"
    :back "[?]>")
   (html-css-attribute
    :submode css-mode
    :face mmm-declaration-submode-face
    :front "style=\""
    :back "\"")
   (html-css-embedded
    :submode css-mode
    :face mmm-declaration-submode-face
    :front "<style\[^>\]*>"
    :back "</style>")
   (html-javascript-embedded
    :submode js2-mode
    :face mmm-code-submode-face
    :front "<script\[^>\]*>"
    :back "</script>")
   (html-javascript-attribute
    :submode js2-mode
    :face mmm-code-submode-face
    :front "\\bon\\w+=\\s-*\""
    :back "\"")))
;;
;; What files to invoke the new html-mode for?
(add-to-list 'auto-mode-alist '("\\.inc\\'" . html-mode))
(add-to-list 'auto-mode-alist '("\\.phtml\\'" . html-mode))
(add-to-list 'auto-mode-alist '("\\.php\\'" . html-mode))
(add-to-list 'auto-mode-alist '("\\.[sj]?html?\\'" . html-mode))
(add-to-list 'auto-mode-alist '("\\.jsp\\'" . html-mode))
;;
;; What features should be turned on in this html-mode?
(add-to-list 'mmm-mode-ext-classes-alist '(html-mode nil html-js))
(add-to-list 'mmm-mode-ext-classes-alist '(html-mode nil embedded-css))
(add-to-list 'mmm-mode-ext-classes-alist '(html-mode nil fancy-html))

;;blog class for mmm mode that make editing blogs "pretty" just
;; need to figure out how to add the key-maps to the submode
(mmm-add-classes
 '((blog-html
    :submode html-mode
    :front "<blog>"
    :back "</blog>"
    :include-back t)))

(mmm-add-mode-ext-class 'blog-mode nil 'blog-html)

;;this is where my twit.el hack will go
;;starting now!
(load-file "~/emacs/site/lisp/twit.el")
;; ;;;###autoload
;; (defun twit-send-svn ()
;;   (interactive)
;;   (twit-post-function twit-update-url (progn
;;                                         (save-window-excursion
;;                                           (set-buffer "*svn-log-edit*")
;;                                           (concat
;;                                            (int-to-string svn-status-commit-rev-number)
;;                                            " "
;;                                            (mapconcat 'svn-status-line-info->filename svn-status-files-to-commit " ")
;;                                            " "
;;                                            (buffer-substring-no-properties (point-min)(point-max)))))))
;; (add-hook 'svn-log-edit-done-hook 'twit-send-svn t)


;; (defun tweet-blog(blog-title)
;;   "tweet the new blog entry"
;;   (interactive)
;;   (twit-post-function twit-update-url (progn
;;                                         (save-window-excursion
;;                                           (concat "http://ariserve.dynup.net/blog New Blog Posted: "
;;                                                   blog-title)))))
;; ;;(add-hook 'blog-mode-write-blog-hook 'tweet-blog)
(add-hook 'html-mode-hook 'abbrev-mode)

;;font lock faces
(setq highline-face '((:background "thistle4")))
(setq highline-vertical-face (quote ((t (:background "lemonChiffon2")))))

(if (window-system)
    (progn
      (setq fit-frame-max-width 110)
      (setq fit-frame-max-height 40)))
;;ruby-mode

   (autoload 'ruby-mode "ruby-mode"
     "Mode for editing ruby source files" t)
   (setq auto-mode-alist
         (append '(("\\.rb$" . ruby-mode)) auto-mode-alist))
   (setq interpreter-mode-alist (append '(("ruby" . ruby-mode))
                     interpreter-mode-alist))
   (autoload 'run-ruby "inf-ruby"
     "Run an inferior Ruby process")
   (autoload 'inf-ruby-keys "inf-ruby"
     "Set local key defs for inf-ruby in ruby-mode")
   (add-hook 'ruby-mode-hook
         '(lambda ()
            (inf-ruby-keys)
   ))
(load-library "rdebug")

(autoload 'hideshowvis-enable "hideshowvis" "Highlight foldable regions")

(dolist (hook (list 'emacs-lisp-mode-hook
                    'c++-mode-hook
                    'ruby-mode-hook
                    'c-sharp-mode-hook
                    'java-mode-hook
                    ))
  (add-hook hook 'hideshowvis-enable))




;;transparency
(set-frame-parameter (selected-frame) 'alpha '(85 50))
(add-to-list 'default-frame-alist '(alpha 85 50))

;;;autocomplete
(require 'rcodetools)
(require 'auto-complete)
(require 'auto-complete-config)
;;(require 'auto-complete-ruby)
;;(require 'auto-complete-yasnippet)
;; (require 'anything)
;; (require 'anything-rcodetools)
;; ;; Command to get all RI entries.
;; (setq rct-get-all-methods-command "PAGER=cat fri -l")
;; ;; See docs
;; (define-key anything-map "\C-z" 'anything-execute-persistent-action)
(setq ri-ruby-script (expand-file-name "~/emacs/site/lisp/ri-emacs.rb"))
(autoload 'ri (expand-file-name "~/emacs/site/lisp/ri-ruby.el") nil t)
(load  (expand-file-name "~/emacs/site/lisp/ri-ruby.el"))
;;(setq ac-omni-completion-sources '((ruby-mode .(("\\.\\=" .(ac-source-rcodetools))))))
(setq rct-debug t)



(ido-mode t)
(setq ido-enable-flex-matching t)
(setq ido-create-new-buffer 'always)

;; (require 'slime)
;; ;;(require 'slime-autoloads)
;; (slime-setup)
;; (slime-setup '(slime-fancy))
;; (slime-setup '(slime-repl))
;; (slime-setup '(slime-asdf))
;; (setq inferior-lisp-program "/usr/bin/clisp")

(provide 'emacs-config)
