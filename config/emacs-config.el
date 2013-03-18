 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; emacs-config.el                                                           ;;
;; This file will hold specific setting I like for emacs out side of         ;;
;; customize.  Mostly requires and such but a few setq's and such as well    ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(if (window-system)
    (tool-bar-mode -1))
(menu-bar-mode -1)
(message "loading emacs-config")
(setq w32-use-full-screen-buffer nil)
(setq uniquify-buffer-name-style t)
(setq uniquify-buffer-name-style (quote post-forward))
(setq uniquify-min-dir-content 1)
(setq cal-tex-diary t)
(setq blog-root "/abturet@turetzky.org:~/blog/")
(add-hook 'diary-display-hook 'fancy-diary-display)
(add-hook 'text-mode-hook ' turn-on-auto-fill)
(autoload 'nuke-trailing-whitespace "whitespace" nil t)
(add-hook 'write-file-hooks 'nuke-trailing-whitespace)
(add-hook 'before-save-hook 'time-stamp)
(setq dired-omit-files-p t)
(add-hook 'dired-load-hook '(lambda () (require 'dired-x)))
(setq TeX-command-list (quote (("TeX" "tex \\\\nonstopmode\\\\input %t" TeX-run-TeX nil t) ("LaTeX" "%l -shell-escape \\\\nonstopmode\\\\input{%t}" TeX-run-LaTeX nil t) ("LaTeX PDF" "pdflatex -shell-escape \\\\nonstopmode\\\\input{%t}" TeX-run-LaTeX nil t) ("View" "%v" TeX-run-discard nil nil) ("Print" "gsview32 %f" TeX-run-command t nil) ("File" "dvips %d -o %f " TeX-run-command t nil) ("BibTeX" "bibtex %s"</FONT> TeX-run-BibTeX nil nil) ("Index" "makeindex %s" TeX-run-command nil t) ("Check" "lacheck %s" TeX-run-compile nil t) ("Other" "" TeX-run-command t t))))

(cond
 ((string="w32" window-system)
  (set-face-attribute 'mode-line nil :family "Century Gothic" :height 1.0 :weight 'ultra-light ))
 ((string="x" window-system)
  (set-face-attribute 'mode-line nil :family "Liberation Mono:bold" :height 1.0 ))
((string="ns" window-system)
 (set-face-attribute 'mode-line nil :family "Monaco" :height 1.0)))

(setq-default compile-command "nmake")
(setq tramp-auto-save-directory "~/tmp")
(setq message-log-max 250)
(require 'load-path-config)
(require 'whitespace)
;;(require 'start-opt)

(add-to-list 'nuke-trailing-whitespace-always-major-modes 'csharp-mode)

(require 'js-comint)
(setq inferior-js-program-command "rhino")
(add-hook 'js2-mode-hook '(lambda ()
                (local-set-key "\C-x\C-e" 'js-send-last-sexp)
                (local-set-key "\C-\M-x" 'js-send-last-sexp-and-go)
                (local-set-key "\C-cb" 'js-send-buffer)
                (local-set-key "\C-c\C-b" 'js-send-buffer-and-go)
                (local-set-key "\C-cl" 'js-load-file-and-go)
                ))

;;decide settings for specific environs
;; (cond
;;  ((string="w32" window-system)
;;   (require 'cygwin-mount)
;;   (setq cygwin-mount-cygwin-bin-directory "c:/cygwin/bin/")
;;   (require 'setup-cygwin)
;;   (set-shell-bash)))


;;(setq default-frame-alist (quote ((foreground-color . "gray") (background-color . "black") (font . "-outline-Liberation Mono-bold-r-normal-normal-14x-97-96-96-c-*-iso8859-1"))))
(cond
 ((string="w32" window-system)
(setq default-frame-alist (quote ((foreground-color . "gray") (background-color . "black") (font . "Consolas-11")))))
 ((string="x" window-system)
(setq default-frame-alist (quote ((foreground-color . "gray") (background-color . "black") (font . "Liberation Mono:bold")))))
((string="ns" window-system)
(setq default-frame-alist (quote ((foreground-color . "gray") (background-color . "black") (font . "Monaco-12:bold"))))))


(setq muse-project-alist
      '(("WikiNew"
         ("H:/Wiki/WikiNew"
          :default "WelcomePage"
          :force-publish ("WelcomePage")
          :major-mode muse-mode)
         (:base "xhtml"
                :path "C:\\Documents and Settings\\All Users\\webdocs\\wwwroot\\WikiNew"
                :style-sheet "<link rel=\"stylesheet\" type=\"text/css\" href=\"http://localhost/WikiNew/style.css\"/>"))))
(setq muse-wiki-interwiki-alist
      '(("EmacsWiki" . "http://www.emavcswiki.org/cgi-bin/wiki/")
        ("WikiPlanner" . "http://localhost/Plans")
        ("WikiNew" . "http://localhost/WikiNew")))
(server-start)



(require 'org)
;; Open links to outlook emails from org
(defun org-open-outlook-url (uid)
  "Open an outlook format url"
  (interactive "sGUID: ")
  (w32-shell-execute nil (format "Outlook:%s" uid)))
;; Open links to notes docs from org
(defun org-open-notes-url (uid)
  "Open an Notes doclink"
  (interactive "sLink: ")
  (w32-shell-execute nil (format "Notes:%s" uid)))
(org-add-link-type "Notes" 'org-open-notes-url)
(org-add-link-type "Outlook" 'org-open-outlook-url)

;;***********remember + Org config*************
(setq org-remember-templates
      '(("Tasks" ?t "* TODO %?\n %i\n %a" "H://todo.org")
        ("Appointments" ?a "* Appointment: %?\n%^T\n%i\n %a" "H://todo.org")))
(setq remember-annotation-functions '(org-remember-annotation))
(setq remember-handler-functions '(org-remember-handler))
(add-hook 'remember-mode-hook 'org-remember-apply-template)
(global-set-key (kbd "C-c r") 'remember)

(setq org-todo-keywords '((type "TODO" "STARTED" "WAITING" "DONE")))
(setq org-todo-keywords-1 '((type "TODO" "STARTED" "WAITING" "DONE")))
(setq org-agenda-include-diary t)
(setq org-agenda-include-all-todo t)





(defun insert-time()
  (interactive)
  (insert (format-time-string "%y-%m-%d-%R")))


(require 'semantic-config)
;;#(load-library "javascript")
;;(require 'javascript-mode)
(require 'js2-mode)
(if (not (string-equal system-type "windows-nt"))(require 'magit))
(require 'highline)
(require 'uniquify)
(require 'sawzall)
;;(require 'vm)
(require 'boxquote)
(require 'rs-info)
(require 'terminal)
(require 'tex-site)
(require 'tramp)
(require 'gist)
(require 'ecb)
(require 'speedbar)
;;(require 'xslt-process)
(require 'semantic-load)
(require 'ls-lisp)
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
;;(require 'color-theme)
(require 'ruby-mode)
;;(require 'ruby-electric)
(require 'org)
(require 'coffee-mode)
(require 'feature-mode)
(require 'ruby-config)
;;(require 'rails)
;;(require 'yasnippet)
;;(require 'zenburn)
;;(require 'vivid-chalk)

(if (window-system)
  ;;  (progn (require 'color-theme)
    ;;       (color-theme-initialize)
           ;;(color-theme-simple-2)
           ;;(color-theme-calm-forest)
           ;;(load-file "~/emacs/site/color-theme/themes/LazyCatTheme.el")
    (load-file "~/emacs/site/color-theme/themes/vivid-chalk.el")
	(vivid-chalk)
	;;(zenburn)
           ;;(color-theme-hash)
       )

;;(add-to-list 'vc-handled-backends 'SVN)
;; (highline-mode t)
;;(require 'bbdb)
;; (if (window-system)
;;     (require 'oneonone-config))
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
(add-to-list 'auto-mode-alist '("\\.t\\'" . perl-mode))
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
;;(load-file "~/emacs/site/lisp/twit.el")
;;;###autoload
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

;;;; ------------------------------------------------------------------------
;;;; --- Frame max toggle - From: "rgb" <rbielaws@...> / gnu.emacs.help / 18 Mar 2005 16:30:32 -0800
;;;; ------------------------------------------------------------------------
(make-variable-frame-local 'my-frame-state)

(defun my-frame-maximize ()
  "Maximize Emacs window in win32"
  (interactive)

  (modify-frame-parameters nil '((my-frame-state . t)))
  (w32-send-sys-command ?\xf030))

 (defun my-frame-restore ()
   "Restore Emacs window in win32"
   (interactive)

   (modify-frame-parameters nil '((my-frame-state . nil)))
   (w32-send-sys-command ?\xF120))

(defun my-frame-toggle ()
  "Maximize/Restore Emacs frame based on `my-frame-state'"
  (interactive)
  (if my-frame-state
          (my-frame-restore)
        (my-frame-maximize)))

(global-set-key (kbd "M-m") 'my-frame-toggle)


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

(setenv "PATH" (concat (getenv "PATH") "/Users/abturet/.rvm/bin:/usr/bin:/bin:/usr/sbin:/sbin:/usr/local/bin:/usr/texbin:/usr/X11/bin:/usr/local/share/npm/bin;c:/CYGWIN/bin;"))
(setenv "CVSROOT" ":ext:mrpy@cctech:/opt/CC/cvs/cvsroot")
(setenv "CVS_RSH" "plink")
(setenv "PLINK_PROTOCOL" "ssh")


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
(setq rct-debug nil)


(setq ac-modes
      (append ac-modes
              '(sql-mode
                sqlplus-mode
                js2-mode
                coffee-mode
                JavaSript-IDE-mode
                text-mode
                css-mode
                html-mode
                perl-mode
                nxhtml-mumamo-mode
                ruby-mode
                eruby-nxhtml-mumamo-mode)))

(require 'cmd-mode)
(require 'ls-lisp)

(dolist (hook (list 'emacs-lisp-mode-hook
                    'c++-mode-hook))
  (add-hook hook 'hideshowvis-enable))
(add-to-list 'write-file-functions 'time-stamp)

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

(global-set-key "\C-cy" '(lambda ()
                           (interactive)
                           (popup-menu 'yank-menu)))
;; kick off flymake for perl
(defun my-perl-startup ()
  "setup perl"
  (interactive)
  (flymake-mode)
  )
(add-hook 'perl-mode-hook 'my-perl-startup)


;; Notes in *scratch* v. 0.2
;; Copyright (c) 2006 by Michal Nazarewicz (mina86/AT/mina86.com)
;; Released under GNU GPL

(defconst scratch-file (expand-file-name "~/.emacs.d/scratch")
"File where content of *scratch* buffer will be read from and saved to.")
(defconst scratch-file-autosave (concat scratch-file ".autosave")
"File where to autosave content of *scratch* buffer.")

(save-excursion
(set-buffer (get-buffer-create "*scratch*"))
(if (file-readable-p scratch-file)
(if (and (file-readable-p scratch-file-autosave)
(file-newer-than-file-p scratch-file-autosave scratch-file)t)
(insert-file-contents scratch-file-autosave nil nil nil t)
(insert-file-contents scratch-file nil nil nil t)
(set-buffer-modified-p nil)))
(auto-save-mode 1)
(setq buffer-auto-save-file-name scratch-file-autosave)
; (setq revert-buffer-function 'scratch-revert)
(fundamental-mode))
(add-hook 'kill-buffer-query-functions 'kill-scratch-buffer)
(add-hook 'kill-emacs-hook 'kill-emacs-scratch-save)

(defun scratch-revert (ignore-auto noconfirm)
(when (file-readable-p scratch-file)
(insert-file-contents scratch-file nil nil nil t)
(set-buffer-modified-p nil)))

(defun kill-scratch-buffer ()
(not (when (string-equal (buffer-name (current-buffer)) "*scratch*")
(delete-region (point-min) (point-max))
(set-buffer-modified-p nil)
(next-buffer)
t)))

(defun kill-emacs-scratch-save ()
(let ((buffer (get-buffer-create "*scratch*")))
(if buffer
(save-excursion
(set-buffer buffer)
(write-region nil nil scratch-file)
(unless (string-equal scratch-file buffer-auto-save-file-name)
(delete-auto-save-file-if-necessary t))))))


(setq mode-line-in-non-selected-windows nil)
;;(setq default-mode-line-format
;;      (quote
;;       (" "
;;        (:propertize global-mode-string
;;                     face 'mode-line-global-face)
;;        mode-line-frame-identification
;;        (:propertize (:eval (if (> (length default-directory) 17)
;;                   (concat "..." (substring default-directory -20))
;;                 default-directory))
;;                     face 'mode-line-folder-face)
;;        (:propertize mode-line-buffer-identification
;;                     face 'mode-line-filename-face)
;;        "   "
;;        (:propertize mode-line-position
;;                     face 'mode-line-folder-face)
;;        "  "
;;        (:propertize mode-name
;;                     help-echo (format-mode-line minor-mode-alist)
;;                     face 'mode-line-mode-face)
;;        (:propertize mode-line-process
;;                     face 'mode-line-process-face)
;;        "   "
;;        (:propertize urgent-org-mode-line
;;                     face 'mode-line-tasks-face)
;;        "   "
;;        "-%-"
;;)))
;;(color-theme-hash)
;;(zenburn)
(vivid-chalk)

(provide 'emacs-config)
