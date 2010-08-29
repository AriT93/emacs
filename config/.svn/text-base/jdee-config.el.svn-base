;;JDEE Configuration Variables
(message "loading jdee-config")
(defun debug-mode-hook()
  (message "Debug Hook")
  (define-key comint-mode-map [f5]   'gud-cont)
  (define-key comint-mode-map [f9]   'gud-break)
  (define-key comint-mode-map [f10]  'gud-next)
  (define-key comint-mode-map [f11]  'menu-bar JDEBug Step Into)
  )

;; If you want Emacs to defer loading the JDE until you open a
;; Java file, edit the following line
;; (setq defer-loading-jde nil)
;; to read:
;;
(setq defer-loading-jde t)
;;

(if defer-loading-jde
     (progn
       (autoload 'jde-mode "jde" "JDE mode." t)
       (setq auto-mode-alist
 	    (append
 	     '(("\\.java\\'" . java-mode))
 	     auto-mode-alist)))
   (require 'jde))


;; Sets the basic indentation for Java source files
;; to two spaces.
(defun my-jde-mode-hook ()
  (setq c-basic-offset 2))

(add-hook 'jde-mode-hook 'my-jde-mode-hook)

(if( string-equal system-name "SYS286558")
	(progn
	  (message "Configuring JDEE for Work system: %s" (system-name))
	  (setq jde-bug-debugger-host-address "127.0.0.1")
	  (setq jde-bug-jdk-directory "C:\\Program Files\\java\\jdk1.6.0_03")
	  (setq jde-bug-jre-home "c:\\Program Files\\Java\\jdk1.d.0_03\\jre")
	  (setq jde-vm-path "c:\\Program Files\\Java\\jdk1.6.0_03\\bin\\java")
	  (setq jde-compile-option-classpath (quote ( "." "..\\"  "H:\\Development\\edu.ilstu.ais\\ILSUT-AIS.jar")))
	  (setq jde-db-option-classpath (quote ("." "H:\\Development\\edu.ilstu.ais\\ILSTU-AIS.jar")))
	  (setq jde-db-source-directories (quote ("." ".." )))
	  (setq jde-jdk-registry (quote (("1.6" . "c:\\Program Files\\Java\\jdk1.6.0_03"))))
	  (setq jde-run-option-classpath (quote ("." ".." "H:\\Development\\edu.ilstu.ais\\ILSTU-AIS.jar")))
      (setq bsh-classpath (quote (("h:\\Development\\edu.ilstu.ais\\ISLTU-AIS.jar"))))
	  ))

(setq jde-bug-stack-info t)
(setq jde-electric-return-mode t)
(setq jde-bug-vm-executable (quote ("javaw")))
(setq jde-bug-vm-includes-jdpa-p t)
(setq jde-build-function (quote (jde-make)))
(setq jde-compile-option-debug (quote ("all" (t nil nil))))
(setq jde-compile-option-sourcepath (quote ("" ".." ".")))
(setq jde-compile-option-target (quote ("1.5")))
(setq jde-compile-option-verbose nil)
(setq jde-compiler (quote ("javac" "")))
(setq jde-db-mode-hook (quote jdb-mode-hook))
(setq jde-db-read-app-args nil)
(setq jde-db-read-vm-args nil)
(setq jde-db-set-initial-breakpoint t)
(setq jde-debugger (quote ("jdb")))
(setq jde-enable-abbrev-mode t)
(setq jde-compile-enable-kill-buffer t)
(setq jde-compile-option-sourcepath (quote (".")))
(setq jde-global-classpath nil)
(setq jde-javadoc-display-doc nil)
(setq jde-jdb-key-bindings (quote (("[f10]" . jde-debug-step-over) ("[f11]" . jde-debug-step-into) ("[S-f11]" . jde-debug-step-out) ("[f5]" . jde-debug-cont) ("[f7]" . jde-debug-run) ("[f9]" . jde-debug-toggle-breakpoint) ("[? ? ?]" . jde-debug-up) ("[? ? ?]" . jde-debug-down))))
(setq jde-run-classic-mode-vm nil)
(setq jde-run-java-vm-w "javaw")
(setq jde-run-read-app-args nil)
(setq jde-run-read-vm-args nil)
(setq jde-sourcepath (quote (".." ".")))



;; (defun jsp-mode() (interactive)
;;   (multi-mode 1
;; 			  'html-mode
;; 			  '("<%--" jde-mode)
;; 			  '("<@" jde-mode)
;; 			  '("<%=" html-mode)
;; 			  '("<%" jde-mode)
;; 			  '("%>" html-mode)
;;               '("<script" javascript-mode)
;;               '("</script" html-mode)))


(require 'cc-mode)
;; FIXME temp hack to get a little better java 1.5 support
(let* ((java-keywords
  (eval-when-compile
    (regexp-opt
     '("catch" "do" "else" "super" "this" "finally" "for" "if"
       ;; Anders Lindgren <****> says these have gone.
       ;; "cast" "byvalue" "future" "generic" "operator" "var"
       ;; "inner" "outer" "rest"
       "implements" "extends" "throws" "instanceof" "new"
       "interface" "return" "switch" "throw" "try" "while"))))
       ;;
       ;; Classes immediately followed by an object name.
       (java-type-names
  `(mapconcat 'identity
    (cons
     ,(eval-when-compile
        (regexp-opt '("boolean" "char" "byte" "short" "int" "long"
          "float" "double" "void")))
     java-font-lock-extra-types)
    "\\|"))
       (java-type-names-depth `(regexp-opt-depth ,java-type-names))
       ;;
       ;; These are eventually followed by an object name.
       (java-type-specs
  (eval-when-compile
    (regexp-opt
     '("abstract" "const" "final" "synchronized" "transient" "static"
       ;; Anders Lindgren <****> says this has gone.
       ;; "threadsafe"
       "volatile" "public" "private" "protected" "native"
       ;; Carl Manning <caroma <at> ai.mit.edu> says this is new.
       "strictfp"))))
       )

(setq java-font-lock-keywords-3
  (append

   (list
    ;; support static import statements
    '("\\<\\(import\\)\\>\\s-+\\(static\\)\\s-+\\(\\sw+\\)"
      (1 font-lock-keyword-face)
      (2 font-lock-keyword-face)
      (3 (if (equal (char-after (match-end 0)) ?\.)
             'jde-java-font-lock-package-face
           'font-lock-type-face))
      ("\\=\\.\\(\\sw+\\)" nil nil
       (1 (if (and (equal (char-after (match-end 0)) ?\.)
                   (not (equal (char-after (+ (match-end 0) 1)) ?\*)))
              'jde-java-font-lock-package-face
            'font-lock-type-face))))
    )

   java-font-lock-keywords-2

   ;;
   ;; More complicated regexps for more complete highlighting for types.
   ;; We still have to fontify type specifiers individually, as Java is hairy.
   (list
    ;;
    ;; Fontify class names with ellipses
    `(eval .
      (cons (concat "\\<\\(" ,java-type-names "\\)\\>\\.\\.\\.[^.]")
      '(1 font-lock-type-face)))
    ;;
    ;; Fontify random types immediately followed by an item or items.
    `(eval .
      (list (concat "\\<\\(\\(?:" ,java-type-names "\\)"
                    "\\(?:\\(?:<.*>\\)\\|\\>\\)\\(?:\\.\\.\\.\\)?\\)"
        "\\([ \t]*\\[[ \t]*\\]\\)*"
        "\\([ \t]*\\sw\\)")
      ;; Fontify each declaration item.
      (list 'font-lock-match-c-style-declaration-item-and-skip-to-next
      ;; Start and finish with point after the type specifier.
      (list 'goto-char (list 'match-beginning
           (+ ,java-type-names-depth 3)))
      (list 'goto-char (list 'match-beginning
           (+ ,java-type-names-depth 3)))
      ;; Fontify as a variable or function name.
      '(1 (if (match-beginning 2)
        font-lock-function-name-face
      font-lock-variable-name-face)))))
    ;;
    ;; Fontify those that are eventually followed by an item or items.
    (list (concat "\\<\\(" java-type-specs "\\)\\>"
      "\\([ \t]+\\sw+\\>"
      "\\([ \t]*\\[[ \t]*\\]\\)*"
      "\\)*")
    ;; Fontify each declaration item.
    '(font-lock-match-c-style-declaration-item-and-skip-to-next
      ;; Start with point after all type specifiers.
      (goto-char (or (match-beginning 5) (match-end 1)))
      ;; Finish with point after first type specifier.
      (goto-char (match-end 1))
      ;; Fontify as a variable or function name.
      (1 (if (match-beginning 2)
       font-lock-function-name-face
     font-lock-variable-name-face))))

      )))
)

(setq c-default-style '((java-mode . "java")))

(defun my-java-mode-hook()
  (setq c-basic-offset 2))
(add-hook 'java-mode-hook 'my-java-mode-hook)
(add-hook 'c-mode-common-hook
	  (lambda () (c-subword-mode 1))
	  (c-toggle-auto-newline 1)
	  (c-toggle-electric-state 1))


(provide 'jdee-config)