;;JDEE Configuration Variables
(message "loading jdee-config")
(defun debug-mode-hook()
  (message "Debug Hook")
  (define-key comint-mode-map [f5]   'gud-cont)
  (define-key comint-mode-map [f9]   'gud-break)
  (define-key comint-mode-map [f10]  'gud-next)
  (define-key comint-mode-map [f11]  'menu-bar JDEBug Step Into)
  )


;; (require 'cc-mode)
;; ;; FIXME temp hack to get a little better java 1.5 support
;; (let* ((java-keywords
;;   (eval-when-compile
;;     (regexp-opt
;;      '("catch" "do" "else" "super" "this" "finally" "for" "if"
;;        ;; Anders Lindgren <****> says these have gone.
;;        ;; "cast" "byvalue" "future" "generic" "operator" "var"
;;        ;; "inner" "outer" "rest"
;;        "implements" "extends" "throws" "instanceof" "new"
;;        "interface" "return" "switch" "throw" "try" "while"))))
;;        ;;
;;        ;; Classes immediately followed by an object name.
;;        (java-type-names
;;   `(mapconcat 'identity
;;     (cons
;;      ,(eval-when-compile
;;         (regexp-opt '("boolean" "char" "byte" "short" "int" "long"
;;           "float" "double" "void")))
;;      java-font-lock-extra-types)
;;     "\\|"))
;;        (java-type-names-depth `(regexp-opt-depth ,java-type-names))
;;        ;;
;;        ;; These are eventually followed by an object name.
;;        (java-type-specs
;;   (eval-when-compile
;;     (regexp-opt
;;      '("abstract" "const" "final" "synchronized" "transient" "static"
;;        ;; Anders Lindgren <****> says this has gone.
;;        ;; "threadsafe"
;;        "volatile" "public" "private" "protected" "native"
;;        ;; Carl Manning <caroma <at> ai.mit.edu> says this is new.
;;        "strictfp"))))
;;        )

;; (setq java-font-lock-keywords-3
;;   (append

;;    (list
;;     ;; support static import statements
;;     '("\\<\\(import\\)\\>\\s-+\\(static\\)\\s-+\\(\\sw+\\)"
;;       (1 font-lock-keyword-face)
;;       (2 font-lock-keyword-face)
;;       (3 (if (equal (char-after (match-end 0)) ?\.)
;;              'jde-java-font-lock-package-face
;;            'font-lock-type-face))
;;       ("\\=\\.\\(\\sw+\\)" nil nil
;;        (1 (if (and (equal (char-after (match-end 0)) ?\.)
;;                    (not (equal (char-after (+ (match-end 0) 1)) ?\*)))
;;               'jde-java-font-lock-package-face
;;             'font-lock-type-face))))
;;     )

;;    java-font-lock-keywords-2

;;    ;;
;;    ;; More complicated regexps for more complete highlighting for types.
;;    ;; We still have to fontify type specifiers individually, as Java is hairy.
;;    (list
;;     ;;
;;     ;; Fontify class names with ellipses
;;     `(eval .
;;       (cons (concat "\\<\\(" ,java-type-names "\\)\\>\\.\\.\\.[^.]")
;;       '(1 font-lock-type-face)))
;;     ;;
;;     ;; Fontify random types immediately followed by an item or items.
;;     `(eval .
;;       (list (concat "\\<\\(\\(?:" ,java-type-names "\\)"
;;                     "\\(?:\\(?:<.*>\\)\\|\\>\\)\\(?:\\.\\.\\.\\)?\\)"
;;         "\\([ \t]*\\[[ \t]*\\]\\)*"
;;         "\\([ \t]*\\sw\\)")
;;       ;; Fontify each declaration item.
;;       (list 'font-lock-match-c-style-declaration-item-and-skip-to-next
;;       ;; Start and finish with point after the type specifier.
;;       (list 'goto-char (list 'match-beginning
;;            (+ ,java-type-names-depth 3)))
;;       (list 'goto-char (list 'match-beginning
;;            (+ ,java-type-names-depth 3)))
;;       ;; Fontify as a variable or function name.
;;       '(1 (if (match-beginning 2)
;;         font-lock-function-name-face
;;       font-lock-variable-name-face)))))
;;     ;;
;;     ;; Fontify those that are eventually followed by an item or items.
;;     (list (concat "\\<\\(" java-type-specs "\\)\\>"
;;       "\\([ \t]+\\sw+\\>"
;;       "\\([ \t]*\\[[ \t]*\\]\\)*"
;;       "\\)*")
;;     ;; Fontify each declaration item.
;;     '(font-lock-match-c-style-declaration-item-and-skip-to-next
;;       ;; Start with point after all type specifiers.
;;       (goto-char (or (match-beginning 5) (match-end 1)))
;;       ;; Finish with point after first type specifier.
;;       (goto-char (match-end 1))
;;       ;; Fontify as a variable or function name.
;;       (1 (if (match-beginning 2)
;;        font-lock-function-name-face
;;      font-lock-variable-name-face))))

;;       )))
;; )

(setq c-default-style '((java-mode . "java")))

(defun my-java-mode-hook()
  (setq c-basic-offset 2))
(add-hook 'java-mode-hook 'my-java-mode-hook)
(add-hook 'c-mode-common-hook
          (lambda () (c-subword-mode 1))
          (c-toggle-auto-newline 1)
          (c-toggle-electric-state 1))


(provide 'jdee-config)
