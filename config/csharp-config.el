;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;              C# Mode support
;;;
(message "loading csharp-mode")
(autoload 'csharp-mode "csharp-mode" "Major mode for editing C# code." t)
(require 'csharp-mode)
  ;;semantic-ific
(require 'semantic-load)

;;  (c-add-style "myC#Style"
;;    '("java"
;;    (c-basic-offset . 2)
;;    (c-comment-only-line-offset . (0 . 0))))

(defun my-csharp-mode-hook ()
  (message "C# Rocks!")
  (set (make-local-variable 'compile-command)
       "nant -buildfile:"))

;; Patterns for finding Microsoft C# compiler error messages:
(require 'compile)
(push '("^\\(.*\\)(\\([0-9]+\\),\\([0-9]+\\)): error" 1 2 3 2) compilation-error-regexp-alist)
(push '("^\\(.*\\)(\\([0-9]+\\),\\([0-9]+\\)): warning" 1 2 3 1) compilation-error-regexp-alist)

(defun csharp-lineup-string-cont (langelem)
  "Like `c-lineup-string-cont' but works with csharp string continuations."
  (save-excursion
    (back-to-indentation)
    (and (looking-at "@?\\s\"")
         (let ((quote (if (equal (char-after) ?@)
                          (char-after (1+ (point)))
                        (char-after)))
               pos)
           (while (and (progn (c-backward-syntactic-ws)
                              (when (eq (char-before) ?+)
                                (backward-char)
                                (c-backward-syntactic-ws))
                              (eq (char-before) quote))
                       (c-safe (c-backward-sexp) t)
                       ;; uncomment this to lineup under the @
                       ;;(progn (if (eq (char-before) ?@) (backward-char)) t)
                       (/= (setq pos (point)) (c-point 'boi))))
           (when pos
             (goto-char pos)
             (vector (current-column)))))))

  (add-hook 'csharp-mode-hook 'my-csharp-mode-hook)
(setq auto-mode-alist
      (append '(
		("\\.cs$" . csharp-mode)
		) auto-mode-alist ))
(setq c-offsets-alist (append '(statement-cont . (my-lineup-csharp-string-cont +))))
(provide 'csharp-config)