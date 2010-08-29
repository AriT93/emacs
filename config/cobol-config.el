(message "loading cobol-config")
(autoload 'cobol-mode "cobol" "COBOL Editing mode" t)

(add-hook 'cobol-mode-hook
          '(lambda ()
             (set (make-local-variable 'dabbrev-case-fold-search) t)
             (set (make-local-variable 'dabbrev-case-replace) t)))

(setq auto-mode-alist (cons '("\\.cbl\\'" . cobol-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.cob\\'" . cobol-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.COB\\'" . cobol-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.CBL\\'" . cobol-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.xdr\\'" . xml-mode) auto-mode-alist))


    ;; to associate the mode with the file-extension ".cbl"

(speedbar-add-supported-extension "cs")
(speedbar-add-supported-extension "CBL")
(speedbar-add-supported-extension "cob")
(speedbar-add-supported-extension "cbl")
(speedbar-add-supported-extension "COB")


(provide 'cobol-config)