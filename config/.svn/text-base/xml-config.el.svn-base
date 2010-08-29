(message "loading xml-config")
;; XSL mode
(autoload 'xsl-mode "xslide" "Major mode for XSL stylesheets." t)

;; Uncomment if you want to use `xsl-grep' outside of XSL files.
;(autoload 'xsl-grep "xslide" "Grep for PATTERN in files matching FILESPEC." t)

;; Uncomment if you want to use `xslide-process' in `xml-mode'.
;(autoload 'xsl-process "xslide-process" "Process an XSL stylesheet." t)
;(add-hook 'xml-mode-hook
;	  (lambda ()
;	    (define-key xml-mode-map [(control c) (meta control p)]
;	      'xsl-process)))

;; Turn on font lock when in XSL mode
(add-hook 'xsl-mode-hook
	  'turn-on-font-lock)
;; Uncomment if using abbreviations
;;(abbrev-mode t)
;; Set syntax coloring for PSGML
(setq-default sgml-set-face t)


(add-hook 'sgml-mode-hook 'xslt-process-mode)
(add-hook 'xml-mode-hook 'xslt-process-mode)
(add-hook 'nxml-mode-hook 'xslt-process-mode)
(add-hook 'xsl-mode 'xslt-process-mode)

;;  (setq-default mode-line-buffer-identification
;;  	      '((buffer-file-name
;;  		 #("%12f" 0 4 (face bold))
;;  		 #("%12b" 0 4 (face bold)))))


(provide 'xml-config)