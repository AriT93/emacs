(message "loading html-config")
;
; html helper
;
;;(autoload 'html-helper-mode "html-helper-mode" "HTML Helper Mode" t)


 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; HTML mode
;
;; (setq html-helper-basic-offset 4)
;; (setq html-helper-never-indent nil)
;; (setq tempo-interactive t)
;; (setq html-helper-do-write-file-hooks t)
;; (setq html-helper-build-new-buffer t)
;; (setq html-helper-address-string "
;;<a href=\"http://www.ais.ilstu.edu\">Ari Turetzky</a>
;;<a href=\"mailto:abturet@ilstu.edu\">&lt;abturet@ilstu.edu&gt;</a>
;;")
;;(setq html-helper-htmldts-version "<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Strict//EN\"\n \"http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd\">")
;;(add-hook 'html-mode-hook 'turn-off-auto-fill)

;;; css- mode;;

;;(add-hook 'html-mode-hook 'mumamo-alias-html-mumamo-mode)
(autoload 'css-mode "css-mode")

(provide 'html-config)