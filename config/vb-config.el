(message "loading vb-config")
;;///////////////////////VB Mode///////////////////////////
(autoload 'visual-basic-mode "visual-basic-mode" "Visual Basic Mode." t)
(setq auto-mode-alist (append'(("\\.\\(frm\\|bas\\|cls\\|vbs\\)$".
				visual-basic-mode)) auto-mode-alist))

(provide 'vb-config)