;;; VB editing
(autoload 'visual-basic-mode "visual-basic-mode" "Visual Basic mode." t)
(setq auto-mode-alist (append '(("\\.\\(frm\\|bas\\|cls\\)$" .
                                  visual-basic-mode)) auto-mode-alist))
(autoload 'vbp-mode "visual-basic-mode" "VBP mode." t)
(setq auto-mode-alist (append '(("\\.\\(vbg\\|vbg\\)$" .
                                  vbp-mode)) auto-mode-alist))
(setq visual-basic-ide-pathname "E:/Program Files/DevStudio/VB/VB5.EXE")

(autoload 'vbp-mode "vbp-mode" "VBP mode." t)
(setq auto-mode-alist (append '(("\\.vbp$" .
                     vbp-mode)) auto-mode-alist))

