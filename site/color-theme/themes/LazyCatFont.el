;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 字体设置 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar emacs-english-font "Liberation Mono:Bold"
  "The font name of English.")
(defvar emacs-cjk-font "Liberation Mono:Bold"
  "The font name of CJK.")
(defvar emacs-font-size 10
  "The default font size.")

;; 英文字体
(set-frame-font (format "%s-%s" (eval emacs-english-font) (eval emacs-font-size)))
;; 中文字体
(if window-system
(set-fontset-font (frame-parameter nil 'font)
                  'unicode (eval emacs-cjk-font)))

(provide 'LazyCatFont)
