(message "loading oneonone-config")
(eval-after-load "replace" '(progn (require 'replace+)))
(eval-after-load "isearch" '(require 'isearch+))
(eval-after-load "thingatpt" '(require 'thingatpt+))
(require 'compile+)
(eval-after-load "window" '(require 'window+))
(require 'compile-)
(require 'frame-cmds)
(require 'frame-fns)
;;(require 'fit-frame)
(require 'highlight)
(require 'mouse+)
(require 'buff-menu+)
(require 'misc-fns)
(global-set-key [down-mouse-2]        'mouse-flash-position-or-M-x)
(global-set-key [S-down-mouse-2]      'mouse-scan-lines-or-M-:)
(global-set-key [mode-line C-mouse-1] 'mouse-tear-off-window)
(global-set-key [f6] 'fit-frame)
(global-set-key [f3] 'ffap-other-window)
(substitute-key-definition 'query-replace 'query-replace-w-options global-map)

(require 'strings)

(eval-after-load "simple" '(require 'simple+))
(require 'grep+)
(eval-after-load "frame" '(require 'frame+))
;;(require 'autofit-frame)
;;(add-hook 'after-make-frame-functions 'fit-frame)
;;(add-hook 'temp-buffer-show-hook
  ;;        'fit-frame-if-one-window 'append)
(eval-after-load "faces" '(require 'faces+))

(eval-after-load "files" '(require 'files+))
(eval-after-load "ring" '(progn (require 'ring+)))

(require 'dired+)


;;(require 'thumb-frm)
;;(require 'oneonone)

(provide 'oneonone-config)
