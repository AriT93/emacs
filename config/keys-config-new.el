;; -*- lexical-binding: t; -*-
 (global-set-key [home] 'beginning-of-line)
 (global-set-key [end]  'end-of-line)
 (global-set-key [C-home] 'beginning-of-buffer)
 (global-set-key [C-end] 'end-of-buffer)

 (global-set-key [pause] 'erase-buffer)
 (global-set-key [f4] 'goto-line )

(global-set-key [f12] 'write-blog)
(global-set-key [S-f12] 'write-blog-daily)

(global-set-key [f5] 'compile)
(define-key comint-mode-map [f5]   'gud-cont)
(define-key comint-mode-map [f9]   'gud-break)
(define-key comint-mode-map [f10]  'gud-next)

(global-set-key (kbd "C-c o") 'occur)

(global-set-key (kbd "M-+") 'text-scale-adjust)
(global-set-key (kbd "M--") 'text-scale-adjust)
(global-set-key (kbd "M-0") 'text-scale-adjust)

;; (global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
;; (global-set-key (kbd "C->") 'mc/mark-next-like-this)
;; (global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
;; (global-set-key (kbd "C-c C->") 'mc/mark-all-like-this)

(require 'ari-custom-new)
(global-set-key (kbd "C-9") (lambda()(interactive)(djcb-opacity-modify)))
(global-set-key (kbd "C-8") (lambda()(interactive)(djcb-opacity-modify t)))
(global-set-key (kbd "C-0") (lambda()(interactive)
                               (modify-frame-parameters nil `((alpha . 100)))))

;; S-C-left and right will horizontally resize windows
 ;; S-C-up and down will vertically resize them
 (global-set-key (kbd "S-C-<left>") 'shrink-window-horizontally)
 (global-set-key (kbd "S-C-<right>") 'enlarge-window-horizontally)
 (global-set-key (kbd "S-C-<down>") 'shrink-window)
 (global-set-key (kbd "S-C-<up>") 'enlarge-window)
;; (global-set-key (kbd "M-o") 'ace-window)

(global-set-key (kbd "C-h f") #'helpful-callable)
(global-set-key (kbd "C-h F") #'helpful-function)
(global-set-key (kbd "C-h C") #'helpful-command)
(global-set-key (kbd "C-h v") #'helpful-variable)
(global-set-key (kbd "C-h k") #'helpful-key)
(global-set-key (kbd "C-c C-d") #'helpful-at-point)
(setq counsel-describe-function-function #'helpful-callable)
(setq counsel-describe-variable-function #'helpful-variable)

(global-set-key "\C-cd" `dash-at-point)

(provide 'keys-config-new)
