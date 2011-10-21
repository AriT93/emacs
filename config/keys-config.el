(message "loading keys-config")
(global-set-key [home] 'beginning-of-line)
(global-set-key [end]  'end-of-line)
(global-set-key [C-home] 'beginning-of-buffer)
(global-set-key [C-end] 'end-of-buffer)
(global-set-key [f2] 'senator-jump)
(global-set-key [f8] 'jde-debug)
(global-set-key [S-f5] 'jde-run)
(global-set-key [pause] 'erase-buffer)
(global-set-key [f4] 'goto-line )
(global-set-key [f12] 'write-blog)
(global-set-key [S-f12] 'write-blog-daily)
(global-set-key [f5] 'compile)
(global-set-key (kbd "C-c o") 'occur)
(global-set-key (kbd "M-+") 'text-scale-adjust)
(global-set-key (kbd "M--") 'text-scale-adjust)
(global-set-key (kbd "M-0") 'text-scale-adjust)
(define-key comint-mode-map [f5]   'gud-cont)
(define-key comint-mode-map [f9]   'gud-break)
(define-key comint-mode-map [f10]  'gud-next)

;; adding a coment
;; C-8 will increase opacity (== decrease transparency)
;; C-9 will decrease opacity (== increase transparency
;; C-0 will returns the state to normal
(global-set-key (kbd "C-9") '(lambda()(interactive)(djcb-opacity-modify)))
(global-set-key (kbd "C-8") '(lambda()(interactive)(djcb-opacity-modify t)))
(global-set-key (kbd "C-0") '(lambda()(interactive)
                               (modify-frame-parameters nil `((alpha . 100)))))

;; S-C-left and right will horizontally resize windows
;; S-C-up and down will vertically resize them
(global-set-key (kbd "S-C-<left>") 'shrink-window-horizontally)
(global-set-key (kbd "S-C-<right>") 'enlarge-window-horizontally)
(global-set-key (kbd "S-C-<down>") 'shrink-window)
(global-set-key (kbd "S-C-<up>") 'enlarge-window)
(provide 'keys-config)
