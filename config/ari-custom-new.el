;;; ari-custom.el --- holds all my own private stuff
;;add a comment
;;; Commentary:
;;; History:
;;; Code:

(defun dos-to-unix()
  (interactive)
  (save-excursion
    (beginning-of-buffer)
    (replace-string "\r\n" "\n")))


(defun unix-to-dos()
  (interactive)
  (save-excursion
    (beginning-of-buffer)
    (replace-string"\n" "\r\n")))

(define-key isearch-mode-map (kbd "C-o")
  (lambda ()
    (interactive)
    (let ((case-fold-search isearch-case-fold-search))
      (occur (if isearch-regexp isearch-string
               (regexp-quote isearch-string))))))

(defun djcb-opacity-modify (&optional dec)
  "modify the transparency of the emacs frame; if DEC is t,
decrease the transparency, otherwise increase it in 10%-steps"
  (let* ((alpha-or-nil (frame-parameter nil 'alpha)) ; nil before setting
         (oldalpha (if alpha-or-nil alpha-or-nil 100))
         (newalpha (if dec (- oldalpha 10) (+ oldalpha 10))))
    (when (and (>= newalpha frame-alpha-lower-limit) (<= newalpha 100))
      (modify-frame-parameters nil (list (cons 'alpha newalpha))))))

(provide 'ari-custom-new)
;;; ari-custom.el ends here
