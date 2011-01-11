;;; ari-custom.el --- holds all my own private stuff
;;add a comment
;;useful for passing files back and forth from work

;;; Commentary:
;; testing vc-svn-hacks and plink


;;; History:
;;
(message "loading ari-custom")
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


;;ebuild mode for gentoo development
(defun ebuild-mode ()
  "Mode for ebuilds."
(shell-script-mode)
  (sh-set-shell "bash")
  (make-local-variable 'tab-width)
  (setq tab-width 4))
(setq auto-mode-alist (cons '("\\.ebuild$" . ebuild-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.eclass$" . ebuild-mode) auto-mode-alist))

;;find stupid spam crap on my blog
(defun arit93/find-blog-spam()
  "Find comment files with spam."
  (interactive)
  (find-grep-dired (concat blog-root "plugins/state/writeback/") "a href\\|porn\\|sex\\|A href\\|\\[url\\|\\[URL"))


(defun winamp ()
      "Print what winamp is playing."
      (interactive)
      (let
((str (shell-command-to-string "c://WINDOWS/system32/tasklist.exe /v")))
    (when (string-match "^winamp.exe.*?[0-9]+\\. \\(.*?\\)[ \t]*$" str)
      (match-string 1 str))))


(defun arit93/winamp ()
  "print the winamp song "
  (interactive)
(cond
 ((string="w32" window-system)
  (let
    ((str (winamp)))
  (if (string-match "^.*?[-]\\(.*?\\)[-].*$" str)
      (message "%s"(match-string 1 str))
    (message "Not Found"))))))

;;;###autoload
(defun arit93/hs-minor-mode-setup()
  "Turn on `hs-minor-mode' and hide everything."
  (hs-minor-mode 1)
  (hs-hide-all)
  (set(make-variable-buffer-local 'my-hs-hide) t))
;;;   (push '(csharp-mode
;;;           "{" "}"
;;;           "\\(^\\s *#\\s *region\\b\\)\\|{"
;;;           "\\(^\\s *#\\s *endregion\\b\\)\\|}"
;;;           nil
;;;           hs-c-like-adjust-block-beginning)
;;;        hs-special-modes-alist)
;;stolen ruthlessly from sacha chua.  changed func prefix from sacha to arit93
;; for some level of consistency in my configs
;; code originally found at:   http://sachachua.com/notebook/emacs/hideshow-config.el
(load-library "hideshow")
(defvar my-hs-hide t "current state of hideshow for toggling all.")
;;;###autoload
(defun my-toggle-hideshow-all()
  "Toggle hideshow all."
  (interactive)
  (set (make-variable-buffer-local 'my-hs-hide) (not my-hs-hide))
  (if my-hs-hide
      (hs-hide-all)
    (hs-show-all)))

(add-hook 'csharp-mode-hook 'arit93/hs-minor-mode-setup)

;; Maybe make this a define-key for hs-minor-mode-map to be proper?
(global-set-key (kbd "C-c @ @") 'my-toggle-hideshow-all)
(global-set-key (kbd "C-c @ h") 'hs-hide-block)
(global-set-key (kbd "C-c @ s") 'hs-show-block)
(global-set-key (kbd "C-c @ SPC") 'hs-show-block)

(defun cc-mode-add-keywords (mode)
  (font-lock-add-keywords
   mode
   '(("\t+" (0 'my-tab-face append))
     ("^.\\{81,\\}$" (0 'my-long-line-face t))
     ;;("^.\\{81,\\}$" (0 'my-long-line-face append))
     ("[ \t]+$"      (0
                      'my-trailing-space-face t)))))

(cc-mode-add-keywords 'csharp-mode)

(define-key isearch-mode-map (kbd "C-o")
  (lambda ()
    (interactive)
    (let ((case-fold-search isearch-case-fold-search))
      (occur (if isearch-regexp isearch-string
               (regexp-quote isearch-string))))))

;;; dealing with xml in buffers does some nice pretty printing
(defun xml-pretty-print-region (start end)
  (interactive "r")
  (let ((cb (current-buffer))
        (buf (get-buffer-create "*xml*")))
    (set-buffer buf)
    (erase-buffer)
    (set-buffer cb)
    (copy-to-buffer buf start end)
    (switch-to-buffer-other-window buf)
    (xml-mode)
    (join-broken-lines (point-min) (point-max))
    (sgml-pretty-print (point-min) (point-max))
    (other-window -1)))

(defconst cr (string ?\n))
(defconst *broken-line-regex* cr)

(defun join-broken-lines (start end)
  (interactive "r")
  (goto-char start)
  (while (re-search-forward *broken-line-regex* end t)
    (replace-match "" nil nil)))

(defun xml-pretty-print-current ()
  (interactive)
  (save-excursion
    (end-of-line nil)
    (re-search-backward ">" 1)
    (let ((e (+ 1 (point))))
      (beginning-of-line nil)
      (re-search-forward "<?xml[^>]*>" e)
      (xml-pretty-print-region (point) e))))

;transparent emacs!!1!!1!one!!
(defun djcb-opacity-modify (&optional dec)
  "modify the transparency of the emacs frame; if DEC is t,
decrease the transparency, otherwise increase it in 10%-steps"
  (let* ((alpha-or-nil (frame-parameter nil 'alpha)) ; nil before setting
(oldalpha (if alpha-or-nil alpha-or-nil 100))
(newalpha (if dec (- oldalpha 10) (+ oldalpha 10))))
    (when (and (>= newalpha frame-alpha-lower-limit) (<= newalpha 100))
      (modify-frame-parameters nil (list (cons 'alpha newalpha))))))



(provide 'ari-custom)

;;; ari-custom.el ends here
