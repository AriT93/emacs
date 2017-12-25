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
;; (defun arit93/hs-minor-mode-setup()
;;   "Turn on `hs-minor-mode' and hide everything."
;;   (hs-minor-mode 1)
;;   (hs-hide-all)
;;   (set(make-variable-buffer-local 'my-hs-hide) t))
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
;; (load-library "hidershow")
;; (defvar my-hs-hide t "current state of hideshow for toggling all.")
;; ;;;###autoload
;; (defun my-toggle-hideshow-all()
;;   "Toggle hideshow all."
;;   (interactive)
;;   (set (make-variable-buffer-local 'my-hs-hide) (not my-hs-hide))
;;   (if my-hs-hide
;;       (hs-hide-all)
;;     (hs-show-all)))

;; (add-hook 'csharp-mode-hook 'arit93/hs-minor-mode-setup)

;; Maybe make this a define-key for hs-minor-mode-map to be proper?
;; (global-set-key (kbd "C-c @ @") 'my-toggle-hideshow-all)
;; (global-set-key (kbd "C-c @ h") 'hs-hide-block)
;; (global-set-key (kbd "C-c @ s") 'hs-show-block)
;; (global-set-key (kbd "C-c @ SPC") 'hs-show-block)

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

;; (defun color-theme-hash ()
;;   (interactive)
;;   (color-theme-install
;;    '(color-theme-hash
;;      ((background-color . "#121212")
;;       (background-mode  . dark)
;;       (border-color     . "#000000")
;;       (cursor-color     . "#ddffdd")
;;       (foreground-color . "#cccccc")
;;       (mouse-color      . "black"))
;;      (fringe ((t (:background "#121212"))))
;;      (vertical-border ((t (:foreground "#111111"))))
;;      (mode-line ((t (:inherit variable-pitch :foreground "#555555" :background "#000000" :box (:color "#000000" :line-width 6)))))
;;      (mode-line-inactive ((t (:inherit variable-pitch :foreground "#151515" :background "#000000" :box (:color "#000000" :line-width 6)))))
;;      (region ((t (:foreground "#666666" :background "#303030"))))
;;      (font-lock-builtin-face ((t (:foreground "#82b8f2"))))
;;      (font-lock-comment-face ((t (:inherit variable-pitch :foreground "#8d6d6d" :italic t))))
;;      (font-lock-comment-delimiter-face ((t (:foreground "#403b3b"))))
;;      (font-lock-constant-face ((t (:foreground "#6a88d7"))))
;;      (font-lock-doc-string-face ((t (:foreground "#2b2b2b" :italic t))))
;;      (font-lock-doc-face ((t (:inherit variable-pitch :foreground "#6b9b8b" :italic t))))
;;      (font-lock-reference-face ((t (:foreground "red"))))
;;      (font-lock-reference-name-face ((t (:foreground "red"))))
;;      (font-lock-operator-face ((t (:foreground "#bbccbb" :bold t))))
;;      (font-lock-negation-char-face ((t (:foreground "#dd4444" :bold t))))
;;      (font-lock-function-name-face ((t (:foreground "#dd8888" :bold t))))
;;      (font-lock-keyword-face ((t (:foreground "#8aa8e7"))))
;;      (font-lock-preprocessor-face ((t (:foreground "#cb99e1"))))
;;      (font-lock-string-face ((t (:foreground "#cb99e1"))))
;;      (font-lock-type-face ((t (:foreground"#b7e234" :bold t))))
;;      (font-lock-variable-name-face ((t (:foreground "#888888"))))
;;      (font-lock-paren-face ((t (:foreground "#666677" :bold t))))
;;      (minibuffer-prompt ((t (:foreground "#85c0ff" :bold t))))
;;      (font-lock-warning-face ((t (:foreground "#ffbbbb"))))
;;      (show-paren-match-face ((t (:foreground "black" :background "#85c0ff" :bold t))))
;;      (show-paren-mismatch-face ((t (:foreground "black" :background "#dd4444" :bold t))))
;;      (org-hide ((t (:foreground "#121212"))))
;;      (org-link ((t (:foreground "#777788" :italic t :height 0.9))))
;;      (org-date ((t (:foreground "#88bbff"))))
;;      (org-agenda-date ((t (:foreground "#88bbff"))))
;;      (org-level-1 ((t (:foreground "#eeeeee" :bold t :height 1.3))))
;;      (org-level-2 ((t (:foreground "#8888bb" :height 1.1))))
;;      (org-level-3 ((t (:foreground "#666699" :height 1.0))))
;;      (org-level-4 ((t (:foreground "#666666" :style italic))))
;;      (org-tag ((t (:foreground "lightgreen"))))
;;      (org-todo ((t (:foreground "#ffccee" :bold t))))
;;      (org-done ((t (:foreground "#44ff88" :bold t))))
;;      (org-warning ((t (:foreground "#775555" :italic t))))
;;      (org-special-keyword ((t (:background "#000000" :foreground "#eeccaa"))))
;;      (org-verbatim ((t (:foreground) "#6666ff")))
;;      (org-block ((t (:foreground) "#6666ff")))
;;      (org-quote ((t (:foreground) "#6666ff")))
;;      (org-verse ((t (:foreground) "#6666ff")))
;;      (org-table ((t (:inherit fixed-pitch))))
;;      (eshell-prompt ((t (:foreground "#444444"))))
;;      (mode-line-global-face ((t (:foreground "#b7e234" :bold t))))
;;      (mode-line-folder-face ((t (:foreground "#888899"))))
;;      (mode-line-filename-face ((t (:foreground "#bbbbff" :bold t))))
;;      (mode-line-mode-face ((t (:foreground "#b7e234" :bold t))))
;;      (mode-line-mode-process-face ((t (:foreground "#ffc234"))))
;;      (mode-line-tasks-face ((t (:foreground "#ffaaaa"))))
;;     )))


(provide 'ari-custom)


;;; ari-custom.el ends here
