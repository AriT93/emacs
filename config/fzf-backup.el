;; -*- lexical-binding: t; -*-
;; FZF CONFIGURATION BACKUP
;; This file contains the fzf configuration that was removed from emacs-config.org
;; To restore fzf integration, copy this configuration back to emacs-config.org
;; and uncomment the lines.

;; #+BEGIN_SRC elisp
;;   (use-package fzf
;;     :bind
;;     ;; Don't forget to set keybinds!
;;     :config
;;     (setq fzf/args "-x --color bw --print-query --margin=1,0 --no-hscroll"
;;           fzf/preview-command "bat --style=numbers,changes --color=always --line-range :40 {}"
;;           fzf/args-for-preview "bat --style=numbers,changes --color=always --line-range :40 {}"
;;           fzf/executable "fzf"
;;           fzf/git-grep-args "-i --line-number %s"
;;           ;; command used for `fzf-grep-*` functions
;;           ;; example usage for ripgrep:
;;           ;; fzf/grep-command "rg --no-heading -nH"
;;           fzf/grep-command "grep -nrH"
;;           ;; If nil, the fzf buffer will appear at the top of the window
;;           fzf/position-bottom t
;;           fzf/window-height 15))
;; #+END_SRC

;; Original comment: "Lets use some fzf here too"
;; Replaced with: "Use the modern completion stack (vertico, consult, embark, orderless) for enhanced search and navigation" 