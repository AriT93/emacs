;; -*- lexical-binding: t; -*-


(message "loading erc-config")
(setq erc-email-userid "AriT93")
(setq erc-minibuffer-notice nil)
(setq erc-modules (quote (autoaway autojoin button fill irccontrols match netsplit noncommands pcomplete completion readonly ring scrolltobottom services smiley stamp track unmorse )))
(setq erc-nick "AriT93")
(setq erc-part-reason (quote erc-part-reason-zippy))
(setq erc-prompt-for-password nil)
(setq erc-prompt-for-nickserv-password nil)
(setq erc-use-auth-source-for-nickserv-password t)
(setq erc-quit-reason (quote erc-quit-reason-zippy))
(setq erc-show-my-nick t)
(setq erc-track-showcount t)
(setq erc-user-full-name "Ari T")
(setq erc-autojoin-channels-alist '(("irc.libera.chat" "#emacs" "#ruby" "#systemcrafters")))
(setq erc-interpret-mirc-color t)
;;(setq erc-nick "AriT93" erc-user-full-name "Ari T")
;; Highlight the entire message where current nickname occurs
(setq erc-current-nick-highlight-type 'all)

;; Set char limit so that long lines conform to word wrap
(setq erc-fill-column 80)

;; Rename server buffers to reflect the current network name instead of SERVER:PORT (e.g., "freenode" instead of "irc.freenode.net:6667").
(setq erc-rename-buffers t)

;; Max buffer size
(setq erc-max-buffer-size 10000)

;; Hide timestamps or not
(setq erc-hide-timestamps nil)

;; Use sensible names for irc buffers
(setq erc-rename-buffers t)

;; Spellcheck - requires 'aspell' package
;;
(require 'erc-spelling)
(erc-spelling-mode t)
(setq erc-hide-list '("JOIN" "PART" "QUIT" "NICK"))

(setq erc-track-exclude-types '("JOIN" "NICK" "PART" "QUIT" "MODE"
                                "324" "329" "332" "333" "353" "477"))
(setq erc-prompt-for-password nil)


(global-set-key "\C-ceb" (lambda () (interactive)
                           (erc-tls :server "irc.libera.chat"
                                :port "6697"
                                :nick "AriT93")))
(setq erc-default-server "irc.libera.chat")
(setq erc-lurker-hide-list '("JOIN" "PART" "QUIT"))
(use-package erc-image
  :ensure t)
(require 'erc-image)
(use-package erc-hl-nicks
  :ensure t)
(add-to-list 'erc-modules 'image)
(add-to-list 'erc-modules 'hl-nicks)
(erc-update-modules)

(setq erc-log-channels-directory "~/.emacs.d/logs/")
(setq erc-save-buffer-on-part nil)
(setq erc-save-queries-on-quit nil
      erc-log-write-after-send t
      erc-log-write-after-insert t)
(setq erc-kill-buffer-on-part t)

;; Kill buffers for private queries after quitting the server
(setq erc-kill-queries-on-quit t)

;; Kill buffers for server messages after quitting the server
(setq erc-kill-server-buffer-on-quit t)
(provide 'erc-config)
