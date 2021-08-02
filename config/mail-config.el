;;;mail-config --- Summary
;;; Commentary:
;;; Code:

(require 'smtpmail)
(add-to-list 'load-path "/usr/local/Cellar/mu/1.6.0/share/emacs/site-lisp/mu/mu4e/")
(require 'mu4e)

(setq   mail-user-agent 'mu4e-user-agent)
(setq user-full-name "Ari Turetzky")
(setq user-mail-address "arit93@gmail.com")
(setq   send-mail-function 'smtpmail-send-it)
(setq   message-send-mail-function 'smtpmail-send-it)
(setq   smtpmail-default-smtp-server "smtp.gmail.com")
(setq   smtpmail-smtp-server "smtp.gmail.com")
(setq   smtpmail-starttls-credentials '(("smtp.gmail.com" 587 nil nil)))
(setq   starttls-use-gnutls t)
(setq   smtpmail-auth-credentials '(("smtp.gmail.com" 587 "arit93@gmail.com" nil)))
(setq   smtpmail-smtp-service 587)



;; ;;mu4e


(setq mu4e-maildir (expand-file-name "~/.mail"))
(setq mu4e-completing-read-function #'ivy-completing-read)
(setq mu4e-drafts-folder "/gmail/[Gmail]/.Drafts")
(setq mu4e-sent-folder   "/gmail/[Gmail]/.Sent Mail")
(setq mu4e-trash-folder  "/gmail/[Gmail]/.Trash")
;;mu4e-html2text-command "w3m -T text/html"
;;(setq   mu4e-html2text-command "textutil -stdin -format html -convert txt -stdout")
(setq mu4e-update-interval 600)
(setq mu4e-headers-auto-update t)
(setq mu4e-compose-signature-auto-include nil)
(setq mu4e-change-filenames-when-moving t)
(add-to-list 'mu4e-view-actions '("ViewinBrowser" . mu4e-action-view-in-browser) t)
(add-to-list 'mu4e-view-actions '("XWidget View" . mu4e-action-view-with-xwidget) t)
(setq mu4e-view-show-images t)
(setq mu4e-view-show-addresses t)
(set-face-attribute 'mu4e-view-body-face nil :family "Helvetica")
(setq mu4e-use-fancy-chars t)
(require 'org-mu4e)
(require 'mu4e-view-xwidget)
(when (fboundp 'imagemagick-register-types)
  (imagemagick-register-types))
(setq mu4e-mu-binary "/usr/local/bin/mu")
(setq mu4e-compose-reply-to-address nil)

;; don't save message to Sent Messages, GMail/IMAP will take care of this
(setq mu4e-sent-messages-behavior 'delete)


(setq mu4e-contexts
      (list
       (make-mu4e-context
        :name "Gmail"
        :match-func (lambda (msg)
                      (when msg
                        (string-match-p "^/gmail" (mu4e-message-field msg :maildir))))
        ;;                        (mu4e-message-contact-field-matches msg :to "arit93@gmail.com")))
        :vars '( ( user-mail-address . "arit93@gmail.com")
                 (user-full-name   . "Ari Turetzky")
                 (mu4e-sent-folder . "/gmail/[Gmail]/.Sent Mail")
                 (mu4e-trash-folder . "/gmail/[Gmail]/.Trash")
                 (mu4e-drafts-folder . "/gmail/[Gmail]/.Drafts")
                 (smtpmail-default-smtp-server . "smtp.gmail.com")
                 (smtpmail-smtp-server .  "smtp.gmail.com")
                 (smtpmail-starttls-credentials '(("smtp.gmail.com" 587 nil nil)))
                 (smtpmail-auth-credentials '(("smtp.gmail.com" 587 "arit93@gmail.com" nil)))
                 (smtpmail-smtp-service .  587)
                 (mu4e-compose-reply-to-address . "arit93@gmail.com")
                 )
        )
       (make-mu4e-context
        :name "Workday"
        :match-func(lambda (msg)
                     (when msg
                       (string-match-p "^/work" (mu4e-message-field msg :maildir))))
        :vars '( ( user-mail-address . "ari.turetzky@workday.com")
                 (user-full-name   . "Ari Turetzky")
                 (mu4e-trash-folder . "/work/Deleted Items")
                 (mu4e-sent-folder . "/work/Sent Items")
                 (mu4e-drafts-folder . "/work/Drafts")
                 (smtpmail-default-smtp-server . "outlook.office365.com")
                 (smtpmail-smtp-server .  "outlook.office365.com")
                 (smtpmail-starttls-credentials   '(("outlook.office365.com" 587 nil nil)))
                 (smtpmail-auth-credentials '(("outlook.office365.com" 587 "ari.turetzky@workdayinternal.com" nil)))
                 (smtpmail-smtp-service . 587)
                 (mu4e-compose-reply-to-address . "ari.turetzky@workday.com")
                 )

        )))

(setq mu4e-context-policy 'pick-first)
(setq mu4e-compose-context-policy nil)

(add-to-list 'mu4e-bookmarks
             '(:name "Workday Unread"
                     :key ?i
                     :query "flag:unread AND (maildir:/work/Inbox)"))

;; setup some handy shortcuts
(setq mu4e-maildir-shortcuts
      '(("/gmail/Inbox"             . ?i)
        ("/gmail/[Gmail]/.Sent Mail" . ?s)
        ("/gmail/[Gmail]/.Trash"     . ?t)
        ("/work/Inbox" . ?w)))

;; allow for updating mail using 'U' in the main view:
(use-package pinentry
  :ensure t)
(setq mu4e-get-mail-command "mbsync -a")
(setq mu4e-get-mail-command (format "INSIDE_EMACS=%s mbsync -a" emacs-version) epa-pinentry-mode 'ask)
(pinentry-start)
(require `mu4e-icalendar)
(require 'gnus-icalendar)
(gnus-icalendar-setup)
(setq message-kill-buffer-on-exit t)
(setq mu4e-view-use-gnus t)
(mu4e-icalendar-setup)
(setq gnus-icalendar-org-capture-file "~/Documents/notes/agenda.org")
(setq gnus-icalendar-org-capture-headline '("Calendar"))
(gnus-icalendar-org-setup)
(setq abt/mu4e-inbox-query
      "(maildir:/work/Inbox OR maildir:/gmail/Inbox ) AND flag:unread")
(setq mu4e-view-prefer-html nil
      mu4e-html2text-command 'mu4e-shr2text
      shr-color-visible-luminance-min 80)
(setq mm-discouraged-alternatives '("text/html" "text/richtext" "image/.*"))
(use-package mu4e-alert
  :after mu4e
  :ensure t
  :config
  (setq mu4e-alert-interesting-mail-query abt/mu4e-inbox-query)
  (mu4e-alert-set-default-style 'notifier)
  (setq mu4e-alert-notify-repeated-mails nil)
  (mu4e-alert-enable-notifications))


(defun abt/mu4e-view-settings ()
  "settings for mu4e-view-mode"
  (visual-line-mode)
  (olivetti-mode)
  (variable-pitch-mode)
  (flyspell-mode)
  )
(add-hook 'mu4e-view-mode-hook #'abt/mu4e-view-settings)
(provide 'mail-config)
