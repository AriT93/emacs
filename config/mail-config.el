;;;mail-config --- Summary
;;; Commentary:
;;; Code:

(require 'smtpmail)

(setq
 mail-user-agent 'mu4e-user-agent
 send-mail-function 'smtpmail-send-it
 message-send-mail-function 'smtpmail-send-it
 smtpmail-default-smtp-server "smtp.gmail.com"
 smtpmail-smtp-server "smtp.gmail.com"
 smtpmail-starttls-credentials '(("smtp.gmail.com" 587 nil nil))
 starttls-use-gnutls t
 smtpmail-auth-credentials '(("smtp.gmail.com" 587 "arit93@gmail.com" nil))
 smtpmail-smtp-service 587)

(setq user-full-name "Ari Turetzky")
(setq user-mail-address "arit93@gmail.com")


;; ;;mu4e
(add-to-list 'load-path "/usr/local/Cellar/mu/1.4.15/share/emacs/site-lisp/mu/mu4e/")
(require 'mu4e)

(setq mu4e-maildir (expand-file-name "~/Maildir"))
(setq mu4e-completing-read-function #'ivy-completing-read)
(setq mu4e-drafts-folder "/[Gmail].Drafts")
(setq mu4e-sent-folder   "/[Gmail].Sent Mail")
(setq mu4e-trash-folder  "/[Gmail].Trash")
(setq
 ;;mu4e-html2text-command "w3m -T text/html"
 mu4e-html2text-command "textutil -stdin -format html -convert txt -stdout"
 mu4e-update-interval 600
 mu4e-headers-auto-update t
 mu4e-compose-signature-auto-include nil
 mu4e-change-filenames-when-moving t
 )

(setq mu4e-view-show-images t
      mu4e-view-show-addresses t)
(require 'org-mu4e)

(when (fboundp 'imagemagick-register-types)
  (imagemagick-register-types))
;; (setq mu4e-mu-binary "/usr/local/bin/mu")
(setq mu4e-compose-reply-to-address "arit93@gmail.com")

;; don't save message to Sent Messages, GMail/IMAP will take care of this
(setq mu4e-sent-messages-behavior 'delete)


;; setup some handy shortcuts
(setq mu4e-maildir-shortcuts
      '(("/INBOX"             . ?i)
        ("/[Gmail].Sent Mail" . ?s)
        ("/[Gmail].Trash"     . ?t)))

;; allow for updating mail using 'U' in the main view:
(setq mu4e-get-mail-command "offlineimap")
(setq message-kill-buffer-on-exit t)

(setq abt/mu4e-inbox-query
      "(maildir:/INBOX) AND flag:unread")

(use-package mu4e-alert
  :after mu4e
  :ensure t
  :config
  (setq mu4e-alert-interesting-mail-query abt/mu4e-inbox-query)
  (setq mu4e-alert-notify-repeated-mails nil)
  (mu4e-alert-enable-notifications))
(provide 'mail-config)
