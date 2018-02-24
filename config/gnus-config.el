(message "loading gnus-config")
(require 'smtpmail)

(setq send-mail-function 'smtpmail-send-it
      message-send-mail-function 'smtpmail-send-it
      smtpmail-auth-credentials (expand-file-name "~/.authinfo.gpg")
      smtp-smtp-server "https://outlook.office365.com/EWS/Exchange.asmx"
      smtpmail-stream-type 'starttls
      smtpmail-smtp-service 587)
(setq user-full-name "Ari Turetzky")
(setq user-mail-address "aturetzky@quantcast.com")
(setq starttls-use-gnutls t)
(provide 'gnus-config)
