(message "loading gnus-config")
(require 'smtpmail)


(setq send-mail-function 'smtpmail-send-it)
(setq message-send-mail-function 'smtpmail-send-it)
(setq smtpmail-default-smtp-server  "smtp.gmail.com")
(setq smtpmail-smtp-server  "smtp.gmail.com")
(setq smtpmail-starttls-credentials '(("smtp.gmail.com" 587 nil nil)))
(setq smtpmail-auth-credentials '(("smtp.gmail.com" 587 "arit93@gmail.com" nil)))
(setq smtpmail-smtp-service   587)
(setq user-full-name "Ari Turetzky")
(setq user-mail-address "arit93@gmail.com")
(setq starttls-use-gnutls t)
(setq gnus-nntp-server "news.eternal-september.org")
(setq nntp-authinfo-file "~/.authinfo.gpg")

(use-package nnreddit
  :ensure t)
(setq nnreddit-python-command "python3")
(add-to-list 'gnus-secondary-select-methods '(nnreddit ""))


;; Set the default value of mm-discouraged-alternatives.
(with-eval-after-load "gnus-sum"
  (add-to-list
   'gnus-newsgroup-variables
   '(mm-discouraged-alternatives
     . '("text/html" "image/.*"))))

;; Display ‘text/html’ parts in nnrss groups.
(add-to-list
 'gnus-parameters
 '("\\`nnrss:" (mm-discouraged-alternatives nil)))
(require 'gnus-sum)
(require 'gravatar)
(require 'gnus-gravatar)
(require 'mm-util)
(all-the-icons-gnus-setup)

(provide 'gnus-config)
