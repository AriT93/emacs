;; -*- lexical-binding: t; -*-
;;; gnus-config --- Summary
;;; Commentary:
;;; Code:

(message "loading gnus-config")
(require 'smtpmail)


(setq send-mail-function 'smtpmail-send-it)
;;(setq message-send-mail-function 'smtpmail-send-it)
;;(setq smtpmail-default-smtp-server  "smtp.gmail.com")
;;(setq smtpmail-smtp-server  "smtp.gmail.com")
;;(defvar smtpmail-starttls-credentials '(("smtp.gmail.com" 587 nil nil)))
;;(defvar smtpmail-auth-credentials '(("smtp.gmail.com" 587 "arit93@gmail.com" nil)))
;;(setq smtpmail-smtp-service   587)
(setq user-full-name "Ari Turetzky")
;;(setq user-mail-address "arit93@gmail.com")
(defvar starttls-use-gnutls t)
(defvar gnus-nntp-server "news.eternal-september.org")
(setq gnus-select-method '(nntp "news-eternal-september.org"(nntp-port 119)))
(defvar nntp-authinfo-file "~/.authinfo.gpg")
(add-hook 'gnus-article-mode-hook 'visual-line-mode)

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
;;(all-the-icons-gnus-setup)
(setq gnus-summary-line-format "%U%R%z%I%[%{%&user-date;%}%] %(%[%-20,20f%]%) %s\n")
(setq gnus-user-date-format-alist '((t .
    #("%Y-%m-%d %H:%M" ))))
(provide 'gnus-config)
;;; gnus-config.el ends here
