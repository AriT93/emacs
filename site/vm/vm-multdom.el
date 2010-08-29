;;; vm-multdom.el --- multiple-domain reply header manipulation for VM

;; Copyright (C) 1992, 1993, 1994, 1995, 1996, 1997 Noah S. Friedman

;; Author: Noah Friedman <friedman@splode.com>
;; Maintainer: friedman@splode.com

;; $Id: vm-multdom.el,v 1.4 1997/12/23 14:35:39 friedman Exp $

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, you can either send email to this
;; program's maintainer or write to: The Free Software Foundation,
;; Inc.; 59 Temple Place, Suite 330; Boston, MA 02111-1307, USA.

;;; Commentary:

;; This file contains enhancements to VM for processing mail from multiple
;; domains that is forwarded to one site.  Specifically, it looks for the
;; address the sender used to send you mail, and sets your `From' (and
;; optionally, `Reply-To') header to use the same address in replies.

;; To use this package, put the following in your .emacs or .vm file:
;;
;;    (require 'vm-multdom)
;;    (vm-multdom-install)
;;    (setq vm-multdom-user-addresses
;;          '("regexps" "matching" "each" "of" "your" "email" "addresses"))
;;
;; You may want to use regexps which match addresses for mail to specific
;; machines, e.g. "friedman@.*\\.splode\\.com\\|friedman@splode\\.com".

;; This package requires Emacs/XEmacs 19 or later, VM version 6.30 or
;; later, and fmailutils 1.10 or later.

;;; Code:

(require 'vm-message)
(require 'fmailutils)

(defvar vm-multdom-user-addresses nil
  "*List of regular expressions which match your email addresses.
Any address in the address-headers of a message to which you are replying
or forwarding that match any of these regexps, is a candidate for your
sending address in your forwarding/replying message.

If a member of this list is itself a list, that sublist should be of the form

         \(regexp address full-name\)

Where `address' is an rfc822-compliant email address, and
`full-name' is an rfc822-compliant \"address comment\" \(e.g. your full name\).
The `full-name' field is optional; it defaults to your account's full name.

Do not surround your regexps with \"^\" or \"$\"; that is done for you.")

(defvar vm-multdom-set-reply-address-function 'vm-multdom-set-reply-address
  "*If non-nil, call this function to set your reply address in outbound mail.
This function is always called with at least one argument, the address to set.
It may be called with one additional argument, an \"address comment\"
 for the the address.  If this function is not called with this second
 argument, it should probably use your real name \(as returned by
 and environment variable or the function `user-login-name'\).

This function is only called if there is an address in the message to
which you are replying \(or forwarding\) which matches one of the regular
expressions in `vm-multdom-user-addresses'.  In other words, this function
is only invoked if you are explicitly addressed in the message.  Set
the variable `user-mail-address' to set a default return address for
outbound messages.")

(defvar vm-multdom-set-reply-address-style '<>
  "*Choose address style for `vm-multdom-set-reply-address'.
If the value of this variable is the symbol `<>', then the function
vm-multdom-set-reply-address will set your return address in the style

\tFrom: Noah Friedman <friedman@prep.ai.mit.edu>

Otherwise, it will use the style

\tFrom: friedman@prep.ai.mit.edu \(Noah Friedman\)")

(defvar vm-multdom-set-reply-address-replyto t
  "*If non-nil, vm-multdom-set-reply-address sets `Reply-To' header.
When this variable is set to `nil', only the `From' header is set.
When non-nil, both are set.")

;; Users should probably not change this.
(defconst vm-multdom-recipient-address-headers
  '("Resent-To" "Resent-Cc" "To" "Apparently-To" "Cc")
  "Mail headers which can include recipient addresses.")


(defun vm-multdom-install ()
  "Perform runtime installation of the vm-multdom package.
Set `From' and `Reply-To' address in replied messages to match the address
to which the mail was delivered in the first place, so that e.g. if mail is
sent to you@another.domain, set that address as the From address in the
reply even if you are normally using a different default address."
  (interactive)
  (add-hook 'vm-reply-hook
            'vm-multdom-reply-set-recipient-address)
  (add-hook 'vm-forward-message-hook
            'vm-multdom-forward-set-recipient-address))

(defun vm-multdom-uninstall ()
  "Uninstall the runtime hooks which invoke the vm-multdom package."
  (interactive)
  (remove-hook 'vm-reply-hook
               'vm-multdom-reply-set-recipient-address)
  (remove-hook 'vm-forward-message-hook
               'vm-multdom-forward-set-recipient-address))

(defun vm-multdom-reply-set-recipient-address ()
  (vm-multdom-set-recipient-address vm-reply-list))

(defun vm-multdom-forward-set-recipient-address ()
  (vm-multdom-set-recipient-address vm-forward-list))

(defun vm-multdom-set-recipient-address (vm-data-list)
  (let ((addr (vm-multdom-find-recipient-address vm-data-list)))
    (cond ((null vm-multdom-set-reply-address-function))
          ((consp addr)
           (apply vm-multdom-set-reply-address-function addr))
          (addr
           (funcall vm-multdom-set-reply-address-function addr)))))

(defun vm-multdom-find-recipient-address (vm-data-list)
  (let* ((envelope-end (vm-headers-of (car vm-data-list)))
         (visible-header-end (vm-text-of (car vm-data-list)))
         (buffer (marker-buffer envelope-end))
         (header-str nil)
         (recipients nil))
    (save-excursion
      (set-buffer buffer)
      (save-restriction
        (widen)
        (setq header-str (buffer-substring envelope-end
                                           visible-header-end))))

    (let ((tmpbuf (generate-new-buffer " *recipient addresses*")))
      (save-excursion
        (set-buffer tmpbuf)
        (let ((mail-header-separator "")
              ;; avoid consing any undo records
              (buffer-undo-list t)
              (header-list vm-multdom-recipient-address-headers)
              (addrs nil))
          (insert header-str)

          (while header-list
            (setq addrs (fmailutils-get-header-contents (car header-list)))
            (setq header-list (cdr header-list))
            (while addrs
              (setq recipients
                    (append recipients (rfc822-addresses (car addrs))))
              (setq addrs (cdr addrs))))))
      (kill-buffer tmpbuf))
    (vm-multdom-match-address recipients)))

(defun vm-multdom-match-address (recipients &optional re-list)
    (or re-list
        (setq re-list vm-multdom-user-addresses))
    (let ((found nil)
          addr-list current-addr re use-addr)
      (while re-list
        (setq re (car re-list))
        (setq re-list (cdr re-list))
        (cond ((consp re)
               (setq use-addr (cdr re))
               (setq re (car re)))
              (t
               (setq use-addr nil)))

        (setq addr-list recipients)
        (while addr-list
          (setq current-addr (car addr-list))
          (setq addr-list (cdr addr-list))

          (cond ((string-match (concat "^" re "$") current-addr)
                 (setq found (or use-addr current-addr))
                 (setq addr-list nil)
                 (setq re-list nil)))))

      (cond ((and (consp found)
                  (null (cdr found)))
             (car found))
            (t
             found))))

(defun vm-multdom-set-reply-address (&optional addr name)
  (fmailutils-set-from-address addr name
                               vm-multdom-set-reply-address-style
                               vm-multdom-set-reply-address-replyto))

(defun vm-multdom-address-list-regexp (addrs)
  (concat "^" (mapconcat (function (lambda (addr)
                                     (if (consp addr)
                                         (car addr)
                                       addr)))
                         addrs "\\|") "$"))

(provide 'vm-multdom)

;;; vm-multdom.el ends here.
