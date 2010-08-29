;;; vm-spamassassin.el --- Spam filter for VM using spamassassin

;; Copyright (C) 2002 by Markus Mohnen

;; Author: Markus Mohnen <moh...@informatik.rwth-aachen.de>
;; Maintainer: Markus Mohnen <moh...@informatik.rwth-aachen.de>
;; Created: 22 Jul 2002
;;
;; Version: 1.1
;; - bug fixed for empty folders
;;   reported by "John Covici" <cov...@ccs.covici.com>
;;
;; - no redisplay of vm folder after assassinating mail
;;  
;; Version: 1.0
;;

;; Keywords:

;; This file is NOT part of XEmacs.

;; XEmacs is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; XEmacs is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with XEmacs; see the file COPYING.  If not, write to the Free
;; Software Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA
;; 02111-1307, USA.

;;; Synched up with: Not in FSF

;;; This package integrates SpamAssassin, a mail filter to identify spam, in VM.

;;;  Requirements:
;;
;;  You need two external programs:
;;  1. spamassassin
;;     Get it from http://spamassassin.sourceforge.net/
;;  2. formail, which is part of procmail
;;     Get it from http://www.procmail.org/
;;
;;  In addition, you need the vm-avirtual library
;;  Get it from http://www.robf.de/Hacking/elisp/vm-avirtual.el
;;

;;; Installation:
;;
;;  Put this file on your Emacs-Lisp load path and add following into your
;;  ~/.vm startup file
;;
;;      (require 'vm-spamassassin)
;;

;;; Usage:
;;
;;  Whenever you get new mail spamassassin will be invoked on them. Mail detected as
;;  spam will be marked for deletion in vm.
;;
;;  If you would like to see the spam level of all mails in the VM summary, use
;;  %UP (vm-summary-function-P) in vm-summary-format

;;; Customization:
;;
;;  M-x customize RET vm-spamassassin

;;; Code:

(require 'vm-avirtual)

;;; Customisation:

(defgroup vm-spamassassin nil
  "VM Spam Filter Options"
  :group 'vm)

(defcustom vm-spamassassin-vfolder "vm-spamassassin-spam"
  "*Name of the virtual folder to use for filtering spam.
Do not add this to VM-VIRTUAL-FOLDER-ALIST manually!"
  :group 'vm-spamassassin
  :type 'string)

(defcustom vm-spamassassin-program "spamassassin"
  "*Name of the spamassassin program."
  :group 'vm-spamassassin
  :type 'string)

(defcustom vm-spamassassin-program-options ""
  "*Options for the spamassassin program. Since we use spamassassin as a filter, '-P'
must be one of the options."
  :group 'vm-spamassassin
  :type 'string)

(defcustom vm-spamassassin-formail-program "formail"
  "*Name of the program used to split a sequence of mails."
  :group 'vm-spamassassin
  :type 'string)

(defcustom vm-spamassassin-formail-program-options "-s"
  "*Options for the 'vm-spamassassin-formail-program'. After this arguments, the name
of the spamassassin program will be passed."
  :group 'vm-spamassassin
  :type 'string)

(defun vm-spamassassin-arrived-message ()
  "The function used to do the actual filtering. It is used as a value for
vm-retrieved-spooled-mail-hook."
  (save-excursion
    (vm-save-restriction
     (let ((tail-cons (vm-last vm-message-list))
           (buffer-read-only nil))
       (widen)
       (if (null tail-cons)
           (goto-char (point-min))
         (goto-char (vm-text-end-of (car tail-cons)))
         (forward-line)
         )
       (message "Assassinating new mails... ")
       (call-process-region (point) (point-max)
                            (or shell-file-name "sh")
                            t t nil shell-command-switch
                            (concat vm-spamassassin-formail-program " "
                                    vm-spamassassin-formail-program-options " "
                                    vm-spamassassin-program " "
                                    vm-spamassassin-program-options))
       (message "Assassinating new mails... done")
       )
     )
    )
  )

(defun vm-delete-spam-messages ()
  "The function used to mark the spam detected by SpamAssassin for deletion in VM.
This code is stolen from vm-avirtual and is based on the VM-FAQ."
  (interactive)
  (let ((old-vm-virtual-folder-alist vm-virtual-folder-alist)
        (old-vm-virtual-auto-delete-message-selector vm-virtual-auto-delete-message-selector))
    (setq vm-virtual-folder-alist (cons (list vm-spamassassin-vfolder
                                              `(("received")
                                                (and (undeleted)
                                                     (header "^X-Spam-Flag: YES"))))
                                        vm-virtual-folder-alist))
    (setq vm-virtual-auto-delete-message-selector vm-spamassassin-vfolder)
    (vm-virtual-auto-delete-messages)
    (setq vm-virtual-folder-alist old-vm-virtual-folder-alist)
    (setq vm-virtual-auto-delete-message-selector
          old-vm-virtual-auto-delete-message-selector)
    )
  )

(defun vm-summary-function-P (m)
  "A convinient function for VM's summary."
  (or (vm-get-header-contents m "X-Spam-Level:") ""))

;;; Hooking into VM

(add-hook 'vm-retrieved-spooled-mail-hook 'vm-spamassassin-arrived-message)

(add-hook 'vm-arrived-messages-hook 'vm-delete-spam-messages)

(provide 'vm-spamassassin)

;;; vm-spamassassin.el ends here 