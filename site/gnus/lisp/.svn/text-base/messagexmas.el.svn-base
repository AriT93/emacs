;;; messagexmas.el --- XEmacs extensions to message

;; Copyright (C) 1996, 1997, 1998, 1999, 2000, 2003
;;      Free Software Foundation, Inc.

;; Author: Lars Magne Ingebrigtsen <larsi@gnus.org>
;; Keywords: mail, news

;; This file is part of GNU Emacs.

;; GNU Emacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.	 See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;;; Code:

(eval-when-compile (require 'cl))
(require 'nnheader)

(defvar message-xmas-dont-activate-region t
  "If t, don't activate region after yanking.")

(defvar message-xmas-glyph-directory nil
  "*Directory where Message logos and icons are located.
If this variable is nil, Message will try to locate the directory
automatically.")

(defvar message-use-toolbar (if (featurep 'toolbar)
				'default-toolbar
			      nil)
  "*If nil, do not use a toolbar.
If it is non-nil, it must be a toolbar.  The five valid values are
`default-toolbar', `top-toolbar', `bottom-toolbar',
`right-toolbar', and `left-toolbar'.")

(defvar message-toolbar
  '([message-spell ispell-message t "Spell"]
    [message-help (Info-goto-node "(Message)Top") t "Message help"])
  "The message buffer toolbar.")

(defun message-xmas-find-glyph-directory (&optional package)
  (setq package (or package "message"))
  (let ((dir (symbol-value
	      (intern-soft (concat package "-xmas-glyph-directory")))))
    (if (and (stringp dir) (file-directory-p dir))
	dir
      (nnheader-find-etc-directory package))))

(defun message-xmas-setup-toolbar (bar &optional force package)
  (let ((dir (message-xmas-find-glyph-directory package))
	(xpm (if (featurep 'xpm) "xpm" "xbm"))
	icon up down disabled name)
    (unless package
      (setq message-xmas-glyph-directory dir))
    (when dir
      (while bar
	(setq icon (aref (car bar) 0)
	      name (symbol-name icon)
	      bar (cdr bar))
	(when (or force
		  (not (boundp icon)))
	  (setq up (concat dir name "-up." xpm))
	  (setq down (concat dir name "-down." xpm))
	  (setq disabled (concat dir name "-disabled." xpm))
	  (if (not (file-exists-p up))
	      (setq bar nil
		    dir nil)
	    (set icon (toolbar-make-button-list
		       up (and (file-exists-p down) down)
		       (and (file-exists-p disabled) disabled)))))))
    dir))

(defun message-setup-toolbar ()
  (and message-use-toolbar
       (message-xmas-setup-toolbar message-toolbar)
       (set-specifier (symbol-value message-use-toolbar)
		      (cons (current-buffer) message-toolbar))))

(defun message-xmas-exchange-point-and-mark ()
  "Exchange point and mark, but allow for XEmacs' optional argument."
  (exchange-point-and-mark message-xmas-dont-activate-region))

(defun message-xmas-maybe-fontify ()
  (when (featurep 'font-lock)
    (font-lock-set-defaults)))

(defun message-xmas-make-caesar-translation-table (n)
  "Create a rot table with offset N."
  (let ((i -1)
	(table (make-string 256 0))
	(a (mm-char-int ?a))
	(A (mm-char-int ?A)))
    (while (< (incf i) 256)
      (aset table i i))
    (concat
     (substring table 0 A)
     (substring table (+ A n) (+ A n (- 26 n)))
     (substring table A (+ A n))
     (substring table (+ A 26) a)
     (substring table (+ a n) (+ a n (- 26 n)))
     (substring table a (+ a n))
     (substring table (+ a 26) 255))))

(add-hook 'message-mode-hook 'message-xmas-maybe-fontify)

(defun message-xmas-redefine ()
  "Redefine message functions for XEmacs."
  (defalias 'message-exchange-point-and-mark
    'message-xmas-exchange-point-and-mark)
  (defalias 'message-mark-active-p
    'region-exists-p)
  (when (>= emacs-major-version 20)
    (defalias 'message-make-caesar-translation-table
      'message-xmas-make-caesar-translation-table))
  (defalias 'message-make-overlay 'make-extent)
  (defalias 'message-delete-overlay 'delete-extent)
  (defalias 'message-overlay-put 'set-extent-property))

(message-xmas-redefine)

(provide 'messagexmas)

;;; messagexmas.el ends here
