;;; csde-which-method.el --- Print current method in mode line

;; Adapted from the JDE by Matt Bruce <matt.bruce@morganstanley.com>

;; Copyright (C) 2001 by Matt Bruce
;; Maintainer:  Matt Bruce

;; JDE version Copyright (C) 1994, 1997, 1998 Free Software Foundation, Inc.

;; JDE Author: Paul Kinnucan (paulk@mathworks.com)
;; JDE version Inspired by Alex Rezinsky's which-func package.

;; Keywords: mode-line, tools

;; This file is not part of GNU Emacs.

;; GNU Emacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; The latest version of the CSDE is available at
;; <URL:http://www.sourceforge.com/>.

;; Please send any comments, bugs, or upgrade requests to
;; Matt Bruce (matt.bruce@morganstanley.com)

;; This package displays the name of the method at point
;; in the Emacs mode line. 

;;; History:

;;  $Log: csde-which-method.el,v $
;;  Revision 1.1  2001/02/12 05:57:51  paulk
;;  Initial XEmacs revision.
;;
;;  Revision 1.8  2000/11/27 06:18:41  paulk
;;  Miscellaneous bug fixes and minor enhancements.
;;
;;  Revision 1.7  2000/10/20 04:12:12  paulk
;;  *** empty log message ***
;;
;;  Revision 1.6  2000/10/10 06:39:24  paulk
;;  Moved some which method customization variables into the which method customization group.
;;
;;  Revision 1.5  2000/10/08 12:55:39  paulk
;;  *** empty log message ***
;;
;;  Revision 1.4  2000/10/06 05:41:28  paulk
;;  Now optionally truncates method name. 
;;  Thanks to klaus.berndl@sdm.de. Also, moves method 
;;  name after point location display in mode line.
;;
;;  Revision 1.3  2000/09/05 04:42:22  paulk
;;  Fixed a number of bugs.
;;
;;  Revision 1.2  2000/09/04 05:03:16  paulk
;;  Added test for existence of method map.
;;
;;  Revision 1.1  2000/08/31 05:27:49  paulk
;;  Initial revision.
;;

;;; Code:

;; Variables for customization
;; ---------------------------
;;  

(require 'csde-parse)


(defgroup csde-which-method nil
  "Mode to display the current function name in the modeline."
  :group 'csde)

;;;###autoload
(defcustom csde-which-method-mode t
  "Enables the CSDE's which method mode.
When which method mode is enabled, the current method name is
displayed in the mode line."
  :group 'csde-which-method
  :type  'boolean)

(defcustom csde-which-method-format '("[" csde-which-method-current "]")
  "Format for displaying the function in the mode line."
  :group 'csde-which-method
  :type 'sexp)

(defcustom csde-mode-line-format 
  '("-" 
    mode-line-mule-info
    mode-line-modified
    mode-line-frame-identification
    mode-line-buffer-identification
    "   "
    global-mode-string
    "   %[(" mode-name mode-line-process minor-mode-alist "%n" ")%]--"
    (line-number-mode "L%l--")
    (column-number-mode "C%c--")
    (-3 . "%p")
    (csde-which-method-mode
     ("--" csde-which-method-format "--"))
    "-%-")
  "Format for the CSDE source buffer mode line."
  :group 'csde
  :type 'sexp)

(defcustom csde-which-method-max-length 20
  "Specify the maximum length of the which-method-string \(see
`csde-which-method-format'). If nil, the string is not \
truncated."
  :type '(choice (const :tag "No truncation" :value nil)
                 (integer :tag "Max. length"))
  :group 'csde-which-method)

(defcustom csde-which-method-class-min-length 4
  "Specifies the minimum length of the class part of the full method
name after truncation of the class name, but only if the class 
is displayed and if `csde-which-method-max-length'
is not nil. If the full method name is still greater than
`csde-which-method-max-length', the method part of the name is truncated."
  :type '(integer :tag "Min. length")
  :group 'csde-which-method)


(defcustom csde-which-method-abbrev-symbol "~"
"Symbol used to indicate abbreviated part of a method name."
    :group 'csde-which-method
    :type  'string)

;;; Code, nothing to customize below here
;;; -------------------------------------
;;;

(defvar csde-which-method-unknown "???"
  "String to display in the mode line when the current method is unknown.")

(defvar csde-which-method-current  csde-which-method-unknown)
(make-variable-buffer-local 'csde-which-method-current)

(defvar csde-which-method-current-point-loc -1)
(make-variable-buffer-local 'csde-which-method-current-point-loc)

(defvar csde-which-method-current-method-bounds (cons -1 -1))
(make-variable-buffer-local 'csde-which-method-current-method-bounds)


(defun csde-which-method-truncate-begin (str truncation)
  (if (> truncation (length csde-which-method-abbrev-symbol))
	 (concat csde-which-method-abbrev-symbol (substring str truncation))
       str))

(defun csde-which-method-truncate-end (str truncation)
  (let ((str-length (length str)))
    (if (> truncation (length csde-which-method-abbrev-symbol))
        (concat (substring str 0 (- str-length truncation)) 
		csde-which-method-abbrev-symbol)
      str)))

(defun csde-which-method-update ()
  (interactive)
  (condition-case info
      (let ((p (point)))
	(if (or
	     (= csde-which-method-current-point-loc p)
	     (and
	      (>= p (car csde-which-method-current-method-bounds))
	      (<= p (cdr csde-which-method-current-method-bounds))))
	    (setq csde-which-method-current-point p)
	  (let ((name ;; (csde-parse-get-method-at-point)
 		      (if csde-parse-the-method-map
			  (csde-parse-method-map-get-method-at csde-parse-the-method-map))
		      ))
	   (if name
	       (let* ((name-pair (car name))
		      (class (car name-pair))
		      (method (cdr name-pair))
		      (bounds (cdr name))
                      (class-length (length class))
                      (method-length (length method))
                      ;; initialize the truncation with 0!
                      (trunc-class 0)
                      (trunc-method 0)
                      (trunc-complete 0))
                 (when csde-which-method-max-length
                   ;; compute necessary truncation of method and/or class
                   (if csde-parse-buffer-contains-multiple-classes-p
                       (when (> (+ class-length method-length 1)
                                csde-which-method-max-length)
                         (setq trunc-complete (- (+ class-length
                                                    method-length 1)
                                                 csde-which-method-max-length))
                         (if (< (- class-length trunc-complete)
                                csde-which-method-class-min-length)
                             (setq trunc-class
                                   (- class-length
                                      csde-which-method-class-min-length)
                                   trunc-method (- trunc-complete
                                                   trunc-class))
                           (setq trunc-method 0
                                 trunc-class trunc-complete)))
                     (when (> method-length csde-which-method-max-length)
                       (setq trunc-method (- method-length
                                             csde-which-method-max-length)))))
                 ;; truncate method and class with the computed truncation
                 ;; (possible 0, then no truncation is done in fact)
                 (setq class (csde-which-method-truncate-end class trunc-class)
                       method (csde-which-method-truncate-end method trunc-method))
                 ;; set the displayed string from the (possible truncated)
                 ;; class and method parts according to
                 ;; csde-parse-buffer-contains-multiple-classes-p.
                 (setq csde-which-method-current
                       (if csde-parse-buffer-contains-multiple-classes-p
                           (format "M:%s.%s" class method)
                         (format "M:%s" method)))
		 (setq csde-which-method-current-point-loc p)
		 (setq csde-which-method-current-method-bounds bounds))
	     (progn
	       (setq name (csde-parse-get-innermost-class-at-point))
	       (setq csde-which-method-current-point-loc p)
	       (setq csde-which-method-current-method-bounds (cons -1 -1))
	       (if name
                   (let* ((class (car name))
                          (class-length (length class)))
                     ;; possible truncate the string to display
                     (when (and csde-which-method-max-length
                                (> class-length csde-which-method-max-length))
                       (setq class (csde-which-method-truncate-begin class
                                                       (- class-length
                                                          csde-which-method-max-length))))
                     (setq csde-which-method-current (format "C:%s" class)))
		 (setq csde-which-method-current csde-which-method-unknown)))))))
    (error
     ;; (debug)
     (remove-hook 'post-command-hook 'csde-which-method-update)
     (message "Error in csde-which-method-update: %s" info))))

(defun csde-which-method-update-on-entering-buffer ()
  (setq csde-which-method-current-point-loc 0)
  (setq csde-which-method-current-method-bounds (cons -1 -1))
  (csde-which-method-update))

(provide 'csde-which-method)

;; csde-which-method.el ends here
