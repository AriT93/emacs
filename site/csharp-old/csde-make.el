;;; csde-make.el -- Integrated Development Environment for Csharp.
;; $Revision: 1.2 $ 

;; Adapted from the JDE by Matt Bruce <matt.bruce@morganstanley.com>
;; Maintainer:  Matt Bruce

;; Copyright (C) 2001 by Matt Bruce

;; JDE Author: Paul Kinnucan <paulk@mathworks.com>
;; JDE Maintainer: Paul Kinnucan
;; Keywords: csharp, tools

;; JDE version Copyright (C) 1997, 1998 Paul Kinnucan.

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

;; The latest version of the CSDE is available at
;; <URL:http://www.sourceforge.com/>.

;; Please send any comments, bugs, or upgrade requests to
;; Matt Bruce (matt.bruce@morganstanley.com)

(require 'compile)

(defcustom csde-make-program "make"
  "*Specifies name of make program."
 :group 'csde-project
 :type 'string)

(defcustom csde-make-working-directory ""
  "*Path of the working directory to use in 'make' build mode.
If this string is empty, the 'make' build mode uses the current file
location as its working directory"
  :group 'csde-project
  :type 'string)

(defcustom csde-make-args ""
  "*Specifies arguments to be passed to make program."
  :group 'csde-project
  :type 'string)


(defvar csde-interactive-make-args ""
"String of compiler arguments entered in the minibuffer.")

(defcustom csde-read-make-args nil
"*Specify whether to prompt for additional make arguments.
If this variable is non-nil, and if `csde-build-use-make' is non nil
the csde-build command prompts you to enter additional make
arguments in the minibuffer. These arguments are appended to those 
specified by customization variables. The CSDE maintains a history 
list of arguments entered in the minibuffer."
  :group 'csde-project
  :type 'boolean
)


(defun csde-make-make-command (more-args)
  "Constructs the csharp compile command as: csde-compiler + options + buffer file name."
  (concat csde-make-program " " csde-make-args
	  (if (not (string= more-args ""))
	      (concat " " more-args))
	  " "))


;;;###autoload
(defun csde-make ()
  "Run the CSDE make program."
  (interactive)
  (if csde-read-make-args
      (setq csde-interactive-make-args
	      (read-from-minibuffer 
	       "Make args: "
	       csde-interactive-make-args
	       nil nil
	       '(csde-interactive-make-arg-history . 1))))

  (let ((make-command
	 (csde-make-make-command 
	  csde-interactive-make-args))
	(default-directory 
	  (if (string= csde-make-working-directory "")
	      default-directory
	    csde-make-working-directory)))

    ;; Force save-some-buffers to use the minibuffer
    ;; to query user about whether to save modified buffers.
    ;; Otherwise, when user invokes csde-make from
    ;; menu, save-some-buffers tries to popup a menu
    ;; which seems not to be supported--at least on
    ;; the PC.
    (if (and (eq system-type 'windows-nt)
	     (not csde-xemacsp))	
	(let ((temp last-nonmenu-event))
	  ;; The next line makes emacs think that csde-make
	  ;; was invoked from the minibuffer, even when it
	  ;; is actually invoked from the menu-bar.
	  (setq last-nonmenu-event t)
	  (save-some-buffers (not compilation-ask-about-save) nil)
	  (setq last-nonmenu-event temp))
      (save-some-buffers (not compilation-ask-about-save) nil))
    (compile-internal make-command "No more errors")))

; (defun csde-make (args)
;   "Run the CSDE make program."
;   (interactive
;    (list (if (string= csde-make-args "")
; 	     (read-from-minibuffer (concat csde-make-program " ")
; 				   (nth 0 minibuffer-history))
; 	   csde-make-args)))

;   (compile 
;    (concat
;     csde-make-program
;     " "
;     (csde-run-make-arg-string
;      (csde-run-parse-args args)))))

(provide 'csde-make)

;; $Log: csde-make.el,v $
;; Revision 1.2  2001/02/12 05:38:26  paulk
;; CSDE 2.2.7
;;
;; Revision 1.7  2000/08/09 03:29:26  paulk
;; Added csde-make-working-directory variable. Thanks to Laurent Latil <Laurent.Latil@france.sun.com>
;;
;; Revision 1.6  1999/04/27 16:44:49  paulk
;; Updated to allow interactive entry of make arguments. Thanks to Yarek J. Kowalik <jgk@klg.com> for providing this enhancement.
;;
;; Revision 1.5  1999/01/17 00:43:57  paulk
;; Removed two line feeds at the end of make command as they appeared to
;; confuse GNU make for NT.
;;
;; Revision 1.4  1998/11/27 09:38:23  paulk
;; Changed to use compile mode as suggested by Robert Grace <rmg2768@draper.com>.
;;
;; Revision 1.3  1998/05/29 01:46:39  paulk
;; Added dummy function for csde-make-mode to facilitate autoloading.
;;
;; Revision 1.2  1998/05/27 06:04:52  paulk
;; Added autoload comments.
;;
;; Revision 1.1  1998/03/27 04:44:36  kinnucan
;; Initial revision
;;

;; End of csde-make.el
