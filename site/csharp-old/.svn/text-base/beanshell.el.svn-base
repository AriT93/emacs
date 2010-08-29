;;; beanshell.el
;; $Revision: 1.4 $ 

;; Adapted from the JDE by Matt Bruce <matt.bruce@morganstanley.com>
;; Maintainer:  Matt Bruce

;; JDE Author: Paul Kinnucan <paulk@mathworks.com>
;; JDE Maintainer: Paul Kinnucan

;; Keywords: csharp, tools

;; Copyright (C) 2001 Matt Bruce

;; The JDE is Copyright (C) 1997, 1998, 1999, 2000, 2001 Paul Kinnucan.

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

;; Need csde-run only to get the definition for 
;; save-w32-show-window macro.


(defcustom bsh-startup-timeout 10
  "*Length of time the CSDE waits for the Beanshell to startup.
Increase the value of this variable if you get Lisp errors
on BeanShell startup on Unix."
  :group 'bsh
  :type 'integer)

(defcustom bsh-eval-timeout 20
  "*Length of time in seconds the CSDE waits for the Beanshell to evaluate
a Csharp expression before giving up and signaling an error."
  :group 'bsh
  :type 'integer)

(defcustom bsh-vm-args nil
  "*Specify arguments to be passed to the Csharp vm.
This option allows you to specify one or more arguments to be passed
to the Csharp vm that runs the BeanShell. Note that the value of this
variable should be a list of strings, each of which represents an
argument. When customizing this variable, use a separate text field
for each argument."
  :group 'csde-run-options
  :type '(repeat (string :tag "Argument")))


(defun bsh()
"*Starts BeanShell, a Csharp interpreter developed by Pat Niemeyer."
  (interactive)
  (bsh-internal t))


(eval-when-compile
  (if (not (fboundp 'save-w32-show-window))
      (defmacro save-w32-show-window (&rest body)
	"Saves the value of the w32-start-process-show-window variable
before evaluating body and restores the value afterwards."
	`(if (and (eq system-type 'windows-nt)
		  (not (string-match "XEmacs" (emacs-version))))
	     (if (boundp 'win32-start-process-show-window)
		 (let ((save win32-start-process-show-window))
		   (setq win32-start-process-show-window t)
		   ,@body
		   (setq win32-start-process-show-window save))
	       (let ((save w32-start-process-show-window))
		 (setq w32-start-process-show-window t)
		 ,@body
		 (setq w32-start-process-show-window save)))	 
	   ,@body))))

(defun bsh-internal (&optional display-buffer) 
  (let ((bsh-buffer-name "*bsh*"))
    (if (not (comint-check-proc bsh-buffer-name))
	(let* ((bsh-buffer (get-buffer-create bsh-buffer-name))
	       (csde-csharp-directory
		(concat
		 (csde-find-csde-data-directory)
		 "csharp/"))
	       (vm (if (eq system-type 'windows-nt)
		       csde-run-csharp-vm-w
		     csde-run-csharp-vm))
	       (vm-args
		(list
		 "-classpath"
		 (csde-build-classpath
		  (append
		   (list
		    (expand-file-name "bsh-commands" csde-csharp-directory)
		    (expand-file-name "lib/csde.jar" csde-csharp-directory)
		    (expand-file-name "lib/bsh.jar" csde-csharp-directory))
		   (if csde-global-classpath
		       csde-global-classpath 
		     (split-string (getenv "CLASSPATH") 
				   csde-classpath-separator))))))
	       (dir (if (buffer-file-name)
			(file-name-directory (buffer-file-name))
		      default-directory)))

	  (setq vm-args (append vm-args bsh-vm-args))
	  (setq vm-args (append vm-args (list "bsh.Interpreter")))

	  (save-excursion
	    (set-buffer bsh-buffer)
	    (erase-buffer)

	    (cd dir)
	    (insert (concat "cd " dir "\n"))
	    (insert 
	     (concat vm " "
		     (mapconcat (lambda (x) x) vm-args " ") "\n\n"))

	    (comint-mode)
	    (setq comint-prompt-regexp "bsh % "))
	 (save-w32-show-window
	   ;; (message "%s" (nth 1 vm-args))
	   (message "%s" "Starting the BeanShell. Please wait...")
	   (comint-exec bsh-buffer "bsh" vm nil vm-args))

	 (let ((bsh-process (get-buffer-process bsh-buffer)))
	   (process-kill-without-query bsh-process))

	 (if display-buffer
	      (pop-to-buffer bsh-buffer-name)))
      (when display-buffer
	  (message "The Csharp interpreter is already running.")
	  (pop-to-buffer bsh-buffer-name)))))


(setq bsh-tq-reply nil)

(defun bsh-eval-filter (process result)
  (let ((end-of-result (string-match ".*bsh % " result)))
    ;; Check for case
    ;;   %bsh\n...eval output...%bsh\n
    ;; This can happen because the beanshell outputs two or more
    ;; prompts after evaluating some expressions.
    ;; Thanks to Stephane Nicolas.
    ;; (if (eq end-of-result 0)
    ;; (accept-process-output process 0 5))
    (if end-of-result
	(setq bsh-tq-reply (concat bsh-tq-reply (substring result 0 end-of-result)))
      (setq bsh-tq-reply (concat bsh-tq-reply result))
      (accept-process-output process bsh-eval-timeout 5))))

(defun bsh-eval (expr &optional eval-return)
  "Uses the BeanShell Csharp interpreter to evaluate a Csharp statement.
If the interpreter is not already running, this function starts
the interpreter. This function returns any text output by the
Csharp interpreter's standard out or standard error pipes.
If the optional argument eval-return is non-nil, this function
returns the result of evaluating the Csharp output as a Lisp
expression."
  (let* ((bsh-process
	  (if (get-process "bsh")
	      (get-process "bsh")
	    (let (proc)
	      (bsh-internal)
	      (setq proc (get-process "bsh"))
	      (if (eq system-type 'windows-nt)
		  (accept-process-output proc)
		(while (accept-process-output proc bsh-startup-timeout 0)))
	      proc)))
	 (comint-filter (if bsh-process (process-filter bsh-process))))
    (when bsh-process
      (setq bsh-tq-reply nil)
      (set-process-filter bsh-process 'bsh-eval-filter)
      ;; (message "Evaluating: %s" expr)
      (process-send-string bsh-process (concat expr "\n"))
      (if (not (accept-process-output bsh-process bsh-eval-timeout 100))
	  (error "No reply from BeanShell"))
      (set-process-filter bsh-process comint-filter)
      (if (string-match "// Error:" bsh-tq-reply)
	  (progn
	    (message 
	     "Beanshell expression evaluation error.\n  Expression: %s\n  Error: %s"
	     expr bsh-tq-reply)
	    (error "Beanshell eval error. See messages buffer for details.")))
      ;; (if eval-return (message "Evaluating reply: %s" bsh-tq-reply))
      (if eval-return
	  (if bsh-tq-reply
	      (condition-case eval-error
		  (eval (read bsh-tq-reply))
		(error
		 (message "Error evaluating Lisp result of Csharp expression evaluation.")
		 (message "  Csharp expression: %s." expr)
		 (message "  Csharp evaluation result: %s." bsh-tq-reply)
		 (error "Error evaluating Csharp expresson. See *Messages* buffer.")))
	    (progn
	      (message "bsh-eval-r error: Beanshell result is null. Cannot evaluate.")
	      (message "  Expression: %s" expr)))
	bsh-tq-reply))))

(defun bsh-eval-r(csharp-statement) 
  "Convenience function for evaluating Csharp statements
that return Lisp expressions as output. This function 
invokes bsh-eval with the evaluate-return option set to
t."
  (bsh-eval csharp-statement t))



(provide 'beanshell);

;; $Log: beanshell.el,v $
;; Revision 1.4  2001/02/25 04:45:07  paulk
;; Fixed bug in handling CLASSPATH environment variable.
;;
;; Revision 1.18  2001/02/03 07:44:26  paulk
;; Now uses csde-build-classpath to build BeanShell classpath. This allows enviromnent variables in the classpath.
;;
;; Revision 1.17  2000/10/08 12:55:38  paulk
;; *** empty log message ***
;;
;; Revision 1.16  2000/08/10 09:09:47  paulk
;; Now handles Lisp eval errors gracefully.
;;
;; Revision 1.15  2000/08/07 05:11:38  paulk
;; Adds bsh-vm-args variable.
;;
;; Revision 1.14  2000/08/04 02:51:19  paulk
;; Added bsh-eval-timeout variable.
;;
;; Revision 1.13  2000/02/16 04:39:28  paulk
;; Implemented Cygwin/XEmacs compatiblity fix provided by Fred Hart
;; <cfhart@Z-TEL.com> in bsh-internal.
;;
;; Revision 1.12  2000/02/02 05:51:00  paulk
;; Expanded doc string.
;;
;; Revision 1.11  2000/01/28 04:28:00  paulk
;; Fixed startup timing bug that cause commands that use the beanshell to
;; failt the first time on Unix systems.
;;
;; Revision 1.10  2000/01/15 08:00:03  paulk
;; Corrected typo.
;;
;; Revision 1.9  1999/11/01 03:13:07  paulk
;; No change.
;;
;; Revision 1.8  1999/09/17 06:55:26  paulk
;; Set comint-prompt-regexp to the beanshell prompt.
;; Fixed bug where Emacs was querying user whether to kill the beanshell
;; buffer on exit from Emacs.
;;
;; Revision 1.7  1999/01/15 22:18:41  paulk
;; Added Andy Piper's NT/XEmacs compatibility changes.
;;
;; Revision 1.6  1998/12/13 22:10:04  paulk
;; Add check for chunked traffic between Emacs and the BeanShell.
;;
;; Revision 1.5  1998/12/09 00:59:43  paulk
;; Added a startup message for beanshell.
;;
;; Revision 1.4  1998/11/27 10:07:57  paulk
;; Use CLASSPATH environment variable if csde-global-classpath is nil.
;;
;; Revision 1.3  1998/11/22 23:14:28  paulk
;; Fixed path separator bug.
;;
;; Revision 1.2  1998/11/22 18:11:56  paulk
;; Changed path to use csde.jar.
;;
;; Revision 1.1  1998/10/22 00:07:56  paulk
;; Initial revision
;;


;; End of beanshell.el
