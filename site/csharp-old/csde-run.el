;; csde-run.el --- runs the Csharp app in the current buffer.
;; $Revision: 1.3 $ $Date: 2001/02/12 05:38:26 $

;; Adapted from the JDE by Matt Bruce <matt.bruce@morganstanley.com>
;; Maintainer:  Matt Bruce

;; Copyright (C) 2001 by Matt Bruce

;; JDE Author: Paul Kinnucan <paulk@mathworks.com>
;; JDE Maintainer: Paul Kinnucan

;; Keywords: tools, processes

;; JDE version Copyright (C) 1997, 1998, 1999, 2000, 2001 Paul Kinnucan

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

(defcustom csde-run-mode-hook nil
  "*List of hook functions run by `csde-run-mode' (see `run-hooks')."
  :group 'csde-project
  :type 'hook)

(defcustom csde-run-application-class ""
  "*Name of the Csharp class to run. 
This is the class that is run if you select CSDE->Run App from the CSDE
menu or type C-c C-v C-r. If this option is the empty string, the CSDE
runs the class corresponding to the source file in the current
buffer. Note that the specified class must have a static public main
method."
  :group 'csde-project
  :type 'string)

(defcustom csde-run-working-directory ""
  "*Path of the working directory for this application.
If you specify a path, the CSDE launches the application from the
directory specified by the path."
  :group 'csde-project
  :type 'file)


(defcustom csde-run-csharp-vm "csharp"
  "*Specify Csharp interpreter for non-Windows platforms."
  :group 'csde-project
  :type 'file)

(defcustom csde-run-csharp-vm-w "csharpw"
  "*Specify Csharp interpreter for Windows platforms.
On NTEmacs, the CSDE must use the NTEmacs show window option in order
to run the Csharp interpreter. This in turn requires that the CSDE use
the csharpw version of the JDK Csharp interpreter to prevent a console
window from appearing every time you run an application. Sound
confusing? It is, but it works."
  :group 'csde-project
  :type 'file)


(defcustom csde-run-classic-mode-vm nil
"Runs applications in the classic (i.e., not HotSpot) mode."
  :group 'csde-project
  :type 'boolean)


(defcustom csde-run-read-vm-args nil
"*Read vm arguments from the minibuffer.
If this variable is non-nil, the csde-run command reads vm arguments
from the minibuffer and appends them to those specified by
the `csde-run-option' variable group."
  :group 'csde-project
  :type 'boolean)

(defvar csde-run-interactive-vm-arg-history nil
"History of vm arguments read from the minibuffer")

(defcustom csde-run-read-app-args nil
"*Read arguments to be passed to application from the minibuffer."
  :group 'csde-project
  :type 'boolean)

(defvar csde-run-interactive-app-arg-history nil
"History of application arguments read from the minibuffer")

(defgroup csde-run-options nil
  "CSDE Interpreter Options"
  :group 'csde
  :prefix "csde-run-option-")

(defcustom csde-run-option-classpath nil
"*Specify paths of classes required to run this application.
The CSDE uses the specified paths to construct a -classpath
argument to pass to the Csharp interpreter. This option overrides the
`csde-global-classpath' option."
  :group 'csde-run-options
  :type '(repeat (file :tag "Path")))
 
(defcustom csde-run-option-verbose (list nil nil nil)
  "*Print messages about the running process.
The messages are printed in the run buffer."
  :group 'csde-run-options
  :type '(list :indent 2
	       (checkbox :format "\n  %[%v%] %h \n"
			 :doc "Print classes loaded.
Prints a message in the run buffer each time a class is loaded.")
	       (checkbox :format "%[%v%] %h \n"
			 :doc "Print memory freed.
Prints a message in the run buffer each time the garbage collector
frees memory.")
	       (checkbox :format "%[%v%] %h \n"
			 :doc "Print JNI info.
Prints JNI-related messages including information about which native
methods have been linked and warnings about excessive creation of
local references.")))

(defcustom csde-run-option-properties nil
  "*Specify property values.
Enter the name of the property, for example, awt.button.color, in the
Property Name field; enter its value, for example, green, in the
Property Value field. You can specify as many properties as you like."
  :group 'csde-run-options
  :type '(repeat (cons 
		  (string :tag "Property Name") 
		  (string :tag "Property Value"))))

(defcustom csde-run-option-heap-size (list
				     (cons 1 "megabytes")
				     (cons 16 "megabytes"))
"*Specify the initial and maximum size of the interpreter heap."
:group 'csde-run-options
:type '(list
	(cons (integer :tag "Start")
	     (radio-button-choice (const "bytes")
				  (const "kilobytes")
				  (const "megabytes")))
	(cons (integer :tag "Max")
	       (radio-button-choice (const "bytes")
				    (const "kilobytes")
				    (const "megabytes")))))


(defcustom csde-run-option-stack-size (list
				      (cons 128 "kilobytes")
				      (cons 400 "kilobytes"))
  "*Specify size of the C and Csharp stacks."
  :group 'csde-run-options
  :type '(list
	  (cons (integer :tag "C Stack")
	       (radio-button-choice (const "bytes")
				    (const "kilobytes")
				    (const "megabytes")))
	  (cons (integer :tag "Csharp Stack")
	       (radio-button-choice (const "bytes")
				    (const "kilobytes")
				    (const "megabytes")))))

(defcustom csde-run-option-garbage-collection (list t t)
  "*Specify garbage collection options."
  :group 'csde-run-options
  :type '(list :indent 2
	       (checkbox :format "%[%v%] %t \n"
			 :tag "Collect garbage asynchronously.")
	       (checkbox :format "%[%v%] %t \n"
			 :tag "Collect unused classes.")))

(defcustom csde-run-option-csharp-profile (cons nil "./csharp.prof")
  "*Enable Csharp profiling."
  :group 'csde-run-options
  :type '(cons boolean
	       (file :tag "File"
		     :help-echo 
"Specify where to put profile results here.")))

(defcustom csde-run-option-heap-profile (cons nil
						   (list "./csharp.hprof"
							 5
							 20
							 "Allocation objects"))
"*Output heap profiling data."
  :group 'csde-run-options
  :type '(cons boolean
	       (list
		(string :tag "Output File Path")
		(integer :tag "Stack Trace Depth")
		(integer :tag "Allocation Sites")
		(radio-button-choice :format "%t \n%v"
				     :tag "Sort output based on:"
		 (const "Allocation objects")
		 (const "Live objects")))))
		 
(defcustom csde-run-option-verify (list nil t)
  "*Verify classes."
  :group 'csde-run-options
  :type '(list :indent 2
	       (checkbox :format "%[%v%] %t \n"
			 :tag "Executed code in all classes.")
	       (checkbox :format "%[%v%] %t \n"
			 :tag "Classes loaded by a classloader.")))

(defcustom csde-run-option-vm-args nil
  "*Specify arguments to be passed to the Csharp vm.
This option allows you to specify one or more arguments to be passed
to the Csharp interpreter. It is an alternative to using CSDE Run Option
variables, such as `csde-run-option-stack-size', to specify Csharp
interpreter options. Also, it makes it possible to use the CSDE with
interpreters that accept command line arguments not supported by 
the CSDE Run Option variable set."
  :group 'csde-run-options
  :type '(repeat (string :tag "Argument")))


(defcustom csde-run-option-application-args nil
  "*Specify command-line arguments to pass to the application.
The CSDE passes the specified arguments to the application on
the command line."
  :group 'csde-run-options
  :type '(repeat (string :tag "Argument")))

(defcustom csde-run-applet-viewer ""
  "*Specify name of viewer to use to display page containing the applet."
  :group 'csde-project
  :type 'file)

(defcustom csde-run-applet-doc ""
  "*Specify name of document containing applet to be viewed.
If no document is specified, CSDE assumes that the document name is
APPLET.html, where APPLET is the name of the applet to be viewed."
  :group 'csde-project
  :type 'file)


(defcustom csde-appletviewer-option-encoding ""
"*Specify encoding of the HTML file displayed by the appletviewer."
  :group 'csde-run-options
  :type 'string)


(defcustom csde-appletviewer-option-vm-args nil
  "*Specify arguments (e.g., -Xmx16m) to the vm that runs appletviewer.
This option allows you to set the environment of the
virtual machine that runs appletviewer."
  :group 'csde-run-options
  :type '(repeat (string :tag "Argument")))


(defcustom csde-run-executable ""
  "*Specifies the executable to be run by the CSDE's run command.
If you do not specify an executable, the CSDE runs the vm specified
by `csde-run-csharp-vm' or `csde-run-csharp-vm-w'."
  :group 'csde-project
  :type 'file)

(defcustom csde-run-executable-args nil
  "*Specify arguments to be passed to the application executable.
This option allows you to specify one or more arguments to be passed
to the executable specified by `csde-run-executable'."
  :group 'csde-run-options
  :type '(repeat (string :tag "Argument")))


(defun csde-run-parse-args (s)
 "Converts a string of command-line arguments to a list of arguments.
Any substring that is enclosed in single or double quotes or does not include
whitespace is considered a parameter."
   (let ((n (string-match "[^\" ][^ ]*\\|\"[^\"]*\"\\|'[^']*'" s))
 	(i 0)
 	(tokens '()))
     (while n
       (setq tokens (append tokens (list (match-string 0 s))))
       (setq n (match-end 0))
       (setq n (string-match "[^\" ][^ ]*\\|\"[^\"]*\"\\|'[^']*'" s n)))
     tokens))

(defun csde-run-make-arg-string (args)
"Converts a list of command-line arguments to a string of arguments."
  (let ((str "")
	(n (length args))
	(i 0))
    (while (< i n)
      (if (not (string= str ""))
	  (setq str (concat str " ")))
      (setq str (concat str (nth i args)))
      (setq i (+ i 1)))
    str))

;;;###autoload
(defun csde-run-set-vm (vm)
  "Specifies the Csharp interpreter used to run Csharp applications
on non-Windows platforms . The default is csharp."
  (interactive
   "sEnter name of Csharp interpreter: ")
  (setq csde-run-csharp-vm vm))

;;;###autoload
(defun csde-run-set-vm-w (vm)
  "Specifies the Csharp interpreter used to run Csharp applications
on Windows platforms . The default is csharpw."
  (interactive
   "sEnter name of Csharp interpreter: ")
  (setq csde-run-csharp-vm-w vm))

;;;###autoload
(defun csde-run-set-app (app)
  "Specify the name of the application class to run."
  (interactive 
   "sEnter application class: ")
  (setq csde-run-application-class app))

;;;###autoload
(defun csde-run-set-args (args)
  "Specify arguments to be passed to the Csharp vm.
This command serves as an alternative to using the CSDE Run Options
panel to specify command-line arguments for the Csharp interpreter."
  (interactive 
   "sEnter arguments: ")
  (setq csde-run-option-vm-args (csde-run-parse-args args)))


;;;###autoload
(defun csde-run-set-app-args (args)
  "Specify the arguments to be passed to the Csharp application class.
This command provides an alternative to using the CSDE Run Options panel
to specify command-line arguments to pass to the application when starting
the application."
  (interactive 
   "sEnter arguments: ")
  (setq csde-run-option-application-args (csde-run-parse-args args)))

;;;###autoload
(defun csde-run-set-applet-viewer (viewer)
  "Sets the viewer to be used to view an applet. The default is 
appletviewer."
  (interactive
   "sEnter viewer name: ")
  (setq csde-run-applet-viewer viewer))

;;;###autoload
(defun csde-run-set-applet-doc (doc)
  "Specify the doc to be used to view an applet.
This command provides an alternative to using the CSDE Options
panel to specifying the applet document."
  (interactive
   "sEnter applet doc name: ")
  (if (string= doc "")
      (setq csde-run-applet-doc nil)
    (setq csde-run-applet-doc doc)))

(defun csde-run-get-vm-args ()
  "Builds a command-line argument string to pass to the Csharp vm.
This function builds the string from the values of the CSDE
Run Option panel variables."
  (let (options
	(memory-unit-abbrevs
	 (list (cons "bytes" "")
	       (cons "kilobytes" "k")
	       (cons "megabytes" "m"))))

    ;; Set classic-mode option
    (if csde-run-classic-mode-vm
	(setq options
	      (nconc
	       (list "-classic"))))

    ;; Set the classpath option. Use the local
    ;; classpath, if set; otherwise, the global
    ;; classpath.
    (setq options
	  (nconc
	   options
	   (list
	    "-classpath"
	    (csde-build-classpath
	     (if csde-run-option-classpath
		 csde-run-option-classpath
	       csde-global-classpath)))))
    

    ;; Set the verbose options.
    (let ((print-classes-loaded
	   (nth 0 csde-run-option-verbose))
	  (print-memory-freed
	   (nth 1 csde-run-option-verbose))
	  (print-jni-info
   (nth 2 csde-run-option-verbose)))
      (if print-classes-loaded
	  (setq options (nconc options (list "-v"))))
      (if print-memory-freed
	  (setq options (nconc options '("-verbosegc"))))
      (if print-jni-info
	  (setq options (nconc options '("-verbosejni")))))

    ;; Set properties arguments.
    (if csde-run-option-properties
	(let ((count (length csde-run-option-properties))
	      (n 0))
	  (while (< n count)
	    (let ((prop (nth n csde-run-option-properties)))
	      (setq options 
		    (nconc options
			   (list (concat "-D" (car prop) "=" (cdr prop))))))    
	    (setq n (1+ n)))))

    ;; Set heap size options.
    (let* ((start-cons (nth 0 csde-run-option-heap-size))
	   (start-size (format "%d%s" (car start-cons) 
			       (cdr (assoc (cdr start-cons)
				      memory-unit-abbrevs))))
	   (max-cons (nth 1 csde-run-option-heap-size))
	   (max-size (format "%d%s" (car max-cons) 
			     (cdr (assoc (cdr max-cons)
				    memory-unit-abbrevs)))))
      (if (not (string= start-size "1m"))
	  (setq options 
		(nconc options (list (concat "-Xms" start-size)))))
      (if (not (string= max-size "16m"))
	  (setq options 
		(nconc options (list (concat "-Xmx" max-size))))))

    ;; Set stack size options.
    (let* ((c-cons (nth 0 csde-run-option-stack-size))
	   (c-size (format "%d%s" (car c-cons) 
			       (cdr (assoc (cdr c-cons)
				      memory-unit-abbrevs))))
	   (csharp-cons (nth 1 csde-run-option-stack-size))
	   (csharp-size (format "%d%s" (car csharp-cons) 
			     (cdr (assoc (cdr csharp-cons)
				    memory-unit-abbrevs)))))
      (if (not (string= c-size "128k"))
	  (setq options 
		(nconc options (list (concat "-Xss" c-size)))))
      (if (not (string= csharp-size "400k"))
	  (setq options 
		(nconc options (list (concat "-Xoss" csharp-size))))))

    ;; Set garbage collection options.
    (let ((no-gc-asynch (not 
			 (nth 0 csde-run-option-garbage-collection)))
	  (no-gc-classes (not 
			  (nth 1 csde-run-option-garbage-collection))))
      (if no-gc-asynch
	  (setq options (nconc options '("-Xnoasyncgc"))))
      (if no-gc-classes
	  (setq options (nconc options '("-Xnoclassgc")))))

    ;; Set Csharp profile option.
    (let ((profilep (car csde-run-option-csharp-profile))
	  (file (cdr csde-run-option-csharp-profile)))
      (if profilep
	  (if (string= file "./csharp.prof")
	      (setq options (nconc options '("-Xprof")))
	    (setq options 
		  (nconc options 
			 (list (concat "-Xprof:" file)))))))

    ;; Set heap profile option.
    (let* ((profilep (car csde-run-option-heap-profile))
	   (prof-options (cdr csde-run-option-heap-profile))
	   (file (nth 0 prof-options))
	   (depth (nth 1 prof-options))
	   (top (nth 2 prof-options))
	   (sort 
	    (downcase (substring (nth 3 prof-options) 0 1))))
      (if profilep
	  (if (and (string= file "./csharp.hprof")
		   (equal depth 5)
		   (equal top 20)
		   (string= sort "a"))
	      (setq options (nconc options '("-Xhprof")))
	    (setq options
		  (nconc options
			 (list
			  (format 
			   "-Xhprof:file=%s,depth=%d,top=%d,sort=%s"
			   file depth top sort)))))))

    ;; Set verify options.
    (let ((verify-all (nth 0 csde-run-option-verify))
	  (verify-remote (nth 1 csde-run-option-verify)))
      (if verify-all
	  (setq options (nconc options '("-Xverify"))))
;      (if verify-remote
;	  (setq options (concat options "-Xverifyremote")))
      (if (and
	   (not verify-all)
	   (not verify-remote))
	  (setq options (nconc options '("-Xnoverify")))))

    ;; Set command line args.
    (if csde-run-option-vm-args
	(let ((len (length csde-run-option-vm-args))
	      (n 0))
	  (while (< n len)
	    (setq options (nconc options
				 (csde-run-parse-args
				  (nth n csde-run-option-vm-args))))
	    (setq n (1+ n)))))
	      
    options))


;;;###autoload
(defun csde-run ()
  "Run the Csharp application specified by `csde-run-executable', if
not the null string. Otherwise run the class specified by 
`csde-run-application-class', if non-null; otherwise the class in
the current buffer. This command also creates a comint buffer to allow
you to interacti with the program."
  (interactive)
  (if (string= mode-name "CSDE")
      (if (string= csde-run-executable "")
	  (csde-run-main-class)
	(csde-run-executable))
    (error "The csde-run command works only in a Csharp source buffer.")))

(defun csde-run-get-main-class () 
  "Gets the main class for the application to which the current
source buffer belongs."
  (let ((main-class csde-run-application-class))
    (if (or
	 (not main-class)
	 (string= main-class ""))
	(setq main-class
	      (concat (csde-db-get-package)
		      (file-name-sans-extension 
		       (file-name-nondirectory (buffer-file-name))))))
    main-class))



(defun csde-run-main-class()
  "Runs the Csharp program named by `csde-run-application-class' in
a buffer, piping output from the program to the buffer and 
input from the buffer to the program."
  (interactive)
    (csde-run-internal (csde-run-get-main-class)))


(defmacro save-w32-show-window (&rest body)
  "Saves the value of the w32-start-process-show-window variable
before evaluating body and restores the value afterwards."
  `(let ((win32-start-process-show-window t)
	 (w32-start-process-show-window t)
	 (windowed-process-io t))		 
     ,@body))

(defun csde-run-unquote (string)
  (if (eq (aref string 0) ?\")
      (substring string 1 (- (length string) 1))
    string))

(defun csde-run-application-running-p ()
  "*Returns t if the application to which the current
buffer belongs is running."
  (let ((run-buf-name (concat "*" (csde-run-get-main-class) "*")))
    (comint-check-proc run-buf-name)))

(defun csde-run-internal(app-class)
  (let ((run-buf-name (concat "*" app-class "*"))
	(source-directory default-directory)
	(working-directory (if (string= csde-run-working-directory "")
			       default-directory
			     csde-run-working-directory)))
    (if (not (comint-check-proc run-buf-name))
	(let* ((run-buffer (get-buffer-create run-buf-name))
	       (win32-p (eq system-type 'windows-nt))
	       (prog (if (and
			  win32-p
			  (string= csde-run-csharp-vm "csharp"))
			 csde-run-csharp-vm-w
		       csde-run-csharp-vm))
	       (prog-args (append
			   (csde-run-get-vm-args)
			   (if csde-run-read-vm-args
			       (csde-run-parse-args
				(read-from-minibuffer
				 "Vm args: "
				 (car csde-run-interactive-vm-arg-history)
				 nil nil
				 'csde-run-interactive-vm-arg-history)))
			   (list app-class)
			   csde-run-option-application-args
			   (if csde-run-read-app-args
			       (csde-run-parse-args
				(read-from-minibuffer
				 "Application args: "
				 (car csde-run-interactive-app-arg-history)
				 nil nil
				 'csde-run-interactive-app-arg-history)))
			   ))
	       (command-string (concat prog " " 
				       (csde-run-make-arg-string
					prog-args)
				       "\n\n")))
	  (save-excursion
	    (set-buffer run-buffer)
	    (erase-buffer)
	    (cd working-directory)
	    (insert (concat "cd " working-directory "\n"))
	    (insert command-string)
	    (csde-run-mode))
; 	  (save-w32-show-window
; 	   (comint-exec run-buffer app-class prog nil prog-args))
	  (let ((win32-start-process-show-window t)
		(w32-start-process-show-window t)
		(windowed-process-io t))
	    (comint-exec run-buffer app-class prog nil prog-args))
	  (pop-to-buffer run-buffer)
	  (cd source-directory))
      (message "An instance of %s is running." app-class)
      (pop-to-buffer run-buf-name))))

(defun csde-run-executable()
  (let* ((prog-name (file-name-sans-extension
		     (file-name-nondirectory csde-run-executable)))
	 (run-buf-name (concat "*" prog-name "*"))
	 (source-directory default-directory)
	 (working-directory (if (string= csde-run-working-directory "")
				default-directory
			      csde-run-working-directory)))
    (if (not (comint-check-proc run-buf-name))
	(let* ((run-buffer (get-buffer-create run-buf-name))
	       (prog-args (append
			   csde-run-executable-args
			   (if csde-run-read-app-args
			       (csde-run-parse-args
				(read-from-minibuffer
				 "Application args: "
				 csde-run-interactive-app-args
				 nil nil
				 '(csde-run-interactive-app-arg-history . 1))))
			   ))
	       (command-string (concat csde-run-executable " " 
				       (mapconcat (lambda (arg) arg)
						  prog-args " ")
				       "\n\n")))
	  (save-excursion
	    (set-buffer run-buffer)
	    (erase-buffer)
	    (cd working-directory)
	    (insert (concat "cd " working-directory "\n"))
	    (insert command-string)
	    (csde-run-mode))
	  (save-w32-show-window
	   (comint-exec run-buffer prog-name csde-run-executable nil prog-args))
	  (pop-to-buffer run-buffer)
	  (cd source-directory))
      (message "An instance of %s is running." prog-name)
      (pop-to-buffer run-buf-name))))


;;;###autoload
(defun csde-run-mode()
  "Mode for running Csharp programs."
  (interactive)
  (comint-mode)
  (setq major-mode 'csde-run-mode)
  (run-hooks 'csde-run-mode-hook)
)

(defun csde-get-appletviewer-options ()
  (let (options)
    (if (not (string= csde-appletviewer-option-encoding ""))
	(setq options (list 
			"-encoding"
		        csde-appletviewer-option-encoding)))
    (if csde-appletviewer-option-vm-args
	(let ((len (length csde-appletviewer-option-vm-args))
	      (n 0))
	  (while (< n len)
	    (setq options
		  (nconc 
		   options
		   (list 
		    (concat "-J"
			    (nth n csde-appletviewer-option-vm-args)))))
	    (setq n (1+ n)))))
    options))
 
(defun csde-run-applet-exec (buffer name command startfile switches)
  "A version of comint-exec patched to start an applet viewer as
a command shell subprocess rather than as a subprocess of Emacs. This
is necessary to avoid displaying a DOS window when starting a viewer
under Windows."
  (save-excursion
    (set-buffer buffer)
    (let ((proc (get-buffer-process buffer)))	; Blast any old process.
      (if proc (delete-process proc)))
    ;; Crank up a new process
    (let ((proc (csde-run-applet-exec-1 name buffer command switches)))
      (set-process-filter proc 'comint-output-filter)
      (make-local-variable 'comint-ptyp)
      (setq comint-ptyp process-connection-type) ; T if pty, NIL if pipe.
      ;; Jump to the end, and set the process mark.
      (goto-char (point-max))
      (set-marker (process-mark proc) (point))
    (run-hooks 'comint-exec-hook)
    buffer)))

;; This auxiliary function cranks up the process for csde-run-applet-exec in
;; the appropriate environment.

(defun csde-run-applet-exec-1 (name buffer command switches)
  (let ((process-environment
	 (nconc
	  ;; If using termcap, we specify `emacs' as the terminal type
	  ;; because that lets us specify a width.
	  ;; If using terminfo, we specify `dumb' because that is
	  ;; a defined terminal type.  `emacs' is not a defined terminal type
	  ;; and there is no way for us to define it here.
	  ;; Some programs that use terminfo get very confused
	  ;; if TERM is not a valid terminal type.
	  (if (and (boundp 'system-uses-terminfo) system-uses-terminfo)
	      (list "TERM=dumb"
		    (format "COLUMNS=%d" (frame-width)))
	    (list "TERM=emacs"
		  (format "TERMCAP=emacs:co#%d:tc=unknown:" (frame-width))))
	  (if (getenv "EMACS") nil (list "EMACS=t"))
	  process-environment))
	(default-directory
	  (if (file-directory-p default-directory)
	      default-directory
	    "/")))
    (apply 'start-process-shell-command name buffer command switches)))

(defun csde-run-applet-internal (doc)
  (let* ((doc-file-name (file-name-nondirectory doc))
	 (doc-directory (file-name-directory doc))
	 (doc-name (file-name-sans-extension doc-file-name))
	 (run-buf-name (concat "*" doc-name "*")))

    (if (not (comint-check-proc run-buf-name))
	(let* ((run-buffer (get-buffer-create run-buf-name))
	       (win32-p (eq system-type 'windows-nt))
	       (prog csde-run-applet-viewer)
	       (prog-args
		(append (csde-get-appletviewer-options)
			(list doc-file-name)))
	       (command-string (concat prog " "
				       (csde-run-make-arg-string
					prog-args)
				       "\n\n")))
	  (save-excursion
	    (set-buffer run-buffer)
	    (erase-buffer)
	    (cd doc-directory)
	    (insert (concat "cd " doc-directory "\n"))
	    (insert command-string)
	    (csde-run-mode))
	  (csde-run-applet-exec run-buffer doc-name prog nil prog-args)
	  (pop-to-buffer run-buffer))
      (message "An instance of the applet in %s is running." doc-name)
      (pop-to-buffer run-buf-name))))


(defun csde-run-find-html-files ()
  "If (buffer-file-name) is /a/b/c.xxx (where xxx can be anything), 
return (\"/a/b/c.html\") if it exists, else return (\"/a/b/c.htm\") 
if it exists, else return a list of all *.html files in /a/b/
directory."
  (let ((basename (file-name-sans-extension (buffer-file-name)))
	f)
    (cond 
     ((file-exists-p (setq f (concat basename ".html")))
      (list f))
     ((file-exists-p (setq f (concat basename ".htm"))) ;; for poor winXX souls
      (list f))
     (t
      (mapcan (lambda (file)
		(if (or
		     (string-match "[.]html$" file)
		     (string-match "[.]htm$" file))
		    (list file)))       
	      (directory-files 
	       (file-name-directory (buffer-file-name)) t))))))



(setq csde-run-applet-last-doc nil) 

;;;###autoload
(defun csde-run-applet (&optional doc)
  "Runs an applet. This function prompts you to enter the path of an
html document that displays the applet. If you enter return without
specifying a document, this function next checks whether
`csde-run-applet-doc' specifies a document. If so, it displays that
specified document. Next, it checks whether the current directory
contains any html files. If the current directory contains an html
file with the same root name as the Csharp file in the current buffer,
it displays the file. If not, it displays the first html file that it
finds in the current directory. If if cannot find an html file, it
signals an error.  This function uses the viewer specified by
`csde-run-applet-viewer' to display the specified document. Note that
if you run two html applet files successively with the same name, you
must kill the buffer created to run the first file before running the
second file. Otherwise, this command will simply redisplay the first
file."
  (interactive
   (let ((insert-default-directory nil))
     (list (read-file-name "Applet doc: " nil nil nil csde-run-applet-last-doc))))
  (setq csde-run-applet-last-doc doc)
  (let ((applet-doc (if (and csde-run-applet-last-doc
			     (not (string= csde-run-applet-last-doc "")))
			csde-run-applet-last-doc
		      (if (and csde-run-applet-doc
			       (not (string= csde-run-applet-doc "")))
			    csde-run-applet-doc
			  (car (csde-run-find-html-files))))))
    (if applet-doc
	(if (string-match "appletviewer" csde-run-applet-viewer)
	    (csde-run-applet-internal applet-doc)
	  (if (or
	       (string= csde-run-applet-viewer "")
	       (string-match "browse-url" csde-run-applet-viewer))
	      (browse-url applet-doc browse-url-new-window-p)
	    (csde-run-applet-internal (concat default-directory applet-doc))))
      (signal 'error "Could not find html document to display applet."))))


(defun csde-run-menu-run-applet ()
  (interactive)
  (csde-run-applet))
   

(provide 'csde-run)


;; Change History
;; $Log: csde-run.el,v $
;; Revision 1.3  2001/02/12 05:38:26  paulk
;; CSDE 2.2.7
;;
;; Revision 1.43  2001/02/03 08:23:53  paulk
;; Changed declaration of customized variables so you can use completion on path variables.
;;
;; Revision 1.42  2001/02/03 07:30:27  paulk
;; Now uses csde-build-classpath instead of csde-run-build-classpath to build classpath argument.
;;
;; Revision 1.41  2001/02/01 06:13:47  paulk
;; *** empty log message ***
;;
;; Revision 1.40  2000/10/20 04:11:07  paulk
;; Fix that allows the CSDE to be used with NT/XEmacs.
;;
;; Revision 1.39  2000/10/08 12:55:39  paulk
;; *** empty log message ***
;;
;; Revision 1.38  2000/09/21 04:45:32  paulk
;; Updates csde-run-applet to work with the appletviewer in JDK 1.3.
;;
;; Revision 1.37  2000/08/19 07:02:02  paulk
;; Changed variable name.
;;
;; Revision 1.36  2000/03/16 05:18:11  paulk
;; Miscellaneous small bug fixes and enhancements.
;;
;; Revision 1.35  2000/02/16 04:40:33  paulk
;; Implemented Cygwin/XEmacs compatiblity fixes provided by Fred Hart
;; <cfhart@Z-TEL.com> in bsh-internal.
;;
;; Revision 1.34  2000/02/01 04:11:56  paulk
;; ReleaseNotes.txt
;;
;; Revision 1.33  1999/12/27 08:02:13  paulk
;; Enhanced CSDE->Run App command to run executables.
;;
;; Revision 1.32  1999/12/14 05:15:09  paulk
;; CSDE->Run Applet now looks in the current Csharp source directory for an
;; html file having the same root name as the current Csharp source
;; buffer. If it finds such a file, it runs it. Otherwise, it runs the first html file that it encounters in the directory. Thanks to  Richard Y. Kim <ryk@coho.net> for providing a patch implementing this change.
;;
;; Revision 1.31  1999/12/03 08:22:00  paulk
;; Updated CSDEbug to run under JDK 1.3beta.
;;
;; Revision 1.30  1999/09/28 04:06:59  paulk
;; Supplied missing left parentheses.
;;
;; Revision 1.29  1999/09/05 04:33:28  paulk
;; Added support for running vm in classic mode.
;;
;; Revision 1.28  1999/08/29 04:29:18  paulk
;; Patches provided by Michael Ernst <mernst@alum.mit.edu>
;;
;; Revision 1.27  1999/07/04 03:31:11  paulk
;; Added csde-run-application-running-p predicate function.
;;
;; Revision 1.26  1999/05/07 23:22:09  paulk
;; Changed csde-run-parse-args to accept any substring enclosed in single or double
;; quotes or that does not contain white space as an arg.
;;
;; Revision 1.25  1999/02/10 18:29:03  paulk
;; Added support for appletviewer options.
;;
;; Revision 1.24  1999/02/05 21:59:30  paulk
;; Added file-completion and default-to-last-entry to csde-run-applet
;; command.
;;
;; Revision 1.23  1999/02/03 01:08:34  paulk
;; Enhanced csde-run-applet to look in current directory for html file
;; to display, if you do not specify a file in the minibuffer or via
;; csde-run-applet-doc. Also fixed a bug in the minibuffer version of
;; csde-run-applet that forced you always to specify the name of an
;; html document.
;;
;; Revision 1.22  1999/01/15 21:59:34  paulk
;; Added Andy Piper's NT/XEmacs compatibility changes.
;;
;; Revision 1.21  1998/12/06 02:19:36  paulk
;; Fixed bug with csde-run-options-properties. (The bug was putting a space before the
;; -D switch.)
;;
;; Revision 1.20  1998/09/11 23:53:32  paulk
;; Added a csde-run-working-directory customization variable. If set to a valid
;; path, the CSDE starts the application from the directory specified by the
;; path. If the value of this variable is the empty string (the default),
;; the CSDE starts the application from the default directory of the current
;; source buffer. The default directory is usually the directory containing
;; the source file.
;;
;; Revision 1.19  1998/08/28 12:49:23  paulk
;; Updated to support NT/Emacs 20.3
;;
;; Revision 1.18  1998/07/02 05:36:00  paulk
;; Added $ to the set of characters recognized by the CSDE as valid
;; in vm and Csharp app command-line arguments.
;;
;; Revision 1.17  1998/06/30 21:10:28  paulk
;; Fixed csde-run-parse-args to recognize % as an argument
;; character.
;;
;; Revision 1.16  1998/06/30 03:32:37  paulk
;; Added the variables `csde-run-read-vm-args' and `csde-run-read-app-args'.
;; The first cause the csde-run command to read vm arguments from the
;; minibuffer and append them to the vm arguments specified by
;; the `csde-run-option' group of customization variables. The second
;; causes csde-run to read arguments to be passed to the application
;; from the minibuffer and append them to the arguments specified
;; by `csde-run-applications-args'. The CSDE maintains separate histories
;; for both types of arguments.
;;
;; Revision 1.15  1998/05/27 06:01:04  paulk
;; Added autoload comments.
;;
;; Revision 1.14  1998/03/04 04:08:21  kinnucan
;; Fixed bug in csde-run.
;;
;; Revision 1.13  1998/02/27 21:55:04  kinnucan
;; * Added support for Emacs customization feature.
;;
;; Revision 1.12  1997/10/26 05:57:22  kinnucan
;; Fixed bug where csde-run was incorrectly parsing command line arguments
;; containing an equal (=) sign.
;;
;; Revision 1.11  1997/10/05 21:21:59  kinnucan
;; Unquoted classpath as quotes are only necessary for compilation (because
;; the CSDE uses a shell to run the compiler).
;;
;; Revision 1.10  1997/10/05 17:15:44  kinnucan
;; Added the function csde-run-set-app-args, which allows you to
;; specify command line arguments for the application you are running.
;;
;; Also, changed the value of csde-run-args from a string to a list.
;;
;; Revision 1.9  1997/09/16 02:37:16  kinnucan
;; Changed w32-start-process-show-window to win32-start-process-show-window
;;
;; Revision 1.8  1997/09/04 03:54:34  kinnucan
;; Added csde-run-applet command, which runs a Csharp applet.
;;
;; Revision 1.7  1997/08/29 03:19:04  kinnucan
;; Fixed bug in save-w32-show-window.
;;
;; Revision 1.6  1997/08/26 08:46:41  kinnucan
;; Tweaked version number.
;;
;; Revision 1.5  1997/08/26 08:33:16  kinnucan
;; Deleted superfluous comments.
;;
;; Revision 1.4  1997/08/26 08:31:36  kinnucan
;; 1. Ported csde-run onto comint mode.
;;
;;    This allows you to interact with a Csharp application in the
;;    run buffer, if the application accepts command line input.
;;    You can use the comint history features to facilitate interaction
;;    with such an application.
;;
;; 2. Added the csde-run-set-csharp-vm and csde-run-set-csharp-vm-w
;;    commands, which let you specify the Csharp interpreter to use to
;;    run on non-Windows and Windows platforms, respectively.
;;
;;    Note that you must use csharpw on Windows platforms to avoid
;;    opening a superfluous command shell window.
;;
;; 3. Added the csde-run-set-args command and associated csde-run-args
;;    variable, which let you specify Csharp interpreter options via
;;    command-line arguments.
;;
;;    csde-run passes the value of csde-classpath (defined in csde.el
;;    and set via the csde-set-classpath command) and csde-run-args
;;    to the Csharp interpreter.
;;
;;   This means that you can use a common classpath definition for
;;   compiling and running applications, while passing other
;;   runtime arguments via csde-run-set-args.
;;
;; Revision 1.3  1997/07/05 04:20:44  kinnucan
;; Modified csde-run command to derive the class name from the name of the file in
;; the current buffer rather than the buffer name. This avoids an incorrect derivation
;; when more than one buffer is open on the same source file.
;;
;; Revision 1.2  1997/06/29 08:23:21  kinnucan
;; 1. Added csde-run-set-app function, which lets you specify the application
;;    class to run.
;;
;; 2. Updated csde-run to run either the app specified by csde-run-set-app or
;;    the class whose source is in the current buffer. In the latter case,
;;    csde-run extracts the package of the app class from the source buffer.
;;
;; Revision 1.1  1997/06/18 17:23:28  paulk
;; Initial revision
;;

;;; csde-run.el ends here
