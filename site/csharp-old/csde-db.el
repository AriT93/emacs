;;; csde-db.el -- Debugger mode for jdb.
;; $Revision: 1.3 $ $Date: 2001/02/12 05:38:24 $ 

;; Adapted from the JDE by Matt Bruce <matt.bruce@morganstanley.com>
;; Maintainer:  Matt Bruce

;; Copyright (C) 2001 by Matt Bruce

;; JDE Author: Paul Kinnucan <paulk@mathworks.com>
;; JDE Maintainer: Paul Kinnucan

;; Keywords: csharp, tools

;; JDE version Copyright (C) 1997, 2000, 2001 Paul Kinnucan.

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
;; Boston, MA 02111-1307, US
;;; Commentary:


;; The latest version of the CSDE is available at
;; <URL:http://www.sourceforge.com/>.

;; Please send any comments, bugs, or upgrade requests to
;; Matt Bruce (matt.bruce@morganstanley.com)

;; See end of this file for change history.

;;; Code:

(require 'gud)
(require 'csde-parse)

;; ======================================================================
;; csde-db variables

(makunbound 'csde-db-debugger)
(defcustom csde-db-debugger (cons "jdb" (cons "" "Executable"))
"Specify the debugger you want to use to debug Csharp programs.  Select
CSDEbug, if you want to use the CSDE's builtin debugger.  Select jdb, if
you want to use the version of jdb that comes with JDK 1.3 (or later)
or the Linux version of JDK 1.2.2.  Select oldjdb, if you want to use
the version of jdb that comes with JDK 1.1.x (or earlier) versions of
the JDK or with the Solaris and Windows versions of JDK 1.2.x.
Otherwise, select Other. Other may be an executable or a Csharp
class. If Other is a class, the CSDE launches a vm to run the
debugger. If you choose jdb or oldjdb, you must ensure that the jdb
executable is in your system's command path.  If you choose Other,
select the debugger's type (executable or class) and enter the path
name of the debugger, if Other is an
executable, or a fully qualified package name, if Other is a class."
  :group 'csde-project
  :type '(cons
	  (radio-button-choice :format "%t \n%v"
			       :tag "Debugger "
			       (const "CSDEbug")
			       (const "jdb")
			       (const "oldjdb")
			       (const "Other"))
	  (cons :tag "Other Debugger Info"
	   (file :tag "Path")
	   (radio-button-choice :format "%t \n%v"
			       :tag "Type "
		 (const "Executable")
		 (const "Class"))))
  :set '(lambda (sym val)
	  (mapc 
	   (lambda (buff)
	     (save-excursion	       
	       (set-buffer buff)
	       (if (string= (car val) "CSDEbug")
		   (csde-bug-install-csdebug-menu)
		 (csde-bug-remove-csdebug-menu))))
	     (csde-get-csharp-source-buffers))
	  (set-default sym val)))

(makunbound 'csde-db-source-directories)
(defcustom csde-db-source-directories nil
  "*List of source directory paths.  The CSDE uses this list to locate
source files corresponding to class files when debugging or building
applications.  When entering paths in the custom buffer, enter each
path as a separate item in a separate edit field. Do NOT put more than
one path in the same edit field. You'll only confuse CSDE.
Paths may contain environment variables."
  :group 'csde-project
  :type '(repeat (file :tag "Path")))

(defcustom csde-db-mode-hook nil
  "*Customization hook for csde-db inferior mode."
  :group 'csde-project
  :type 'hook
)

(defcustom csde-db-set-initial-breakpoint t
  "*Set breakpoint in main and run application.
If this variable is non-nil, the CSDE issues the following 
debugger commands at startup:

  stop in app-class.main
  run

where app-class is the qualified name of your application's
main class. This variable is non-nil by default. Set it to
nil, if you want to set an initial breakpoint yourself."
  :group 'csde-project
  :type 'boolean)

(defcustom csde-db-startup-commands nil
  "*Commands to run at debugger startup."
  :group 'csde-project
  :type '(repeat (string :tag "Command"))
)

(defcustom csde-db-read-vm-args nil
"*Read vm arguments from the minibuffer.
If this variable is non-nil, the csde-db command reads vm arguments
from the minibuffer and appends them to those specified by
the `csde-db-option' variable group."
  :group 'csde-project
  :type 'boolean)

(defvar csde-db-interactive-vm-arg-history nil
"History of vm arguments read from the minibuffer")

(defcustom csde-db-read-app-args nil
"*Read arguments to be passed to application from the minibuffer."
  :group 'csde-project
  :type 'boolean)

(defvar csde-db-interactive-app-arg-history nil
"History of application arguments read from the minibuffer")


(defgroup csde-db-options nil
  "CSDE Debugger Options"
  :group 'csde
  :prefix "csde-run-option-")

(defcustom csde-db-option-classpath nil
"*Specify paths of classes required to run this application.
The CSDE uses the specified paths to construct a -classpath
argument to pass to the Csharp interpreter. This option overrides the
`csde-global-classpath' option."
  :group 'csde-db-options
  :type '(repeat (file :tag "Path")))
 
(defcustom csde-db-option-verbose (list nil nil nil)
  "*Print messages about the running process.
The messages are printed in the run buffer."
  :group 'csde-db-options
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

(defcustom csde-db-option-properties nil
  "*Specify property values.
Enter the name of the property, for example, awt.button.color, in the
Property Name field; enter its value, for example, green, in the
Property Value field. You can specify as many properties as you like."
  :group 'csde-db-options
  :type '(repeat (cons 
		  (string :tag "Property Name") 
		  (string :tag "Property Value"))))

(defcustom csde-db-option-heap-size (list
				    (cons 1 "megabytes")
				    (cons 16 "megabytes"))
"*Specify the initial and maximum size of the interpreter heap."
:group 'csde-db-options
:type '(list
	(cons (integer :tag "Start")
	     (radio-button-choice (const "bytes")
				  (const "kilobytes")
				  (const "megabytes")))
	(cons (integer :tag "Max")
	       (radio-button-choice (const "bytes")
				    (const "kilobytes")
				    (const "megabytes")))))


(defcustom csde-db-option-stack-size (list
				     (cons 128 "kilobytes")
				     (cons 400 "kilobytes"))
  "*Specify size of the C and Csharp stacks."
  :group 'csde-db-options
  :type '(list
	  (cons (integer :tag "C Stack")
	       (radio-button-choice (const "bytes")
				    (const "kilobytes")
				    (const "megabytes")))
	  (cons (integer :tag "Csharp Stack")
	       (radio-button-choice (const "bytes")
				    (const "kilobytes")
				    (const "megabytes")))))

(defcustom csde-db-option-garbage-collection (list t t)
  "*Specify garbage collection options."
  :group 'csde-db-options
  :type '(list :indent 2
	       (checkbox :format "%[%v%] %t \n"
			 :tag "Collect garbage asynchronously.")
	       (checkbox :format "%[%v%] %t \n"
			 :tag "Collect unused classes.")))

(defcustom csde-db-option-csharp-profile (cons nil "./csharp.prof")
  "*Enable Csharp profiling."
  :group 'csde-db-options
  :type '(cons boolean
	       (file :tag "File"
		     :help-echo 
"Specify where to put profile results here.")))

(defcustom csde-db-option-heap-profile (cons nil
					    (list "./csharp.hprof"
						  5
						  20
						  "Allocation objects"))
"*Output heap profiling data."
  :group 'csde-db-options
  :type '(cons boolean
	       (list
		(string :tag "Output File Path")
		(integer :tag "Stack Trace Depth")
		(integer :tag "Allocation Sites")
		(radio-button-choice :format "%t \n%v"
				     :tag "Sort output based on:"
		 (const "Allocation objects")
		 (const "Live objects")))))
		 
(defcustom csde-db-option-verify (list nil t)
  "*Verify classes."
  :group 'csde-db-options
  :type '(list :indent 2
	       (checkbox :format "%[%v%] %t \n"
			 :tag "Executed code in all classes.")
	       (checkbox :format "%[%v%] %t \n"
			 :tag "Classes loaded by a classloader.")))

(defcustom csde-db-option-vm-args nil
  "*Specify arguments to be passed to the Csharp vm.
This option allows you to specify one or more arguments to be passed
to the Csharp interpreter. It is an alternative to using CSDE Run Option
variables, such as `csde-run-option-stack-size', to specify Csharp
interpreter options. Also, it makes it possible to use the CSDE with
interpreters that accept command line arguments not supported by 
the CSDE Run Option variable set."
  :group 'csde-db-options
  :type '(repeat (string :tag "Argument")))


(defcustom csde-db-option-application-args nil
  "*Specify command-line arguments to pass to the application.
The CSDE passes the specified arguments to the application on
the command line."
  :group 'csde-db-options
  :type '(repeat (string :tag "Argument")))


;;;###autoload
(defun csde-db-set-debugger (name is-executable)
  "Specify the pathname of the debugger, if an executable, or the
debugger's fully qualified class name, if a class."
  (interactive
   "sEnter name of Csharp interpreter: \nsIs %s executable? (yes): ")
  (let ((db name)
	(type
	 (if (stringp is-executable)
	     (if (or
		  (string= is-executable "")
		  (eq (aref is-executable 0) ?y))
		 "Executable"
	       "Class")
	   "Executable")))
    (setq csde-db-debugger (cons "Other" (cons db type)))))

;;;###autoload
(defun csde-db-set-args (args)
  "Specify the arguments (except -classpath) to be passed to the debugger."
  (interactive 
   "sEnter arguments: ")
  (setq csde-db-option-vm-args (csde-run-parse-args args)))

;;;###autoload
(defun csde-db-set-app-args (args)
  "Specify the arguments to be passed to the Csharp application class."
  (interactive 
   "sEnter arguments: ")
  (setq csde-db-option-application-args (csde-run-parse-args args)))

(defun csde-db-get-vm-args ()
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
	       (list "-classic")))

    ;; Set the classpath option. Use the local
    ;; classpath, if set; otherwise, the global
    ;; classpath.
    (setq options
	  (nconc
	   options
	   (list
	    "-classpath"
	   (csde-build-classpath
	    (if csde-db-option-classpath
		csde-db-option-classpath
	      csde-global-classpath)))))

    ;; Set the verbose options.
    (let ((print-classes-loaded
	   (nth 0 csde-db-option-verbose))
	  (print-memory-freed
	   (nth 1 csde-db-option-verbose))
	  (print-jni-info
	   (nth 2 csde-db-option-verbose)))
      (if print-classes-loaded
	  (setq options (nconc options (list "-v"))))
      (if print-memory-freed
	  (setq options (nconc options '("-verbosegc"))))
      (if print-jni-info
	  (setq options (nconc options '("-verbosejni")))))

    ;; Set properties arguments.
    (if csde-db-option-properties
	(let ((count (length csde-db-option-properties))
	      (n 0))
	  (while (< n count)
	    (let ((prop (nth n csde-db-option-properties)))
	      (setq options 
		    (nconc options
			   (list (concat "-D" (car prop) "=" (cdr prop))))))    
	    (setq n (1+ n)))))

    ;; Set heap size options.
    (let* ((start-cons (nth 0 csde-db-option-heap-size))
	   (start-size (format "%d%s" (car start-cons) 
			       (cdr (assoc (cdr start-cons)
				      memory-unit-abbrevs))))
	   (max-cons (nth 1 csde-db-option-heap-size))
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
    (let* ((c-cons (nth 0 csde-db-option-stack-size))
	   (c-size (format "%d%s" (car c-cons) 
			       (cdr (assoc (cdr c-cons)
				      memory-unit-abbrevs))))
	   (csharp-cons (nth 1 csde-db-option-stack-size))
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
			 (nth 0 csde-db-option-garbage-collection)))
	  (no-gc-classes (not 
			  (nth 1 csde-db-option-garbage-collection))))
      (if no-gc-asynch
	  (setq options (nconc options '("-Xnoasyncgc"))))
      (if no-gc-classes
	  (setq options (nconc options '("-Xnoclassgc")))))

    ;; Set Csharp profile option.
    (let ((profilep (car csde-db-option-csharp-profile))
	  (file (cdr csde-db-option-csharp-profile)))
      (if profilep
	  (if (string= file "./csharp.prof")
	      (setq options (nconc options '("-Xprof")))
	    (setq options 
		  (nconc options 
			 (list (concat "-Xprof:" file)))))))

    ;; Set heap profile option.
    (let* ((profilep (car csde-db-option-heap-profile))
	   (prof-options (cdr csde-db-option-heap-profile))
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
    (let ((verify-all (nth 0 csde-db-option-verify))
	  (verify-remote (nth 1 csde-db-option-verify)))
      (if verify-all
	  (setq options (nconc options '("-Xverify"))))
;      (if verify-remote
;	  (setq options (concat options "-Xverifyremote")))
      (if (and
	   (not verify-all)
	   (not verify-remote))
	  (setq options (nconc options '("-Xnoverify")))))

    ;; Set command line args.
    (if csde-db-option-vm-args
	(let ((len (length csde-db-option-vm-args))
	      (n 0))
	  (while (< n len)
	    (setq options (nconc options
				 (csde-run-parse-args
				  (nth n csde-db-option-vm-args))))
	    (setq n (1+ n)))))
	      
    options))


(defvar csde-db-last-package ()
  "Package that the debugger is currently visiting.")

(defvar csde-db-xemacs-menu
  '(["Continue"          gud-cont t]
    ["Next Line"         gud-next t]
    ["Step Line"         gud-step t]
    ["Print"             gud-print t]
    ["Down Stack"        gud-down t]
    ["Up Stack"          gud-up t]
    ["Set Breakpoint"    gud-break t]
    ["Remove Breakpoint" gud-remove t]
    )
  "XEmacs 19 menu for csharp debugger.")

(defun csde-db-xemacs-menu ()
  (cons "Jdb" csde-db-xemacs-menu))

;; (makunbound 'csde-db-marker-regexp)
(defcustom csde-db-marker-regexp
  "^.*: thread=.*, \\(\\(.*[.]\\)*\\)\\([^\$]*\\)\\(\$.*\\)*[.].+(), line=\\([0-9]*\\),"
"*Regular expression used to find a jdb breakpoint position marker.
The regular expression must have two subexpressions. The first matches
the name of the class in which the breakpoint occurs; the second, the
line number at which the breakpoint occurs. The default expression
matches breakpoint messages emitted by jdb. You may need to change
the expression to accommodate other debuggers."
  :group 'csde-project
  :type 'string)

(defvar csde-db-oldjdb-marker-regexp
  "^Breakpoint hit: .*(\\([^\$]*\\).*:\\([0-9]*\\))"
"*Regular expression used to find an oldjdb breakpoint position marker.
The regular expression must have two subexpressions. The first matches
the name of the class in which the breakpoint occurs; the second, the
line number at which the breakpoint occurs. The default expression
matches breakpoint messages emitted by oldjdb.")

(defcustom csde-db-nodebug-marker-regexp
  "^Breakpoint hit: .*(pc \\([0-9]*\\))"
"*Regular expression to match breakpoint message for which no
line number information is available.")

;; I'm not sure the following is necessary anymore. PK.

;; Thanks to "David J. Biesack" <sasdjb@unx.sas.com> for this function
;; and its use in csde-db-marker-filter.
;; Amended by "Patrick J. McNerthney" <pat@mcnerthney.com> to allow
;; package names to begin with underscores.
(defun csde-db-make-qualified-class-name-regexp (class)
  "Constructs a regular expression to extract a qualified class name from a jdb
breakpoint message."
  (concat "\\(\\(\\(\\(\\w\\|[_]\\)*\\.\\)*\\)" class "\\)\\(\\b\\|\\$\\)"))

;; David Biesack's original expression.
;;(concat "\\b\\(\\(\\(\\(\\w\\|[_]\\)*\\.\\)*\\)" class "\\)\\(\\b\\|\\$\\)"))

;; There's no guarantee that Emacs will hand the filter the entire
;; marker at once; it could be broken up across several strings.  We
;; might even receive a big chunk with several markers in it.  If we
;; receive a chunk of text which looks like it might contain the
;; beginning of a marker, we save it here between calls to the
;; filter.
(defvar csde-db-marker-acc "")
(make-variable-buffer-local 'csde-db-marker-acc)

(defvar csde-db-stack-depth "")
(make-variable-buffer-local 'csde-db-stack-depth)


(defun csde-db-marker-filter (input)

  ;; Accumulate next chunk of debugger output.
  (setq csde-db-marker-acc (concat csde-db-marker-acc input))

  ;; This is a hack to accommodate reorder of message chunks
  ;; on Solaris at debugger startup.
  (if (string-match "running ...\n" csde-db-marker-acc)
      (setq csde-db-marker-acc
	    (concat "running ...\n"
		    (substring csde-db-marker-acc 0 (match-beginning 0))
		    (substring csde-db-marker-acc (match-end 0)))))
		    
  (let* ((output "")
	 (oldjdbp (if (string= (car csde-db-debugger) "oldjdb") t))
	 (marker-regexp 
	  (if oldjdbp csde-db-oldjdb-marker-regexp csde-db-marker-regexp))
	 (marker-regexp-class-index (if oldjdbp 1 3))
	 (marker-regexp-line-index (if oldjdbp 2 5)))

    ;; (message (concat "jdb output:" input))
    ;; (message (concat "acc = " csde-db-marker-acc))
    
    ;; Process all the complete markers in this chunk.
    (if (string-match marker-regexp csde-db-marker-acc)
	;; Extract the frame position from the marker.
	(let ((premarker (substring csde-db-marker-acc 0 (match-beginning 0)))
	      (marker (substring csde-db-marker-acc (match-beginning 0) (match-end 0)))
	      (class (substring 
		      csde-db-marker-acc  
		      (match-beginning marker-regexp-class-index) 
		      (match-end marker-regexp-class-index)))
	      (line-no (string-to-int 
			(substring 
			 csde-db-marker-acc
			 (match-beginning marker-regexp-line-index)
			 (match-end marker-regexp-line-index))))
	      (rest (substring csde-db-marker-acc (match-end 0))))

	  (setq gud-last-frame (cons (concat class ".cs") line-no))

	  ;; Extract package path from input.
	  (setq csde-db-last-package "")
	  (let ((case-fold-search nil)) ;; Make sure search is case-sensitive
	    (and (string-match (csde-db-make-qualified-class-name-regexp class) marker)
		 (setq csde-db-last-package
		       (substring marker (match-beginning 2) (match-end 2))))

	    ;; (message "csde-db package: %s. marker = %s" csde-db-last-package marker)
	    ;;(message "case-fold-search = %s" (if case-fold-search "true" "false"))
	    )

	  ;; Insert debugger output into debugger buffer.
	  (setq output (concat premarker marker))

	  ;; Set the accumulator to the remaining text.
	  (setq csde-db-marker-acc rest)))

    ;; Handle case where there is no line number info in current class.
    (if (string-match csde-db-nodebug-marker-regexp csde-db-marker-acc) 
	(let ((premarker (substring csde-db-marker-acc 0 (match-beginning 0)))
	      (marker (substring csde-db-marker-acc (match-beginning 0) (match-end 0)))
	      (pc (substring csde-db-marker-acc (match-beginning 1) (match-end 1)))
	      (rest (substring csde-db-marker-acc (match-end 0))))

	  (setq output (concat premarker marker))))

    ;; Thanks to Michael Ernst <mernst@cs.washington.edu> for the following
    ;; stack-related code.
    ;;
    ;; Extract the index of the current stack frame from the jdb prompt, where
    ;; the prompt is of the form 
    ;;
    ;;   thread[stack_index]
    ;; 
    ;; e.g.,
    ;;
    ;;   main[1]
    ;;
    ;; The user can move the debugger up and down the stack via the up and
    ;; down commands. The debugger indicates the current location by the
    ;; stack index portion of its prompt.
    (if (string-match "^[-a-zA-Z0-9_$]+\\[\\([0-9]+\\)\\] " csde-db-marker-acc)
	(setq csde-db-stack-depth (match-string 1 csde-db-marker-acc)))

    ;; The following form processes the output of the jdb where
    ;; command, which lists the current stack. An example of the output
    ;; is
    ;;
    ;;   [1] jmath.LinearSystem$InnerClass.print (LinearSystem.cs:36)
    ;;   [2] jmath.LinearSystem.<init> (LinearSystem.cs:52)
    ;;   [3] jmath.Test.main (Test.cs:38)
    ;;
    ;; The form positions the source line cursor at the position that
    ;; matches the current location of the debugger in the program's
    ;; stack (set by the jdb up and down stack commands).
    (if (string-match (concat "^  \\[" csde-db-stack-depth
			      "\\] .*(\\([^\$\n]*\\).*:\\([0-9]+\\))")
		      csde-db-marker-acc)
	(let ((marker (match-string 0 csde-db-marker-acc))
	      (class (match-string 1 csde-db-marker-acc))
	      (line-no (string-to-int (match-string 2 csde-db-marker-acc))))
	  (if (equal ".cs" (substring class -5))
	      (setq class (substring class 0 -5)))
	  (setq gud-last-frame (cons (concat class ".cs") line-no))
	  ;; Extract package path from input.
	  (setq csde-db-last-package "")
	  (let ((case-fold-search nil)) ;; Make sure search is case-sensitive
	    (and (string-match (csde-db-make-qualified-class-name-regexp class) marker)
		 (setq csde-db-last-package
		       (substring marker (match-beginning 2) (match-end 2)))))))

    ;; Does the remaining text look like it might end with the
    ;; beginning of another marker?  If it does, then keep it in
    ;; csde-db-marker-acc until we receive the rest of it.  Since we
    ;; know the full marker regexp above failed, it's pretty simple to
    ;; test for marker starts.
    (if (string-match "\\(^Breakpoint hit:\\)\\|\\(^Step completed:\\)" csde-db-marker-acc)
	(progn
	  ;; Everything before the potential marker start can be output.
	  (setq output (concat output (substring csde-db-marker-acc
						 0 (match-beginning 0))))

	  ;; Everything after, we save, to combine with later input.
	  (setq csde-db-marker-acc
		(substring csde-db-marker-acc (match-beginning 0))))

      (setq output (concat output csde-db-marker-acc)
	    csde-db-marker-acc ""))    

    output))


(defun csde-db-contains-file-p (dir file)
  "Return t if DIR contains FILE."
  (file-exists-p (expand-file-name file dir)))


(defun csde-db-pkg-to-path (package)
  "Return PACKAGE as a directory path."
  (let ((n (string-match "\\." package))
	(output (concat package)))
    (while n
      (aset output n ?/)
      (setq n (string-match "\\." output (+ n 1))))
    output))
      

(defun csde-db-contains-package-p (dir package)
   "Return t if DIR contains PACKAGE."
   (let ((pkg-path (csde-db-pkg-to-path package)))
     (file-exists-p (expand-file-name pkg-path dir))))

; (defun csde-test (dir package)
;   (interactive
;    "sDir: \nsPackage: ")
;   (message (if (csde-db-contains-package-p dir package) "true" "false")))


(defun csde-db-search-src-dirs (file package)
  "Return the directory containing the source FILE for a class in PACKAGE."
  (catch 'found
    (let ((len (length csde-db-source-directories))
	  (n 0))
      (while (< n len)
	(let ((curr-dir 
	       (substitute-in-file-name (elt csde-db-source-directories n))))
	  (cond
	   ((csde-db-contains-file-p curr-dir file)
	    (throw 'found curr-dir))
	   ((and (csde-db-contains-package-p curr-dir package)
		 (csde-db-contains-file-p
		  (expand-file-name (csde-db-pkg-to-path package) curr-dir) file))
	    (throw 'found
		   (expand-file-name  
			   (csde-db-pkg-to-path package) curr-dir)))))
	(setq n (1+ n))))))

;; Fixes a bug in gud-make-debug-menu
(defun csde-db-make-debug-menu ()
  "Make sure the current local map has a [menu-bar debug] submap.
If it doesn't, replace it with a new map that inherits it,
and create such a submap in that new map."
  (if (and (current-local-map)
	   (lookup-key (current-local-map) [menu-bar debug]))
      nil
    (use-local-map (gud-new-keymap (current-local-map)))
    (define-key (current-local-map) [menu-bar debug]
      (nconc (list "Jdb") gud-menu-map))))



(defun csde-db-find-file (f)
  ;;(interactive "sFile: " )
  (let ((source-dir 
	 (csde-db-search-src-dirs f csde-db-last-package)))
    (if source-dir
	(let ((source-file
	       (concat source-dir f)))	
	  (save-excursion
	    (let ((buf (find-file-noselect source-file)))
	      (set-buffer buf)
	      (if (string-match "XEmacs\\|Lucid" emacs-version)
		  (if (and (boundp 'current-menubar)
			   current-menubar)
		      (if (fboundp 'add-submenu)
			  (add-submenu nil (csde-db-xemacs-menu))
			(add-menu nil "Jdb" csde-db-xemacs-menu)))
		(progn
		  (csde-db-make-debug-menu)
		  (local-set-key [menu-bar debug refresh] nil)
		  (local-set-key [menu-bar debug stepi] nil)
		  (local-set-key [menu-bar debug up] '("Up Stack" . gud-up))
		  (local-set-key [menu-bar debug down] '("Down Stack" . gud-down))
		  (local-set-key [menu-bar debug where] '("Where" . gud-where))))
	      buf)))
	  (message (concat "Error: could not find %s. "
			   "See csde-db-source-directories.") f)
	  (if csde-xemacsp (window-buffer) nil))))

(defun csde-find-class-source (class)
  "*Find the source file for a specified class.
CLASS is the fully qualified name of the class. This
function searchs the source file paths specified by 
`csde-db-source-directories' for the source file 
corresponding to CLASS. If it finds the source file,
it opens the file in a buffer."
  (interactive "sClass: ")
  (string-match "^\\(\\(\\(\\w\\|[_]\\)*[.]\\)*\\)\\(\\(\\w\\|[_]\\)+$\\)" class)  
  (let* ((package-name
	  (substring class (match-beginning 1) (match-end 1)))
	 (class-name
	  (substring class (match-beginning 4) (match-end 4)))
	 (file-name (concat class-name ".cs"))
	 (source-dir
	  (csde-db-search-src-dirs
	   file-name
	   package-name)))
    (if source-dir
	(find-file (concat source-dir file-name))
      (message (concat "CSDE error: Could not find source for %s. "
		       "See `csde-db-source-directories' for more information." )
		       class))))

(defvar csde-db-minibuffer-local-map nil
  "Keymap for minibuffer prompting of jdb startup command.")
(if csde-db-minibuffer-local-map
    ()
  (setq csde-db-minibuffer-local-map (copy-keymap minibuffer-local-map))
  (define-key
    csde-db-minibuffer-local-map "\C-i" 'comint-dynamic-complete-filename))


(defun class-from-file-name (file-name)
  (file-name-sans-extension (file-name-nondirectory file-name)))

;;; The csde-db-call function must do the right thing whether its invoking
;;; keystroke is from the GUD buffer itself (via major-mode binding)
;;; or a Csharp buffer.  In the former case, we want to supply data from
;;; gud-last-frame.  Here's how we do it:
;;; Note: this is adapted from the gud-format-command function
;;; in gud.el.

(defun csde-db-format-command (str arg)
  (let ((insource (not (eq (current-buffer) gud-comint-buffer)))
	(frame (or gud-last-frame gud-last-last-frame))
	result)
    (while (and str (string-match "\\([^%]*\\)%\\([acdeflp]\\)" str))
      (let ((key (string-to-char (substring str (match-beginning 2))))
	    (group1 (substring str (match-beginning 1) (match-end 1)))
	    subst)
	(setq str (substring str (match-end 2)))
	(cond
	 ((eq key ?f)
	  (setq subst (file-name-nondirectory (if insource
						  (buffer-file-name)
						(car frame)))))
	 ((eq key ?c)
	  (setq subst (concat (csde-db-get-package)
			      (or
			       (if insource
				   (csde-db-get-class)
				 (save-excursion
				   (set-buffer (gud-find-file (car frame)))
				   (csde-db-get-class)))
			       (error "Could not determine containing class.")))))



	 ((eq key ?d)
	  (setq subst (file-name-directory (if insource
					       (buffer-file-name)
					     (car frame)))))
	 ((eq key ?l)
	  (setq subst (if insource
			  (save-excursion
			    (beginning-of-line)
			    (save-restriction (widen)
					      (1+ (count-lines 1 (point)))))
			(cdr frame))))
	 ((eq key ?e)
	  (setq subst (if (fboundp 'gud-find-c-expr)
			  (gud-find-c-expr)
			(find-c-expr))))
	 ((eq key ?a)
	  (setq subst (gud-read-address)))
	 ((eq key ?p)
	  (setq subst (if arg (int-to-string arg) ""))))
	(setq result (concat result group1 
			     (if (integerp subst) (int-to-string subst) subst)))))
    ;; There might be text left in STR when the loop ends.
    (concat result str)))

(defun csde-db-call (fmt &optional arg)
  (let ((msg (csde-db-format-command fmt arg)))
    (message "Command: %s" msg)
    (sit-for 0)
    (gud-basic-call msg)))

(defmacro csde-db-def (func cmd key &optional doc)
  "Define FUNC to be a command sending CMD and bound to KEY, with
optional doc string DOC.  Certain %-escapes in the string arguments
are interpreted specially if present.  These are:

  %f name (without directory) of current source file.
  %c fully qualified class name
  %d directory of current source file.
  %l number of current source line
  %e text of the C lvalue or function-call expression surrounding point.
  %a text of the hexadecimal address surrounding point
  %p prefix argument to the command (if any) as a number

  The `current' source file is the file of the current buffer (if
we're in a Csharp file) or the source file current at the last break or
step (if we're in the jdb buffer).
  The `current' line is that of the current buffer (if we're in a
source file) or the source line number at the last break or step (if
we're in the jdb buffer)."
  (list 'progn
	(list 'defun func '(arg)
	      (or doc "")
	      '(interactive "p")
	      (list 'csde-db-call cmd 'arg))
	(if key
	    (list 'define-key
		  '(current-local-map)
		  (concat "\C-c" key)
		  (list 'quote func)))
	(if key
	    (list 'global-set-key
		  (list 'concat 'gud-key-prefix key)
		  (list 'quote func)))))

(defun csde-db-get-vm-args-from-user ()
  (if csde-db-read-vm-args
      (csde-run-parse-args
       (read-from-minibuffer
	"Vm args: "
	(car csde-db-interactive-vm-arg-history)
	nil nil
	'csde-db-interactive-vm-arg-history))))

(defun csde-db-get-app-args-from-user ()
  (if csde-db-read-app-args
      (csde-run-parse-args
       (read-from-minibuffer
	"Application args: "
	(car csde-db-interactive-app-arg-history)
	nil nil
	'csde-db-interactive-app-arg-history))))

(defun csde-db-init (app-class marker-filter find-file)
  (let ((debug-buf-name (concat "*debug" app-class "*"))
	(source-directory default-directory)
	(working-directory (if (string= csde-run-working-directory "")
			       default-directory
			     csde-run-working-directory)))
    (if (not (comint-check-proc debug-buf-name))
	(let* ((debug-buffer (get-buffer-create debug-buf-name))
	       (debugger (car csde-db-debugger))
	       (program 
		(cond
		 ((string= debugger "jdb") "jdb")
		 ((string= debugger "oldjdb") "jdb")
		 ((string= debugger "Other")
		  (if (string= (cdr (cdr csde-db-debugger)) "Executable")
		      (car (cdr csde-db-debugger))
		    csde-run-csharp-vm))
		 (t
		  (error "Unknown debugger: %s" debugger))))
	       (prog-args
		(let ((debugger-args
		       (append 
			(csde-db-get-vm-args)
			(csde-db-get-vm-args-from-user)
			(list app-class)
			csde-db-option-application-args
			(csde-db-get-app-args-from-user))))
		      (if (string= program "jdb")
			  debugger-args
			(if (string= (cdr (cdr csde-db-debugger)) "Executable")
			    debugger-args
			  (append
			   (list (car (cdr csde-db-debugger)))
			   debugger-args)))))
	       (command-string (concat
				program " "
				(csde-run-make-arg-string prog-args) "\n\n")))
	  (save-excursion
	    (set-buffer debug-buffer)
	    ;; Do not erase the last transcript; user may wish to view it.
	    ;; (erase-buffer)
	    (goto-char (point-max))
	    (cd working-directory)
	    (insert (concat "cd " working-directory "\n"))
	    (insert command-string)
	    (comint-mode))
	  (comint-exec debug-buffer app-class program nil prog-args)
	  (pop-to-buffer debug-buffer)
	  (cd source-directory)
	  (gud-mode)
	  (make-local-variable 'gud-marker-filter)
	  (setq gud-marker-filter marker-filter)
	  (make-local-variable 'gud-find-file)
	  (setq gud-find-file find-file)
	  (set-process-filter (get-buffer-process (current-buffer)) 'gud-filter)
	  (set-process-sentinel (get-buffer-process (current-buffer)) 'gud-sentinel)
	  (gud-set-buffer)
	  (if csde-db-startup-commands
	    (mapc 'gud-basic-call csde-db-startup-commands)
	   (when csde-db-set-initial-breakpoint
	    (gud-basic-call (concat "stop in " app-class ".main"))
	    (gud-basic-call "run"))))
      (message "An instance of %s is running." app-class)			
      (pop-to-buffer debug-buf-name))))			   
	  

(defun csde-db-get-package ()
  "Return the package of the class whose source file resides in the current
buffer."
  (save-excursion
    (goto-char (point-min))
    (if (re-search-forward "^[ \t]*\\<\\(package\\) *\\([^ \t\n]*\\) *;" (point-max) t)
	(concat (buffer-substring-no-properties (match-beginning 2) (match-end 2))
		"."))))

(defun csde-db-get-class () "Lookups and return fully qualified class
name, e.g. A$B if point is in inner class B of A."
  (interactive)
  (let ((class-info (csde-parse-get-innermost-class-at-point)))
    (if class-info
      (save-excursion
	(goto-char (cdr class-info))
	(let ((parent (csde-db-get-class)))
	(if (not parent)
	    (car class-info)
	    (concat parent "$" (car class-info))))))))


;;;###autoload		   
(defun csde-db ()
  "Run jdb on Csharp application whose source resides in the current buffer.
This command determines the main class of the application either from
the variable `csde-run-application-class' or from the source in the current 
buffer. If `csde-run-application-class' does not specify a class, the main class
is assumed to be the class defined by the current source buffer. This command
creates a command buffer named *gud-CLASS* for the debug session.
CLASS is the name of the class you are debugging."
  (interactive)

  ;; test for XEmacs
  (if (string-match "XEmacs\\|Lucid" emacs-version)
      (gud-overload-functions
       '((gud-marker-filter . csde-db-marker-filter)
	 (gud-find-file . csde-db-find-file)
	 )))

  (let ((app-class csde-run-application-class))
    (if (or
	 (not app-class)
	 (string= app-class ""))
	(setq app-class
	      (if (buffer-file-name)
	      (concat (csde-db-get-package)
		      (file-name-sans-extension 
			   (file-name-nondirectory (buffer-file-name))))
		(read-string "Csharp class to debug: "))))
    (csde-db-init app-class 'csde-db-marker-filter 'csde-db-find-file))

  (csde-db-def gud-break  "stop at %c:%l"  "\C-b" "Set breakpoint at current line.")
  (csde-db-def gud-remove "clear %c:%l"    "\C-d" "Remove breakpoint at current line")
  (csde-db-def gud-step   "step"           "\C-s" "Step one source line with display.")
  (csde-db-def gud-next   "next"           "\C-n" "Step one line (skip functions).")
  (csde-db-def gud-cont   "cont"           "\C-r" "Continue with display.")
  (csde-db-def gud-up     "up %p"          "<"    "Up N stack frames (numeric arg).")
  (csde-db-def gud-down   "down %p"        ">"    "Down N stack frames (numeric arg).")
  (csde-db-def gud-where  "where"          "\C-w" "Display location on the stack.")
  (csde-db-def gud-print  "print %e"       "\C-p" "Print object.")
  ;; (local-set-key [menu-bar debug up]   '("Up Stack" . gud-up))
  ;; (local-set-key [menu-bar debug down] '("Down Stack" . gud-down))
  ;; (local-set-key [menu-bar debug where] '("Where" . gud-where))
  (make-local-variable 'comint-prompt-regexp)
  (setq comint-prompt-regexp "\\(^> *\\)\\|\\(^.*\\[[0-9]+\\] *\\)")
  (make-local-variable 'paragraph-start)
  (setq paragraph-start comint-prompt-regexp)
  (run-hooks 'csde-db-mode-hook))

(defun check-source-path (path) 
  "Return a valid path or nil if path is not found."  
  ;; Ensure that path ends in a slash.
  (let ((p (if (not (string= (substring path (- (length path) 1)) "/"))
	       (concat path "/")
	     path)))
    (if (file-directory-p p)
	p
      nil)))


(defun csde-db-applet-init (applet-class marker-filter find-file applet-doc-path)
  (let* ((debug-buf-name (concat "*debug-" applet-class "*"))
	 (applet-doc (file-name-nondirectory applet-doc-path))
	 (applet-doc-directory (file-name-directory applet-doc-path))
	 (source-directory default-directory)
	 (working-directory 
	  (if applet-doc-directory
	      applet-doc-directory
	    source-directory)))
    (if (not (comint-check-proc debug-buf-name))
	(let* ((debug-buffer (get-buffer-create debug-buf-name))
	       (program "appletviewer")
	       (prog-args (append
			   (csde-get-appletviewer-options)
			   (list "-debug" applet-doc)))
	       (command-string (concat
				program " "
				(csde-run-make-arg-string prog-args) "\n\n")))
	  (save-excursion
	    (set-buffer debug-buffer)
	    (erase-buffer)
	    (cd working-directory)
	    (insert (concat "cd " working-directory "\n"))
	    (insert command-string)
	    (comint-mode))
	  (comint-exec debug-buffer applet-class program nil prog-args)
	  (pop-to-buffer debug-buffer)
	  (cd source-directory)
	  (gud-mode)
	  (make-local-variable 'gud-marker-filter)
	  (setq gud-marker-filter marker-filter)
	  (make-local-variable 'gud-find-file)
	  (setq gud-find-file find-file)
	  (set-process-filter (get-buffer-process (current-buffer)) 'gud-filter)
	  (set-process-sentinel (get-buffer-process (current-buffer)) 'gud-sentinel)
	  (gud-set-buffer)
	  (if csde-db-startup-commands
	    (mapc 'gud-basic-call csde-db-startup-commands)
	   (when csde-db-set-initial-breakpoint
	    (gud-basic-call (concat "stop in " applet-class ".init"))
	    (gud-basic-call "run")))
	  )
      (message "An instance of %s is running." applet-class)			
      (pop-to-buffer debug-buf-name))))			   
	  
   
(defun csde-db-applet-internal (applet-doc)
  ;; test for XEmacs
  (if (string-match "XEmacs\\|Lucid" emacs-version)
      (gud-overload-functions
       '((gud-marker-filter . csde-db-marker-filter)
	 (gud-find-file . csde-db-find-file)
	 )))

  (let ((applet-class csde-run-application-class))
    (if (or
	 (not applet-class)
	 (string= applet-class ""))
	(setq applet-class
	      (concat (csde-db-get-package)
		      (file-name-sans-extension 
		       (file-name-nondirectory (buffer-file-name))))))
    (csde-db-applet-init applet-class 'csde-db-marker-filter 'csde-db-find-file applet-doc))

  (csde-db-def gud-break  "stop at %c:%l"  "\C-b" "Set breakpoint at current line.")
  (csde-db-def gud-remove "clear %c:%l"    "\C-d" "Remove breakpoint at current line")
  (csde-db-def gud-step   "step"           "\C-s" "Step one source line with display.")
  (csde-db-def gud-next   "next"           "\C-n" "Step one line (skip functions).")
  (csde-db-def gud-cont   "cont"           "\C-r" "Continue with display.")
  (csde-db-def gud-up     "up %p"          "<" "Up N stack frames (numeric arg).")
  (csde-db-def gud-down   "down %p"        ">" "Down N stack frames (numeric arg).")
  (csde-db-def gud-print  "print %e"       "\C-p" "Print object.")
  (local-set-key [menu-bar debug up]   '("Up Stack" . gud-up))
  (local-set-key [menu-bar debug down] '("Down Stack" . gud-down))
  (make-local-variable 'comint-prompt-regexp)
  (setq comint-prompt-regexp "\\(^> *\\)\\|\\(^.*\\[[0-9]+\\] *\\)")
  (make-local-variable 'paragraph-start)
  (setq paragraph-start comint-prompt-regexp)
  (run-hooks 'csde-db-mode-hook)
  )

;;;###autoload 
(defun csde-db-applet (&optional doc) 
  "Runs an applet in the debugger. This function prompts you to enter
the path of an html document that displays the applet. If you 
do not enter a path, this function next checks
whether `csde-run-applet-doc' specifies a document. If so, it displays
that specified document. Next, it checks whether the current directory
contains any html files. If so, it displays the first html file that
it finds. If if cannot find an html file, it signals an error.  This
function runs appletviewer in jdb to permit debugging. On startup, it
sets a breakpoint in the init method of the class specified by 
`csde-run-application-class' or in the class corresponding to the Csharp
file in the current buffer."
  (interactive
   (let ((insert-default-directory nil))
     (list (read-file-name "Applet doc: " nil nil nil csde-run-applet-last-doc))))
  (setq csde-run-applet-last-doc doc)
  (let ((applet-doc-path 
	 (if doc 
	     doc
	   (if (and csde-run-applet-doc
		    (not (string= csde-run-applet-doc "")))
	       csde-run-applet-doc
	     (car (csde-run-find-html-files))))))
    (if applet-doc-path 
	(csde-db-applet-internal applet-doc-path) 
      (signal 'error "Could not find html document to display applet."))))


(defun csde-db-menu-debug-applet ()
  (interactive)
  (csde-db-applet))

(provide 'csde-db)


;; Change History
;; $Log: csde-db.el,v $
;; Revision 1.3  2001/02/12 05:38:24  paulk
;; CSDE 2.2.7
;;
;; Revision 1.69  2001/02/03 08:44:56  paulk
;; Changed declaration of customized variables to allow path completion.
;; Now allows environment variables in csde-db-source-directories.
;;
;; Revision 1.68  2001/02/03 07:28:06  paulk
;; Now uses generalized csde-build-classpath function to build classpath argument to debugger.
;;
;; Revision 1.67  2000/12/18 05:22:45  paulk
;; *** empty log message ***
;;
;; Revision 1.66  2000/10/10 06:36:59  paulk
;; Fixed bug where selecting Other and Executable as the debugger results in the executable name being inserted twice.
;;
;; Revision 1.65  2000/10/08 12:55:39  paulk
;; *** empty log message ***
;;
;; Revision 1.64  2000/08/19 06:46:22  paulk
;; Updated to handle JDK 1.3 version of jdb. Source pointer now moves to
;; current location in current stack frame.
;;
;; Revision 1.63  2000/06/12 08:37:43  paulk
;; Now displays CSDEbug menu when running XEmacs.
;;
;; Revision 1.62  2000/05/10 05:41:32  paulk
;; The CSDEbug menu now appears or disappears when you select or deselect CSDEbug as the current debugger.
;;
;; Revision 1.61  2000/03/16 05:08:25  paulk
;; Added CSDEbug option to csde-db-debugger.
;;
;; Revision 1.60  2000/03/08 05:40:01  paulk
;; csde-db-format-command now signals an error it it cannot determine the containing class.
;;
;; Revision 1.59  2000/02/10 02:50:32  paulk
;; Replaced csde expand file name function with expand-file-name.
;;
;; Revision 1.58  2000/02/01 04:08:12  paulk
;; Modified the Jdb->Set Breakpoint command (gud-break) to set breakpoints correctly
;; in inner classes.
;;
;; Revision 1.57  2000/01/15 08:01:52  paulk
;; Reimplemented directory search functions.
;;
;; Revision 1.56  1999/11/16 05:58:17  paulk
;; Added trace method commands and skeletons for trace class and cancel
;; trace commands.
;;
;; Revision 1.55  1999/11/04 05:49:10  paulk
;; Amended csde-db-make-qualified-class-name-regexp to permit package names to begin
;; with non-word characters, e.g., underscores. Contributed by "Patrick J. McNerthney"
;; <pat@mcnerthney.com>.
;;
;; Revision 1.54  1999/09/28 04:06:59  paulk
;; Supplied missing left parentheses.
;;
;; Revision 1.53  1999/09/05 04:33:28  paulk
;; Added support for running vm in classic mode.
;;
;; Revision 1.52  1999/03/10 16:55:02  paulk
;; Fixed csde-db-find-file to return the current buffer if it cannot find a file and
;; XEmacs is the editor.
;;
;; Revision 1.51  1999/03/06 00:55:38  paulk
;; Changed default value of csde-db-source-directories to be nil.
;;
;; Revision 1.50  1999/03/06 00:44:08  paulk
;; Make sure that case-sensitive matching is used when extracting package names from
;; debugger breakpoint messages.
;;
;; Revision 1.49  1999/02/26 15:52:52  paulk
;; Catch non-existent directory errors when searching for source
;; files and packages. Thanks to Thanh Nguyen <Thanh.Nguyen@Eng.Sun.COM>
;; for finding and providing a fix for this bug.
;;
;; Revision 1.48  1999/02/25 15:24:43  paulk
;; Fixed csde-db-find-file so that it displays an error when it cannot find a file instead of
;; opening an empty source buffer.
;;
;; Provided a set-value function for csde-db-source-directories that appends a slash to
;; the end of each path if the path does not already end in a slash.
;;
;; Defined a new command, csde-find-class-source, that finds and opens the source file
;; for a specified class.
;;
;; Improved the regular expression used by csde-db-get-package to ignore tabs at the
;; beginning of a line.
;;
;; Revision 1.47  1999/02/15 02:02:35  paulk
;; Forgot to concatenate in last fix.
;;
;; Revision 1.46  1999/02/15 00:52:44  paulk
;; Fixed bug in qualified-class-name-regexp.
;;
;; Revision 1.45  1999/02/10 18:35:51  paulk
;; Added support for appletviewer -encoding and -J options.
;;
;; Revision 1.44  1999/02/08 17:18:17  paulk
;; csde-db-applet now supports file completion and remembers the last path entered.
;;
;; Revision 1.43  1999/02/06 03:55:11  paulk
;; Fixed bug and generalized regular expression in csde-db-make-qualified-class-name-regexp.
;;
;; Revision 1.42  1999/02/03 18:12:03  paulk
;; Fixed regular expression in csde-db-get-package to eliminate spurious hits, e.g.
;; commented out package statements. Thanks to Frederic Baumann <baumann@ilog.fr>
;; for reporting this bug.
;;
;; Revision 1.41  1999/02/03 17:48:34  paulk
;; Patched csde-db-get-app-args-from-user to remember arguments.
;; Thanks to Brian Burton <brian@burton-computer.com>
;;
;; Revision 1.40  1999/02/03 17:41:56  paulk
;; Fixed csde-db-make-qualified-class-name-regexp to handle packages with underscores.
;; Thanks to Brian Burton <brian@burton-computer.com>.
;;
;; Revision 1.39  1999/02/03 17:26:46  paulk
;; Changed csde-db-make-qualified-class-name-regexp to handle inner classes.
;; Thanks to Michael Lepore <lepore@process.com> for this fix.
;;
;; Revision 1.38  1999/02/03 01:53:49  paulk
;; Fixed csde-db-applet to check the current directory for the html file to run.
;;
;; Revision 1.37  1999/02/02 16:06:01  paulk
;; Added the csde-db-applet command. This command allows you to debug an applet, using
;; appletviewer.
;;
;; Revision 1.36  1999/02/02 15:25:28  paulk
;; Removed unwanted space in -D (properties) debug option.
;;
;; Revision 1.35  1999/01/17 00:36:43  paulk
;; Now uses gud-find-c-expr or find-c-expr, whichever is bound.
;;
;; Revision 1.34  1999/01/13 22:18:08  paulk
;; Added Andy Piper's NT/XEmacs 21 compatibility changes.
;; Changed find-c-expr to gud-findc-expr
;;
;; Revision 1.33  1998/11/22 18:18:36  paulk
;; Made comint-prompt-regexp and  paragraph-start local variables.
;;
;; Revision 1.32  1998/11/04 02:59:09  paulk
;; Corrected verbiage in Csde Debugger Options description.
;;
;; Revision 1.31  1998/09/12 00:05:57  paulk
;; Debugger now runs application from directory specified by csde-run-working-directory.
;;
;; Revision 1.30  1998/06/30 04:03:19  paulk
;; Added variables `csde-db-read-vm-args' and `csde-db-read-app-args'. The use of
;; these variables is the same as the corresponding csde-run variables.
;;
;; Revision 1.29  1998/06/29 02:50:44  paulk
;; Fixed bug in marker filter.
;;
;; Revision 1.28  1998/06/27 03:34:31  paulk
;; Provided a hack to handle reordering of threaded messages on Solaris.
;;
;; Provided code to handle case where current class has no line number
;; information.
;;
;; Revision 1.27  1998/06/25 04:27:23  paulk
;; Removed debug messages from csde-db-marker-filter.
;;
;; Revision 1.26  1998/06/25 04:21:10  paulk
;; Modified csde-db-marker-filter to accummulate debugger output
;; in chunks. Fixes bug reported by Eric Prud'hommeaux (eric@w3.org).
;;
;; Revision 1.25  1998/06/22 03:52:28  paulk
;; Added csde-db-startup-commands variable. This variable allows you to
;; specify debugger commands to run when the debugger is started.
;;
;; Revision 1.24  1998/06/21 00:09:43  paulk
;; Added a customizable feature, csde-db-set-initial-breakpoint, that causes
;; the CSDE to set an initial breakpoint in an app's main routine and run
;; to the breakpoint on debugger startup. The feature is enabled by default.
;;
;; Revision 1.23  1998/06/20 23:42:07  paulk
;; Made csde-db-marker-regexp a custom variable to facilitate the use of the CSDE
;; with debuggers other than jdb.
;;
;; Changed the marker regular expression to detect only jdb breakpoint messages,
;; i.e., messages of the form
;;
;;   Breakpoint hit: qualified.class.name (class:line)
;;
;; This should eliminate the problem of spurious hits when exceptions occur and
;; stack traces are printed.
;;
;; Revision 1.22  1998/05/27 06:09:46  paulk
;; Added autoload comments.
;;
;; Revision 1.21  1998/03/27 04:16:12  kinnucan
;; Fixed typo in the code that displays the jdb menu on XEmacs.
;;
;; Revision 1.20  1998/03/27 04:14:53  kinnucan
;; Modified csde-db-search-src-dirs to take current package as an
;; argument rather than use a global variable. This allows
;; it to be used by csde-csharp-build function.
;;
;; Revision 1.19  1998/03/18 03:54:06  kinnucan
;; Changed csde-db-marker-regexp to account for inner classes.
;; Thanks to Andreas Rasmusson <Andreas.Rasmusson@sics.se> for
;; providing this fix.
;;
;; Revision 1.18  1998/03/04 04:28:36  kinnucan
;; Added test for csde-run-application-class = "" to csde-db
;;
;; Revision 1.17  1998/02/27 22:16:34  kinnucan
;; Changed copyright to Paul Kinnucan.
;; Have not yet assigned rights to FSF.
;;
;; Revision 1.16  1998/02/27 22:15:24  kinnucan
;; Added support for Emacs customization feature.
;;
;; Revision 1.15  1998/02/17 04:16:38  kinnucan
;; Fixed bug in csde-deb-set-source-paths that caused the last
;; directory to not be normalized (i.e., slash appended).
;;
;; Revision 1.14  1998/02/12 05:15:38  kinnucan
;; Changed the csde-db-search-src-dirs to search the source directory list from
;; front to back instead of back to front. The former search order did not allow newer versions of the same class to shadow older versions. Thanks to "David J. Biesack" <sasdjb@unx.sas.com> for supplying this fix.
;;
;; Revision 1.13  1998/02/12 04:57:13  kinnucan
;; Fixed bug in csde-db-marker-filter that sometimes prevented the CSDE from
;; loading the correct source file. Thanks to David J. Biesack
;; <sasdjb@unx.sas.com> for supplying the fix.
;;
;; Revision 1.12  1997/10/30 05:42:37  kinnucan
;; Made configuration variables settable.
;;
;; Revision 1.11  1997/10/26 05:49:59  kinnucan
;; Applied Derek Young's patch to cause csde to qualify class names
;; when setting a breakpoint.
;;
;; Revision 1.10  1997/10/20 05:27:48  kinnucan
;; Removed reference to deleted function csde-db-massage-args
;;
;; Revision 1.9  1997/10/11 01:36:05  kinnucan
;; Fixed bug in csde-db-search-src-dirs discovered by Jonathan Payne.
;;
;; Revision 1.8  1997/10/06 14:40:53  kinnucan
;; Fixed bugs in csde-db-set-debugger command.
;;
;; Revision 1.7  1997/10/05 21:20:15  kinnucan
;; 1. Added the variables csde-db-debugger and csde-db-debugger-is-executable
;;    and the associated setter function csde-db-set-debugger. These allow
;;    you to specify a custom debugger for the CSDE>
;;
;; 2. Added csde-db-args and csde-db-app-args and the associated setter
;;    functions. These allow you to specify debugger and application
;;    command line arguments.
;;
;; Revision 1.6  1997/10/05 04:53:04  kinnucan
;; Fixed bug in print object menu item.
;;
;; Revision 1.5  1997/08/26 14:53:39  paulk
;; Fixed bug in check-source-path.
;;
;; Revision 1.4  1997/08/26 08:52:14  kinnucan
;; Tweaked CSDE Version number for CSDE 1.8 release.
;;
;; Revision 1.3  1997/07/05 04:18:10  kinnucan
;; Updated make-jdb-command to run either the class previously specifed with
;; the csde-run-set-app command or the class corresponding to the code in the
;; current buffer.
;;
;; Revision 1.2  1997/06/18 18:45:11  paulk
;; Added error-checking to csde-db-set-source-paths function. Now checks for
;; existence of specified directories and appends a terminal slash to paths
;; that lack it.
;;
;; Revision 1.1  1997/06/18 17:21:59  paulk
;; Initial revision
;;

;;; end of csde-db.el
