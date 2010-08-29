;;; csde-compile.el -- Integrated Development Environment for Csharp.
;; $Revision: 1.4 $ $Date: 2001/02/20 05:29:19 $ 

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

;;; Commentary:

;; The latest version of the CSDE is available at
;; <URL:http://www.sourceforge.com/>.

;; Please send any comments, bugs, or upgrade requests to
;; Matt Bruce (matt.bruce@morganstanley.com)

;;; Code:


(defgroup csde-compile-options nil
  "CSDE Compiler Options"
  :group 'csde
  :prefix "csde-compile-option-")

(defcustom csde-compile-option-command-line-args ""
  "*Specify options as a string of command-line arguments.
The value of this variable should be a string of switches understood
by the compiler, for example, -depend -g. This variable is intended to
be used to set compile options not otherwise defined by the CSDE, in
particular, options not defined by csharpc but used by another compiler
that you might want to use with the CSDE."
  :group 'csde-compile-options
  :type 'string)

(defcustom csde-compile-option-classpath nil
"*Specify paths of classes required to compile this project.
The CSDE uses the specified paths to construct a -classpath
argument to pass to the compiler. This option overrides the
`csde-global-classpath' option."
  :group 'csde-compile-options
  :type '(repeat (file :tag "Path")))

(defcustom csde-compile-option-sourcepath nil
"*Specify the source code path to search for class or interface definitions.

As with the user class path, source path entries  can be directories, JAR 
archives, or ZIP archives. If packages are used, the local path name within 
the directory or archive must reflect the package name. 

Note that classes found through the classpath are subject to automatic 
recompilation if their sources are found."
  :group 'csde-compile-options
  :type '(repeat (file :tag "Path")))

(defcustom csde-compile-option-directory ""
  "*Specifies the root directory of the class file hierarchy.
The compiler places compiled classes in the specified
directory. For example, specifying the class
directory as: 
  
  C:\\users\\dac\\classes

causes the class files for the classes in the MyProgram.csharp source
file to be saved in the directory C:\\users\\dac\\classes. If your class 
is in the package demos\\awt, the class files would be placed in directory
C:\\users\\dac\\classes\\demos\\awt."
  :group 'csde-compile-options
  :type 'file)

(defcustom csde-compile-option-deprecation nil
  "*Warn use or override of a deprecated member or class. 
A member or class is deprecated if its documentation comment contains
the @deprecated tag. The compiler will emit a warning at the end of
compilation whether or not the deprecation option is on; this option
causes the location of each individual use or override to be noted.

Deprecated members or classes are deliberately not mentioned if the
source file containing the deprecation is being recompiled.  This can
happen because the file is on the command line or because the depend
option is on and the source file is out of date.
"
  :group 'csde-compile-options
  :type 'boolean)


(defcustom csde-compile-option-debug 
  (list "selected" (list t nil nil))
  "*Include debug information in classes.
The compiler includes line number information by default.

Before JDK 1.2, the the debug and optimize options were
mutually exclusive. In JDK 1.2, it is possible to combine debug and
optimize, but the shortcuts taken by optimized code may occasionally
produce surprising debugging results. For example, declared variables
may not exist and code may appear to move or not be executed at all."
  :group 'csde-compile-options
  :type '(list 
	  (radio-button-choice 
	   :format "%t \n%v"
	   :tag "Debug info to include in class:"
	   (const "all")
	   (const "none")
	   (const "selected"))
	  (list
	   :tag "    info"
	   :indent 4
	   (checkbox :format "%[%v%] %t \n"
		     :tag "Line Numbers")
	   (checkbox :format "%[%v%] %t \n"
		     :tag "Variables")
	   (checkbox :format "%[%v%] %t \n"
		     :tag "Source")))
	   
)


(defcustom csde-compile-option-optimize nil
"*Directs the compiler to try to generate faster code. 
This may slow down compilation, make larger class files, and/or make
it difficult to debug.

Prior to 1.2, the optimize option tried to inline methods across
classes. This created compatibility problems and sometimes generated
illegal bytecode. The optimize option also implicitly turned on the
depend option and implicitly turned off the debug option.

In JDK 1.2, the optimize option no longer inlines across classes and
so may safely be used for any csharp compilation. Optimize no longer
implicitly turns on depend or implicitly turns off debug."
  :group 'csde-compile-options
  :type 'boolean)


(defcustom csde-compile-option-depend nil
"*Analyze dependencies.
Causes recompilation of class files on which the source files given as
command line arguments recursively depend. Without this option, only
files that are directly depended on and missing or out-of-date will be
recompiled. Recompilation does not extend to missing or out-of-date
files only depended on by already up-to-date class files.

Note: if you are using a compiler other than post JDK 1.1.6 versions
of csharpc, you may need to specify the command-line switch used by
the compiler to specify dependency checking. See 
`csde-compile-option-depend-switch' for more information."
  :group 'csde-compile-options
  :type 'boolean)

(defcustom csde-compile-option-depend-switch (list "-Xdepend")
"*Specify command line switch for depend option.
This option is necessary because the command-line switch for
dependency checking differs among Csharp compilers. Choose
from the following options:

  -Xdepend  Full dependency checking (post JDK 1.1.6)
  -depend   Full dependency checking (jikes and pre-JDK 1.1.6)
  +F        Check everything except jar and zip files (jikes only)
  +U        Check everything including jar and zip files (jikes only)"
  :group 'csde-compile-options
  :type '(list 
	  (radio-button-choice 
	   :format "%t \n%v"
	   :tag "Select -Xdepend (csharpc) or -depend (jikes):"
	   (const "-Xdepend")
	   (const "-depend")
	   (const "+F")
	   (const "+U"))))

(defcustom csde-compile-option-vm-args nil
"*Specify command-line arguments for Csharp interpreter.
Passes the specified arguments to the Csharp interpreter that runs the
compiler. The argument should not contain spaces. This is useful for
adjusting the compiler's execution environment or memory usage."
  :group 'csde-compile-options
  :type '(repeat (string :tag "Option")))

(defcustom csde-compile-option-verbose nil
"*Print verbose messages.
Causes the compiler and linker to print out messages about what source
files are being compiled and what class files are being loaded."
  :group 'csde-compile-options
  :type 'boolean)

(defcustom csde-compile-option-nowarn nil
"*Turn off warnings.
If this option is specified, the compiler does not print out any
warnings."
  :group 'csde-compile-options
  :type 'boolean)

(defcustom csde-compile-option-encoding nil
"*Specify the source file encoding name, such as EUCJIS\\SJIS.
If this option is not specified, then the platform default converter
is used."
  :group 'csde-compile-options
  :type 'boolean)

;;(makunbound 'csde-compile-option-target)
(defcustom csde-compile-option-target (list "1.1")
"*Generate class files that will work on VMs with the specified version.
 
The default is to generate class files to be compatible with both
1.1 and 1.2 VMs. The versions supported by csharpc in JDK1.2 are: 

  1.1     Ensure that generated class files will be compatible 
          with 1.1 and 1.2 VMs. This is the default.
  
  1.2     Generate class files that will run on 1.2 VMs, but 
          not on 1.1 VMs.

  1.3     Generate class files that will run on VMs in the 
          Csharp 2 SDK, v 1.3 and later, but will not run 
          on 1.1 or 1.2 VMs

By default, classes are compiled against the bootstrap and extension classes
of the JDK that csharpc shipped with. But csharpc also supports cross-compiling, 
where classes are compiled against a bootstrap and extension classes of a 
different Csharp platform implementation. It is important to use 
`csde-compile-option-bootclasspath' and `csde-compile-option-extdirs' when 
cross-compiling."
  :group 'csde-compile-options
  :type '(list
	  (radio-button-choice 
	   :format "%t \n%v"
	   :tag "Target VM:"
	   (const "1.1")
	   (const "1.2")
	   (const "1.3"))))

(defcustom csde-compile-option-bootclasspath nil
"*Cross-compile against the specified set of boot classes.
As with the user class path, boot class path entries can be 
directories, JAR archives, or ZIP archives."
  :group 'csde-compile-options
  :type '(repeat (file :tag "Path")))

(defcustom csde-compile-option-extdirs nil
"*Cross-compile against the specified extension directories. 
Each JAR archive in the specified directories is searched for class files."
  :group 'csde-compile-options
  :type '(repeat (file :tag "Path")))

;;(makunbound 'csde-compile-option-verbose-path)
(defcustom csde-compile-option-verbose-path nil
"*Describe how paths and standard extensions were searched to find
source and class files.

   ***NOTE***

   This option is supported only by the versions of csharpc shipped
   with JDK 1.1.x and 1.2.x and oldcsharpc in JDK 1.3."

  :group 'csde-compile-options
  :type 'boolean)

;; Cygwin style paths will not be understood by the csharp tools.  Call
;; csde-convert-cygwin-path before passing path information to a
;; csharp tool.
(defun csde-build-path-arg (path-type path-list quote)
"Build a path argument from a list of paths."
  (let ((path 
	 (mapconcat (lambda (x) x) path-list csde-classpath-separator)))
    ;; CFH - Call csde-convert-cygwin-path to handle cygwin paths
    (setq path (csde-convert-cygwin-path path csde-classpath-separator))
    (if quote
        (setq path (concat "\"" path "\"")))
    (setq path (concat path-type " " path))))


(defun csde-build-classpath-arg (path-list quote)
"Build a classpath from a list of paths."
 (csde-build-path-arg "-classpath" path-list quote))

(defun csde-get-compile-options ()
"Constructs a command-line argument string for compiler.
The string consists of the contents of the csde-compile-options
variable concatenated with the various csde-compile-option
settings.
"
  (let (options)

    (if csde-compile-option-classpath
	(setq options 
	      (csde-build-classpath-arg
	       csde-compile-option-classpath csde-quote-classpath))
      (if csde-global-classpath
	  (setq options
		(csde-build-classpath-arg
		 csde-global-classpath csde-quote-classpath))))

    (if csde-compile-option-sourcepath
	(setq options 
	      (concat options " "
	      (csde-build-path-arg
	       "-sourcepath"
	       csde-compile-option-sourcepath 
	       csde-quote-classpath))))

    (if csde-compile-option-bootclasspath
	(setq options 
	      (concat options " "
	      (csde-build-path-arg
	       "-bootclasspath"
	       csde-compile-option-bootclasspath 
	       csde-quote-classpath))))

    (if csde-compile-option-extdirs
	(setq options 
	      (concat options " "
	      (csde-build-path-arg
	       "-extdirs"
	       csde-compile-option-extdirs
	       csde-quote-classpath))))

    ;; Debug option.
    (let* ((include-option (nth 0 csde-compile-option-debug))
	   (selected (nth 1 csde-compile-option-debug))
	   (lines (nth 0 selected))
	   (vars (nth 1 selected))
	   (src (nth 2 selected)))
      (cond
       ((and
	 (string= include-option "selected")
	 lines
	 (not vars)
	 (not src)))
       ((string= include-option "all")
	(setq options (concat options " -g")))
       ((string= include-option "none")
	(setq options (concat options " -g:none")))
       ((and
	 (string= include-option "selected")
	 (or lines vars src))
	(setq options 
	      (concat options 
		      " -g:"
		      (if lines
			  (if (or vars src) "lines,"
			    "lines"))
		      (if vars
			  (if vars
			      (if src "vars," "vars")))
		      (if src "source"))))))      

    (if (not (string= csde-compile-option-directory ""))
	(setq options
	      (concat options 
		" -d "
		(csde-normalize-path csde-compile-option-directory))))

    (if csde-compile-option-deprecation
	(setq options (concat options " -deprecation")))

    (if csde-compile-option-optimize
	(setq options (concat options " -O")))

    (if csde-compile-option-depend
	(setq options 
	      (concat options " " (car csde-compile-option-depend-switch))))

    (if csde-compile-option-vm-args
	(setq options
	      (concat 
	       options	      
	       (mapconcat
		(lambda (arg)
		  (concat " -J" arg))
		csde-compile-option-vm-args
		""))))

    (if csde-compile-option-verbose
	(setq options (concat options " -verbose")))

    (if csde-compile-option-verbose-path
	(setq options (concat options " -Xverbosepath")))

    (if csde-compile-option-nowarn
	(setq options (concat options " -nowarn")))

    (if (not (string= csde-compile-option-command-line-args ""))
	(setq options (concat options " " 
			      csde-compile-option-command-line-args)))

    (let ((target (car csde-compile-option-target)))
      (if (not (string= target "1.1"))
	  (setq options (concat options " -target " target))))
	
    options))

;;;###autoload
(defun csde-set-compile-options (options)
  "Sets the compile options.
Enter the options as you would on the command line, e.g.,
-depend -verbose."
  (interactive
   "sEnter options: ")
  (setq csde-compile-option-command-line-args options))


;;;###autoload
(defun csde-compile ()
  "Compile the Csharp program in the current buffer.
This command invokes the compiler specified by `csde-compiler'
with the options specified by the CSDE customization variables
that begin with `csde-compile'. If the variable
`csde-read-compile-args' is non-nil, this command reads
additional compilation options from the minibuffer, with
history enabled."
  (interactive)

  (if csde-read-compile-args
      (setq csde-interactive-compile-args
	      (read-from-minibuffer 
	       "Compile args: "
	       csde-interactive-compile-args
	       nil nil
	       '(csde-interactive-compile-arg-history . 1))))

  (let ((compile-command
	 (csde-make-compile-command 
	  csde-interactive-compile-args)))  

    ;; Force save-some-buffers to use the minibuffer
    ;; to query user about whether to save modified buffers.
    ;; Otherwise, when user invokes csde-compile from
    ;; menu, save-some-buffers tries to popup a menu
    ;; which seems not to be supported--at least on
    ;; the PC.
    (if (and (eq system-type 'windows-nt)
	     (not csde-xemacsp))	
	(let ((temp last-nonmenu-event))
	  ;; The next line makes emacs think that csde-compile
	  ;; was invoked from the minibuffer, even when it
	  ;; is actually invoked from the menu-bar.
	  (setq last-nonmenu-event t)
	  (save-some-buffers (not compilation-ask-about-save) nil)
	  (setq last-nonmenu-event temp))
      (save-some-buffers (not compilation-ask-about-save) nil))

    (setq compilation-finish-function 
      (lambda (buf msg) 
	(run-hook-with-args 'csde-compile-finish-hook buf msg)
	(setq compilation-finish-function nil)))

    (compile-internal compile-command "No more errors")))

(provide 'csde-compile)

;; Change History
;; $Log: csde-compile.el,v $
;; Revision 1.4  2001/02/20 05:29:19  paulk
;; CSDE 2.27 updates
;;
;; Revision 1.10  2001/02/20 05:15:10  paulk
;; You can now use environment variables, tilde notation, and cygwin syntax in csde-compile-option-directory path.
;;
;; Revision 1.9  2001/02/17 17:43:34  paulk
;; Added support for JDK 1.3 targets.
;;
;; Revision 1.8  2001/02/03 08:18:44  paulk
;; Changed declarations of customized variables so that you can now use completion (M tab) to complete path names.
;;
;; Revision 1.7  2000/09/21 02:05:11  paulk
;; Fixes bug in formatting csde-compile-option-vm-args for the command line.
;;
;; Revision 1.6  2000/08/19 07:04:28  paulk
;; Adds compile finish hook.
;;
;; Revision 1.5  2000/08/11 05:04:45  paulk
;; Added csde-compile-finish-hook variable.
;;
;; Revision 1.4  2000/04/10 05:27:30  paulk
;; Compile command now supports Cygwin-style class paths.
;;
;; Revision 1.3  1999/01/15 22:04:15  paulk
;; Added Andy Piper's NT/XEmacs compatibility changes.
;;
;; Revision 1.2  1998/12/07 01:35:28  paulk
;; Updated compile options to reflect changes in command-line options
;; accepted by csharpc.
;;
;; Revision 1.1  1998/12/06 02:37:54  paulk
;; Initial revision
;;

;; End of csde-compile.el
