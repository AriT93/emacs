;;; csde.el -- Integrated Development Environment for Csharp.
;; $Revision: 1.5 $ $Date: 2001/02/25 05:10:13 $ 

;; Adapted from the JDE by Matt Bruce <matt.bruce@morganstanley.com>

;; Copyright (C) 2001 by Matt Bruce
;; Maintainer:  Matt Bruce

;; JDE Author: Paul Kinnucan <paulk@mathworks.com>
;; KDE Maintainer: Paul Kinnucan

;; Keywords: csharp, tools

;; JDE Copyright (C) 1997, 1998, 1999, 2000, 2001 Paul Kinnucan.

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


;;;###autoload
(defconst csde-version "0.0.1"
  "CSDE version number.")

(defconst csde-xemacsp (string-match "XEmacs" (emacs-version))
  "Non-nil if we are running in the XEmacs environment.")

(defconst csde-xemacs20p (and csde-xemacsp (>= emacs-major-version 20)))

;; (eval-when (eval load compile)		;-- sgr 5-Sept-2000
(eval-when-compile
  (defconst csde-xemacsp (string-match "XEmacs" (emacs-version))
    "Non-nil if we are running in the XEmacs environment.")

  (defconst csde-xemacs20p (and csde-xemacsp (>= emacs-major-version 20))))


(require 'easymenu)
(require 'cl)
(require 'font-lock)
(require 'cc-mode)
(require 'cus-edit)
(require 'csde-compile)
(require 'csde-db)
(require 'csde-run)
(require 'csde-make)
(require 'csde-gen)
(require 'compile)
(require 'imenu)
(require 'speedbar)
(require 'browse-url)
(require 'beanshell)
(require 'csde-wiz)
(require 'csde-parse)
(require 'csde-help)
(require 'csde-bug)
(require 'csde-complete)
(require 'csde-csharpdoc)
(require 'csde-csharpdoc-gen)
(require 'csde-stat)
(require 'csde-which-method)
(require 'csde-csharp-font-lock)
(require 'csde-import)
(require 'senator)
(require 'csde-package)
;; (require 'csde-project)

;; From custom web page for compatibility between versions of custom:
(eval-and-compile
  (condition-case ()
      (require 'custom)
    (error nil))
  (if (and (featurep 'custom) (fboundp 'custom-declare-variable))
      nil ;; We've got what we needed
    ;; We have the old custom-library, hack around it!
    (defmacro defgroup (&rest args)
      nil)
    (defmacro defface (var values doc &rest args)
      (` (progn
	   (defvar (, var) (quote (, var)))
	   ;; To make colors for your faces you need to set your .Xdefaults
	   ;; or set them up ahead of time in your .emacs file.
	   (make-face (, var))
	   )))
    (defmacro defcustom (var value doc &rest args)
      (` (defvar (, var) (, value) (, doc))))))

(if (not (fboundp 'custom-set-default))
    (defalias 'custom-set-default 'set-default))

(defgroup csde nil
  "Csharp Development Environment"
  :group 'tools
  :prefix "csde-")

(defgroup csde-project nil
  "CSDE Project Options"
  :group 'csde
  :prefix "csde-")


;; (makunbound 'csde-key-bindings)
(defcustom csde-key-bindings
  (list 
   (cons "[?\C-c ?\C-v ?\C-a]" 'csde-run-menu-run-applet)
   (cons "[?\C-c ?\C-v ?\C-b]" 'csde-build)
   (cons "[?\C-c ?\C-v ?\C-c]" 'csde-compile)
   (cons "[?\C-c ?\C-v ?\C-d]" 'csde-debug)
   (cons "[?\C-c ?\C-v ?\C-f]" 'csde-wiz-implement-interface)
   (cons "[?\C-c ?\C-v ?j]"     'csde-csharpdoc-generate-csharpdoc-template)
   (cons "[?\C-c ?\C-v ?\C-k]" 'bsh)
   (cons "[?\C-c ?\C-v ?\C-l]" 'csde-gen-println)
   (cons "[?\C-c ?\C-v ?\C-n]" 'csde-browse-jdk-doc)
   (cons "[?\C-c ?\C-v ?\C-p]" 'csde-save-project)
   (cons "[?\C-c ?\C-v ?\C-q]" 'csde-wiz-update-class-list)
   (cons "[?\C-c ?\C-v ?\C-r]" 'csde-run)
   (cons "[?\C-c ?\C-v ?\C-s]" 'speedbar-frame-mode)
   (cons "[?\C-c ?\C-v ?\C-t]" 'csde-db-menu-debug-applet)
   (cons "[?\C-c ?\C-v ?\C-w]" 'csde-help-symbol)
   (cons "[?\C-c ?\C-v ?\C-y]" 'csde-show-class-source)
   (cons "[?\C-c ?\C-v ?\C-z]" 'csde-import-find-and-import)
   (cons "[(control c) (control v) (control ?.)]" 'csde-complete-at-point-menu)
   (cons "[(control c) (control v) ?.]" 'csde-complete-at-point)
   )
  "*Specifies key bindings for the CSDE.
The value of this variable is an association list. The car of
each element specifies a key sequence. The cdr specifies 
an interactive command that the key sequence executes. To enter
a key with a modifier, type C-q followed by the desired modified
keystroke. For example, to enter C-s (Control s) as the key to be
bound, type C-q C-s in the key field in the customization buffer.
You can use the notation [f1], [f2], etc., to specify function keys."
  :group 'csde-project
  :type '(repeat
	  (cons :tag "Key binding"
		(string :tag "Key")
		(function :tag "Command")))
  :set '(lambda (sym val)
	  (mapc
	   (lambda (buf)
	     (save-excursion
	       (set-buffer buf)
	       (when (boundp 'csde-mode-map)
		 ;; Unmap existing key bindings
		 (if (and (boundp 'csde-key-bindings)
			  csde-key-bindings)
		     (mapc 
		      (lambda (binding)
			(let ((key (car binding)))
			  (if (string-match "\\[.+]"key)
			      (setq key (car (read-from-string key))))
			  (local-unset-key key)))
		      csde-key-bindings))
		 ;; Map new key bindings.
		 (mapc 
		  (lambda (binding)
		    (let ((key (car binding))
			  (fcn (cdr binding)))
		      (if (string-match "\\[.+]" key)
			  (setq key (car (read-from-string key))))
		      (define-key (current-local-map) key fcn)))
		  val))))
	   (csde-get-csharp-source-buffers))
	  (set-default sym val)))

(defcustom csde-project-context-switching-enabled-p t
  "*Enable project context switching.
If non-nil, the CSDE reloads a buffer's project file when you switch to the buffer from
another buffer belonging to another project. You can disable this feature if you prefer
to load project files manually. The debugger uses this variable to disable context-switching
temporarily when stepping through code."
  :group 'csde-project
  :type 'boolean
)

;; Used by debugger to disable context-switching temporarily while 
;; stepping through code.
(setq csde-project-cs-enabled-p t)

;;(makunbound 'csde-jdk-doc-url)
(defcustom csde-jdk-doc-url "http://www.csharpsoft.com/j2se/1.3/docs/index.html"
  "*URL of JDK documentation. 
This can point to a remote or local copy of the documentation. By
default, this variable points to the copy stored at CsharpSoft's
website."
  :group 'csde-project
  :type 'file)

;;(makunbound 'csde-global-classpath)
(defcustom csde-global-classpath nil
  "Specify a common classpath for compile, run, and debug commands.
Use this variable if you want to the CSDE to use the same classpath for
compiling, running,and debugging an application. Note that the value
of this variable is a list of strings, each of which specifies a
path. The CSDE converts this list to a colon- or semicolon-separated
list before inserting in the compiler or vm command line. The paths
may start with a tilde (~) and may include environment variables. The
CSDE replaces the ~ with your home directory and replaces each instance
of an environment variable with its value before inserting it into
a command line.

You cans specify different classpaths for compiling, running and
debugging applicaitons. Use `csde-compile-option-classpath' to specify
the compilation classpath, `csde-run-option-classpath' to specify the
run classpath, and/or `csde-db-option-classpath' to specify the debug
classpath. You can use these variables together. For example, suppose
that you need to use one classpath for compilation and other for
running and debugging. You could do this by setting
`csde-compile-option-classpath' to the compile classpath and
`csde-global-classpath' to the run and debug classpath. If you set
`csde-global-classpath', the CSDE uses it to construct the classpath for
any operation for which you do not set the operation-specific
classpath variable (e.g., `csde-compile-option-classpath'). 

If you do not set `csde-global-classpath', the CSDE uses the operation-specific
classpath if it is set. If neither the global nor the
operation-specific classpath is set, the CSDE does not generate a
-classpath argument for the operation.

Note: do not use environment variables in the paths that you set via
csde-global-classpath."
  :group 'csde-project
  :type '(repeat (file :tag "Path")))

(defcustom csde-quote-classpath t
  "*Quote the classpath argument.
Set this option on when using the bash shell with Windows 95 or NT.
The semicolons in the classpath confuse the shell."
  :group 'csde-project
  :type 'boolean)

(defvar csde-project-name "default"
"Specifies name of project to which the current buffer belongs.")


(defcustom csde-project-file-name "prj.el"
  "*Specify name of CSDE project file.
When it loads a Csharp source file, the CSDE looks for a lisp file of
this name (the default is prj.el in the source file hierarchy. If it
finds such a file, it loads the file. You can use this file to set the
classpath, compile options, and other CSDE options on a
project-by-project basis."
  :group 'csde-project
  :type 'string)

(defcustom csde-compiler "csc"
  "*Csharp compiler.
Specifies the path to the compiler to be used to compile the source
in the current buffer. The default is the JDK compiler (csharpc)."
  :group 'csde-project
  :type 'file)

(defcustom csde-read-compile-args nil
"*Specify whether to prompt for additional compiler arguments.
If this variable is non-nil, the csde-compile command prompts
you to enter additional compiler arguments in the minibuffer.
These arguments are appended to those specified by customization
variables. The CSDE maintains a history list of arguments 
entered in the minibuffer."
  :group 'csde-project
  :type 'boolean
)

(defvar csde-interactive-compile-args ""
"String of compiler arguments entered in the minibuffer.")

(defvar csde-interactive-compile-arg-history nil
"History of compiler arguments entered in the minibuffer.")

(defcustom csde-compile-finish-hook 
  '(csde-compile-finish-refresh-speedbar csde-compile-finish-flush-completion-cache)
  "List of functions to be invoked when compilation of a 
Csharp source file terminates. Each function should accept
two arguments: the compilation buffer and a string 
describing how the compilation finished."
  :group 'csde
  :type 'hook)


(defun csde-compile-finish-flush-completion-cache (buf msg) 
  "Flush the classinfo cache at the end of compilation.
Flush the entire cache as we don't know which classes
were recompiled."
  (csde-complete-flush-classinfo-cache))


(defun csde-compile-finish-refresh-speedbar (buf msg) 
  "Refresh speedbar at the end of a compilation."
  (if (and (frame-live-p speedbar-frame)
	    (frame-visible-p speedbar-frame))
       (speedbar-refresh)))

(defcustom csde-build-use-make nil
"*If true, use make to build CSDE projects."
  :group 'csde-project
  :type 'boolean)

(defcustom csde-enable-abbrev-mode nil
"*Enable expansion of abbreviations in csde-mode.
See `csde-mode-abbreviations' for more information."
  :group 'csde-project
  :type 'boolean)

(defcustom csde-mode-abbreviations
  (list 
   (cons "ab" "abstract")
   (cons "bo" "boolean")
   (cons "br" "break")
   (cons "by" "byte")
   (cons "byv" "byvalue")
   (cons "cas" "cast")
   (cons "ca" "catch")
   (cons "ch" "char")
   (cons "cl" "class")
   (cons "co" "const")
   (cons "con" "continue")
   (cons "de" "default")
   (cons "dou" "double")
   (cons "el" "else")
   (cons "ex" "extends")
   (cons "fa" "false")
   (cons "fi" "final")
   (cons "fin" "finally")
   (cons "fl" "float")
   (cons "fo" "for")
   (cons "fu" "future")
   (cons "ge" "generic")
   (cons "go" "goto")
   (cons "impl" "implements")
   (cons "impo" "import")
   (cons "ins" "instanceof")
   (cons "in" "int")
   (cons "inte" "interface")
   (cons "lo" "long")
   (cons "na" "native")
   (cons "ne" "new")
   (cons "nu" "null")
   (cons "pa" "package")
   (cons "pri" "private")
   (cons "pro" "protected")
   (cons "pu" "public")
   (cons "re" "return")
   (cons "sh" "short")
   (cons "st" "static")
   (cons "su" "super")
   (cons "sw" "switch")
   (cons "sy" "synchronized")
   (cons "th" "this")
   (cons "thr" "throw")
   (cons "throw" "throws")
   (cons "tra" "transient")
   (cons "tr" "true")
   (cons "vo" "void")
   (cons "vol" "volatile")
   (cons "wh" "while")
   )
"*Abbreviations used for Csharp keywords.
To use these abbreviations, you must enable abbrev-mode (see
`csde-enable-abbrev-mode'). To use an abbreviation, enter the
abbreviation followed by a white-space character. To suppress
expansion, enter C-q white-space."
   :group 'csde-project
  :type '(repeat 
	  (cons :tag "csde-mode abbreviation"
		(string :tag "Abbreviation")
		(string :tag "Expansion"))))

(defvar csde-mode-abbrev-table nil
  "Abbrev table for use in CSDE-mode buffers.")


(defun csde-init-abbrev-table ()
  "Load the abbrev table with a set of abbrevs that invoke an anonymous
function that  does the expansion only if point is not in a quoted string 
or a comment."

  ;; Note the use of lexical-let - must have the common lisp packages
  ;; around, since the anonymous function needs the closure provided by
  ;; lexical-let.
  (interactive)
  (setq local-abbrev-table (make-abbrev-table))
  (mapc 
   (lambda (x)
     (lexical-let
	 ((abbrev (car x))		; this is the abbrev, lexically scoped
	  (expansion (cdr x)))		; this is the expansion
       (define-abbrev 
	 local-abbrev-table
	 abbrev
	 ""
	 (lambda ()
	   (if (csde-parse-comment-or-quoted-p)
	       (insert abbrev)		; insert the abbrev in quote/comment
	     (insert expansion)))       ; proceed with expansion elsewhere
	 0)))
   csde-mode-abbreviations)

  (if csde-gen-cflow-enable
      (csde-gen-load-cflow-abbrevs))

  (setq abbrevs-changed nil))

;; The next two functions contributed by s.nicolas@videotron.ca
(defun csde-abbrev-mode ()
"*Activates  or deactivates the abbreviation mode in CSDE
without altering the project settings.
See `csde-mode-abbreviations' for more information."
 (interactive)
  (setq csde-enable-abbrev-mode (not csde-enable-abbrev-mode))
  (setq abbrev-mode csde-enable-abbrev-mode)
  (when csde-enable-abbrev-mode
     ;; Define abbreviations.a
    (csde-init-abbrev-table))
  (if csde-enable-abbrev-mode
    (message "abbreviation mode on")
    (message "abbreviation mode off")))

(defun csde-show-abbrevs ()
"*Shows a popup menu containing all available expansions.
See `csde-mode-abbreviations' for more information."
  (interactive)
   (let* ((expansions
          (mapcar
            (lambda(x) (cons (cdr x) (car x)))
              csde-mode-abbreviations))
         (expansion (car (imenu--mouse-menu expansions (if csde-xemacsp nil
t) "Abbreviations"))))
  (insert expansion)))


;;;###autoload
(defun csde-set-compiler (compiler)
  "Specify the pathname of the compiler to be used to compile the
current buffer. Default is csharpc."
  (interactive
   "sEnter compiler (csharpc): ")
   (if (string= compiler "")
       (setq csde-compiler "csharpc")
     (setq csde-compiler compiler)))

(defvar csde-classpath-separator (if (eq system-type 'cygwin32) 
				    ";" path-separator)
  "The separator to use in a classpath.
This is usually the same as `path-separator'")

;;;###autoload
(defun csde-set-global-classpath (classpath)
  "Specify the value of the -classpath argument for the Csharp compiler and
interpreter."
  (interactive 
   "sEnter classpath: ")
  (setq csde-global-classpath (split-string classpath csde-classpath-separator)))

;;;###autoload
(defun csde-browse-jdk-doc ()
  "Displays the JDK doc in a web browser. This function uses the URL
stored in the variable csde-jdk-doc-url to locate the JDK documentation."
  (interactive)
  (if (or
       (string-match "http:" csde-jdk-doc-url)
       (string-match "file:" csde-jdk-doc-url)
       (file-exists-p csde-jdk-doc-url))
      (browse-url csde-jdk-doc-url browse-url-new-window-p)
    (error "The JDK documentation file, %s, does not exist." csde-jdk-doc-url)))

(defun csde-make-compile-command (more-args)
  "Constructs the csharp compile command as: csde-compiler + options + buffer file name."
  (concat csde-compiler " " 
	  (csde-get-compile-options) 
	  (if (not (string= more-args ""))
	      (concat " " more-args))
	  " "
	  (file-name-nondirectory buffer-file-name)))

(defun csde-show-compile-options ()
  "Show the CSDE Compile Options panel."
  (interactive)
  (customize-apropos "csde-compile-options" 'groups))

(defun csde-show-run-options ()
  "Show the CSDE Run Options panel."
  (interactive)
  (customize-apropos "csde-run-options" 'groups))

(defun csde-show-debug-options ()
  "Show the CSDE Debug Options panel."
  (interactive)
  (customize-apropos "csde-db-options" 'groups))

(defun csde-show-project-options ()
  "Show the CSDE Debug Options panel."
  (interactive)
  (customize-apropos "csde-project" 'groups))

(defun csde-show-autocode-options ()
  "Show the CSDE Autocode panel."
  (interactive)
  (customize-apropos "csde-gen" 'groups))


;;;###autoload
(defun csde-csharp-build ()
  "Use csharpc -depend to build the application whose main class is
specified by `csde-run-application-class'."
 (interactive)
  (cond 
   ((string= csde-run-application-class "")
    (message "No application main class specified."))
   (t
    (string-match "\\(\\(\\w*\\.\\)*\\)\\(\\w*\\b\\)"
		csde-run-application-class)
    (let* ((b1 (match-beginning 1))
	   (e1 (match-end 1))
	   (b2 (match-beginning 3))
	   (e2 (match-end 3))
	   (file (concat
		  (substring csde-run-application-class b2 e2)
		  ".cs"))
	   (package (if e1
			(substring csde-run-application-class b1 e1)))
	   (directory (csde-db-search-src-dirs file package)))
      (cond
       (directory
	(let ((file-path 
	       (concat directory 
		       file))
	      (save-depend csde-compile-option-depend))
	  (find-file file-path)
	  (setq csde-compile-option-depend t)
	  (csde-compile)
	  (setq csde-compile-option-depend save-depend)))
       (t
	(message (concat "Could not find source for "
			 csde-run-application-class))))))))
    
;;;###autoload
(defun csde-build ()
  "Rebuild the entire project.
This command has two operating modes: csharp and make. In csharp mode,
this command uses csharpc's built-in make facility to rebuild a
project. In make mode, this command uses a user-specified make program
to rebuild the project. CSDE configuration variables control which mode
is used.  In particular, if the variable `csde-build-use-make' is
non-nil, this command invokes the make program specified by the
variable `csde-make-program'. If the variable `csde-make-args' is a
non-empty string, this function uses its contents to invoke make;
otherwise, it prompts you to enter command-line arguments for make. If
`csde-build-use-make' is nil, this function invokes csharpc on the source
file specified by `csde-run-app-class', with the -depend option. This
causes csharpc to recompile all missing or out-of-date files required
to run the application's main class."
  (interactive)
  (if csde-build-use-make
      (csde-make)
    (csde-csharp-build)))

(defun csde-mode-internal () 
 ;; Define buffer-local variables.
  (make-local-variable 'csde-project-name)
  (make-local-variable 'csde-run-applet-document)

  ;; Load the project file for this buffer. The project file
  ;; defines CSDE options for a project.
  (if (not (csde-debugger-running-p))
	   (csde-load-project-file))

  ;; Enable support for automatic project switching.
  ;; This feature loads the appropriate project settings whenever
  ;; a user switches from a Csharp buffer belonging to one project
  ;; to a buffer belonging to another.
  (make-local-hook 'post-command-hook)
  (unless (find 'csde-detect-csharp-buffer-activation post-command-hook)
    (add-hook 'post-command-hook 'csde-detect-csharp-buffer-activation nil t))

  (make-local-hook 'after-change-functions)
  (setq after-change-functions (default-value 'after-change-functions))
  (add-hook 'after-change-functions 'csde-parse-buffer-changed-hook nil t)

  (if csde-xemacsp
      (csde-insert-menu-in-XEmacs-menubar))

  (if csde-use-font-lock
      (csde-setup-syntax-coloring))

  ;; Define underscore as a word constituent. This is needed
  ;; to support coding styles the begin fields with an underscore.
  (modify-syntax-entry ?_ "w")

  (setq csde-current-project 
	(csde-find-project-file default-directory))

  (when csde-enable-abbrev-mode
     ;; Define abbreviations.
    (csde-init-abbrev-table)
    (abbrev-mode 1))

  ;; Reset the key bindings in case csde-mode-keymap
  ;; was not bound at startup.
  (custom-initialize-reset 'csde-key-bindings nil)

  (if (and
       csde-setnu-mode-enable
       (< (point-max) csde-setnu-mode-threshold))
      (setnu-mode 1))

  (if (string= (car csde-db-debugger) "CSDEbug")
      (csde-bug-install-csdebug-menu))

;;; david@dponce.com
;; (make-local-variable 'semantic-toplevel-bovine-table)
  (csde-parse-semantic-default-setup)
;;;   (setq semantic-toplevel-bovine-table csde-parse-bovine-csharp-grammar)

;;;   (when csde-imenu-enable
;;;     (setq imenu-create-index-function 'csde-create-imenu-index)
;;;     (imenu-add-to-menubar "Classes"))
 
  (make-local-variable 'mode-line-format)
  (setq mode-line-format csde-mode-line-format)

  (when csde-which-method-mode
     (add-hook 'post-command-hook 'csde-which-method-update nil t))

  (make-local-hook 'semantic-after-toplevel-bovinate-hook)
  (add-hook 'semantic-after-toplevel-bovinate-hook 
	    'csde-parse-update-after-parse nil t)

  (senator-minor-mode 1)

  ; When looking for a tag that has multiple matches
  ; in the TAGS file, prefer (find first) the
  ; occurrence in the _current_ buffer.
  ; Contributed by Charles Rich, Mitsubishi Electric Research Laboratories,
  ; Cambridge, MA>
  (make-local-variable 'tags-table-format-hooks)
  (setq tags-table-format-hooks '(csde-etags-recognize-tags-table
				  recognize-empty-tags-table))
  (relabel-menu-item '("Java") "C#")

)

;; This is actually a no-op to get csde auto-loaded.
;;;###autoload
(defun csde-mode ()
  "Major mode for developing Csharp applications and applets."
  nil)

(define-derived-mode 
  csde-mode java-mode "CSDE"
  "Major mode for developing Csharp applications and applets.
  \\{csde-mode-map}"

  (csde-mode-internal)
)


;; Make csde-mode the default mode for Csharp source code buffers.
;; Prepend the csde-mode entry so that it shadows the java-mode
;; entry already in the list.
;;;###autoload
(setq auto-mode-alist
  (append
   '(("\\.cs\\'" . csde-mode))
	auto-mode-alist))

(defvar csde-menu-definition
  (list "CSDE"
	["Compile"           csde-compile t]
	;; ["Run App"           csde-run (not (csde-run-application-running-p))]
	["Run App"           csde-run t]
	["Debug App"         csde-debug t]
	"-"
	;;["-"                 ignore nil]
	["Run Applet"        csde-run-menu-run-applet t]
	["Debug Applet"      csde-db-menu-debug-applet t]
	"-"  
	["Build"             csde-build t]
	["Interpret"         bsh t]
        (list "Documentation"
	      ["Add"             csde-csharpdoc-autodoc-at-line (csde-csharpdoc-enable-menu-p)]
	      ["Check This"      csde-csharpdoc-checkdoc-at-line (csde-csharpdoc-enable-menu-p)]
	      ["Check All"       csde-csharpdoc-checkdoc t]
	      ["Generate"        csde-csharpdoc-make t]
	)
        "-" 
	(list "Templates"
	      ["Get/Set Pair..."  csde-gen-get-set t]
	      ["Println..."       csde-gen-println t]
	      (list "Listener"
		    ["Action"          csde-gen-action-listener t]
		    ["Window"          csde-gen-window-listener t]
		    ["Mouse"           csde-gen-mouse-listener t]
		    )
	      ["Other..."        csde-gen-code t]
	      )
	(list "Wizards"
	      ["Import class"        csde-import-find-and-import t]
	      ["Override Method"     csde-wiz-override-method t]
	      ["Implement Interface" csde-wiz-implement-interface t]
	      ["Delegate Methods"    csde-wiz-delegate t]
              "-"
	      ["Update Class List"   csde-wiz-update-class-list t]
	      )
	["Speedbar"          speedbar-frame-mode t]
	(list "Project"
	      (list "Options"
		    ["General"         csde-show-project-options t]
		    ["Compile"         csde-show-compile-options t]
		    ["Run"             csde-show-run-options t]
		    ["Debug"           csde-show-debug-options t]
		    ["Autocode"        csde-show-autocode-options t]
		    ["Csharpdoc"         csde-csharpdoc-customize t]
		    )
	      (list "Project File"
		    ["Save"     csde-save-project t]
		    ["Load"     csde-load-project-file t]
		    ["Load All" csde-load-all-project-files t]
		    )
	      )
	(list "Help"
	      ["CSDE Users Guide"       csde-show-help t]
	      ["JDK"                   csde-browse-jdk-doc t]
	      ["Symbol at point"       csde-help-symbol t]
	      "-"
	      ["Submit problem report" csde-submit-problem-report t]
	      "-"
	      (concat "CSDE " csde-version)
	      )
	)
  "Menu for CSDE.")

;; Define CSDE menu for FSF Emacs.
(if (or (not csde-xemacsp) (featurep 'infodock))
    (easy-menu-define csde-menu 
		      csde-mode-map
		      "Menu for CSDE."
		      csde-menu-definition))

(defun csde-insert-menu-in-XEmacs-menubar ()
  "Insert CSDE menu in the XEmacs menu bar."
  (if (and 
       (not (featurep 'infodock))
       (not (memq 'infodock c-emacs-features))
       (boundp 'current-menubar)
       current-menubar)
      (if (fboundp 'add-submenu)
	  (add-submenu nil csde-menu-definition)
	(add-menu nil "CSDE" (cdr csde-menu-definition)))))


(defvar csde-new-buffer-menu
  (list
   "CSDE New"
   ["Class..."         csde-gen-class-buffer t]
   ["Console..."       csde-gen-console-buffer t]
   ["Other..."         csde-gen-buffer t]
   )
  "Menu for creating new Csharp buffers.")

;; Add CSDE New menu to Emacs Files menu.
(if (not csde-xemacsp)
    (let* ((mb (assq 'menu-bar global-map))
	   (files (assq 'files mb))
	   (menu (if (fboundp 'easy-menu-create-menu)
		     (easy-menu-create-menu 
		      (car csde-new-buffer-menu) (cdr csde-new-buffer-menu))
		   (easy-menu-create-keymaps 
		    (car csde-new-buffer-menu) (cdr csde-new-buffer-menu))))     
	   (menu-name (car csde-new-buffer-menu)))
      (define-key-after (cdr (cdr files)) [csde-new]
	(cons menu-name menu)
	'open-file))
  (unless (featurep 'infodock)
    (add-submenu '("File") csde-new-buffer-menu "Insert File...")))



;; Project File Functions

(defun csde-cygpath (path &optional direction)
  "Converts a path from cygwin to DOS form if DIRECTION is nil.
Otherwise, it converts the path to cygwin form.  Requires that cygpath
be in your path."
  (interactive "sPath: ")
  (if (which "cygpath")
      (save-excursion
	(let ((buf-name "*cygwin-output*")
	      (output-type (if direction "-u" "-w")))
	  (shell-command 
	   (concat "cygpath " output-type " -p '" path "'") buf-name)
	  (set-buffer buf-name)
	  (let ((output (buffer-substring (point-min) (point-max))))
	    (kill-buffer buf-name)
	    (remove ?\n output))))
    (error "Cannot find cygpath executable.")))

(defun csde-cygwin-path-converter-cygpath (path)
  (interactive "sPath: ")
  (if (string-match "^[a-zA-Z]:" path)
      path
    (csde-cygpath path)))

(defun csde-cygwin-path-converter-internal (path)
  "Convert cygwin style PATH to a form acceptable to csharp vm.  Basically
converts paths of the form: '//C/dir/file' or '/cygdrive/c/dir/file' to
'c:/dir/file'.  This function will not modify standard unix style paths
unless they begin with '//[a-z]/' or '/cygdrive/[a-z]/'."
  (interactive "sPath: ")
  (let* ((path-re "/\\(cygdrive\\)?/\\([a-zA-Z]\\)/")
         (subexpr 2)
         (index1 (* 2 subexpr))
         (index2 (1+ index1)))
    (if (string-match (concat "^" path-re) path)
	(let ((new-path
	       (concat (substring path 
				  (nth index1 (match-data)) 
				  (nth index2 (match-data)))
		       ":/" 
		       (substring path (match-end 0)))))
	  (while (string-match (concat ":" path-re) new-path)
	    (setq new-path
		  (concat
		   (substring new-path 0 (match-beginning 0))
		   ";"
		   (substring new-path 
				  (nth index1 (match-data)) 
				  (nth index2 (match-data)))
		   ":/" 
		   (substring new-path (match-end 0)))))
	  (substitute ?\\ ?\/ new-path))
      path)))

;;(makunbound 'csde-cygwin-path-converter)
(defcustom csde-cygwin-path-converter '(csde-cygwin-path-converter-internal)
  "Function to use to convert cygwin paths to DOS paths.  
Choose csde-cygwin-path-converter-internal, csde-cygwin-path-converter-cygpath,
or \"custom-function.\" csde-cygwin-path-converter-cygpath handles all
cygwin-style paths, including mount points, e.g.,/bin. 
csde-cygwin-path-converter-internal does not handle mount
paths. However, it is much faster as it does not require running a
subprocess every time the CSDE needs to convert a path. Choose
\"custom-function\" if you want the CSDE to use a function that you
supply. Replace \"custom-function\" with the name of the function that
you want to use."
  :group 'csde-project
  :type  '(list
	   (radio-button-choice :format "%t \n%v"
			       :tag "Converter: "
			       :entry-format "  %b %v"
			       (const csde-cygwin-path-converter-internal)
			       (const csde-cygwin-path-converter-cygpath)
			       (function custom-function))))
		       

(defun csde-convert-cygwin-path (path &optional separator)
  "Convert cygwin style PATH to a form acceptable to csharp vm, using
the conversion function specified by `csde-cygwin-path-converter'."
  (interactive "sPath: ")
  (funcall (car csde-cygwin-path-converter) 
	   (if separator (substitute ?\: (string-to-char separator) path) path)))

(defun csde-normalize-path (path) 
  "This function performs
 the following transformation on PATH:

  * Replaces environment variables of the form
    $VAR or ${VAR} with their values. Note
    that you must use the Unix notation for
    environment variables on the native Windows 
    versions of Emacs and XEmacs.

  * Replaces the tilde character with the 
    value of the home directory, typically
    specified by the HOME environment variable.

  * Converts Cygwin style paths to DOS notation
    on Windows.

This function does not expand relative paths."
  (let ((p (substitute-in-file-name path)))
    (if (not (eq (aref p 0) ?.))
	(setq p (expand-file-name p)))
    (csde-convert-cygwin-path
     p)))

(defun csde-build-classpath (paths)
  "Builds a classpath from PATHS.
PATHS is a list of paths. "
  (mapconcat
   (lambda (path)
     (csde-normalize-path path))
   paths
   csde-classpath-separator))

(defun csde-global-classpath ()
  (csde-build-classpath csde-global-classpath))


(defun csde-build-path-arg (arg path-list &optional quote)
"Build a command-line path argument from a list of paths."
  (let ((path (csde-build-classpath path-list)))
    (if quote
        (setq path (concat "\"" path "\"")))
    (setq path (concat arg " " path))))


(defun csde-build-classpath-arg (path-list &optional quote)
"Build a classpath from a list of paths."
 (csde-build-path-arg "-classpath" path-list quote))

(defun csde-root-dir-p (dir)
  (let ((parent (concat dir "../")))
    (cond 
     ((and
       (fboundp 'ange-ftp-ftp-name)
       (ange-ftp-ftp-name dir))
      (ange-ftp-get-file-entry parent))
     ((eq system-type 'windows-nt)
      (not (file-exists-p parent)))
     ((eq system-type 'cygwin32)
      (or (string= (file-truename dir) "/") 
	  (not (file-exists-p (file-truename dir)))))
     (t
      (or (or (not (file-readable-p dir))
	      (not (file-readable-p parent)))
	  (and 
	   (string= (file-truename dir) "/")
	   (string= (file-truename parent) "/")))))))

(defun csde-find-project-file (dir)
  "Finds the project file for the Csharp source file in the current
buffer. Returns nil if it cannot find a project file in the
source file directory or an ascendant directory."
  (let ((file (find csde-project-file-name
		    (directory-files dir) :test 'string=)))
    (if file
	(expand-file-name file dir)
      (if (not (csde-root-dir-p dir))
	  (csde-find-project-file (concat dir "../"))))))

(defun csde-load-project-file ()
  "Loads the project file for the Csharp source file in the current
buffer. Searches for the project file first in the buffer directory,
then in ascendant directories. Uses the first file that it encounters.
If no project file is found, set each CSDE variable to the value defined
in your .emacs file or, if your .emacs file does not define a value, to
the value defined by the CSDE."
  (interactive)
  (let ((prj-file (csde-find-project-file default-directory)))
    (if prj-file
	(load-file prj-file)
      (csde-set-variables-init-value))))


(defun csde-load-all-project-files ()
  (interactive)
  "Loads the project file associated with each Csharp source buffer."
  (mapc
   (lambda (csharp-buffer)
     (save-excursion
       (set-buffer csharp-buffer)
       (message "Loading project file for %s ..." 
		(buffer-file-name csharp-buffer))
       (csde-load-project-file)))
   (csde-get-csharp-source-buffers)))

;;;###autoload
(defun csde-open-project-file ()
  "Opens the project file for the Csharp source file in the
current buffer."
  (interactive)
  (let ((prj-file (csde-find-project-file default-directory)))
    (if prj-file
	(find-file prj-file)
      (message "%s" "Project file not found."))))


(defun csde-save-delete (symbol)
  "Delete the call to SYMBOL from project file.
Leave point at the location of the call, or after the last expression."
  (save-excursion
    (let ((project-file (or
			 (csde-find-project-file default-directory)
			 (concat "./" csde-project-file-name))))
      (set-buffer (find-file-noselect project-file)))

    (goto-char (point-min))
    (catch 'found
      (while t
	(let ((sexp (condition-case nil
			(read (current-buffer))
		      (end-of-file (throw 'found nil)))))
	  (when (and (listp sexp)
		     (eq (car sexp) symbol))
	    (delete-region (save-excursion
			     (backward-sexp)
			     (point))
			   (point))
	    (throw 'found nil)))))
    (unless (bolp)
      (princ "\n"))))

(defvar csde-symbol-list nil
  "A list of csde variables which are processed by csde-save-variables")

(defun csde-symbol-list ()
  "Return a list of variables processed by csde-save-variables.
The first time this is called, the list is saved in csde-symbol-list"
  (or csde-symbol-list
      (mapatoms
       (lambda (symbol)
         (if (and (string-match "csde-" (symbol-name symbol))
                  (get symbol 'custom-type))
             (setq csde-symbol-list (cons symbol csde-symbol-list))))))
  csde-symbol-list)


(defun csde-save-variables ()
  "Save all CSDE variables in project file."
  (csde-save-delete 'csde-set-project-name)
  (csde-save-delete 'csde-set-variables)
  (let ((standard-output (get-buffer csde-project-file-name)))
    (unless (bolp)
      (princ "\n"))

    (princ "(csde-set-project-name ")
    (prin1 csde-project-name)
    (princ ")\n")

    (princ "(csde-set-variables ")
    (mapcar
     (lambda (symbol)
       (when 
	   (and (string-match "csde-" (symbol-name symbol))
		(get symbol 'custom-type))
	 (let ((value (symbol-value symbol)))	   
	     (princ "\n '(")
	     (princ symbol)
	     (princ " ")
	     (prin1 (custom-quote value))
	     ;; Check whether the user has changed the value of this
	     ;; variable in a customization buffer. If so, save flag
	     ;; so that custom knows that this value differs from
             ;; standard value.
	     (if (get symbol 'customized-value)
		 (princ " t)")
	       (princ ")"))		 
	     )))
	(csde-symbol-list))
      (princ ")")
      (save-excursion
	(set-buffer (get-buffer csde-project-file-name))
	(unless (looking-at "\n")
	  (princ "\n"))
	(save-buffer))
      (kill-buffer (get-buffer csde-project-file-name))))

(defun csde-set-project-name (name)
  (setq csde-project-name name))

(defun csde-set-variables (&rest args)
  "Initialize CSDE customization variables.  

Takes a variable number of arguments. Each argument 
should be of the form:

  (SYMBOL VALUE)

The value of SYMBOL is set to VALUE.
"
  (while args 
    (let ((entry (car args)))
      (if (listp entry)
	  (let* ((symbol (nth 0 entry))
		 (value (nth 1 entry))
		 (customized (nth 2 entry))
		 (set (or (get symbol 'custom-set) 'set-default)))
	    (if customized
		(put symbol 'customized-value (list value)))
	    (when (default-boundp symbol)
		   ;; Something already set this, overwrite it
		   (funcall set symbol (eval value)))
	    (setq args (cdr args)))))))

(defun csde-set-variables-init-value ()
  "Set each CSDE variable to the value it has at Emacs startup."
  (interactive)
  (message "Setting CSDE variables to startup values...")
  (mapcar 
   (lambda (symbol) 
     (when 
	 (and (string-match "csde-" (symbol-name symbol))
	      (get symbol 'custom-type))
       (let ((saved-val (get symbol 'saved-value))
	     (std-val (get symbol 'standard-value))
	     (set (or (get symbol 'custom-set) 'set-default)))
	 (if saved-val
	     (funcall set symbol (eval (car saved-val)))
	   (funcall set symbol (eval (car std-val)))))))
	(csde-symbol-list)))
 
;;;###autoload
(defun csde-save-project (proj-name)
  "Saves local source file buffer options in project file.
This command provides an easy way to create and update a
project file for a Csharp project. Simply open a source file,
set the desired options, using the CSDE Options menu, then
save the settings in the project file, using this command.
Now, whenever you open a source file from the same directory
tree, the saved settings will be restored for that file."
  (interactive
   (list 
    (let (prompt)
      (if (string= csde-project-name "")
	  (setq prompt "Enter project name: ")
	(setq prompt
	      (format "Enter project name (%s): " 
		      csde-project-name)))
      (read-string prompt))))
  (unless (string= proj-name "")
      (setq csde-project-name proj-name))
  (csde-save-variables))

(defun csde-convert-prj-file (file) 
"Converts a pre-CSDE-2.0.7 project file to CSDE-2.0.7 format.
Note: old project files did not preserve information about 
whether a saved value differed from the standard (CSDE-defined)
value of a variable. Thus, all values are saved in the
converted file as though they were standard values. This means
that when CSDE reloads the file, a custom buffer will customized
values as though they were standard. If you want to restore
a customized value to a standard value, simply make some
innocuous edit to the customized value and choose 
'Set for current session' from the customization buffer's
Set menu. Custom will then enable the Set menu option that
allows you to restore the value to its default value."
  (interactive "F")
  (let ((olddef (symbol-function 'csde-set-variables))
	(newdef 
	 (lambda (&rest args)
	   (while args 
	     (let ((entry (car args)))
	       (if (listp entry)
		   (let* ((symbol (nth 0 entry))
			  (value (nth 1 entry))
			  (set (or (get symbol 'custom-set) 'set-default)))
		     (when (default-boundp symbol)
		       ;; Something already set this, overwrite it
		       (funcall set symbol value))
		     (setq args (cdr args)))))))))
    (defalias 'csde-set-variables newdef)
    (require 'cus-edit)
    (load-file file)
    (csde-save-project csde-project-name)
    (defalias 'csde-set-variables olddef)))

;; Code to update CSDE customization variables when a user switches
;; from a Csharp source buffer belonging to one project to a buffer
;; belonging to another.

(setq csde-current-project "")

(defun csde-reload-project-file ()
  "If project context-switching is enabled (see
`csde-project-context-switching-enabled-p'), reloads the project file
for a newly activated Csharp buffer when the new buffer's project
differs from the old buffer's."
  (interactive)
  (let ((project-file-path (csde-find-project-file default-directory)))
    (if (not project-file-path) (setq project-file-path ""))
    (if (and 
	 csde-project-context-switching-enabled-p
	 (not (csde-debugger-running-p))
	 (not (string= csde-current-project project-file-path)))
	(progn
	  (setq csde-current-project project-file-path)
	  (csde-load-project-file)
	  (csde-wiz-update-class-list)))))

(defun csde-debugger-running-p () 
  (and 
   (csde-dbs-debugger-running-p)
   (csde-dbs-get-target-process)))

(defcustom csde-entering-csharp-buffer-hook 
  '(csde-reload-project-file 
    csde-which-method-update-on-entering-buffer)
"*Lists functions to run when entering a Csharp source buffer."
  :group 'csde-project
  :type 'hook)


(setq csde-current-buffer (current-buffer))

(defun csde-detect-csharp-buffer-activation ()
"Detects when a user activates a buffer.
If the activated buffer is a Csharp buffer, runs the 
`csde-entering-csharp-buffer-hook' hooks."
  (let ((curr-buff (current-buffer)))
    (if (not
	 (equal curr-buff csde-current-buffer))
	(progn
	  (setq csde-current-buffer curr-buff)
	  (if (eq major-mode 'csde-mode)
		(run-hooks 'csde-entering-csharp-buffer-hook))))))


(defun csde-count-open-csharp-buffers ()
  "Returns non-nil if any csharp buffers are open."
  (count 
   ".cs"
   (buffer-list)
   :test
   (lambda (file-type buffer)
     (let ((file-name (buffer-file-name buffer)))
       (if file-name
	   (string-match file-type file-name))))))
	 

(defun csde-remove-csde-hook ()
  "Removes `csde-detect-csharp-buffer-activation-hook' when
all Csharp source buffers have been closed."
  (unless (> (csde-count-open-csharp-buffers) 1)
  (remove-hook 'post-command-hook 'csde-detect-csharp-buffer-activation)))

(add-hook 'kill-buffer-hook 'csde-remove-csde-hook)


;; CSDE help

(defun csde-find-csde-data-directory ()
  "Return the path of the CSDE data directory.
Returns the path of the directory containing the
CSDE csharp and documentation directories;  nil if the 
directory cannot be found. If XEmacs, returns the location of
the data directory in the XEmacs distribution hierarchy. On all other Emacs versions, 
the CSDE expects to find the documentation and Csharp class directories
in the same directory that contains the CSDE lisp directory."
  (let (dir)
    (if csde-xemacsp
	(progn
	  (setq dir (locate-data-directory "csde"))
	  (when (not dir)
	      (setq dir (file-name-directory (locate-library "csde")))
	      (setq dir (substring dir 0 (- (length dir) 5)))))
      (setq dir (file-name-directory (locate-library "csde"))))
    (if dir
	(nsubstitute ?/ ?\\ dir))
    (if (not csde-xemacsp)
	(setq dir (substring dir 0 (- (length dir) 5))))
    dir))

(defun csde-find-csde-doc-directory ()
  "Return the path of the CSDE documentation directory.
Returns  nil if the directory cannot be found. At some
point, XEmacs will include the CSDE. Versions of XEmacs
that include CSDE will store the CSDE doc in a data
directory called csde. On all other Emacs versions, the CSDE
expects to find the documentation in a subdirectory 
named doc of the directory that contains the file
csde.el."
  (csde-find-csde-data-directory))

;;;###autoload
(defun csde-show-help ()
  "Displays the CSDE User's Guide in a browser."
  (interactive)
  (let* ((csde-dir (csde-find-csde-doc-directory))
         (csde-help
          (if csde-dir
              (if (and csde-xemacsp
                       (locate-data-directory "csde"))
                  (expand-file-name "csde-ug.html" csde-dir)
                (expand-file-name "doc/html/csde-ug/csde-ug.html" csde-dir)))))       
    (if (and
         csde-help
         (file-exists-p csde-help))
        (browse-url (concat "file://" (csde-convert-cygwin-path csde-help))
                    browse-url-new-window-p)
      (signal 'error '("Cannot find CSDE help file.")))))

(defun csde-debug ()
"*Runs the debugger specified by `csde-db-debugger'."
  (interactive)
  (if (string= (car csde-db-debugger) "CSDEbug")
      (csde-bug-debug-app)
    (csde-db)))

;;
;; Problem reporting functions contributed by Phillip Lord <plord@hgmp.mrc.ac.uk>.
;;
(defvar csde-problem-report-mail-address "pkinnucan@mediaone.net" )

(defun csde-submit-problem-report()
  "Submit a problem report for the CSDE" 
  (interactive)
  (require 'reporter)
  (and 
   (y-or-n-p "Do you want to submit a problem report on the CSDE? ")
   (progn
     (message "Preparing problem report...")
     ;;prepare the basic buffer
     (reporter-submit-bug-report
      csde-problem-report-mail-address
      (concat "CSDE version " csde-version)
      (csde-problem-report-list-all-variables)
      nil
      'csde-problem-report-post-hooks
      "Please enter the details of your bug report here" )
     (message "Preparing bug report...done"))))


(defun csde-problem-report-post-hooks()
  "Function run the reporter package done its work.
It looks for a CSDEBug buffer and inserts the contents of that, and then prompts 
for insertion of the .emacs file"
  (save-excursion 
    (goto-char (point-max))
    (let* ((debug-buffer (get-buffer "*CSDEbug*"))
	  (messages-buffer 
	   (get-buffer
	    (if csde-xemacsp " *Message-Log*" "*Messages*")))
	  (backtrace-buffer (get-buffer "*Backtrace*"))
	  (process 
	   (let ((proc (csde-dbs-get-target-process)))
	     (if (not proc)
		 (let ((dead-proc-alist 
			(oref csde-dbs-the-process-morgue proc-alist)))
		   (if dead-proc-alist
		       (setq proc (cdr (car dead-proc-alist))))))
	     proc))
	  (cli-buffer (if (and process (slot-boundp process 'cli-buf))
			(oref process cli-buf)))
	  (locals-buffer (if (and process (slot-boundp process 'locals-buf))
			(oref process locals-buf)))
	  )

      ;;insert the contents of the debug buffer if it is there. 
      (if debug-buffer
	  (progn
	    (insert-string "\n\n\nThe contents of the *CSDEBug* buffer were\n\n")
	    (insert-buffer debug-buffer)
	    (goto-char (point-max))
	    (insert-string "\n\n\nEnd Insert *CSDEbug* buffer" ))
	(insert-string "\n\n\nThere was no *CSDEBug* buffer" ))

      ;;insert the contents of the CLI buffer if it exists.
      (if cli-buffer
	  (progn
	    (insert-string "\n\n\nThe contents of the CLI buffer are\n\n")
	    (insert-buffer cli-buffer)
	    (goto-char (point-max))
	    (insert-string "\n\n\nEnd Insert CLI buffer" ))
	(insert-string "\n\n\nThere is no CLI buffer" ))


      ;;insert the contents of the locals buffer if it exists.
      (if locals-buffer
	  (progn
	    (insert-string "\n\n\nThe contents of the locals buffer are\n\n")
	    (insert-buffer locals-buffer)
	    (goto-char (point-max))
	    (insert-string "\n\n\nEnd Insert locals buffer" ))
	(insert-string "\n\n\nThere is no locals buffer" ))

      ;;insert the contents of the backtrace buffer if it is there. 
      (if backtrace-buffer
	  (progn
	    (insert-string "\n\n\nThe contents of the *Backtrace* buffer were\n\n")
	    (insert-buffer backtrace-buffer)
	    (goto-char (point-max))
	    (insert-string "\n\n\nEnd Insert *Backtrace* buffer" ))
	(insert-string "\n\n\nThere was no *Backtrace* buffer" ))


      ;;insert the contents of the messages buffer if it is there. 
      (if messages-buffer
	  (progn
	    (insert-string "\n\n\nThe contents of the *Messages* buffer were\n\n")
	    (insert-buffer messages-buffer)
	    (goto-char (point-max))
	    (insert-string "\n\n\nEnd Insert *Messages* buffer" ))
	(insert-string "\n\n\nThere was no *Messages* buffer" )))

    (when process-environment
      (insert-string "\n\n\nProcess environment: \n\n")
      (insert-string (mapconcat (lambda (var) var) process-environment "\n")))

    (let ((buf (get-buffer-create "*Insert .emacs*"))
	  (mail-buf (current-buffer)))
      
      (set-buffer buf)
      (widget-insert "It is requested that you send the entire contents of your .emacs file.\n")
      (widget-insert "This is because it has been found that those parts of the .emacs file\n" )
      (widget-insert "which appear not to be CSDE related often do in fact contain the cause of\n")
      (widget-insert "reported bugs.\n\n")
      (widget-insert "If you do not want to send the contents of your .emacs or you load a large\n" )
      (widget-insert "large number of files from your full .emacs file, then please attempt to\n" )
      (widget-insert "replicate the bug using the minimal .emacs file suggested in the CSDE\n" )
      (widget-insert "documentation, and note that you have done this in this bug report\n" )
      (widget-insert "If you choose to do neither of these things we may not be able to\n" )
      (widget-insert "or necessarily want to help determine the cause of the problem!\n" )
      (switch-to-buffer "*Insert .emacs*")
      
      (set-buffer mail-buf)
      (goto-char (point-max))
      (if (y-or-n-p "Insert your .emacs file into the problem report? " )
	  (progn
	    (insert-string "\n\n\nThe contents of the .emacs file was\n\n\n")
	    (insert-file "~/.emacs")
	    (goto-char (point-max))
	    (insert-string "\n\n\n=====end inserted .emacs file"))
	(insert-string "\n\n\nThe user choose not to insert their .emacs file\n" ))
      ;;clean up the prompt buffer
      (kill-buffer buf))))

(defun csde-problem-report-list-all-variables()
  "List all variables starting with `csde' or `bsh'."
  (let ((vars))
    (mapatoms
     (lambda (symbol)
       (when 
	   (and (or 
		 (string-match "bsh-" (symbol-name symbol))
		 (string-match "csde-" (symbol-name symbol)))
		(get symbol 'custom-type))
	 (setq vars (cons symbol vars)))))
    vars))


;; Line numbering support.
(eval-when (compile)
  (require 'setnu))

(defvar csde-setnu-deletion-check t "deletion check")
(make-variable-buffer-local 'csde-setnu-deletion-check)

(defun csde-setnu-after-change (start end length)
 "When in setnu-mode, toggles setnu-mode off and on."
   (if setnu-mode
       (if (or
	    (and
	     (> length 0)
	     csde-setnu-deletion-check)
	    (string-match 
		  "[\n\r]" 
		  (buffer-substring-no-properties start end)))
	   (run-with-timer 
	    0.001 nil
	    ;; setnu toggler      
	   (lambda () (setnu-mode) (setnu-mode))))
     (setq csde-setnu-deletion-check nil)))

(defun csde-setnu-before-change (start end) 
  "Determines whether any newlines were deleted."
   (if setnu-mode
       (if (> end start) 
	   (setq csde-setnu-deletion-check 
		 (string-match "[\n\r]" (buffer-substring-no-properties start end))))))


(defcustom csde-setnu-mode-threshold 20000
 "Maximum number of bytes in a file (buffer) that can result in
automatic line numbering."
 :group 'csde-project
 :type 'integer)

(defcustom csde-setnu-mode-enable nil
 "Enable numbering of lines in Csharp source buffers."
 :group 'csde-project
 :type 'boolean
 :set '(lambda (sym val)
	 (if val
	     (progn
	       (require 'setnu)
	       (add-hook 
		'after-change-functions 
		'csde-setnu-after-change)
	       (add-hook 
		'before-change-functions 
		'csde-setnu-before-change)
	       (mapc
		(lambda (buf)
		  (save-excursion
		    (set-buffer buf)
		    (if (and
			 (not setnu-mode)
			 (< (point-max) csde-setnu-mode-threshold))
			(setnu-mode 1))))
		  (csde-get-csharp-source-buffers)))
	   (progn
	     (mapc 
	      (lambda (buf)
		(save-excursion
		  (set-buffer buf)
		  (if (and (boundp 'setnu-mode)
			   setnu-mode)
		      (setnu-mode))))
	      (csde-get-csharp-source-buffers))))	 
	 (set-default sym val)))

;; csde-describe-map is Ehud Karni's describe map with csde prepended.
(defun csde-keymap-test (var)           ; internal function for keymap checking
       (and (boundp var)
            (keymapp (symbol-value var))))

(defun csde-describe-map (map)          ; display map binding
 "Display binding for MAP which must be a quoted keymap variable"
  (interactive
       (let ((map (intern (completing-read "Key map: " obarray 'csde-keymap-test 1))))
           (list map)))
       (let ((val (symbol-value map)))
           (or (keymapp val)
               (error "%s is not a keymap !" (symbol-name map)))
           (with-output-to-temp-buffer "*Help*"
               (princ (format "Binding for keymap %s is:\n" (symbol-name map)))
               (princ (substitute-command-keys "\\{val}" ))
               (print-help-return-message))))

(defun csde-keys ()
  "Displays CSDE key bindings. Use `csde-bug-keys' to display CSDEbug keybindings ."
  (interactive)
  (csde-describe-map 'csde-mode-map))


;; Contributed by John Ciolfi, jciolfi@mathworks.com.
(defun csde-compile-file-if-necessary (file)
  "Compile the CSDE file FILE if necessary.
This is done if FILE.el is newer than FILE.elc or if FILE.elc doesn't exist."
  (if (string= (file-name-extension file) "el")
      (let* ((root (file-name-sans-extension file))
	     (elc-file (concat root ".elc")))
	(if (or (not (file-exists-p elc-file))
		(file-newer-than-file-p file  elc-file))
	    (progn
	      (message (format "Byte-compiling %s..." 
			       (file-name-nondirectory file)))
	      (byte-compile-file file))))))

;;;###autoload
(defun csde-compile-csde ()
  "Byte-compile all uncompiled files of csde."

  ;; Be sure to have . in load-path since a number of files in csde
  ;; depend on other files and we always want the newer one even if
  ;; a previous version of csde exists.

  (interactive)
  (let ((load-path (append '(".") load-path))
	(buffer-directory (file-name-directory (buffer-file-name)))
	(csde-lisp-directory (expand-file-name "lisp" (csde-find-csde-data-directory))))
    (save-excursion 
      (dired csde-lisp-directory)
      (mapcar 
       (function csde-compile-file-if-necessary)
       (mapcar 
	(lambda (file)
	  (expand-file-name file csde-lisp-directory))
	(directory-files csde-lisp-directory))))))

;; Provided for XEmacs compatibility.
(if (not (fboundp 'subst-char-in-string))
    (defun subst-char-in-string (fromchar tochar string &optional inplace)
      "Replace FROMCHAR with TOCHAR in STRING each time it occurs.
Unless optional argument INPLACE is non-nil, return a new string."
      (let ((i (length string))
	    (newstr (if inplace string (copy-sequence string))))
	(while (> i 0)
	  (setq i (1- i))
	  (if (eq (aref newstr i) fromchar)
	      (aset newstr i tochar)))
	newstr)))


(provide 'csde)

;; Change History 

;;
;; $Log: csde.el,v $
;; Revision 1.5  2001/02/25 05:10:13  paulk
;; Update for CSDE 2.2.7beta3
;;
;; Revision 1.4  2001/02/20 05:29:19  paulk
;; CSDE 2.27 updates
;;
;; Revision 1.146  2001/02/20 05:09:04  paulk
;; The CSDE now expands paths that begin with a tilde but not a period (.).
;;
;; Revision 1.145  2001/02/17 07:27:01  paulk
;; - Fixed regression bug in context-switching that resulted from setting
;;   csde-current-project to the path of the current prj.el file.
;;
;; - No longer expand classpaths to avoid expanding relative path (.) notation.
;;
;; Revision 1.144  2001/02/03 08:14:26  paulk
;; Changed declarations for all path variables from string to file. This means that you can now use completion (M tab) to complete paths in customization buffers.
;;
;; Revision 1.143  2001/02/03 07:32:29  paulk
;; Made quote argument optional in csde-build-path-arg and csde-build-classpath-arg.
;;
;; Revision 1.142  2001/01/16 04:25:30  paulk
;; Adds csde-abbrev-mode and csde-show-abbrev commands. Thanks to s.nicolas@videotron.ca.
;;
;; Revision 1.141  2000/12/25 09:32:27  paulk
;; * Adds csde-cygwin-path-converter variable.
;;
;; * Adds support for environment variable substitution in class and sourcepaths.
;;
;; Revision 1.140  2000/12/18 05:22:46  paulk
;; *** empty log message ***
;;
;; Revision 1.139  2000/12/10 06:52:58  paulk
;; Added csde-compile-csde.
;;
;; Revision 1.138  2000/11/27 06:18:41  paulk
;; Miscellaneous bug fixes and minor enhancements.
;;
;; Revision 1.137  2000/11/20 05:15:16  paulk
;; Added csde-import-organize command. Moved all import-related code from
;; csde-wiz.el to a new package named csde-import.el.
;;
;; Revision 1.136  2000/10/22 07:56:25  paulk
;; Add a Documentation submenu.
;;
;; Revision 1.135  2000/10/20 04:12:13  paulk
;; *** empty log message ***
;;
;; Revision 1.134  2000/10/12 02:31:55  paulk
;; *** empty log message ***
;;
;; Revision 1.133  2000/10/08 12:55:40  paulk
;; *** empty log message ***
;;
;; Revision 1.132  2000/09/05 04:59:57  paulk
;; Bug fixes.
;;
;; Revision 1.131  2000/08/19 06:48:36  paulk
;; Control flow abbreviations now optional.
;;
;; Revision 1.130  2000/08/11 05:02:52  paulk
;; Now refreshes speedbar at the end of a compilation.
;;
;; Revision 1.129  2000/07/29 03:18:41  paulk
;; Add support for line numbering via the setnu package.
;;
;; Revision 1.128  2000/07/28 06:27:47  paulk
;; Committing all modified files.
;;
;; Revision 1.127  2000/07/13 05:22:49  paulk
;; *** empty log message ***
;;
;; Revision 1.126  2000/06/12 08:20:19  paulk
;; Integrated David Ponce's jdok package.
;;
;; Revision 1.125  2000/05/10 05:38:41  paulk
;; The CSDEbug menu now appears or disappears when you select or deselect CSDEbug as the current debugger.
;;
;; Revision 1.124  2000/04/20 04:32:09  paulk
;; User can now supply customized imenu regular expressions. See `csde-imenu-regex-function'.
;;
;; Revision 1.123  2000/04/14 07:23:30  paulk
;; Added option csde-imenu-recognize-tag-comments-p. When on, this option causes the imenu symbol declaration indexer to recognize variables and method declarations witn prefixed tag comments.
;;
;; Revision 1.122  2000/03/16 05:08:25  paulk
;; Added CSDEbug option to csde-db-debugger.
;;
;; Revision 1.121  2000/03/03 06:52:20  paulk
;; Moved Browse JDK doc item to Help menu. Other cosmetic changes to the
;; Help menu.
;;
;; Revision 1.120  2000/02/17 06:40:15  paulk
;; Fixed key bindings to show function keys on menu labels.
;;
;; Revision 1.119  2000/02/16 04:40:33  paulk
;; Implemented Cygwin/XEmacs compatiblity fixes provided by Fred Hart
;; <cfhart@Z-TEL.com> in bsh-internal.
;;
;; Revision 1.118  2000/02/16 03:11:50  paulk
;; *** empty log message ***
;;
;; Revision 1.117  2000/02/14 06:19:38  paulk
;; Implemented up and down stack commands.
;;
;; Revision 1.116  2000/01/29 02:25:15  paulk
;; You can now use the notation [f1], [f2], etc., to specify function
;; keys when customizing csde-key-bindings.
;;
;; Revision 1.115  2000/01/18 07:11:26  paulk
;; Added csde-show-class-source. Thanks to Phil Lord for the initial
;; implementation of this command.
;;
;; Revision 1.114  2000/01/02 08:07:55  paulk
;; Added attach process commands.
;;
;; Revision 1.113  1999/12/23 04:04:02  paulk
;; * csde-mode now defines underscore as a word constituent to allow
;; variable names to start with underscores.
;; * Added Project menu
;; * Added Project Files->Load and Load All commands
;; * Added csde-project-context-switching-enabled-p variable.
;; * Context-switching now suspended when CSDEbug is running a debuggee process.
;;
;; Revision 1.112  1999/12/19 06:57:43  paulk
;; Made buffer activation hook truly buffer local. Thanks to "Phillip
;; Lord" <plord@hgmp.mrc.ac.uk> for this fix.
;;
;; Revision 1.111  1999/12/14 05:16:13  paulk
;; Changed key binding for csde-help-class from C-c C-v C-h to C-c C-v C-w
;; because C-h is a reserve binding. Thanks to Richard Y. Kim <ryk@coho.net> for suggesing this change.
;;
;; Revision 1.110  1999/12/13 05:54:09  paulk
;; Added csde-bug-vm-executable and csde-bug-jre-home variables.
;; Fixed csde-dbs-launch-process command so that it fails gracefully.
;;
;; Revision 1.109  1999/11/28 07:01:23  paulk
;; Changed key binding for csde-complete-at-point to C-c C-v C-.
;;
;; Revision 1.108  1999/11/27 05:13:50  paulk
;; Added commands for tracing classes.
;;
;; Revision 1.107  1999/11/01 03:08:05  paulk
;; Added csde-submit-bug-report contributed by Phillip Lord.
;;
;; Revision 1.106  1999/10/01 05:59:25  paulk
;; Added csde-wiz-update-class-list function to the Wizards menu.
;; Added csde-help-class to the Help menu.
;;
;; Revision 1.105  1999/09/30 04:48:53  paulk
;; Cache csde variables to speed switching between projects. Enhancement
;; provide by David Biesack.
;;
;; Revision 1.104  1999/09/17 06:51:07  paulk
;; Fixed Wizards->Import Class menu item to invoke
;; csde-wiz-find-and-import instead of csde-wiz-import.
;;
;; Revision 1.103  1999/09/08 05:40:47  paulk
;; Updated debugger code to take advantage of new unbound slot capability
;; of eieio.
;;
;; Revision 1.102  1999/09/05 04:37:24  paulk
;; Fixed bug in csde-show-help function that prevented it from working
;; with Internet Explorer on Windows/NT.
;;
;; Revision 1.101  1999/08/27 05:27:53  paulk
;; Provided initial support for multiple processes.
;; Fixed csde-find-data-directory to work on XEmacs with a standard
;; CSDE distribution.
;; Ported breakpoint highlighting code to XEmacs. Still has bugs though.
;; Now includes csde-db-option options on vm command-line for process.
;;
;; Revision 1.100  1999/08/24 06:29:43  paulk
;; Reimplemented the constructor for csde-dbs-proc the right way. Renamed
;; csde-bug-counter to csde-bug-breakpoint-counter.
;;
;; Revision 1.99  1999/08/23 03:52:01  paulk
;; Updated csde-show-help function to reflect new location of user's guide
;; in the CSDE help hierarchy.
;;
;; Revision 1.98  1999/08/23 01:44:25  paulk
;; Updated to use Eric Ludlam's eieio object system.
;;
;; Revision 1.97  1999/08/15 23:48:32  paulk
;; Provided support for initial implementation of CSDEbug.
;;
;; Revision 1.96  1999/06/27 01:04:14  paulk
;; Changed release number to 2.1.6beta4.
;;
;; Revision 1.95  1999/06/26 00:15:09  paulk
;; Kill project file buffer after saving to prevent cross-contamination of simultaneously open project files.
;;
;; Revision 1.94  1999/06/22 21:16:24  paulk
;; Added key binding for `csde-help-class'.
;; Updated revision to 2.1.6beta3.
;;
;; Revision 1.93  1999/05/10 13:23:07  paulk
;; Added require statement for csde-parse.
;;
;; Revision 1.92  1999/05/07 23:25:13  paulk
;; Changed version number to 2.1.6beta1.
;;
;; Revision 1.91  1999/05/07 21:13:45  paulk
;; Changed key binding C-c C-v C-z to invoke
;; csde-wiz-find-and-import instead of csde-wiz-import.
;;
;; Changed csde-build to support interactive entry of make arguments.
;;
;; Revision 1.90  1999/03/10 18:56:43  paulk
;; Fixed in bug in csde-find-csde-data-directory
;;
;; Revision 1.89  1999/03/10 17:00:09  paulk
;; Changed version to 2.1.5.
;;
;; Revision 1.88  1999/02/26 15:56:38  paulk
;; Version 2.1.5b4
;;
;; Revision 1.87  1999/02/17 19:17:11  paulk
;; 2.1.5b3 version number.
;;
;; Revision 1.86  1999/02/15 01:15:09  paulk
;; Updated version number.
;;
;; Revision 1.85  1999/02/12 15:26:09  paulk
;; Added menu item (Wizards->Import Class) for generating import statements.
;;
;; Revision 1.84  1999/02/08 18:02:57  paulk
;; *** empty log message ***
;;
;; Revision 1.83  1999/02/04 01:38:37  paulk
;; Provided a fix for ensuring that key bindings are always set. The fix is
;; to do a custom-initialize-reset on the csde-key-bindings variable in the csde-mode
;; function. The csde-mode-map is updated with the key bindings as a side effect of
;; resetting the variable.
;;
;; Revision 1.82  1999/02/04 01:22:23  paulk
;; Fixed some keybindings. Also backed out Matthew Moore's fix for ensuring
;; that csde-mode-keymap gets set at startup since it seems to break
;; java-mode. I'll try to come up with another fix later.
;;
;; Revision 1.81  1999/02/03 22:57:31  paulk
;; *** empty log message ***
;;
;; Revision 1.80  1999/02/03 01:54:46  paulk
;; Minor fix to debug applet item.
;;
;; Revision 1.79  1999/02/03 01:12:11  paulk
;; Fixed a bug in the initialization code for csde-key-bindings.
;; Thanks to Matthew Moore <matthew.moore@Schwab.COM> for this fix.
;;
;; Added a menu item for debugging applets.
;;
;; Revision 1.78  1999/01/15 22:11:04  paulk
;; Some XEmacs patches that I missed.
;;
;; Revision 1.77  1999/01/15 21:57:48  paulk
;; Added XEmacs compatibility changes from Andy Piper.
;;
;; Revision 1.76  1998/12/09 00:56:31  paulk
;; Put csde-compile variables in a new file csde-compile.el.
;;
;; Revision 1.75  1998/11/27 10:10:03  paulk
;; Updated CSDE version number to 2.1.3.
;;
;; Revision 1.74  1998/11/22 18:13:57  paulk
;; Added menu items for the BeanShell and method override and interface wizards.
;;
;; Revision 1.73  1998/09/13 02:01:53  paulk
;; Fixed a small bug in key binding code.
;;
;; Revision 1.72  1998/09/13 01:49:35  paulk
;; Added support for customization of CSDE key bindings via the
;; variable csde-key-bindings.
;;
;; Revision 1.71  1998/09/13 00:32:48  paulk
;; Added System.out.println template to the Generate menu.
;;
;; Revision 1.70  1998/09/07 02:50:31  paulk
;; This version includes the latest version of csde-gen.el, which was inadvertently
;; replaced by an older version in the last release. This version also includes
;; a newer version of speedbar.el that seems to work better with NT/Emacs 20.3.1
;; than the one that comes with the 20.3.1 distribution.
;;
;; Revision 1.69  1998/08/28 12:56:06  paulk
;; *** empty log message ***
;;
;; Revision 1.68  1998/08/28 12:52:52  paulk
;; Updated version number.
;;
;; Revision 1.67  1998/07/28 03:15:33  paulk
;; Removed a diagnostic message.
;;
;; Revision 1.66  1998/07/28 03:12:40  paulk
;; Updated version number to 2.0.9.
;;
;; Revision 1.65  1998/07/28 03:12:05  paulk
;; Fixed the following project file bugs:
;;
;;   * CSDE does not store the project name in the project file.
;;   * CSDE does not save variables whose value is nil.
;;   * CSDE does not reset variables to initial values when
;;     switching to a buffer that is not part of a project.
;;
;; Revision 1.64  1998/07/22 00:10:07  paulk
;; Now requires cus-edit. This fixes custom-quote is void bug.
;;
;; Fixed bug in csde-set-variables that prevented loading of
;; project files in the new format.
;;
;; Revision 1.63  1998/07/10 00:49:24  paulk
;; Changed csde-save-variables to mark variables that have been customized\n in the current session. Changed csde-set-variables to store the value\n of a customized variable in the customized-value property of the\n variable. This enables Custom to recognize the variable as customized.\n\n  Added csde-convert-prj-file, a function that converts old project files to \n \
;; CSDE-2.0.7 format.\n\n Fixed a bug in the function that finds the CSDE documentation.
;;
;; Revision 1.62  1998/07/09 04:33:57  paulk
;; Change the way that the CSDE saves and restores project-specific values of
;; customization variables to be compatible with custom. This fixes the bug
;; that caused errors when loading customized CSDE variables from a .emacs file.
;; .
;; .
;; .
;; Old entries deleted to save space.
;;
;; Revision 1.8  1997/06/18 17:20:00  paulk
;; Initial checkin.
;;
