;;;; xslt-process.el -- Invoke an XSLT processor on an Emacs buffer

;; Package: xslt-process
;; Author: Ovidiu Predescu <ovidiu@cup.hp.com>
;; Author: Tony Addyman <A.M.Addyman@salford.ac.uk>

;; Created: December 2, 2000
;; Time-stamp: <August 20, 2001 01:05:25 ovidiu>
;; Keywords: XML, XSLT
;; URL: http://www.geocities.com/SiliconValley/Monitor/7464/
;; Compatibility: XEmacs 21.1, Emacs 21.2

;; This file is part of XEmacs

;; Copyright (C) 2000, 2001 Ovidiu Predescu
;; Copyright (C) 2002 Tony Addyman


;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2 of the
;; License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA
;; 02111-1307, USA.

;;; Comentary:

;; To use this package, put the lisp/ directory from this package in
;; your Emacs load-path and do:
;;
;; (autoload 'xslt-process-mode "xslt-process" "Emacs XSLT processing" t)
;; 
;; If you're using DocBook, you may also want to add this line:
;; 
;; (autoload 'xslt-process-install-docbook "xslt-process"
;;  "Register the DocBook package with XSLT-process" t)
;;
;; Then, while being in an XML buffer, use the XSLT menu to either:
;;
;; - run an XSLT processor on the buffer and display the results in a
;; different one
;;
;; - run an XSLT processor in debug mode, so you can view the XSLT
;; processing as it happens
;;

(require 'cl)
(require 'browse-url)
(require 'easymenu)
(require 'string)
(require 'widget)

(defconst xslt-process-version "2.2"
  "The version of the XSLT-process mode.")


(eval-and-compile
  (require 'wid-edit)
  (if (featurep 'xemacs)
      nil
    (defun remassoc (key list)
      "Delete any elements in LIST whose car is `equal' to KEY. This
function has no side effects as in XEmacs, so use `setq' to setup the
modified value to the result of this function."
      (if (null list)
	  nil
	(let* ((elem (car list))
	       (elem-key (car elem)))
	  (if (equal key elem-key)
	      (remassoc key (cdr list))
	    (cons elem (remassoc key (cdr list)))))))
    (defun mapvector (function seq)
      "Apply FUNCTION to each element in SEQUENCE. Return a vector of
the results."
      (let ((result nil))
	(loop for elem across seq do
	  (setq result (append result (list (apply function (list elem))))))
	(vconcat result)))))

(eval-and-compile
  (if (eq system-type 'windows-nt)
      (defun urlize (filename)
	"Replace a series of \\\\ with a single /, so that the file
names conform to the URI definition."
	(if (null filename) filename
	  (let ((new (string-replace-match "\\\\" filename "/" t t)))
	    (if new new filename))))
    (defun urlize (filename)
      "On Unix systems the file names already conform to the URI definition."
      filename)))

;; Setup some definitions for older versions of XEmacs
(unless (fboundp 'line-beginning-position)
  (if (fboundp 'point-at-bol)
      (defalias 'line-beginning-position 'point-at-bol)))
(unless (fboundp 'line-end-position)
  (if (fboundp 'point-at-eol)
      (defalias 'line-end-position 'point-at-eol)))

(defun xslt-process-escape (string)
  "Escape double quotes within STRING."
  (let ((lst (split-string string "\"")))
    (concat "\"" (mapconcat (lambda (x) x) lst "\\\"") "\"")))

(defun xslt-process-unescape (string)
  "Translate the escape sequences in the corresponding characters."
  (save-excursion
    (let ((tmpbuf (get-buffer-create " xslt-process-temp"))
	  (rex "\\(\\\\\\\\\\|\\\\b\\|\\\\d\\|\\\\e\\)"))
      (set-buffer tmpbuf)
      (erase-buffer)
      (goto-char (point-min))
      (insert-string string)
      (goto-char (point-min))
      (while (re-search-forward rex (point-max) t)
	(goto-char (- (point) 2))
	(cond ((looking-at "\\\\b") (replace-match "^"))
	      ((looking-at "\\\\d") (replace-match "\""))
	      ((looking-at "\\\\e") (replace-match "$"))
	      ((looking-at "\\\\\\\\") (replace-match "\\\\"))))
      (buffer-string))))

(defvar xslt-process-dir-separator
  (if (eq system-type 'windows-nt) "\\" "/")
  "The directory separator. Usually / on Unix systems and \\ on Windows.")

(defun xslt-process-make-glyph (glyph)
  "GNU Emacs compatibility function for the make-glyph function of XEmacs."
  (if (featurep 'xemacs)
      (make-glyph glyph)
    glyph))

(defconst xslt-process-home-web-site "http://xslt-process.sourceforge.net/"
  "XSLT-process' home on the Web.")

(defconst xslt-process-web-mailing-list
  "https://sourceforge.net/mail/?group_id=23425"
  "The public mailing lists of the XSLT-process mode.")

(defconst xslt-process-maintainer-address "ovidiu@xemacs.org"
  "The email address of the current maintainer.")

(defconst xslt-process-mailing-list "xslt-process-users@lists.sourceforge.net"
  "The email address of the mailing list.")

(defun xslt-process-find-xslt-data-directory ()
  "Return the path to the xslt-process directory. On XEmacs check
whether XSLT-process is installed as a package, in which case the
directory structure looks a little different."
  (let ((xemacs-dir (if (featurep 'xemacs)
			(let ((dir (locate-data-directory "xslt-process")))
			  (when dir
			    (setq dir (file-truename dir)))
			  dir)
		      nil))
	(package-dir (file-truename (concat (file-name-directory
					     (locate-library "xslt-process"))
					    ".." xslt-process-dir-separator))))
    (if (null xemacs-dir)
	package-dir
      (if (equal xemacs-dir package-dir)
	  xemacs-dir
	package-dir))))

;; Add the directory containing the images to the load-path, so
;; speedbar can find the image files.
(pushnew (concat (xslt-process-find-xslt-data-directory) "etc") load-path)

;; Import speedbar here, so that it finds the resource images
(require 'xslt-speedbar)

;; From "custom" web page at http://www.dina.dk/~abraham/custom/
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

;;; User defaults

(defgroup xslt-process nil
  "Run an XSLT processor on an Emacs buffer."
  :group 'tools
  :prefix "xslt-process-")

(defgroup xslt-process-key-bindings nil
  "The key bindings of the XSLT-process mode."
  :group 'xslt-process
  :prefix "xslt-process-")

(defcustom xslt-process-default-processor (list 'Saxon)
  "*The default XSLT processor to be applied to an XML document."
  :group 'xslt-process
  :type '(list
	  (radio-button-choice
	   (const :tag "Saxon 6.5.2" Saxon)
	   (const :tag "Xalan 2.4.1" Xalan))))

(defcustom xslt-process-fop-log-level (list 'info)
  "*The logging level used when running FOP."
  :group 'xslt-process
  :type '(list
	  (radio-button-choice
	   (const :tag "Debug" debug)
	   (const :tag "Info" info)
	   (const :tag "Warn" warn)
	   (const :tag "Error" error)
	   (const :tag "Fatal Error" fatal)
	   (const :tag "Disabled" disabled))))

(if (or (eq system-type 'windows-nt)
	(eq system-type 'cygwin32))
    (defcustom xslt-process-pdf-viewer (list 'xslt-process-win32-open-file)
      "*The PDF viewer under Windows platforms."
      :group 'xslt-process
      :type '(list
	      (choice
	       (const :tag "Default Windows PDF viewer"
		      xslt-process-win32-open-file)
	       (string :tag "Other"))))
  (defcustom xslt-process-pdf-viewer (list 'xpdf)
    "*The PDF viewer to be used when viewing PDF documents."
    :group 'xslt-process
    :type '(list
	    (choice
	     (const :tag "xpdf" xpdf)
	     (const :tag "Acrobat Reader" acroread)
	     (string :tag "Other")))))

(defun xslt-process-win32-open-file (file)
  "*Function to invoke the default registered PDF viewer on FILE."
  (if (featurep 'xemacs)
      (mswindows-shell-execute "open" file)
    (w32-shell-execute "open" file)))

(defun xslt-process-set-default-processor (processor)
  "Set the default XSLT processor to be used."
  (interactive)
  (setq xslt-process-default-processor processor))

(defun xslt-process-create-xslt-processor-submenu ()
  "Return the submenu with the available XSLT processors."
  (let* ((custom-type (plist-get (symbol-plist 'xslt-process-default-processor)
				 'custom-type))
	 (processor-list (cdadr custom-type)))
    (mapcar
     (lambda (x)
       (let ((name (caddr x))
	     (sym (car (cdddr x))))
	 (vector name `(xslt-process-set-default-processor (list ',sym))
		 :style 'radio
		 :selected `(equal xslt-process-default-processor
				   (list ',sym)))))
     processor-list)))

(defcustom xslt-process-registered-stylesheets nil
  "*List of registered stylesheets."
  :group 'xslt-process
  :type '(repeat (file :must-match t :tag "Filename")))

(defcustom xslt-process-xml-xslt-associations nil
  "*List of associations between XML source documents and XSLT stylesheets.
Each XML document will use the XSLT stylesheet declared as a value "
  :group 'xslt-process
  :type '(repeat (cons 
		  :tag "Enter an association between XML and XSLT files:"
		  (file :must-match t :tag "XML file name")
		  (choice
		   :tag "XSLT"
		   :value default
		   (const :tag "Associated stylesheet" default)
		   (file :must-match t :tag "File name")))))

;;;
;;; Disable Cocoon for the moment until we figure out how to hook up
;;; the debugger to it.
;;;

;(defcustom xslt-process-cocoon1-properties-file ""
;  "*The location of the Cocoon 1.x properties file."
;  :group 'xslt-process
;  :type '(file :must-match t :tag "Properties file"))

(defcustom xslt-process-java-program "java"
  "*The name of the java program to invoke when starting the XSLT processor."
  :group 'xslt-process
  :type '(string :tag "Program"))

(defcustom xslt-process-transform-parameters nil
  "*Specify parameter values.
Enter the parameters which get passed to the transformation."
  :group 'xslt-process
  :type '(repeat (cons :tag "Parameter"
		  (string :tag "Name") 
		  (string :tag "Value"))))

(defcustom xslt-process-jvm-arguments nil
  "*Additional arguments to be passed to the JVM.
Use this option to pass additional arguments to the JVM that might be
needed for the XSLT processor to function correctly."
  :group 'xslt-process
  :type '(repeat (string :tag "Argument")))

(defcustom xslt-process-jvm-option-properties nil
  "*Specify property values.
Enter the properties which get passed to the Java Virtual Machine as 
\"-D\" properties."
  :group 'xslt-process
  :type '(repeat (cons :tag "Property"
		  (string :tag "Name") 
		  (string :tag "Value"))))

(defcustom xslt-process-additional-classpath nil
  "*Additional Java classpath to be passed to the JVM that runs the
XSLT processor. Note that modifying this won't have any effect until
you restart the XSLT process. You can do this by explicitly quitting
the XSLT process using the menubar.

You need to setup this only if you plan on using extension functions
which make use of your own Java classes."
  :group 'xslt-process
  :type '(repeat (file :must-match t :tag "Path")))


(defcustom xslt-process-invoke-buffer-view "\C-c\C-xv"
  "*Keybinding for invoking the XSLT processor, and viewing the results
in a buffer.
To enter a normal key, enter its corresponding character. To enter a
key with a modifier, either type C-q followed by the desired modified
keystroke, e.g. C-q C-c to enter Control c. To enter a function key,
use the [f1], [f2] etc. notation."
  :group 'xslt-process-key-bindings
  :type '(string :tag "Key"))

(defcustom xslt-process-invoke-browser-view "\C-c\C-xn"
  "*Keybinding for invoking the XSLT processor and viewing the results
in a browser."
  :group 'xslt-process-key-bindings
  :type '(string :tag "Key"))

(defcustom xslt-process-invoke-pdf-viewer "\C-c\C-xp"
  "*Keybinding for invoking the XSLT processor, followed by the FOP
processor and viewing the results in a PDF viewer."
  :group 'xslt-process-key-bindings
  :type '(string :tag "Key"))

(defcustom xslt-process-toggle-debug-mode "\C-c\C-xd"
  "*Keybinding for toggling the debug mode."
  :group 'xslt-process-key-bindings
  :type '(string :tag "Key"))

(defcustom xslt-process-register-buffer "\C-c\C-xb"
  "*Keybinding for registering the current buffer in the stylesheet registry."
  :group 'xslt-process-key-bindings
  :type '(string :tag "Key"))

(defcustom xslt-process-unregister-buffer "\C-c\C-xu"
  "*Keybinding for unregistering the current buffer from the
stylesheet registry."
  :group 'xslt-process-key-bindings
  :type '(string :tag "Key"))

(defcustom xslt-process-manage-stylesheets "\C-c\C-xm"
  "*Keybinding to show the stylesheet manager buffer."
  :group 'xslt-process-key-bindings
  :type '(string :tag "Key"))

(defcustom xslt-process-manage-associations "\C-c\C-xh"
  "*Keybinding to show the XML/XSLT associations buffer."
  :group 'xslt-process-key-bindings
  :type '(string :tag "Key"))

(defcustom xslt-process-load-registry "\C-c\C-xo"
  "*Keybinding to load a stylesheet registry from a file."
  :group 'xslt-process-key-bindings
  :type '(string :tag "Key"))

(defcustom xslt-process-save-registry "\C-c\C-xs"
  "*Keybinding to save a stylesheet registry to a file."
  :group 'xslt-process-key-bindings
  :type '(string :tag "Key"))

(defcustom xslt-process-associate-stylesheet "\C-c\C-xa"
  "*Keybinding to associate an XML file with a previously registered
XSLT stylesheet."
  :group 'xslt-process-key-bindings
  :type '(string :tag "Key"))

(defcustom xslt-process-set-breakpoint-key "b"
  "*Keybinding for setting up a breakpoint at line in the current buffer.
The buffer has to be in the debug mode for this key to work."
  :group 'xslt-process-key-bindings
  :type '(string :tag "Key"))

(defcustom xslt-process-delete-breakpoint-key "d"
  "*Keybinding for deleting the breakpoint at line in the current buffer.
The buffer has to be in the debug mode for this key to work"
  :group 'xslt-process-key-bindings
  :type '(string :tag "Key"))

(defcustom xslt-process-enable-disable-breakpoint-key "e"
  "*Keybinding for enabling or disabling the breakpoint at line in the
current buffer. The buffer has to be in the debug mode for this key
to work"
  :group 'xslt-process-key-bindings
  :type '(string :tag "Key"))

(defcustom xslt-process-quit-debug-key "q"
  "*Keybinding for exiting from the debug mode. The buffer has to be
in the debug mode for this key to work."
  :group 'xslt-process-key-bindings
  :type '(string :tag "Key"))

(defcustom xslt-process-do-run-key "r"
  "*Keybinding for running the XSLT debugger on an XML file. The
buffer has to be in the debug mode for this key to work."
  :group 'xslt-process-key-bindings
  :type '(string :tag "Key"))

(defcustom xslt-process-do-step-key "s"
  "*Keybinding for doing STEP in the debug mode. The buffer has to be
in the debug mode for this key to work."
  :group 'xslt-process-key-bindings
  :type '(string :tag "Key"))

(defcustom xslt-process-do-next-key "n"
  "*Keybinding for doing NEXT in the debug mode. The buffer has to be
in the debug mode for this key to work."
  :group 'xslt-process-key-bindings
  :type '(string :tag "Key"))

(defcustom xslt-process-do-finish-key "f"
  "*Keybinding for doing FINISH in the debug mode. The buffer has to be
in the debug mode for this key to work."
  :group 'xslt-process-key-bindings
  :type '(string :tag "Key"))

(defcustom xslt-process-do-continue-key "c"
  "*Keybinding for doing CONTINUE in the debug mode. The buffer has to be
in the debug mode for this key to work."
  :group 'xslt-process-key-bindings
  :type '(string :tag "Key"))

(defcustom xslt-process-do-stop-key "a"
  "*Keybinding for aborting a long XSLT processing. The buffer has to
be in debug mode for this key to work."
  :group 'xslt-process-key-bindings
  :type '(string :tag "Key"))

(defcustom xslt-process-do-quit-key "x"
  "*Keybing for exiting the debugger process. The buffer has to be in
debug mode for this key to work."
  :group 'xslt-process-key-bindings
  :type '(string :tag "Key"))

(defface xslt-process-enabled-breakpoint-face
  '((((class color) (background light))
     (:foreground "purple" :background "salmon")))
  "*Face used to highlight enabled breakpoints."
  :group 'font-lock-highlighting-faces)

(defface xslt-process-disabled-breakpoint-face
  '((((class color) (background light))
     (:foreground "purple" :background "wheat3")))
  "*Face used to highlight disabled breakpoints."
  :group 'font-lock-highlighting-faces)

(defface xslt-process-current-line-face
  '((((class color) (background light))
     (:foreground "black" :background "orange")))
  "*Face used to highlight disabled breakpoints."
  :group 'font-lock-highlighting-faces)

(defface xslt-process-indicator-face
  '((((class color) (background light))
     (:foreground "forest green" :background "black")))
  "*Face used to display the enter or exit in an element during debugging."
  :group 'font-lock-highlighting-faces)

;;;###autoload
(defcustom xslt-process-mode-line-string " XSLT"
  "*String displayed in the modeline when the xslt-process minor
mode is active. Set this to nil if you don't want a modeline
indicator."
  :group 'xslt-process
  :type 'string)

(defcustom xslt-process-debug-mode-line-string " XSLTd"
  "*String to appear in the modeline when the XSLT debug mode is active."
  :group 'xslt-process
  :type 'string)

;;; End of user customizations


;;; Other definitions
(defun xslt-process-temp-directory ()
  "Return the temporary directory on this machine."
  (or (if (fboundp 'temp-directory) (temp-directory))
      (if (boundp 'temporary-file-directory)
	  temporary-file-directory)))

;;;###autoload
(defvar xslt-process-mode nil
  "Indicates whether the current buffer is in the XSLT-process minor mode.")

;;;###autoload
(defvar xslt-process-debug-mode nil
  "Indicates whether the current buffer is in debug mode.")

;; Keymaps
;;;###autoload
(defvar xslt-process-mode-map (make-sparse-keymap)
  "Keyboard bindings in normal XSLT-process mode enabled buffers.")

;;;###autoload
(defvar xslt-process-debug-mode-map (make-sparse-keymap)
  "Keyboard bindings for the XSLT debug mode.")

(set-keymap-parent xslt-process-debug-mode-map xslt-process-mode-map)

(make-variable-buffer-local 'xslt-process-mode)
(make-variable-buffer-local 'xslt-process-debug-mode)
(make-variable-buffer-local 'minor-mode-alist)

;; State variables of the mode
(defvar xslt-process-breakpoints (make-hash-table :size 10 :test 'equal)
  "Hash table containing the currently defined breakpoints.")

(defvar xslt-process-comint-process nil
  "The XSLT debugger process.")

(defvar xslt-process-comint-buffer nil
  "The buffer within which the XSLT debugger process runs.")

(defvar xslt-process-selected-position [nil nil nil nil nil nil]
  "An array containing the filename, line, column, extent, annotation
and enter or exit action, describing the line where the debugger
stopped last time.")

(defvar xslt-process-debugger-process-started nil
  "Whether the XSLT debugger process has been started.")

(defvar xslt-process-output-generated nil
  "Boolean value indicating whether any output has been generated
during this XSLT processing.")

(defvar xslt-process-process-state 'not-running
  "The state of the process. It can be either not-running, running or
stopped.")

(defvar xslt-process-debugger-running nil
  "If the `xslt-process-process-state' is set to 'running, this
indicates whether the XSLT debugger is running. If this value is nil,
it indicates that a normal, no debug, XSLT processing is happening.")

(defvar xslt-process-source-frames-stack nil
  "The stack of source frames, an array of entries consisting of a
display name, a file name and a line number, among other things.")

(defvar xslt-process-style-frames-stack nil
  "The stack of style frames, an array of entries consisting of a
display name, a file name and a line number, among other things.")

(defvar xslt-process-global-variables nil
  "The global variables of the current stylesheet.")

(defvar xslt-process-local-variables nil
  "The local variables for the currently selected style frame.")

(defvar xslt-process-breakpoint-extents (make-hash-table :size 10 :test 'equal)
  "Hash table of extents indexed by (filename . lineno). It is used to
keep track of the extents created to highlight lines.")

(defvar xslt-process-execution-context-error-function nil
  "Function to be called by the `xslt-process-report-error'. This
function should reset the proper state depending on the debugger
operation that was invoked.")

(defvar xslt-process-results-process nil
  "The Emacs process object that holds the connection with the socket
on which the XSLT results come from the XSLT processor.")

(defvar xslt-process-message-process nil
  "The Emacs process object that holds the connection with the socket
on which the XSLT messages (generated with xsl:message) come from the
XSLT processor.")

(defvar xslt-process-current-processor
  (symbol-name (car xslt-process-default-processor))
  "The current XSLT processor being used to do the
processing. Changing the default processor through customization has
effect on this variable only the next time the processor starts a new
job.")

(defvar xslt-process-external-java-libraries
  (mapcar (lambda (f)
	    (concat (xslt-process-find-xslt-data-directory)
		      "java" xslt-process-dir-separator f))
	  '("saxon-6.5.2.jar" "bsf.jar" "xalan.jar" "xercesImpl.jar" "xml-apis.jar"
	    "batik.jar" "fop.jar" "avalon-framework-cvs-20020315.jar" "xslt.jar"))
  "Defines the classpath to the XSLT processors that do the real work
of processing an XML document. Be sure you know what you're doing when
you modify this.")

(defvar xslt-process-error-messages nil
  "Collects all the error messages reported during the XSLT
processing. Used only in submitting bug reports.")

;; Other definitions
(defvar xslt-process-comint-process-name "xslt"
  "The name of the comint process.")

(defvar xslt-process-results-process-name "xslt results"
  "The name of the process that receives the result of the XSLT
processing.")

(defvar xslt-process-message-process-name "xslt messages"
  "The name of the process that receives the messages generated using
xsl:message during the XSLT processing.")

(defvar xslt-process-results-buffer-name "*xslt results*"
  "The name of the buffer to which the output of the XSLT processing
goes to.")

(defvar xslt-process-errors-buffer-name "*xslt errors*"
  "The name of the buffer to which error messages generated during XML
parsing or XSLT processing go to.")

(defvar xslt-process-message-buffer-name "*xslt messages*"
  "The name of the buffer to which the messages generated using
xsl:message go to.")

(defvar xslt-process-enter-glyph (xslt-process-make-glyph "=>")
  "Graphic indicator for entering inside an element.")

(defvar xslt-process-exit-glyph (xslt-process-make-glyph "<=")
  "Graphic indicator for exiting from an element.")

(defvar xslt-process-breakpoint-set-hooks nil
  "List of functions to be called after a breakpoint is set.")

(defvar xslt-process-breakpoint-removed-hooks nil
  "List of functions to be called after a breakpoint is removed.")

(defvar xslt-process-breakpoint-enabled-disabled-hooks nil
  "List of functions to be called after a breakpoint is enabled or
disabled.")

(defvar xslt-process-source-frames-changed-hooks nil
  "List of functions to be called when the source frame stack
changes. The functions should take one argument, a list of (name
filename line) that indicate the new source frame stack.")

(defvar xslt-process-style-frames-changed-hooks nil
  "List of functions to be called when the style frame stack
changes. The functions should take one argument, a list of (name
filename line) that indicate the new style frame stack.")

(defvar xslt-process-global-variables-changed-hooks nil
  "List of functions to be called when the global variables change. The
hook functions should take no argument.")

(defvar xslt-process-local-variables-changed-hooks nil
  "List of functions to be called when the local variables change. The
hook functions should take no argument.")

(defvar xslt-process-xslt-processing-finished-hooks nil
  "List of functions to be called when the XSLT processing finishes.")

;; Setup the main keymap
(define-key xslt-process-mode-map
  xslt-process-invoke-buffer-view 'xslt-process-invoke-buffer-view)
(define-key xslt-process-mode-map
  xslt-process-invoke-browser-view 'xslt-process-invoke-browser-view)
(define-key xslt-process-mode-map
  xslt-process-invoke-pdf-viewer 'xslt-process-invoke-pdf-viewer)
(define-key xslt-process-mode-map
  xslt-process-toggle-debug-mode 'xslt-process-toggle-debug-mode)
(define-key xslt-process-mode-map
  xslt-process-register-buffer 'xslt-process-register-buffer)
(define-key xslt-process-mode-map
  xslt-process-unregister-buffer 'xslt-process-unregister-buffer)
(define-key xslt-process-mode-map
  xslt-process-manage-stylesheets 'xslt-process-manage-stylesheets)
(define-key xslt-process-mode-map
  xslt-process-load-registry 'xslt-process-load-registry)
(define-key xslt-process-mode-map
  xslt-process-save-registry 'xslt-process-save-registry)
(define-key xslt-process-mode-map
  xslt-process-associate-stylesheet 'xslt-process-associate-stylesheet)
(define-key xslt-process-mode-map
  xslt-process-manage-associations 'xslt-process-manage-associations)

;; Setup the keymap used for debugging
(define-key xslt-process-debug-mode-map
  xslt-process-quit-debug-key 'xslt-process-quit-debug)
(define-key xslt-process-debug-mode-map
  xslt-process-set-breakpoint-key 'xslt-process-set-breakpoint)
(define-key xslt-process-debug-mode-map
  xslt-process-delete-breakpoint-key 'xslt-process-delete-breakpoint)
(define-key xslt-process-debug-mode-map
  xslt-process-enable-disable-breakpoint-key
  'xslt-process-enable-disable-breakpoint)
(define-key xslt-process-debug-mode-map
  xslt-process-do-run-key 'xslt-process-do-run)
(define-key xslt-process-debug-mode-map
  xslt-process-do-step-key 'xslt-process-do-step)
(define-key xslt-process-debug-mode-map
  xslt-process-do-next-key 'xslt-process-do-next)
(define-key xslt-process-debug-mode-map
  xslt-process-do-finish-key 'xslt-process-do-finish)
(define-key xslt-process-debug-mode-map
  xslt-process-do-continue-key 'xslt-process-do-continue)
(define-key xslt-process-debug-mode-map
  xslt-process-do-stop-key 'xslt-process-do-stop)
(define-key xslt-process-debug-mode-map
  xslt-process-do-quit-key 'xslt-process-do-quit)

(defvar xslt-process-menu-definition
  (list "XSLT"
	["Apply XSLT, View in buffer" xslt-process-invoke-buffer-view
	 :active t]
	["Apply XSLT, View in browser" xslt-process-invoke-browser-view
	 :active t]
	["Apply XSLT, View PDF" xslt-process-invoke-pdf-viewer
	 :active t]
	(list "Stylesheets Registry"
	      ["Register XSLT buffer" xslt-process-register-buffer :active t]
	      ["Unregister XSLT buffer" xslt-process-unregister-buffer
	       :active t]
	      ["Associate XSLT with XML buffer..."
	       xslt-process-associate-stylesheet :active t]
	      "--"
	      ["Manage stylesheets..."
	       xslt-process-manage-stylesheets :active t]
	      ["Manage XML/XSLT associations..."
	       xslt-process-manage-associations :active t]
	      ["Save registry" customize-save-customized :active t])

	"--"

	["Toggle debug mode" xslt-process-toggle-debug-mode :active t]
	["Set breakpoint" xslt-process-set-breakpoint
	 :active (and xslt-process-debug-mode
		      (not (xslt-process-is-breakpoint
			    (xslt-process-new-breakpoint-here))))]
	["Delete breakpoint" xslt-process-delete-breakpoint
	 :active (and xslt-process-debug-mode
		      (xslt-process-is-breakpoint
		       (xslt-process-new-breakpoint-here)))]
	["Breakpoint enabled" xslt-process-enable-disable-breakpoint
	 :active (and xslt-process-debug-mode
		      (xslt-process-is-breakpoint
		       (xslt-process-new-breakpoint-here)))
	 :style toggle
	 :selected (and xslt-process-debug-mode
			(xslt-process-breakpoint-is-enabled
			 (xslt-process-new-breakpoint-here)))]
	"--"
	["Run debugger" xslt-process-do-run
	 :active xslt-process-debug-mode]
	["Step" xslt-process-do-step
	 :active (eq xslt-process-process-state 'stopped)]
	["Next" xslt-process-do-next
	 :active (eq xslt-process-process-state 'stopped)]
	["Finish" xslt-process-do-finish
	 :active (eq xslt-process-process-state 'stopped)]
	["Continue" xslt-process-do-continue
	 :active (eq xslt-process-process-state 'stopped)]
	["Stop" xslt-process-do-stop
	 :active (eq xslt-process-process-state 'running)]

	"--"

	["Quit XSLT processor" xslt-process-do-quit
	 :active xslt-process-debugger-process-started]
	["Speedbar" xslt-process-speedbar-frame-mode
	 :style toggle
	 :selected (and (boundp 'speedbar-frame)
			(frame-live-p speedbar-frame)
			(frame-visible-p speedbar-frame))]

	(cons "XSLT Processor"
	      (xslt-process-create-xslt-processor-submenu))

	["Transformation parameters" xslt-process-manage-transform-parameters
	 :active t]

	(list "Customize"
	      ["XSLT Process"
	       (customize-group 'xslt-process)
	       :active t]
	      ["Faces"
	       (customize-apropos-faces "xslt-process-*")
	       :active t]
	      ["Key bindings"
	       (customize-group 'xslt-process-key-bindings)
	       :active t]
	      ["Browser"
	       (customize-variable 'browse-url-browser-function)
	       :active t]
	      ["PDF Viewer"
	       (customize-variable 'xslt-process-pdf-viewer)
	       :active t]
	      ["DocBook location" xslt-process-install-docbook
	       :active t])
	(list "Help"
	      (concat "XSLT-process " xslt-process-version)
	      "--"
	      ["Info file" (xslt-process-visit-info-file) :active t]
	      ["Home Web site"
	       (browse-url xslt-process-home-web-site)
	       :active t]
	      ["Mailing lists"
	       (browse-url xslt-process-web-mailing-list)
	       :active t]
	      ["Submit bug report" (xslt-process-submit-bug-report)
	       :active t]))
  "XSLT-process menu definition.")

(easy-menu-define xslt-process-menu xslt-process-mode-map
		  "XSLT-process menu"
		  xslt-process-menu-definition)
;(easy-menu-define xslt-process-debug-menu xslt-process-debug-mode-map
;		  "XSLT-process debug menu" xslt-process-menu-definition)

(defun xslt-process-visit-info-file ()
  "Visit the info file for XSLT-process."
  (require 'info)
  (Info-find-node (concat (xslt-process-find-xslt-data-directory)
			  "doc" xslt-process-dir-separator "xslt-process.info")
		  "Top"))

(defun xslt-process-dump-hashtable (sym buffer)
  "Dump a hash table in BUFFER. Used by the
`xslt-process-submit-bug-report' function."
  (let* ((symname (symbol-name sym))
	 (value (symbol-value sym))
	 (count (hash-table-count value)))
    (insert (format "%s (hash table, %s entries:" symname count))
    (cl-maphash (lambda (key value)
		  (insert-string (format " (%s: %s)" key value)))
		value)
    (insert ")\n")))

(defun xslt-process-submit-bug-report ()
  "Submit a bug report against the current version of XSLT-process."
  (require 'reporter)
  (let ((address (concat xslt-process-maintainer-address
			 ", " xslt-process-mailing-list))
	(pkgname (concat "XSLT-process " xslt-process-version))
	(reporter-prompt-for-summary-p "Please enter a short description: "))
     (reporter-submit-bug-report
      address
      pkgname
      (list (cons 'xslt-process-breakpoints 'xslt-process-dump-hashtable)
	    'xslt-process-comint-process
	    'xslt-process-comint-buffer
	    'xslt-process-selected-position
	    'xslt-process-debugger-process-started
	    'xslt-process-process-state
	    'xslt-process-debugger-running
	    'xslt-process-source-frames-stack
	    'xslt-process-style-frames-stack
	    'xslt-process-local-variables
	    'xslt-process-global-variables
	    (cons 'xslt-process-breakpoint-extents 'xslt-process-dump-hashtable)
	    'xslt-process-execution-context-error-function
	    'xslt-process-results-process
	    'xslt-process-current-processor
	    'xslt-process-default-processor
	    'xslt-process-external-java-libraries
	    'xslt-process-error-messages
	    'xslt-process-registered-stylesheets
	    'xslt-process-xml-xslt-associations))))

;;;###autoload
(defun xslt-process-mode (&optional arg)
  "Minor mode to invoke an XSLT processor on the current buffer.

The XSLT processor can be run either to process the file and show the
results in a different buffer, or it can be run in debug mode. In this
mode you can actually see what the XSLT processor is doing, you can
setup breakpoints, look at local and global variables, essentially all
the operations a debugger can do.

With no argument, this command toggles the xslt-process mode. With a
prefix argument ARG, turn xslt-process minor mode on iff ARG is
positive.

Bindings:

\\[xslt-process-mode]: Toggle the XSLT minor mode on the current buffer.
\\[xslt-process-toggle-debug-mode]: Toggle the debug mode on the current buffer.

\\[xslt-process-invoke-buffer-view]: Invoke the XSLT processor on the
current buffer, and view the results in a buffer.
\\[xslt-process-invoke-browser-view]: Invoke the XSLT processor on the
current buffer, and view the results in a browser.
\\[xslt-process-invoke-pdf-viewer]: Assume the resulting XML document is
an XML FO document. This command runs the Apache FOP processor on it
and displays the result in a PDF reader of choice.
\\[xslt-process-do-run]: Start the XSLT debugger on the current
buffer.

\\[xslt-process-set-breakpoint]: Set a breakpoint at the current line.
\\[xslt-process-delete-breakpoint]: Delete breakpoint at the current line.
\\[xslt-process-enable-disable-breakpoint]: Disable or enable breakpoint.

\\[xslt-process-do-step]: Step into the current element.
\\[xslt-process-do-next]: Step over the current element.
\\[xslt-process-do-finish]: Exit from the current xsl:template.
\\[xslt-process-do-continue]: Continue the XSLT processing until the
next breakpoint, or until the processing ends.
\\[xslt-process-do-stop]: Stop the XSLT processor while it is
running. This may be useful for long processings in which you forgot
to setup breakpoints.
\\[xslt-process-do-quit]: Exit the XSLT processor.

Hooks:
`xslt-process-breakpoint-set-hooks' is run each time a breakpoint is set.
`xslt-process-breakpoint-removed-hooks' is run each time a breakpoint
is removed.
`xslt-process-breakpoint-enabled-disabled-hooks' is run each time a
breakpoint is enabled or disabled.
`xslt-process-source-frames-changed-hooks' is run when the XML
document element path changes.
`xslt-process-style-frames-changed-hooks' is run when the XSLT frames change.
`xslt-process-global-variables-changed-hooks' is run when the global
variables change.
`xslt-process-local-variables-changed-hooks' is run when the local
variables change.

For more information please check:

xslt-process:    http://xslt-process.sourceforge.net/

\\{xslt-process-mode-map}"
  (interactive "P")
  (setq xslt-process-mode
	(if (null arg)
	    (not xslt-process-mode)
	  (> (prefix-numeric-value arg) 0)))
  (if xslt-process-mode
      (progn
	(setq easy-menu-precalculate-equivalent-keybindings nil)
	(easy-menu-add xslt-process-menu)
	(xslt-process-setup-minor-mode xslt-process-mode-map
				       xslt-process-mode-line-string)
	)
    (setq minor-mode-alist (remassoc 'xslt-process-mode minor-mode-alist))
    (setq minor-mode-map-alist
	  (remassoc 'xslt-process-mode minor-mode-map-alist))
    (easy-menu-remove '("XSLT"))
    (xslt-process-toggle-debug-mode 0))
  ;; Force modeline to redisplay
  (force-mode-line-update))

(defun xslt-process-toggle-debug-mode (arg)
  "*Setup a buffer in the XSLT debugging mode.
This essentially makes the buffer read-only and binds various keys to
different actions for faster operations."
  (interactive "P")
  (block nil
    (let ((filename (urlize (file-truename (buffer-file-name)))))
      (if (or (and arg (> (prefix-numeric-value arg) 0))
	      (not xslt-process-debug-mode))
	  (progn
	    ;; If debug mode already enabled, don't do anything
	    (if xslt-process-debug-mode
		(return))
	    ;; Enable the debug mode
	    (setq xslt-process-debug-mode t)
	    (toggle-read-only 1)
	    (if (equal filename (xslt-process-selected-position-filename))
		(xslt-process-change-current-line-highlighting t))
	    (xslt-process-change-breakpoints-highlighting t filename)
	    (xslt-process-setup-minor-mode xslt-process-debug-mode-map
					   xslt-process-debug-mode-line-string))
	;; If debug mode already disabled, don't do anything
	(if (not xslt-process-debug-mode)
	    (return))
	(toggle-read-only 0)
	(if (equal filename (xslt-process-selected-position-filename))
	    (xslt-process-change-current-line-highlighting nil))
	(xslt-process-change-breakpoints-highlighting nil filename)
	(xslt-process-setup-minor-mode xslt-process-mode-map
				       xslt-process-mode-line-string)
	;; Disable the debug mode if it's enabled
	(setq xslt-process-debug-mode nil)))))

(defun xslt-process-quit-debug ()
  "*Quit the debugger and exit from the xslt-process mode."
  (interactive)
  (xslt-process-toggle-debug-mode 0))

(put 'Saxon 'additional-params 'xslt-saxon-additional-params)
(put 'Xalan 'additional-params 'xslt-xalan-additional-params)
;(put 'Cocoon1 'additional-params 'xslt-cocoon1-additional-params)

(defun xslt-saxon-additional-params ())
(defun xslt-xalan-additional-params ())

;; Disable Cocoon support for now

;(defun xslt-cocoon1-additional-params ()
;  (if (or (null xslt-process-cocoon1-properties-file)
;	  (equal xslt-process-cocoon1-properties-file ""))
;      (error "No Cocoon properties file specified."))
;  (bsh-eval (concat "xslt.Cocoon1.setPropertyFilename(\""
;		    xslt-process-cocoon1-properties-file "\");"))
;  (setq cocoon-user-agent
;	(if (and
;	     (local-variable-p 'user-agent (current-buffer))
;	     (boundp 'user-agent))
;	    (if (stringp user-agent)
;		user-agent
;	      (symbol-name user-agent))
;	  nil))
;  (bsh-eval (concat "xslt.Cocoon1.setUserAgent(\""
;		    cocoon-user-agent "\");"))
;  (makunbound 'user-agent))

(defun xslt-process-invoke-buffer-view ()
  "*Invokes the XSLT processor of your choice on the current buffer,
and view the results in a buffer."
  (interactive)
  (if (equal (xslt-process-do-run t) 'started)
      (setq xslt-process-xslt-processing-finished-hooks nil)))

(defun xslt-process-invoke-browser-view ()
  "*Invokes the XSLT processor of your choice on the current buffer,
and view the results in a buffer."
  (interactive)
  (let ((filename (expand-file-name
		   "xslt-process-output.html"
		   (xslt-process-temp-directory))))
    (if (equal (xslt-process-do-run t filename) 'started)
	(progn
	  (setq xslt-process-xslt-processing-finished-hooks nil)
	  (add-hook 'xslt-process-xslt-processing-finished-hooks
		    'xslt-process-do-invoke-browser)))))

(defun xslt-process-do-invoke-browser ()
  "Private function invoked to view the file in a browser, after the
XSLT processing finishes."
  (browse-url (concat "file:" xslt-process-output-to-filename)))

(defun xslt-process-invoke-pdf-viewer ()
  "*Assumes the XSLT transformation will generate an XSL FO document.
This command applies the XSLT transformation on the original XML file,
and on the resulting XSL FO document applies the Apache FOP, to obtain
a PDF document. The PDF document is viewed with the PDF viewer
specified by `xslt-process-pdf-viewer'."
  (interactive)
  (let ((fo-filename (expand-file-name
		      "xslt-process-output.fo"
		      (xslt-process-temp-directory))))
    (if (equal (xslt-process-do-run t fo-filename) 'started)
	(progn
	  (setq xslt-process-xslt-processing-finished-hooks nil)
	  (add-hook 'xslt-process-xslt-processing-finished-hooks
		    'xslt-process-do-invoke-fop-processor)))))

(defun xslt-process-do-invoke-fop-processor ()
  "Private function used by `xslt-process-invoke-pdf-viewer' at the
end of the XSLT processing to continue with the FOP processing."
  (let ((fo-filename (expand-file-name
		      "xslt-process-output.fo"
		      (xslt-process-temp-directory)))
	(pdf-filename (expand-file-name
		       "xslt-process-output.pdf"
		       (xslt-process-temp-directory)))
	(log-level (symbol-name (car xslt-process-fop-log-level))))
    (message "XSLT processing finished, running FOP processor...")
    ;; Set the internal state to 'running' to avoid starting a new
    ;; processing in the meantime.
    (setq xslt-process-process-state 'running)
    (setq xslt-process-xslt-processing-finished-hooks nil)
    (add-hook 'xslt-process-xslt-processing-finished-hooks
	      'xslt-process-do-invoke-pdf-viewer)
    (xslt-process-send-command
     (format "toPDF -%s -xml %s -o %s" log-level
	     fo-filename pdf-filename))))

(defun xslt-process-do-invoke-pdf-viewer ()
  "Private function invoked at the end of the FOP processing to start
the PDF viewer."
  (let ((pdf-viewer
	 (cond ((functionp (car xslt-process-pdf-viewer))
		(car xslt-process-pdf-viewer))
	       ((symbolp (car xslt-process-pdf-viewer))
		(symbol-name (car xslt-process-pdf-viewer)))
	       (t
		(car xslt-process-pdf-viewer))))
	(pdf-filename (expand-file-name
		       "xslt-process-output.pdf"
		       (xslt-process-temp-directory))))
    (setq xslt-process-xslt-processing-finished-hooks nil)
    (message "Starting '%s'  PDF viewer..." pdf-viewer)
    (if (functionp pdf-viewer)
	(funcall pdf-viewer pdf-filename)
      (start-process "pdf viewer" nil pdf-viewer pdf-filename))
    (message "Starting PDF viewer '%s'...done." pdf-viewer)))

(defun xslt-process-display-messages (messages msg-buffer out-buffer)
  (set-buffer msg-buffer)
  (insert messages)
  (let ((msg-window (get-buffer-window msg-buffer))
	(out-window (get-buffer-window out-buffer)))
    (if (not msg-window)
	(split-window out-window))
    (display-buffer msg-buffer)))  

;;;
;;; The last selected position
;;;

(defun xslt-process-selected-position-filename (&optional filename)
  "Return the filename of the last selected position."
  (if filename
      (aset xslt-process-selected-position 0 filename))
  (aref xslt-process-selected-position 0))

(defun xslt-process-selected-position-line (&optional position)
  "Return the line of the last selected position."
  (if position
      (aset xslt-process-selected-position 1 position))
  (aref xslt-process-selected-position 1))

(defun xslt-process-selected-position-column (&optional column)
  "Return the column of the last selected position."
  (if column
      (aset xslt-process-selected-position 2 column))
  (aref xslt-process-selected-position 2))

(defun xslt-process-selected-position-extent (&optional extent)
  "Return the extent used to highlight the last selected position."
  (if extent
      (aset xslt-process-selected-position 3 extent))
  (aref xslt-process-selected-position 3))

(defun xslt-process-selected-position-annotation (&optional annotation)
  "Return the extent used to highlight the last selected position."
  (if annotation
      (aset xslt-process-selected-position 4 annotation))
  (aref xslt-process-selected-position 4))

(defun xslt-process-selected-position-enter-exit (&optional action)
  "Return the extent used to highlight the last selected position. If
ACTION is non-nil, it is set as the new value."
  (if action
      (aset xslt-process-selected-position 5 action))
  (aref xslt-process-selected-position 5))

;;;
;;; Source and Style Frames
;;;

(defun xslt-process-frame-display-name (frame)
  "Returns the display name of frame."
  (aref frame 0))

(defun xslt-process-frame-file-name (frame)
  "Returns the file name of a frame."
  (aref frame 1))

(defun xslt-process-frame-line (frame)
  "Returns the line number of a frame."
  (aref frame 2))

(defun xslt-process-frame-is-exiting (frame)
  "Returns the is-exiting indicator."
  (aref frame 3))

(defun xslt-process-frame-position (frame)
  "Returns the position of this frame in the corresponding stack frame."
  (aref frame 4))

;;;
;;; Breakpoints
;;;

(defun xslt-process-new-breakpoint-here ()
  "Returns a breakpoint object at filename and line number of the
current buffer or nil otherwise. By default the breakpoint is enabled."
  (let ((filename (urlize (file-truename (buffer-file-name))))
	(line (save-excursion (progn (end-of-line) (count-lines 1 (point))))))
    (cons filename line)))

(defun xslt-process-intern-breakpoint (breakpoint state)
  "Interns BREAKPOINT into the internal hash table that keeps track of
breakpoints. STATE should be either t for an enabled breakpoint, or
nil for a disabled breakpoint."
  (cl-puthash breakpoint
	      (if state 'enabled 'disabled)
	      xslt-process-breakpoints))

(defun xslt-process-is-breakpoint (breakpoint)
  "Checks whether BREAKPOINT is setup in buffer at line."
  (not (eq (cl-gethash breakpoint xslt-process-breakpoints) nil)))

(defun xslt-process-breakpoint-is-enabled (breakpoint)
  "Returns t if BREAKPOINT is enabled, nil otherwise. Use
`xslt-process-is-breakpoint' before calling this method to find out
whether BREAKPOINT is a breakpoint or not. This method returns nil
either when the breakpoint doesn't exist or when the breakpoint is
disabled."
  (eq (cl-gethash breakpoint xslt-process-breakpoints) 'enabled))

(defun xslt-process-remove-breakpoint (breakpoint)
  "Remove BREAKPOINT from the internal data structures."
  (let ((filename (xslt-process-breakpoint-filename breakpoint))
	(line (xslt-process-breakpoint-line breakpoint)))
    (cl-remhash (cons filename line) xslt-process-breakpoints)))

(defun xslt-process-breakpoint-filename (breakpoint)
  "Returns the filename of the BREAKPOINT."
  (car breakpoint))

(defun xslt-process-breakpoint-line (breakpoint)
  "Return the line number of the BREAKPOINT."
  (cdr breakpoint))

(defun xslt-process-set-breakpoint ()
  "*Set a breakpoint at line in the current buffer or print an error
message if a breakpoint is already setup."
  (interactive)
  (let* ((breakpoint (xslt-process-new-breakpoint-here))
	 (filename (xslt-process-breakpoint-filename breakpoint))
	 (line (xslt-process-breakpoint-line breakpoint)))
    (if (xslt-process-is-breakpoint breakpoint)
	(message "Breakpoint already set in %s at %s" filename line)
      (xslt-process-send-command (format "b %s %s" filename line))
      (xslt-process-intern-breakpoint breakpoint t)
      (xslt-process-highlight-breakpoint breakpoint)
      (run-hooks 'xslt-process-breakpoint-set-hooks)
      (message "Set breakpoint in %s at %s." filename line))))

(defun xslt-process-delete-breakpoint ()
  "*Remove the breakpoint at current line in the selected buffer."
  (interactive)
  (let* ((breakpoint (xslt-process-new-breakpoint-here))
	 (filename (xslt-process-breakpoint-filename breakpoint))
	 (line (xslt-process-breakpoint-line breakpoint)))
    (if (xslt-process-is-breakpoint breakpoint)
	(progn
	  (xslt-process-remove-breakpoint breakpoint)
	  ;; Send the command to the XSLT debugger, but don't start it
	  ;; if it's not started.
	  (xslt-process-send-command (format "d %s %s" filename line) t)
	  (xslt-process-unhighlight-breakpoint breakpoint)
	  (run-hooks 'xslt-process-breakpoint-removed-hooks)
	  (message "Removed breakpoint in %s at %s." filename line))
      (message (format "No breakpoint in %s at %s" filename line)))))

(defun xslt-process-enable-disable-breakpoint ()
  "*Enable or disable the breakpoint at the current line in buffer, depending
on its state."
  (interactive)
  (let* ((breakpoint (xslt-process-new-breakpoint-here))
	 (filename (xslt-process-breakpoint-filename breakpoint))
	 (line (xslt-process-breakpoint-line breakpoint)))
    (if (xslt-process-is-breakpoint breakpoint)
	(progn
	  ;; Toggle the state of the breakpoint
	  (xslt-process-intern-breakpoint
	   breakpoint
	   (not (xslt-process-breakpoint-is-enabled breakpoint)))
	  ;; Change the face to visually show the status of the
	  ;; breakpoint and print informative message
	  (xslt-process-unhighlight-breakpoint breakpoint)
	  (xslt-process-highlight-breakpoint breakpoint)
	  (if (xslt-process-breakpoint-is-enabled breakpoint)
	       (progn
		(xslt-process-send-command (format "ena %s %s" filename line))
		(message "Enabled breakpoint in %s at %s" filename line))
	     (progn
	       (xslt-process-send-command (format "dis %s %s" filename line))
	       (message "Disabled breakpoint in %s at %s" filename line)))
	  (run-hooks 'xslt-process-breakpoint-enabled-disabled-hooks))
      (message (format "No breakpoint in %s at %s" filename line)))))

;;;
;;; Debugger commands
;;;
(defvar xslt-process-output-to-filename nil
  "The name of the filename to which the output goes, nil if the
output goes to Emacs directly.")

(defun xslt-process-do-run (&optional no-debug out-filename)
  "*Send the run/debug command to the command line interpreter to
start either a normal, no debug, XSLT processing, or a debugging
session. Returns 'quit if the XSLT processor is already started and
the user doesn't stop it. Otherwise if the XSLT processing starts
normally returns 'started."
  (interactive)
  (block nil
    (let ((proc-type (if no-debug "processor" "debugger"))
	  (command (if no-debug "run" "debug")))
      (if (not (eq xslt-process-process-state 'not-running))
	  (if (yes-or-no-p
	       (format "The XSLT %s is already running, restart it? "
		       proc-type))
	      (xslt-process-do-quit t)
	    (return 'quit)))
      (setq xslt-process-output-to-filename out-filename)
      (let* ((filename (urlize (buffer-file-name)))
	     (xsl-filename
	      (cdr (assoc filename xslt-process-xml-xslt-associations)))
	     (xml-filename
	      (car (rassoc filename xslt-process-xml-xslt-associations)))
	     complete-command)
	(if xml-filename
	    ;; The current buffer is an XSLT stylesheet. Find a
	    ;; corresponding XML document to run the XSLT processor
	    ;; on.
	    (progn
	      (setq complete-command
		    (concat command
			    " -xml " (xslt-process-escape xml-filename)))
	      (setq complete-command
		    (concat complete-command
			    " -xsl " (xslt-process-escape filename))))
	  ;; The current buffer is an XML document.
	  (setq complete-command
		(concat command " -xml " (xslt-process-escape filename)))
	  (if (and xsl-filename (not (eq xsl-filename 'default)))
	      (setq complete-command
		    (concat complete-command " -xsl "
			    (xslt-process-escape xsl-filename)))))
	(if out-filename
	    (setq complete-command
		  (concat complete-command " -o "
			  (xslt-process-escape out-filename))))
	(if xslt-process-transform-parameters
	    (setq complete-command
		  (concat complete-command " -p "
			  (number-to-string (length xslt-process-transform-parameters)) " "
			  (mapconcat (lambda (x)
				     (xslt-process-escape (concat (car x) "=" (cdr x))))
				     xslt-process-transform-parameters " "))))

	;; Prepare the buffers
	(save-some-buffers)
	;; Set the XSLT processor to be used
	(xslt-process-set-processor)
	(setq xslt-process-process-state 'running)
	(setq xslt-process-debugger-running (not no-debug))
	(xslt-process-send-command complete-command)
	(setq xslt-process-execution-context-error-function
	      (lambda ()
		(setq xslt-process-process-state 'not-running)))
	(speedbar-with-writable
	  (let ((results-buffer (get-buffer xslt-process-results-buffer-name))
		(msgs-buffer (get-buffer xslt-process-message-buffer-name))
		(errors-buffer (get-buffer xslt-process-errors-buffer-name)))
	    (save-excursion
	      (if results-buffer
		  (kill-buffer results-buffer))
	      (if msgs-buffer
		  (kill-buffer msgs-buffer))
	      (if errors-buffer
		  (kill-buffer errors-buffer)))))
	(message "Running the %s %s..."
		 xslt-process-current-processor
		 proc-type))))
  'started)

(defun xslt-process-do-step ()
  "*Send a STEP command to the XSLT debugger."
  (interactive)
  (if (eq xslt-process-process-state 'stopped)
      (progn
	(setq xslt-process-process-state 'running)
	(xslt-process-send-command "s"))
    (message "Will not queue command while debugger is running.")))

(defun xslt-process-do-next ()
  "*Send a NEXT command to the XSLT debugger."
  (interactive)
  (if (eq xslt-process-process-state 'stopped)
      (progn
	(setq xslt-process-process-state 'running)
	(xslt-process-send-command "n"))
    (message "Will not queue command while debugger is running.")))

(defun xslt-process-do-finish ()
  "*Send a FINISH command to the XSLT debugger."
  (interactive)
  (if (eq xslt-process-process-state 'stopped)
      (progn
	(setq xslt-process-process-state 'running)
	(xslt-process-send-command "f"))
    (message "Will not queue command while debugger is running.")))

(defun xslt-process-do-continue ()
  "*Send a CONTINUE command to the XSLT debugger."
  (interactive)
  (if (eq xslt-process-process-state 'stopped)
      (progn
	(setq xslt-process-process-state 'running)
	(xslt-process-send-command "c"))
    (message "Will not queue command while debugger is running.")))

(defun xslt-process-do-stop ()
  "*Send a STOP command to the XSLT debugger, potentially stopping the
debugger from a long processing with no breakpoints setup."
  (interactive)
  (if (eq xslt-process-process-state 'running)
      (xslt-process-send-command "stop")))

(defun xslt-process-do-quit (&optional dont-ask)
  "*Quit the XSLT debugger."
  (interactive)
  (if xslt-process-comint-buffer
      (if (or dont-ask
	      (yes-or-no-p "Really quit the XSLT processor? "))
	  (progn
	    (xslt-process-send-command "q" t)
	    (kill-buffer xslt-process-comint-buffer)
	    ;; Delete maybe the breakpoints?
	    (if (and (not dont-ask)
		      (> (hash-table-count xslt-process-breakpoints) 0)
		     (yes-or-no-p "Delete all breakpoints? "))
		(progn
		  (xslt-process-change-breakpoints-highlighting nil)
		  (cl-clrhash xslt-process-breakpoints)
		  (run-hooks 'xslt-process-breakpoint-removed-hooks)))
	    ;;(xslt-process-debugger-buffer-killed)
	    ))
    (message "XSLT processor not running.")))

;;;
;;; Dealing with the presentation of breakpoints and the current line
;;; indicator
;;;

(defun xslt-process-change-breakpoints-highlighting (flag &optional filename)
  "Highlights or unhighlights, depending on FLAG, all the breakpoints
in the buffer visiting FILENAME. If FILENAME is not specified or is
nil, it changes the highlighting on the breakpoints in all the
buffers. It doesn't affect the current state of the breakpoints."
  (cl-maphash
   (lambda (breakpoint state)
     (let ((fname (xslt-process-breakpoint-filename breakpoint)))
       (if (or (not filename) (equal filename fname))
	   (if flag
	       (xslt-process-highlight-breakpoint breakpoint state)
	     (xslt-process-unhighlight-breakpoint breakpoint)))))
   xslt-process-breakpoints))

(defun xslt-process-make-extent (from to &optional buffer)
  "Make an XEmacs extent or GNU Emacs overlay in BUFFER or the current
buffer, if none is specified."
  (if (featurep 'xemacs)
      (make-extent from to buffer)
    (make-overlay from to buffer)))

(defun xslt-process-delete-extent (extent)
  "Delete XEmacs extent or GNU Emacs overlay identified by EXTENT."
  (if (featurep 'xemacs)
      (delete-extent extent)
    (delete-overlay extent)))

(defun xslt-process-set-extent-property (extent prop value)
  "Set a property for a XEmacs extent or GNU Emacs overlay. The
property names are those of the XEmacs extents, they areir GNU Emacs
correspondents."
  (if (featurep 'xemacs)
      (set-extent-property extent prop value)
    (cond ((eq prop 'begin-glyph) (overlay-put extent 'before-string value))
	  ((eq prop 'begin-glyph-layout) nil)
	  (t (overlay-put extent prop value)))))

(defun xslt-process-make-annotation (glyph)
  "Make a GLYPH annotation at POINT. We use normal extents here
instead of XEmacs annotations to be able to achieve GNU Emacs
compatibility."
  (let ((annotation (xslt-process-make-extent (point) (point))))
    (xslt-process-set-extent-property annotation 'begin-glyph glyph)
    (xslt-process-set-extent-property annotation 'begin-glyph-layout 'text)
    (xslt-process-set-extent-property annotation
				      'face 'xslt-process-indicator-face)
    (xslt-process-set-extent-property annotation 'priority 3)
    (xslt-process-set-extent-property annotation 'read-only t)
    annotation))

(defun xslt-process-change-current-line-highlighting (flag)
  "Highlights or unhighlights, depending on FLAG, the current line
indicator."
  (if (and (xslt-process-selected-position-filename)
	   (xslt-process-selected-position-line))
      (save-excursion
	(let* ((buffer (xslt-process-get-file-buffer
			(xslt-process-selected-position-filename)))
	       (line (xslt-process-selected-position-line))
	       (action (xslt-process-selected-position-enter-exit))
	       annotation)
	  (set-buffer buffer)
	  (goto-line line)
	  (if flag
	      ;; Highlight the last selected line
	      (let ((extent (xslt-process-highlight-line
			     'xslt-process-current-line-face 99)))
		(setq
		 annotation
		 (if (eq action 'is-entering)
		     (xslt-process-make-annotation xslt-process-enter-glyph)
		   (if (eq action 'is-exiting)
		       (xslt-process-make-annotation xslt-process-exit-glyph)
		     nil)))
;		    (annotation
;		     (if glyph (make-annotation glyph (point) 'text) nil)))
;		(if annotation
;		    (progn
;		      (set-annotation-face annotation
;					   'xslt-process-indicator-face)
;		      (set-extent-priority annotation 3)))
		;; Setup the new extent and annotation in the
		;; last-selected-position
		(xslt-process-selected-position-extent extent)
		(xslt-process-selected-position-annotation annotation))
	    ;; Unhighlight the last selected line
	    (xslt-process-unhighlight-last-selected-line))))))

(defun xslt-process-highlight-breakpoint (breakpoint &optional state)
  "Highlight BREAKPOINT depending on it state."
  (let* ((filename (xslt-process-breakpoint-filename breakpoint))
	 (line (xslt-process-breakpoint-line breakpoint))
	 (buffer (xslt-process-get-file-buffer filename)))
    ;; Signal an error if there's no buffer
    (if (not buffer)
	(error "Cannot find the buffer associated with %s" filename)
      ;; If state was not passed as argument to this function, set
      ;; state to the state of the breakpoint
      (if (not state)
	  (setq state (xslt-process-breakpoint-is-enabled breakpoint)))
      ;; Position to the line we want to be highlighted and invoke the
      ;; xslt-process-highlight-line function
      (save-excursion
	(set-buffer buffer)
	(goto-line line)
	(let ((extent (xslt-process-highlight-line
		       (if (or (not (null state)) (eq state 'enabled))
			   'xslt-process-enabled-breakpoint-face
			 'xslt-process-disabled-breakpoint-face)
		       99)))
	  ;; Intern the extent in the extents table
	  (cl-puthash breakpoint extent xslt-process-breakpoint-extents))))))

(defun xslt-process-unhighlight-breakpoint (breakpoint)
  "Remove the highlighting associated with BREAKPOINT."
  (save-excursion
    ;; First remove any extent that exists at this line
    (let* ((filename (xslt-process-breakpoint-filename breakpoint))
	   (line (xslt-process-breakpoint-line breakpoint))
	   (extent (cl-gethash (cons filename line)
			       xslt-process-breakpoint-extents)))
      (if extent
	  (progn
	    (xslt-process-delete-extent extent)
	    (cl-remhash (cons filename line)
			xslt-process-breakpoint-extents))))))

(defun xslt-process-highlight-line (face &optional priority)
  "Sets the face of the current line to FACE. Returns the extent that
highlights the line."
  (save-excursion
      ;; Don't setup a new extent if the face is 'default
      (if (eq face 'default)
	  nil
	;; Otherwise setup a new a new extent at the current line
	(let* ((to (line-end-position))
	       (from (line-beginning-position))
	       (extent (xslt-process-make-extent from to)))
	  (xslt-process-set-extent-property extent 'face face)
	  (if priority
	      (xslt-process-set-extent-property extent 'priority priority))
	  extent))))

;;;
;;; The interaction with the debugger
;;;

(defun xslt-process-set-processor ()
  "Sets the XSLT processor to be used for further processings if
needed. The user can change the XSLT processor either through
customization or by explicitly setting up the `processor' local
variable in the XML buffer."
  (let ((xslt-processor
	 ;; Set the name of the XSLT processor. This is either specified
	 ;; in the local variables of the file or is the default one.
	 (progn
	   ;; Force evaluation of local variables
	   (hack-local-variables t)
	   (or (if (and
		    (local-variable-p 'processor (current-buffer))
		    (boundp 'processor))
		   (if (stringp processor)
		       processor
		     (symbol-name processor)))
	       (symbol-name (car xslt-process-default-processor))))))
    (if (equal xslt-processor xslt-process-current-processor)
	nil
      (setq xslt-process-current-processor xslt-processor)
      (xslt-process-send-command
       (message "set processor %s" xslt-processor)))))

(defun xslt-process-send-command (string &optional dont-start-process)
  "Sends a command to the XSLT process. Start this process if not
already started."
  ;; Reset the execution-context-error-function so that in case of
  ;; errors we don't get error functions called inadvertently
  (setq xslt-process-execution-context-error-function nil)
  (if (and (not dont-start-process)
	   (or (null xslt-process-comint-process)
	       (not (eq (process-status xslt-process-comint-process) 'run))))
      (progn
	(xslt-process-start-debugger-process)
	;; Set any breakpoints which happen to be setup in the
	;; breakpoints hash table at this time in the XSLT debugger
	(cl-maphash
	 (lambda (breakpoint status)
	   (let ((filename (xslt-process-breakpoint-filename breakpoint))
		 (line (xslt-process-breakpoint-line breakpoint)))
	     (comint-simple-send xslt-process-comint-process
				 (format "b %s %s" filename line))
	     (if (eq status 'disabled)
		 (comint-simple-send xslt-process-comint-process
				     (format "dis %s %s" filename line)))))
	 xslt-process-breakpoints)))
  (if (and dont-start-process
	   (null xslt-process-comint-process))
      nil
    (if (eq (process-status xslt-process-comint-process) 'run)
	(comint-simple-send xslt-process-comint-process string))))

(defun xslt-process-start-debugger-process ()
  "*Start the XSLT debugger process."
  (let* ((classpath-separator (if (eq system-type 'cygwin32)
				  ";" path-separator))
	 (classpath (mapconcat (lambda (x) x)
			       xslt-process-external-java-libraries
			       classpath-separator))
	 (bootpathjarfiles (mapcar (lambda (f)
				  (concat (xslt-process-find-xslt-data-directory)
					  "java" xslt-process-dir-separator f))
	  '("saxon-6.5.2.jar" "xalan.jar" "xercesImpl.jar" "xml-apis.jar"
	    )))
	 (bootclasspath (mapconcat (lambda (x) x)
			       bootpathjarfiles
			       classpath-separator))
	 (command nil))
    
    (if xslt-process-additional-classpath
	(setq classpath
	      (concat
	       (mapconcat (lambda (x) x)
			  xslt-process-additional-classpath
			  classpath-separator)
	       classpath-separator classpath)))
    
    (setq command
	  (append (list xslt-process-java-program)
		  '(nil)
		  xslt-process-jvm-arguments
		  (mapcar (lambda (x)
			    (concat "-D" (car x) "=" (cdr x)))
			  xslt-process-jvm-option-properties)
		  (list "-classpath" classpath
			(concat "-Xbootclasspath/p:" bootclasspath)
			"xslt.debugger.cmdline.Controller" "-emacs")
		  (list (format "-%s" xslt-process-current-processor))))
    (setq xslt-process-comint-buffer
	  (apply 'make-comint xslt-process-comint-process-name command))
    (message "Starting XSLT process...")
    (setq xslt-process-comint-process
	  (get-buffer-process xslt-process-comint-buffer))
    (set-process-sentinel
     xslt-process-comint-process
     (lambda (process event)
       (let ((status (process-status process)))
	 (if (or (equal status 'exit)
		 (equal status 'closed)
		 (equal event "hangup\n"))
	   (xslt-process-debugger-buffer-killed)))))
    (save-excursion
      (set-buffer xslt-process-comint-buffer)
;      (make-variable-buffer-local 'kill-buffer-hook)
;      (add-hook 'kill-buffer-hook 'xslt-process-debugger-buffer-killed)
      ;; Set our own process filter, so we get a chance to remove Emacs
      ;; commands from the output sent to the buffer
      (set-process-filter xslt-process-comint-process
			  (function xslt-process-output-from-process))
      (setq comint-prompt-regexp "^xslt> ")
      (setq comint-delimiter-argument-list '(? )))))

(defvar xslt-process-partial-command nil
  "Holds partial commands as output by the XSLT debugger.")

(defun xslt-process-output-from-process (process string)
  "This function is called each time output is generated by the XSLT
debugger. It filters out all the Emacs commands and sends the rest of
the output to the XSLT process buffer.

The results of the processing and the messages generated during
processing do not come in this function; but in the
`xslt-process-process-filter' function."
  (while (and string (not (equal string "")))
;    (message "String is now '%s'" string)
    (let* ((l-start (string-match "\\^" string))
	   (l-end (if l-start (match-end 0) nil))
	   (g-start (string-match "$$" string))
	   (g-end (if g-start (match-end 0) nil)))
      (cond ((and l-start (null g-start))
	     ;; We have the start of a command which doesn't end in
	     ;; this current string, preceded by some output text
;	     (message "case 1: l-start %s, g-start %s" l-start g-start)
	     (let ((output (xslt-process-unescape
			    (substring string 0 l-start))))
	       (if (and output (not (equal output "")))
		   (comint-output-filter process output)))
	     (setq xslt-process-partial-command (substring string l-end))
;	     (message "   xslt-process-partial-command %s"
;		      xslt-process-partial-command)
	     (setq string nil))

	    ((and (null l-start) g-start)
	     ;; We have the end of command followed followed by some
	     ;; output text
;	     (message "case 2: l-start %s, g-start %s" l-start g-start)
	     (setq xslt-process-partial-command
		   (concat xslt-process-partial-command
			   (substring string 0 g-end)))
;	     (message "   xslt-process-partial-command %s"
;		      xslt-process-partial-command)
;	     (message "evaluating xslt-process-partial-command: %s"
;		      xslt-process-partial-command)
	     (eval (read xslt-process-partial-command))
	     (setq xslt-process-partial-command nil)
	     (let ((output (xslt-process-unescape (substring string g-end))))
	       (if (and output (not (equal output "")))
		   (comint-output-filter process output)))
	     (setq string nil))

	    ((and (null l-start) (null g-start))
	     ;; We have some text. Append to
	     ;; `xslt-process-partial-command' if it's not null,
	     ;; otherwise just output to the process buffer.
;	     (message "case 3: l-start %s, g-start %s" l-start g-start)
	     (if xslt-process-partial-command
		 (setq xslt-process-partial-command
		       (concat xslt-process-partial-command string))
	       (comint-output-filter process (xslt-process-unescape string)))
;	     (message "   xslt-process-partial-command %s"
;		      xslt-process-partial-command)
	     (setq string nil))

	    ((< l-start g-start)
	     ;; We have a command embedded into string. Output the
	     ;; preceding text to the process buffer, execute the
	     ;; command and set string to the substring starting at
	     ;; the end of command.
;	     (message "case 4: l-start %s, g-start %s" l-start g-start)
	     (let ((output (xslt-process-unescape
			    (substring string 0 l-start)))
		   (command (substring string l-end g-start)))
	       (if (and output (not (equal output "")))
		   (comint-output-filter process output))
;	       (message "evaluating command: %s" command)
	       (eval (read command))
	       (setq string (substring string g-end))))

	    ((> l-start g-start)
	     ;; We have the end of previously saved command, some
	     ;; output text and maybe another command.
;	     (message "case 5: l-start %s, g-start %s" l-start g-start)
	     (setq xslt-process-partial-command
		   (concat xslt-process-partial-command
			   (substring string 0 g-start)))
;	     (message "   xslt-process-partial-command %s"
;		      xslt-process-partial-command)
;	     (message "evaluating xslt-process-partial-command: %s"
;		      xslt-process-partial-command)
	     (eval (read xslt-process-partial-command))
	     (setq xslt-process-partial-command nil)
	     (let ((output (xslt-process-unescape
			    (substring string g-end l-start))))
	       (if (and output (not (equal output "")))
		   (comint-output-filter process output)))
	     (setq string (substring string l-start)))))))

(defun xslt-process-unhighlight-last-selected-line ()
  "Unselect the last selected line."
  ;; Unselect the last line showing the debugger's position
  (if xslt-process-selected-position
      (let ((extent (xslt-process-selected-position-extent))
	    (annotation (xslt-process-selected-position-annotation)))
	(if extent (xslt-process-delete-extent extent))
;	(if annotation (delete-annotation annotation))
	(if annotation (xslt-process-delete-extent annotation))
	;; Remove extent and annotation from the
	;; last-selected-position. Need to use aset as there is no way
	;; for the setter functions to detect between programmer
	;; passed nil and no argument.
	(aset xslt-process-selected-position 3 nil)
	(aset xslt-process-selected-position 4 nil))))

;;;
;;; Functions called as result of the XSLT processing
;;;

(defun xslt-process-processing-started ()
  "Invoked at the beginning of each XSLT processing or debugging
action. When the processing completes, the
`xslt-process-processing-finished' method is invoked."
; use filename in place of nil, so false message is not produced when
; output goes to browser or PDF viewer. Is this a good fix? Can the
; variable be set effectively after processing by the browser/fo code?
  (setq xslt-process-output-generated xslt-process-output-to-filename))

(defun xslt-process-processing-finished (&optional killed)
  "Called by the XSLT debugger process when the XSLT processing finishes."
  (xslt-process-unhighlight-last-selected-line)
  ;; Reset the source and style frame stacks
  (setq xslt-process-source-frames-stack nil)
  (setq xslt-process-style-frames-stack nil)
  (setq xslt-process-global-variables nil)
  (setq xslt-process-local-variables nil)
  (run-hooks 'xslt-process-source-frames-changed-hooks)
  (run-hooks 'xslt-process-style-frames-changed-hooks)
  (run-hooks 'xslt-process-global-variables-changed-hooks)
  (run-hooks 'xslt-process-local-variables-changed-hooks)
  (setq xslt-process-selected-position [nil nil nil nil nil nil])
  (setq xslt-process-process-state 'not-running)
  (if (not killed)
      (progn
	(message
	 (if (not xslt-process-output-generated)
	     "Done invoking %s; no output generated."
	   "Done invoking %s.")
	 xslt-process-current-processor)
	(if (get-buffer xslt-process-errors-buffer-name)
	    (display-buffer xslt-process-errors-buffer-name))))
  (run-hooks 'xslt-process-xslt-processing-finished-hooks))

(defun xslt-process-report-error (message stack-trace)
  "Called by the XSLT debugger process whenever an error happens."
  (let ((message (xslt-process-unescape message))
	(stack-trace (xslt-process-unescape stack-trace)))
    (setq xslt-process-error-messages
	  (concat xslt-process-error-messages message "\n"
		  stack-trace "\n\n\n"))
    (message stack-trace)
    (message message)
    (if xslt-process-execution-context-error-function
	(funcall xslt-process-execution-context-error-function))
    (let ((buffer (get-buffer-create xslt-process-errors-buffer-name)))
      (save-excursion
	(set-buffer buffer)
	;; If nothing was inserted in the buffer to this point, insert a
	;; dummy cd command just to keep the compilation-mode happy.
	(if (equal (point-min) (point-max))
	    (insert "cd\n\n"))
	(compilation-mode "XSLT")
	(goto-char (point-max))
	(insert message)
	(display-buffer buffer)))))

(defun xslt-process-debugger-stopped-at (filename line column count info)
  "Function called by the XSLT debugger process each time the debugger
hits a breakpoint that causes it to stop."
  (message "Stopped at %s %s: %s" filename line count)
  (setq xslt-process-process-state 'stopped)
  ;; Unselect the previous selected line
  (xslt-process-unhighlight-last-selected-line)
  ;; Now select the new line. Create a buffer for the file, if one
  ;; does not exist already, and put it in the debug mode.
  (let ((buffer (xslt-process-get-file-buffer filename))
	(is-entering (string-match "^entering:" info))
	(is-exiting (string-match "^leaving:" info)))
    (if (not buffer)
	(error "Cannot find the buffer associated with %s" filename)
      (progn
	(pop-to-buffer buffer)
	(goto-line line)
	;; Setup the xslt-process-selected-position variable so we
	;; can call the xslt-process-change-current-line-highlighting
	;; function
	(xslt-process-selected-position-filename filename)
	(xslt-process-selected-position-line line)
	(xslt-process-selected-position-column column)
	(xslt-process-selected-position-enter-exit
	 (if is-entering 'is-entering
	   (if is-exiting 'is-exiting nil)))
	(xslt-process-change-current-line-highlighting t)))))

(defun xslt-process-debugger-process-started ()
  "Called when the debugger process started and is ready to accept
commands. This is called only once, when the JVM process containing
the XSLT processor starts up.

The `xslt-process-processing-started' function is called right before each
XSLT processing or debugging activity starts."
  (setq xslt-process-debugger-process-started t)
  (xslt-process-send-command
   (concat "set processor " xslt-process-current-processor))
  (message "Starting XSLT process...done"))

(defun xslt-process-debugger-buffer-killed ()
  "Called when the comint buffer running the XSLT debugger is killed
by the user."
  (xslt-process-processing-finished t)
  (setq xslt-process-comint-process nil)
  (setq xslt-process-comint-buffer nil)
  (setq xslt-process-message-process nil)
  (setq xslt-process-results-process nil)
  (setq xslt-process-debugger-process-started nil))

(defun xslt-process-source-frames-stack-changed (stack)
  "Called by the debugger process to inform that the source frames
stack has changed. The STACK argument contains the new source frame
stack as a list of (name filename line)."
  (setq xslt-process-source-frames-stack stack)
  (run-hooks 'xslt-process-source-frames-changed-hooks))

(defun xslt-process-style-frames-stack-changed (stack)
  "Called by the debugger process to inform that the style frames
stack has changed. The STACK argument contains the new style frame
stack as a list of (name filename line)."
  (setq xslt-process-style-frames-stack stack)
  (run-hooks 'xslt-process-style-frames-changed-hooks))

(defun xslt-process-stack-frames-changed (source-frame style-frame)
  "Called by the debugger when either the source or the style frame
changes. This happens as an effect of user's actions, usually choosing
a new source/style frame in the speedbar or by entering the
appropriate commands in the command line. The SOURCE-FRAME and
STYLE-FRAME arguments represent indices of the selected frame in the
corresponding stack frame."
  (message "stack frames changed %s %s" source-frame style-frame)
  (setq xslt-process-selected-source-frame source-frame)
  (setq xslt-process-selected-style-frame style-frame)
  (run-hook-with-args 'xslt-process-source-frames-changed-hooks t)
  (run-hook-with-args 'xslt-process-style-frames-changed-hooks t))

(defun xslt-process-global-variables-changed (variables)
  "Called by the debugger process to inform that the global variables
in the current XSLT template have changed."
  (setq xslt-process-global-variables
	(mapvector
	 (lambda (x)
	   (vector (aref x 0) (aref x 1) (xslt-process-unescape (aref x 2))))
	 variables))
  (run-hooks 'xslt-process-global-variables-changed-hooks))

(defun xslt-process-local-variables-changed (variables)
  "Called by the debugger process to inform that the local variables
in the current XSLT template have changed."
  (setq xslt-process-local-variables
	(mapvector
	 (lambda (x)
	   (vector (aref x 0) (aref x 1) (xslt-process-unescape (aref x 2))))
	 variables))
  (run-hooks 'xslt-process-local-variables-changed-hooks))

(defun xslt-process-show-in-minibuffer (string)
  "Called from the Java process to display status information in the
minibuffer area"
  (message (xslt-process-unescape string)))

(defun xslt-process-process-filter (process string)
  "Function called whenever the XSLT processor sends results to its
output stream. The results come via the `xslt-process-results-process'
process."
  (setq xslt-process-output-generated t)
  (unwind-protect
      (let* ((bufname (cond ((eq process xslt-process-results-process)
			     xslt-process-results-buffer-name)
			    ((eq process xslt-process-message-process)
			     xslt-process-message-buffer-name)
			    (t nil)))
	     (buffer (get-buffer-create bufname)))
	(save-excursion
	  (save-selected-window
	    (set-buffer buffer)
	    (setq buffer-read-only nil)
	    ;; Insert the text, moving the marker.
	    (goto-char (point-max))
	    (insert string)
	    (setq buffer-read-only t)
	    (setq pop-up-windows t)
	    (setq split-height-threshold 15)
	    (pop-to-buffer buffer)
	    (set-window-point (get-buffer-window buffer) (point-max)))))))

(defun xslt-process-set-output-port (port)
  "Called by the XSLT debugger to setup the TCP/IP port number on
which it listens for incoming connections. Emacs has to connect to
this port and use it for receiving the result of the XSLT processing."
  (setq xslt-process-results-process
	(open-network-stream xslt-process-results-process-name
			     nil "localhost" port))
  (set-process-filter xslt-process-results-process
		      'xslt-process-process-filter)

  (setq xslt-process-message-process
	(open-network-stream xslt-process-message-process-name
			     nil "localhost" port))
  (set-process-filter xslt-process-message-process
		      'xslt-process-process-filter))

;;;
;;; Setup the minor mode
;;;

(defun xslt-process-setup-minor-mode (keymap mode-line-string)
  "Setup the XSLT-process minor mode. KEYMAP specifies the keybindings
to be used. MODE-LINE-STRING specifies the string to be displayed in
the modeline."
  (if (fboundp 'add-minor-mode)
      (add-minor-mode 'xslt-process-mode
		      mode-line-string
		      keymap
		      nil
		      'xslt-process-mode)
    (setq minor-mode-alist (remassoc 'xslt-process-mode minor-mode-alist))
    (or (assq 'xslt-process-mode minor-mode-alist)
	(setq minor-mode-alist
	      (cons (list 'xslt-process-mode mode-line-string)
		    minor-mode-alist)))

    (setq minor-mode-map-alist
	  (remassoc 'xslt-process-mode minor-mode-map-alist))
    (or (assq 'xslt-process-mode minor-mode-map-alist)
	(setq minor-mode-map-alist
	      (cons (cons 'xslt-process-mode keymap)
		    minor-mode-map-alist))))
  (force-mode-line-update))

;;;
;;; Stylesheet management
;;;

(defun xslt-process-register-stylesheet (filename)
  "Registers a filename in the stylesheet registry."
  (if (member filename xslt-process-registered-stylesheets)
      (message "Filename already registered.")
    (setq xslt-process-registered-stylesheets
	  (cons filename xslt-process-registered-stylesheets))))

(defun xslt-process-unregister-stylesheet (filename)
  "Unregister a filename from the stylesheet registry."
  (if (member filename xslt-process-registered-stylesheets)
      (setq xslt-process-registered-stylesheets
	    (delete filename xslt-process-registered-stylesheets))
    (message "Filename not registered.")))

(defun xslt-process-register-buffer (buffer-name)
  "*Registers the file visited by the current buffer in the stylesheet
registry. The file visited by the buffer should be an XSLT file; no
checks are done to verify this."
  (interactive "bRegister buffer: ")
  (let* ((buffer (get-buffer buffer-name))
	 (filename (urlize (buffer-file-name buffer))))
    (xslt-process-register-stylesheet filename)))

(defun xslt-process-unregister-buffer (buffer-name)
  "*Unregisters the current buffer from the stylesheet registry. The
file visited by the buffer should have been previously registered with
the stylesheet registry. An error is generated if this is not the
case."
  (interactive "bUnregister buffer: ")
  (let* ((buffer (get-buffer buffer-name))
	 (filename (urlize (buffer-file-name buffer))))
    (xslt-process-unregister-stylesheet filename)))

;;; The following ugly code and global variables are needed to work
;;; around the lack of lexical scoping in Emacs Lisp. It would have
;;; been a lot easier to write the inner lambda functions in this
;;; macro with lexical scoping. In fact, this would have been a
;;; function, not a macro.

(defmacro xslt-process-choose-from-list-buffer
  (buffer-name items function selected-value &optional title)
  "Creates a buffer that displays a list of items and two buttons,
'Done' and 'Cancel'. Each item is a pair (name . value), with all the
names being displayed in the buffer as radio-button-choice widget. If
TITLE is specified, it is displayed at the top of the buffer, followed
by the radio buttons.

If the user clicks on the 'Done' button, FUNCTION is called. The value
of the selected item is set in the SELECTED-VALUE variable. If
'Cancel' is selected, FUNCTION is not invoked. In both cases the
buffer displaying the radio buttons is killed."
  `(unwind-protect
       (let* ((buffer (get-buffer-create ,buffer-name))
	      (items ,items)
	      (current-selection (assoc xslt-process-selected-xml-file
					xslt-process-xml-xslt-associations))
	      (value
	       (if current-selection
		   (cdr current-selection)
		 (cdar items))))
	 (setq ,selected-value (cdar items))
	 (switch-to-buffer-other-window (set-buffer buffer))
	 (if ,title (widget-insert (concat ,title "\n\n")))
	 (apply 'widget-create 'radio-button-choice
		:value value
		:notify (lambda (widget &rest ignore)
			  (setq ,selected-value (widget-value widget)))
		(mapcar (lambda (item)
			  (list 'item :tag (car item) :value (cdr item)))
			items))
	 (widget-insert "\n")
	 (widget-create 'push-button
			:notify (lambda (&rest ignore)
				  (kill-buffer (current-buffer))
				  (funcall ,function))
			"Done")
	 (widget-insert " ")
	 (widget-create 'push-button
			:notify (lambda (&rest ignore)
				  (kill-buffer (current-buffer)))
			"Cancel")
	 (use-local-map widget-keymap)
	 (widget-setup))))

(defvar xslt-process-selected-xml-file nil
  "Set by `xslt-process-associate-stylesheet' to the XML filename
that's part of the association. It is used in
`xslt-process-do-associate-stylesheet' as the first part of the
association.")

(defvar xslt-process-selected-xsl-file nil
  "Used by `xslt-process-choose-from-list-buffer' to set the value of
the selected XSLT stylesheet file. Is is used by
`xslt-process-do-associate-stylesheet' as the second part of an
association.")

(defun xslt-process-do-associate-stylesheet ()
  "Called when the user presses the 'Done' button in the XSLT
selection buffer. The selected XSLT file is stored in
`xslt-process-selected-xsl-file'."
  (setq xslt-process-xml-xslt-associations
	(remassoc xslt-process-selected-xml-file
		  xslt-process-xml-xslt-associations))
  (customize-set-variable 'xslt-process-xml-xslt-associations
			  (cons
			   (cons xslt-process-selected-xml-file
				 (urlize xslt-process-selected-xsl-file))
			   xslt-process-xml-xslt-associations)))

(defun xslt-process-associate-stylesheet ()
  "*Associate an XML buffer with a previously registered stylesheet."
  (interactive)
  (setq xslt-process-selected-xml-file (urlize (buffer-file-name)))
  (setq xslt-process-selected-xsl-file nil)
  (if (not xslt-process-selected-xml-file)
      (error "Current buffer does not have a filename!")
    (xslt-process-choose-from-list-buffer
     "*xslt stylesheets*"
     (cons (cons "Use the associated stylesheet specified in PI" 'default)
	   (mapcar (lambda (file) (cons file file))
		   xslt-process-registered-stylesheets))
     'xslt-process-do-associate-stylesheet
     xslt-process-selected-xsl-file
     "Please select the stylesheet to use:")))

;;;
;;; End of the ugly code
;;;

(defun xslt-process-manage-stylesheets ()
  "*Displays the buffer to manage the stylesheets registry.  You can
add or remove stylesheets in the registry."
  (interactive)
  (customize-variable 'xslt-process-registered-stylesheets))

(defun xslt-process-manage-associations ()
  "*Displays the buffer to manage the association between XML and XSLT
files. You can add or remove associations in the registry."
  (interactive)
  (customize-variable 'xslt-process-xml-xslt-associations))

;;;
;;; Processor and tranform parameter selection
;;;

(defun xslt-process-manage-processor ()
  "*Displays the buffer to select the XSLT processor."
  (interactive)
  (customize-variable 'xslt-process-xml-xslt-associations))

(defun xslt-process-manage-transform-parameters ()
  "*Displays the buffer to manage the tranform parameters"
  (interactive)
  (customize-variable 'xslt-process-transform-parameters))

;;;
;;; DocBook support
;;;

(defcustom xslt-process-docbook-install-dir nil
  "*Directory where the DocBook-XSL package is installed."
  :group 'xslt-process
  :type '(choice
	  (const :tag "Not installed" nil)
	  (directory :tag "Directory"))
  :set
  (lambda (sym val)
    (let ((xsl-fo (file-truename (concat val "/fo/docbook.xsl")))
	  (xsl-html (file-truename (concat val "/html/docbook.xsl"))))
      ;; Before uninstalling DocBook, remove the old stylesheets from
      ;; the registry, so that it doesn't become cluttered with
      ;; garbage.
      (if (and (null val) (boundp 'xslt-process-docbook-install-dir))
	  (let ((old-xsl-fo
		 (file-truename (concat xslt-process-docbook-install-dir
					"/fo/docbook.xsl")))
		(old-xsl-html
		 (file-truename (concat xslt-process-docbook-install-dir
					"/html/docbook.xsl"))))
	    (xslt-process-unregister-stylesheet old-xsl-fo)
	    (xslt-process-unregister-stylesheet old-xsl-html))
      (unless (or (null val)
		  (and (file-readable-p xsl-fo) (file-readable-p xsl-html)))
	(setq xslt-process-docbook-install-dir nil)
	(error "Directory doesn't point to a valid DocBook-XSL package!"))
      (setq xslt-process-docbook-install-dir val)
      ;; Register the DocBook-XSL stylesheets.
      (if val
	  (progn
	    (xslt-process-register-stylesheet xsl-fo)
	    (xslt-process-register-stylesheet xsl-html)))))))

;;;###autoload
(defun xslt-process-install-docbook (directory)
  "*Remember the DocBook-XSL installation directory."
  (interactive "DDocBook directory installation: ")
  (customize-save-variable 'xslt-process-docbook-install-dir directory))

;;;
;;; Additional functions
;;;

(defun xslt-process-get-file-buffer (filename)
  "Searches through all the current buffers for a buffer whose true
file name is the same as FILENAME. The true file name is the one in
which all the symlinks in the original file name were expanded. We
don't want to use `get-file-buffer' because it doesn't follow links.

If the buffer does not exists, open the file in a new buffer and
return the buffer."
  (let ((true-filename (urlize (file-truename filename)))
	(buffers-list (buffer-list))
	(found nil))
    (while (and (not found) buffers-list)
      (let* ((buffer (car buffers-list))
	     (filename (urlize (buffer-file-name buffer))))
	(if (equal true-filename
		   (if filename
		       (urlize (file-truename filename))
		     nil))
	    (setq found buffer)
	  (setq buffers-list (cdr buffers-list)))))
    ;; Return the buffer if found, otherwise open the file and return it
    (if found
	found
      (let ((buffer (find-file true-filename)))
	(if buffer
	    (save-excursion
	      (set-buffer buffer)
	      (xslt-process-mode 1)
	      (xslt-process-toggle-debug-mode 1)))
	buffer))))

(provide 'xslt-process)
