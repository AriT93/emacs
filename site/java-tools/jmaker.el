;; jmaker.el --- Java Makefile generator

;; Copyright (C) 1998, 2000 by David Ponce

;; Author: David Ponce <david@dponce.com>
;; Maintainer: David Ponce <david@dponce.com>
;; Created: July 22 1998
;; Version: 2.1
;; Keywords: tools
;; VC: $Id: jmaker.el,v 1.24 2002/01/09 12:00:54 ponce Exp $

;; This file is not part of Emacs

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or (at
;; your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; This package is an add-on to the Java Development Environment
;; (JDE) for Emacs. It automatically generates Makefiles to improve
;; Java projects building using make. The default Makefile template
;; provided uses the java compiler and its options from JDE and generate
;; targets to compile all .java files in the current directory.
;;
;; jmaker can generate "meta Makefiles" too, which could be used to
;; build sets of projects. "meta Makefiles" recursively call make on
;; Makefiles found in a directory tree.

;; Installation:
;;
;; Put this file on your Emacs load-path and add (require 'jmaker) into
;; your Emacs startup file.
;;
;; As jmaker does (require 'jde) you can omit it in your .emacs.

;; Usage:
;;
;; M-x `jmaker-generate-makefile' to generate a new Makefile in the
;; given directory. If an old one already exists the command requires
;; confirmation to overwrite it. The command give also the option to
;; update or create Makefiles in the given directory tree.
;; 
;; CAUTION: jmaker doesn't take back any modifications you could have made
;; to existing Makefiles!
;;
;; To build a project with make you can use the `compile' command or the
;; `jde-build' command if `jde-build-use-make' is set to a non-nil value.
;;
;; A JMaker menu item is added to the menu bar in jde-mode.

;; Customization:
;;
;; Use M-x `jmaker-customize' to change jmaker options.

;; Support
;;
;; This program is available at <http://www.dponce.com/>. Any
;; comments, suggestions, bug reports or upgrade requests are welcome.
;; Please send them to David Ponce at <david@dponce.com>

;;; Change Log:

;; $Log: jmaker.el,v $
;; Revision 1.24  2002/01/09 12:00:54  ponce
;; Version 2.1.
;;
;; Revision 1.23  2001/10/26 22:12:00  ponce
;; Require 'jde-compile instead of 'jde.
;;
;; (jmaker-make-use-jde-settings): New option.  If non-nil use JDE's
;; current compilation options else use value of jmaker variables
;; `jmaker-make-compiler' and `jmaker-make-compiler-options'.
;;
;; (jmaker-make-compiler): New option.  The default compiler used by
;; jmaker.
;;
;; (jmaker-make-compiler-options):  New option.  The default compiler
;; options used by jmaker.
;;
;; (jmaker-make-compiler): New function.  Return the Java compiler to
;; use.
;;
;; (jmaker-make-compiler-options): New function.  Return the Java
;; compiler args.
;;
;; (jmaker-makefile-buffer-template): Use `jmaker-make-compiler' and
;; `jmaker-make-compiler-options'.
;;
;; (jmaker-makefile-generator): Don't load JDE project file if
;; `jmaker-make-use-jde-settings' is nil.
;;
;; Revision 1.22  2000/08/18 14:49:18  david_ponce
;; New major version 2.0 of jmaker.el.
;;
;; There is no more meta Makefiles. New generated Makefiles automatically
;; process all sub-directories of a project :-). So, jmaker provides only
;; one command to generate Makefiles in a project directory tree:
;; `jmaker-generate-makefile'.
;;
;; IMPORTANT: the new Makefile template is not compatible with the one
;; created by previous versions of jmaker. You must reset
;; `jmaker-makefile-buffer-template' to its standard settings and redo
;; any customization you have made. Sorry!
;;
;; Revision 1.21  2000/08/16 14:23:42  david_ponce
;; Improved XEmacs compatibility.
;; Fixed a side effect bug in `jmaker-get-makefiles-in-tree'.
;;
;; Thanks to Stephane Nicolas <s.nicolas@videotron.ca> for providing
;; these fixes.
;;
;; Improved dialog mode. Can use 'q' to cancel a dialog and left mouse to
;; click on dialog button.
;;
;; Changed comments to follow standard Emacs coding convention.
;;
;; Revision 1.20  2000/05/25 09:43:22  david_ponce
;; JMaker menu setup now done in `jde-mode-hook'.
;;
;; Revision 1.19  2000/05/12 13:16:48  david_ponce
;; Added new function `jmaker-set-buffer-end-of-line-style' to force the
;; end-of-line style used when writing a Makefile (for example, this can
;; be useful when using a Unix make program, like the one provided in
;; cygwin 1.1.0, under Windows). The default end-of-line style follows
;; Unix convention (LF). This can be changed by customizing the new
;; variable `jmaker-end-of-line-style'.
;;
;; Revision 1.18  2000/04/14 09:04:35  david_ponce
;; With `directory-sep-char' value set to '\' jmaker did not
;; correctly handle directory pathes when generating meta Makefile.
;; To fix this, jmaker enforce usage of an Unix style separator '/'
;; in `jmaker-sub-makefile-targets' function.
;;
;; `jmaker-convert-directory-to-package' now works with different
;; values of `directory-sep-char'.
;;
;; Revision 1.17  2000/03/31 15:26:42  david_ponce
;; Documentation changes.
;;
;; Revision 1.16  2000/03/31 12:45:16  david_ponce
;; Minor changes in code presentation.
;; Improved version of `jmaker-convert-directory-to-package'.
;;
;; Revision 1.15  2000/03/31 10:56:08  david_ponce
;; Code cleanup and better variables and functions naming.
;;
;; As requested by "David Turland" <dtrurland@axarte.com> I have
;; enhanced the `jmaker-generate-meta-makefile' command to ask for
;; updating or creating the Makefiles in the directory tree
;; before generating the meta Makefile.
;;
;; Revision 1.14  2000/03/31 08:19:31  david_ponce
;; This is a major rewrite of jmaker to add the new
;; `jmaker-generate-all-makefiles' command.
;;
;; Thanks to "David Turland" <dtrurland@axarte.com> who has suggested
;; this enhancement.
;;
;; Revision 1.13  1999/07/05 21:29:17  ebat311
;; May be the jmaker menu works with XEmacs?
;;
;; Revision 1.12  1999-06-04 17:55:19+02  ebat311
;; Added a "Make" option to the menu  (GNU Emacs only)
;; to launch the standard `compile' command.
;;
;; Revision 1.11  1999-04-22 23:50:36+02  ebat311
;; Added `autoload' cookies.
;;
;; Revision 1.10  1998/11/27 09:23:51  ebat311
;; Compatibility issue. jmaker now has its own menu item on
;; the menu bar (GNU Emacs only!). This avoid compatibility
;; problem when the JDE menu structure change (as in 2.1.2).
;;
;; Revision 1.9  1998/10/20 10:05:21  ebat311
;; Have run `untabify' on the whole source (follows a remark from ricky@siemensdc.com).
;;
;; Revision 1.8  1998/10/08 15:40:33  ebat311
;; Usage comments added related to the new meta Makefiles feature.
;;
;; Revision 1.7  1998/10/05 21:34:55  ebat311
;; `jmaker-generate-...' commands now generate file buffers and
;; require confirmation if the file already exists.
;;
;; Revision 1.6  1998/10/05 21:17:22  ebat311
;; Added super Makefile generation.
;; Some coding optimized and simplified.
;;
;; Revision 1.6  1998/09/29 22:34:45  ebat311
;; Outdated (renamed to 'C:\RCS\C\users\dpe\emacs\jmaker.el,v').
;;
;; Revision 1.5  1998/09/29 22:34:18  ebat311
;; New version-number management functions.
;;
;; Revision 1.4  1998/09/28 22:54:19  ebat311
;; Copyright notice updated.
;;
;; Revision 1.3  1998/09/28 22:18:58  ebat311
;; Fixed an error in function `jmaker-version-number' which did not display
;; the version number.
;;
;; Revision 1.2  1998/09/15 11:26:55  ebat311
;; Added support for Emacs 20.3.1.
;; From Paul Kinnucan: "Emacs 20.3.1 fixes a bug in the derive
;; mode macro that caused the jde-mode hook variable to be named
;; `jde-mode-hooks' (note plural) instead of `jde-mode-hook' (singular),
;; which is customary."
;;
;; Revision 1.1  1998/07/22 13:48:17  ebat311
;; Initial revision
;;

;;; Code:
(require 'jde-compile)
(require 'compile)
(require 'wid-edit)

(defconst jmaker-version "2.0 $Date: 2002/01/09 12:00:54 $"
  "jmaker version  information.")

(defgroup jmaker nil
  "Java Makefile generator"
  :group 'tools
  :prefix "jmaker-")

(defcustom jmaker-makefile-buffer-template
  '(
    "\"####\" 'n"
    "\"#### Java Makefile automatically generated by jmaker \""
    "(jmaker-version-number) 'n"
    "\"#### Creation date: \" (current-time-string) 'n"
    "\"####\" 'n"
    "'n"
    "\"#### Java compiler settings\" 'n"
    "\"JAVAC       = \" (jmaker-make-compiler)  'n"
    "\"JAVAC_FLAGS = \" (jmaker-make-compiler-options) 'n"
    "'n"
    "\"#### Targets settings\" 'n"
    "\"CLASS_FILES   = \" (jmaker-make-get-file-targets) 'n"
    "\"SUBDIRS       = \" (jmaker-make-get-subdir-targets) 'n"
    "\"SUBDIRS_CLEAN = $(patsubst %,%.clean,$(SUBDIRS))\" 'n"
    "\"SUBDIRS_MAKE  = $(patsubst %,%.make,$(SUBDIRS))\" 'n"
    "'n"
    "\"#### Main targets\" 'n"
    "'n"
    "\"# Default\" 'n"
    "\"all: $(CLASS_FILES) $(SUBDIRS_MAKE)\" 'n"
    "'n"
    "\"# Cleanup\" 'n"
    "\"clean: $(SUBDIRS_CLEAN)\" 'n"
    "\"\\t$(RM) *.class\" 'n"
    "\"\\t@echo \\\"Cleanup done.\\\"\" 'n"
    "'n"
    "\"# Rebuild\" 'n"
    "\"build: clean all\" 'n"
    "\"\\t@echo \\\"Rebuild done.\\\"\" 'n"
    "'n"
    "\"#### Aux targets\" 'n"
    "'n"
    "\"# Files compilation\" 'n"
    "\"%.class: %.java\" 'n"
    "\"\\t$(JAVAC) $(JAVAC_FLAGS) $<\" 'n"
    "'n"
    "\"# Sub-directories compilation\" 'n"
    "\"%.make:\" 'n"
    "\"\\t$(MAKE) -k -C $(subst .make,,$@)\" 'n"
    "'n"
    "\"# Sub-directories cleanup\" 'n"
    "\"%.clean:\" 'n"
    "\"\\t$(MAKE) -k -C $(subst .clean,,$@) clean\" 'n"
    "'n"
    "\"# Phony Targets\" 'n"
    "\".PHONY: clean build help\" 'n"
    "'n"
    "\"#### Help\" 'n"
    "\"help:\" 'n"
    "\"\\t@echo \\\"Usage: make [targets...]\\\"\" 'n"
    "\"\\t@echo \\\"\\\"\" 'n"
    "\"\\t@echo \\\"where targets include:\\\"\" 'n"
    "\"\\t@echo \\\"\\\"\" 'n"
    "\"\\t@echo \\\"  help           display this help\\\"\" 'n"
    "\"\\t@echo \\\"  all            compile all (default)\\\"\" 'n"
    "\"\\t@echo \\\"  clean          remove all class files\\\"\" 'n"
    "\"\\t@echo \\\"  build          rebuild all inconditionnally\\\"\" 'n"
    "\"\\t@echo \\\"  <class file>   compile the given file\\\"\" 'n"
    "\"\\t@echo \\\"  <subdir>.make  compile the given subdir\\\"\" 'n"
    "\"\\t@echo \\\"  <subdir>.clean clean the given subdir\\\"\" 'n"
    "'n"
    )
  "*Template for Java Makefile.
Setting this variable defines a template instantiation
command `jmaker-insert-makefile', as a side-effect."
  :group 'jmaker
  :type '(repeat string)
  :set '(lambda (sym val)
          (defalias 'jmaker-insert-makefile
            (tempo-define-template "jmaker-makefile-buffer-template"
                                   (jde-gen-read-template val)
                                   nil
                                   "Insert a Java Makefile in the current buffer."))
          (set-default sym val)))

(defcustom jmaker-end-of-line-style 'unix
  "*Specify end-of-line style used when writing a Makefile.
The possible choices are:

- - 'Default' does not change the default end-of-line conversion.
- - 'Unix'    force UNIX end-of-line conversion (the default).
- - 'Dos'     force DOS end-of-line conversion.
- - 'Mac'     force MAC end-of-line conversion."
  :group 'jmaker
  :type '(choice (const :tag "Default" nil )
                 (const :tag "Unix"    unix)
                 (const :tag "Dos"     dos )
                 (const :tag "Mac"     mac )))

;;;
;;; Common functions
;;;
(defun jmaker-version-number ()
  (and (string-match "\\(.*\\)\\s-+\\$Date:" jmaker-version)
       (match-string 1 jmaker-version)))

(defun jmaker-get-java-names ()
  "Return the list of all .java file names without extension found
in `default-directory'."
  (mapcar 'file-name-sans-extension
          (directory-files default-directory nil ".\\.java$")))

(defun jmaker-check-for-java-files (dir)
  "Helper function for `jmaker-contains-java-files-p'."
  (and (file-directory-p dir)
       (not (string-match "\\(\\.\\.?\\|CVS\\)$" dir))
       (let ((flist (directory-files dir t))
             file)
         (while flist
           (setq file (car flist))
           (if (file-regular-p file)
               (if (string-match ".\\.java$" file)
                   (throw 'jmaker-contains-java-files-p file))
             (jmaker-check-for-java-files file))
           (setq flist (cdr flist))))))

(defun jmaker-contains-java-files-p (dir)
  "Return non-nil if directory tree DIR contains some .java files."
  (catch 'jmaker-contains-java-files-p
    (jmaker-check-for-java-files dir)))

(defun jmaker-get-subdirs-with-makefile (dir)
  "Return a list of DIR sub-directories containing a Makefile."
  (let ((flist (directory-files dir t))
        entry dlist)
    (while flist
      (setq entry (car flist))
      (and (not (string-match "\\(\\.\\.?\\|CVS\\)$" entry))
           (file-regular-p (concat entry "/Makefile"))
           (setq dlist (cons (file-relative-name entry dir) dlist)))
      (setq flist (cdr flist)))
    (nreverse dlist)))

(defun jmaker-set-buffer-end-of-line-style ()
  "Set the end-of-line style used when writing a Makefile (see
variable `jmaker-end-of-line-style')."
  (if (and (not jde-xemacsp) (symbolp jmaker-end-of-line-style))
      (let* ((style (symbol-name jmaker-end-of-line-style))
             (oldcs (symbol-name buffer-file-coding-system))
             (newcs (and (string-match "\\(dos\\|unix\\|mac\\)$" oldcs)
                         (intern (replace-match style t t oldcs)))))
        (and (coding-system-p newcs)
             (not (eq newcs buffer-file-coding-system))
             (setq buffer-file-coding-system newcs)
             (message "end-of-line style set to %s" style)))))

;;;
;;; Makefile parameters
;;;
(defcustom jmaker-make-use-jde-settings t
  "*non-nil to use JDE's current compilation options.
Otherwise use value of jmaker variables `jmaker-make-compiler' and
`jmaker-make-compiler-options'."
  :group 'jmaker
  :type 'boolean)

(defcustom jmaker-make-compiler "javac"
  "The default compiler used by jmaker."
  :group 'jmaker
  :type 'string)

(defcustom jmaker-make-compiler-options "-g -deprecation"
  "The default compiler options used by jmaker."
  :group 'jmaker
  :type 'string)

(defun jmaker-make-compiler ()
  "Return the Java compiler to use.
That is the current JDE's one if not the server or value of the
variable `jmaker-make-compiler' by default."
  (let ((compiler (jde-compile-get-the-compiler)))
    (if (or (not jmaker-make-use-jde-settings)
            (oref compiler :use-server-p))
        jmaker-make-compiler
      (expand-file-name (oref compiler :path)))))

(defun jmaker-make-compiler-options ()
  "Return the Java compiler args.
That is args of the current JDE's compiler or value of the variable
`jmaker-make-compiler-options' by default."
  (let ((compiler (jde-compile-get-the-compiler)))
    (if (not jmaker-make-use-jde-settings)
        jmaker-make-compiler-options
      (concat
       (let* ((jde-quote-classpath nil)
              (cp (jde-compile-classpath-arg
                   (jde-compile-get-the-compiler))))
         (format "%s %S " (car cp) (cadr cp)))
       (mapconcat
        #'identity
        (nthcdr 2 (jde-compile-get-args compiler))
        " ")))))

(defun jmaker-make-get-file-targets ()
  "Return a string containing the list of target .class files."
  (mapconcat (function
              (lambda (x)
                (concat x ".class")))
             (jmaker-get-java-names)
             " "))

(defun jmaker-make-get-subdir-targets ()
  "Return a string containing the list of target sub-directories."
  (mapconcat 'identity
             (jmaker-get-subdirs-with-makefile default-directory)
             " "))
;;;
;;; Generation tools
;;;

(defun jmaker-makefile-generator ()
  "Generate a full java Makefile in the current buffer.
Call `jde-load-project-file' to update the JDE project settings."
  (if jmaker-make-use-jde-settings
      (jde-load-project-file))
  (jmaker-insert-makefile))

(defun jmaker-generate-file-noselect (dir name generator &optional over)
  "Edit the file NAME in directory DIR and return the updated file buffer.
GENERATOR is the function used to generate the file buffer contents.
If the file NAME already exists the command requires confirmation to
overwrite it unless OVER is non-nil."
  (let ((file (concat (file-name-as-directory dir) name)))
    (or over
        (and (file-exists-p file)
             (or (y-or-n-p (format "File `%s' exists; overwrite? " file))
                 (error "Command canceled."))))
    (with-current-buffer (find-file-noselect file)
      (erase-buffer)
      (jmaker-set-buffer-end-of-line-style)
      (funcall generator)
      (current-buffer))))

(defun jmaker-generate-makefile-in-dir (dir)
  "Generate a Java Makefile file in directory DIR.
If the Makefile already exists it is overwritten."
  (with-current-buffer
      (jmaker-generate-file-noselect dir
                                     "Makefile"
                                     'jmaker-makefile-generator
                                     'overwrite)
    (save-buffer)
    (kill-buffer (current-buffer))))

(defun jmaker-do-generate-makefile (root)
  "Helper function for `jmaker-generate-makefile' to actually generate
the Makefile in directory ROOT."
  (with-current-buffer
      (jmaker-generate-file-noselect root
                                     "Makefile"
                                     'jmaker-makefile-generator)
    (makefile-mode)
    (goto-char (point-min))
    (switch-to-buffer (current-buffer))))

;;;
;;; Dialogs
;;;

(defvar jmaker-generall-dialog-selected nil
  "Used by `jmaker-generall-dialog' to hold the list of Makefiles
to be generated.")

(defun jmaker-generall-dialog-setup-selected-aux (root)
  "Helper function for `jmaker-generall-dialog-setup-selected'."
  (mapcar (function
           (lambda (entry)
             (and (jmaker-contains-java-files-p entry)
                  (add-to-list 'jmaker-generall-dialog-selected
                               (file-name-as-directory entry))
                  (jmaker-generall-dialog-setup-selected-aux entry))))
          (directory-files root t)))

(defun jmaker-generall-dialog-setup-selected (root)
  "Initialize `jmaker-generall-dialog-selected' with the list of
sub-directories of ROOT where Makefiles could be generated."
  (message "Scanning directories...")
  (setq jmaker-generall-dialog-selected nil)
  (if (file-directory-p root)
      (jmaker-generall-dialog-setup-selected-aux root))
  (message "Scanning directories...Done"))

(defun jmaker-generall-dialog-toggle-selection (widget &rest ignore)
  "Checkbox widget action used by `jmaker-generall-dialog' to select
or unselect a Makefile."
  (let ((value (widget-get widget ':tag))
        (item (widget-get widget ':doc)))
    ;; if value is already in the selected items
    (if (memq value jmaker-generall-dialog-selected)
        ;; then remove it
        (progn
          (setq jmaker-generall-dialog-selected
                (delq value jmaker-generall-dialog-selected))
          (message "%s removed from selection." item))
      ;; else add it
      (progn
        (setq jmaker-generall-dialog-selected
              (nconc (list value) jmaker-generall-dialog-selected))
        (message "%s added to selection." item)))))

(defun jmaker-cancel-dialog (&rest ignore)
  (interactive)
  "Cancel the current dialog."
  (kill-buffer (current-buffer))
  (error "Command canceled."))

(defvar jmaker-dialog-mode-map nil
  "`jmaker-dialog-mode' keymap.")

(if jmaker-dialog-mode-map
    ()
  (setq jmaker-dialog-mode-map (make-sparse-keymap))
  (define-key jmaker-dialog-mode-map "q" 'jmaker-cancel-dialog)
  (define-key jmaker-dialog-mode-map [down-mouse-1] 'widget-button-click)
  (set-keymap-parent jmaker-dialog-mode-map widget-keymap))

(defun jmaker-dialog-mode ()
  "Major mode used in jmaker dialogs.

These are the special commands of jmaker-dialog-mode mode:
    q            -- to cancel the dialog.
    down-mouse-1 -- to click on button."
  (interactive)
  (setq major-mode 'jmaker-dialog-mode)
  (setq mode-name "jmaker-dialog")
  (use-local-map jmaker-dialog-mode-map))

(defconst jmaker-generall-dialog-header
  "Select/Unselect Makefiles to be generated by `jmaker'
in directory tree: %s

  (NEW)  indicates that this Makefile does not exist and will be created.
  (OVER) indicates that this Makefile exists and will be overwritten.

Click on Ok to generate or on Cancel to quit.\n\n"
  "`jmaker-generall-dialog' dialog header.")

(defvar jmaker-generall-dialog-callback-fun nil
  "Buffer local variable used by `jmaker-generall-dialog' to
hold the callback function.")

(defvar jmaker-generall-dialog-callback-arg nil
  "Buffer local variable used by `jmaker-generall-dialog' to
hold the callback argument.")

;;;###autoload
(defun jmaker-generall-dialog (root &optional callback)
  "Show a dialog which allows the user to select or unselect the Makefiles
to be generated in the directory tree ROOT.
CALLBACK is an optional function called with the argument ROOT,
after the command complete (that is after Makefiles were generated)."
  (with-current-buffer (get-buffer-create "*jmaker-generall-dialog*")
    (switch-to-buffer (current-buffer))
    (kill-all-local-variables)

    ;; Save the callback function and its argument in buffer local variables
    (make-local-variable 'jmaker-generall-dialog-callback-fun)
    (make-local-variable 'jmaker-generall-dialog-callback-arg)
    (setq jmaker-generall-dialog-callback-fun callback)
    (setq jmaker-generall-dialog-callback-arg root)
    
    (let ((inhibit-read-only t))
      (erase-buffer))
    (if (not jde-xemacsp)
        (let ((all (overlay-lists)))
          ;; Delete all the overlays.
          (mapcar 'delete-overlay (car all))
          (mapcar 'delete-overlay (cdr all))))
    
    ;; Initialize `jmaker-generall-dialog-selected' so all
    ;; Makefile checkboxes default to selected.
    (jmaker-generall-dialog-setup-selected root)

    ;; Insert the dialog header
    (widget-insert (format jmaker-generall-dialog-header root))

    ;; Insert the list of Makefiles as checkboxes
    (mapcar (function
             (lambda (dir)
               (let ((item (concat dir "Makefile")))
                 (setq item (concat (file-relative-name item root)
                                    (if (file-exists-p item) " (OVER)" " (NEW)")))
                 (widget-create 'checkbox
                                :value  (memq dir jmaker-generall-dialog-selected)
                                :format "%[%v%] %d"
                                :tag    dir
                                :doc    item
                                :notify 'jmaker-generall-dialog-toggle-selection))))
            jmaker-generall-dialog-selected)
    
    (widget-insert "\n\n")

    ;; Insert the Ok button
    (widget-create 'push-button
                   :notify (lambda (&rest ignore)
                             (let ((n (length jmaker-generall-dialog-selected))
                                   (callback jmaker-generall-dialog-callback-fun)
                                   (root jmaker-generall-dialog-callback-arg))
                               (kill-buffer (current-buffer))
                               (mapcar 'jmaker-generate-makefile-in-dir
                                       jmaker-generall-dialog-selected)
                               (message "%S Makefile%s generated."
                                        (if (= n 0) "No" n)
                                        (if (> n 1) "s" ""))
                               (and callback (funcall callback root))))
                   "Ok")

    (widget-insert " ")

    ;; Insert the Cancel button
    (widget-create 'push-button
                   :notify 'jmaker-cancel-dialog
                   "Cancel")

    ;; Display the dialog
    (jmaker-dialog-mode)
    (widget-setup)
    (goto-char (point-min))))

;;;
;;; Commands
;;;

;;;###autoload
(defun jmaker-generate-makefile (root)
  "Generate a Makefile in directory ROOT. If the Makefile already
exists the command requires confirmation to overwrite it. Give also
the option to update or create the Makefiles in the ROOT tree. If so,
displays a dialog to select the Makefiles that will be created or
overwritten, potentially in sub-directories of ROOT where .java files
exist (see also `jmaker-generall-dialog')."
  (interactive "DDirectory: ")
  (if (y-or-n-p (format "Update or create the Makefiles in %s?" root))
      (jmaker-generall-dialog root 'jmaker-do-generate-makefile)
    (jmaker-do-generate-makefile root)))

;;;###autoload
(defun jmaker-customize ()
  "Show the jmaker customization options panel."
  (interactive)
  (customize-group "jmaker"))

;;;
;;; Setup
;;;

(defvar jmaker-menu
  (list "JMaker"
        ["New Makefile..."  jmaker-generate-makefile t]
        ["Options..."       jmaker-customize t]
        ["Make"             compile t]
        ["-"                ignore nil]
        (concat "jmaker " (jmaker-version-number))
        )
  "Menu for jmaker.")

(add-hook 'jde-mode-hook
          (function
           (lambda ()
             (require 'easymenu)
             (if jde-xemacsp
                 (add-submenu '("JDE") jmaker-menu)
               (easy-menu-add-item jde-mode-map '("menu-bar") jmaker-menu)))))

(provide 'jmaker)

;;; jmaker.el ends here
