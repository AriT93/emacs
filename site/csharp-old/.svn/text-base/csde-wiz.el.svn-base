;;; csde-wiz.el;; $Revision: 1.3 $

;; Adapted from the JDE by Matt Bruce <matt.bruce@morganstanley.com>

;; Copyright (C) 2001 by Matt Bruce
;; Maintainer:  Matt Bruce

;; JDE Author: Paul Kinnucan <paulk@mathworks.com>
;; JDE Maintainer: Paul Kinnucan

;; Keywords: csharp, tools

;; JDE Copyright (C) 1997, 1998, 2000 Paul Kinnucan.

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

(require 'beanshell)
(require 'csde-complete)



;;

(defun csde-wiz-escape-backslashes-in-path (path) 
  (mapconcat
   (lambda (char) 
     (if (eq char ?\\) "\\\\" (char-to-string char)))
   path ""))

(defun csde-wiz-update-class-list()
  "Update the class list used to resolve class names.
The first time you invoke a CSDE wizard, the CSDE builds a list of all classes on the classpath
defined by csde-global-classpath. Wizards use this list to resolve unqualified class names. If you add any classes to the classpath after invoking a wizard, you should update the class list."
  (interactive)
  (when (get-process "bsh")
    (message "Rescanning classes...")
    (bsh-eval (concat "csde.util.CsdeUtilities.buildClassList("
		      (csde-wiz-escape-backslashes-in-path
		       (csde-build-path-arg nil csde-global-classpath t))
                    ");"))
    (message "Rescanning classes...Complete")))


(defun csde-wiz-get-package-name ()
  (let ((package-re "^[ \t]*package[ \t]+\\(.*\\)[ \t]*;"))
    (save-excursion
      (goto-char (point-max))
      (when (re-search-backward package-re (point-min) t)
	(looking-at package-re)
	(buffer-substring-no-properties
		       (match-beginning 1)
		       (match-end 1))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Method Interface Implementation wizard                                  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun csde-wiz-get-unqualified-name (name)
  (string-match "[^.]+$" name)
  (substring name (match-beginning 0) (match-end 0)))


(defun csde-wiz-update-implements-clause (interface-name)
   (interactive
    "sEnter interface: ")
  (let ((interface 
	(csde-wiz-get-unqualified-name interface-name)))
    (save-excursion
      (let* ((class-re "class[ \t]+\\([a-zA-z]+[a-zA-Z0-9._]*\\).*[ \n]*")
	     (open-brace-pos
	      (scan-lists (point) -1 1))
	     (class-name-end-pos
	      (when open-brace-pos
		(goto-char open-brace-pos)
		(when (re-search-backward class-re (point-min) t)
		  (looking-at class-re)
		  (match-end 1))))
	     (implements-keyword-end-pos
	      (when (and open-brace-pos class-name-end-pos)
		(goto-char open-brace-pos)
		(if (re-search-backward "implements" class-name-end-pos t)
		    (match-end 0)))))
	(if implements-keyword-end-pos
	    (progn
	      (goto-char implements-keyword-end-pos)
	      (insert (concat " " interface ", ")))
	  (when class-name-end-pos
	    (goto-char (- open-brace-pos 1))
	      (insert (concat " implements " interface " "))))))))


(defun csde-wiz-implement-interface (interface-name)
  "*Generate a skeleton implementation of a specified interface."
  (interactive
   "sInterface name: ")
  (condition-case err
      (let* ((nl-brace-p
	      (find 'before 
		    (cdr (assoc 'defun-open c-hanging-braces-alist))))
	     (code
	      (bsh-eval-r
	       (concat
		"csde.wizards.InterfaceFactory.makeInterface(\""
		interface-name "\", true, true, "
		(if nl-brace-p "true" "false") ");"))))
	(if code 
	    (let ((required-imports
		   (bsh-eval-r
		    "csde.wizards.InterfaceFactory.getImportedClasses();")))
	      (insert code)
	      (if required-imports
		  (csde-import-insert-imports-into-buffer required-imports))
	      (csde-wiz-update-implements-clause interface-name))))	  
    (error
     (message "%s" (error-message-string err)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Method override wizard                                                  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun csde-wiz-get-method-class ()
  (let ((class-re "class[ \t]+\\([a-zA-z]+[a-zA-Z0-9._]*\\).*[ \n]*"))
    (save-excursion
      (let ((open-brace-pos
	     (scan-lists (point) -1 1)))
	(when open-brace-pos
	  (goto-char open-brace-pos)
	  (when (re-search-backward class-re (point-min) t)
	    (looking-at class-re)
	    (buffer-substring-no-properties
		     (match-beginning 1)
		     (match-end 1))))))))


(defun csde-wiz-override-method (method-name)
  "Overrides a method whose name you specify.
This command creates a skeleton implementation of the
overridden method at point. This command infers the
qualified name of the class of the overriden method by 
prepending the package name of the current buffer to
the class containing point. If the class defines
more than one method of the same name, this command
prompts you to select the desired method from a list
of method prototypes.

This command also generates import statements for 
the parameter and return types of the overridden method.
The import statements are inserted after the last 
existing import statement or the package statement
or the first blank line in the source file. Import
statements are generated only for types for which an
import statement does not already exist in the file.

NOTE: this command works only if the overriding class 
      has been previously compiled."
  (interactive
   "sMethod name: ")
  (condition-case err
      (let* ((package-name (csde-wiz-get-package-name))
	     (class-name (csde-wiz-get-method-class))
	     (qualified-class-name 
	      (if (and package-name class-name)
		  (concat package-name "." class-name)
		class-name)))
	(if qualified-class-name
	    (let ((signatures
		   (bsh-eval
		    (concat 
		     "csde.wizards.MethodOverrideFactory.getCandidateSignatures(\""
		     qualified-class-name "\",\"" method-name "\");") t)))
	      (if signatures
		  (if (> (length signatures) 1)
		      (csde-wiz-override-variant-method signatures)
		    (csde-wiz-override-method-internal (car signatures)  signatures))))))
    (error
     (message "%s" (error-message-string err)))))

(defun csde-wiz-override-method-internal (selected-method methods)
  (let* ((variant
	(position selected-method methods :test 'string=))
	(nl-brace-p
	  (find 'before 
		(cdr (assoc 'defun-open c-hanging-braces-alist))))
	(skeleton
	  (bsh-eval-r
	   (concat
	    "csde.wizards.MethodOverrideFactory.getMethodSkeleton("
	    (int-to-string variant) 
	    (if nl-brace-p
		", true"
	      ", false")
	    ");")))
	(required-imports
	  (bsh-eval-r
	   "csde.wizards.MethodOverrideFactory.getImportedClasses();")))
    (insert skeleton)
    (if required-imports
	(csde-import-insert-imports required-imports))))


(defun csde-wiz-override-variant-method (methods) 
  (let ((buf (get-buffer-create "*Choose Method*")))
    (setq csde-wiz-source-buffer (current-buffer))
    (setq csde-wiz-method-variants methods)
    (setq csde-wiz-selected-method (car methods))
    (set-buffer buf)
    (widget-insert "Select the method you want to override.\n")
    (widget-insert "Then click the Ok button.\n\n")
    (let ((args (list
		'radio-button-choice
		:value (car methods)
		:notify (lambda (widget &rest ignore)
			   (setq csde-wiz-selected-method (widget-value widget))
			   (message "You selected: %s"
				    (widget-value widget))))))
	  (setq args (nconc
		      args
		       (mapcar (lambda (x) (list 'item x)) methods)))
	  (apply 'widget-create args)
	  )
    (widget-insert "\n")
    (widget-create 'push-button
		   :notify (lambda (&rest ignore)
			     (let ((dialog-buffer
				    (current-buffer)))
			       (set-buffer csde-wiz-source-buffer)
			       (delete-window)
			       (kill-buffer dialog-buffer)
			       (csde-wiz-override-method-internal
				csde-wiz-selected-method 
				csde-wiz-method-variants)
			       (message "Method inserted.")
			     ))
     		"Ok")
    (use-local-map widget-keymap)
    (widget-setup)
    (pop-to-buffer buf)))


;;
;; Contributed by Rohit Namjoshi <Rohit_Namjoshi@trilogy.com>
;;
(defun csde-browse-class ()
  "Displays class at point in BeanShell class browser."
  (interactive)
  (condition-case err
      (let* ((unqualified-name (thing-at-point 'symbol))
             (class-names
              ;;expand the names into full names, or a list of names
              (bsh-eval-r
               (concat "csde.util.CsdeUtilities.getQualifiedName(\""
		       unqualified-name "\");")))
             ;; Pick first match
             (cmd (concat "browseClass(\"" (car class-names) "\");")))
        (if (eq nil class-names)
            (error "Cannot find %s" unqualified-name))
        (message cmd)
        (bsh-eval cmd))
    (error
     (message "%s" (error-message-string err)))))


;; Required (at least for me) because indent-region does not seem to
;; indent csharpdoc comments properly.  This function is used to indent
;; the code inserted by csde-wiz-delegate.  (It might also be useful to
;; indent the output of csde-wiz-implement-interface)
(defun csde-indent-csharp-region (start end)
  (save-excursion
    (goto-char start)
    (beginning-of-line)
    (setq end (set-marker (make-marker) end))
    (c-indent-line)
    (while (< (point) end)
      (c-indent-line)
      (forward-line 1))))


(defun csde-wiz-delegate (attribute-name)
  "*Generate delegation methods for a given attribute.
This method generates methods in the current buffer that delegate
calls to methods of the given ATTRIBUTE of the current class.  For
example, if the current buffer contains Csharp class A and there is
an attribute in A named b of class type B, this method will
generate in A, all the public methods of class B and delegate
handling of those methods via attribute B."
  (interactive
   "sAttribute name: ")
  (condition-case err
      (let* ((start nil)
             (class-name (or (csde-complete-get-qualified-name
                              (csde-parse-declared-type-of attribute-name))
                             (read-string (concat "Enter fully qualified class name of " 
                                                  attribute-name ": "))))
             (nl-brace-p
	      (find 'before 
		    (cdr (assoc 'defun-open c-hanging-braces-alist))))
	     (code
	      (bsh-eval-r
	       (concat
		"csde.wizards.DelegateFactory.makeDelegates(\""
		attribute-name "\", \"" class-name "\", true, true, "
		(if nl-brace-p "true" "false") ");"))))
	(if code 
	    (let ((required-imports
		   (bsh-eval-r
		    "csde.wizards.DelegateFactory.getImportedClasses();")))
              (font-lock-mode -1)
              (setq start (point))
	      (insert code)
              (csde-indent-csharp-region start (point))
	      (if required-imports
		  (csde-import-insert-imports-into-buffer required-imports))
              (font-lock-mode)
              )))	  
    (error
     (message "%s" (error-message-string err)))))

  
(provide 'csde-wiz)

;; $Log: csde-wiz.el,v $
;; Revision 1.3  2001/02/12 05:38:26  paulk
;; CSDE 2.2.7
;;
;; Revision 1.38  2000/12/21 13:24:12  paulk
;; Changed csde-wiz-insert-imports to csde-import-insert-imports to reflect recent repackaging scheme.
;;
;; Revision 1.37  2000/12/18 05:22:46  paulk
;; *** empty log message ***
;;
;; Revision 1.36  2000/11/27 06:18:41  paulk
;; Miscellaneous bug fixes and minor enhancements.
;;
;; Revision 1.35  2000/11/20 05:15:16  paulk
;; Added csde-import-organize command. Moved all import-related code from
;; csde-wiz.el to a new package named csde-import.el.
;;
;; Revision 1.34  2000/11/17 04:02:24  paulk
;; csde-wiz-update-class-list now uses the current csde-global-classpath when building the class list. This eliminates the need to restart the beanshell when switching projects.
;;
;; Revision 1.33  2000/11/16 05:44:56  paulk
;; Fixed problem in csde-sort-imports command. The problem was that csde-sort-imports temporarily defined sort-fold-case just before invoking sort-lines. Invoking sort-lines caused the sort package to be loaded. Since sort-fold-case is already defined, the sort package did not bother to define it. Then sort-lines returns to csde-sort-lines which proceeded to destroy the temporary copy of sort-fold-case. The fix is to have csde-sort-lines require the sort package before invoking sort-lines.
;;
;; Revision 1.32  2000/11/16 04:53:26  paulk
;; Adds csde-wiz-kill-extra-imports command contributed by David Ponce.
;;
;; Revision 1.31  2000/10/25 04:35:20  paulk
;; Updated sort import function to reflect new bovinator syntax.
;;
;; Revision 1.30  2000/10/08 12:55:39  paulk
;; *** empty log message ***
;;
;; Revision 1.29  2000/09/24 08:42:52  paulk
;; Now sorts import list after adding an import. To disable this feature, set csde-auto-sort-imports off. Thanks to "Jason Stell" <Jason.Stell@globalone.net>
;;
;; Revision 1.28  2000/08/03 04:54:11  paulk
;; Restored use of the radio buttons by the import wizard. Somehow this functionality got deleted.
;;
;; Revision 1.27  2000/07/28 06:27:46  paulk
;; Committing all modified files.
;;
;; Revision 1.26  2000/07/14 05:22:57  paulk
;; Adds a delegation wizard contributed by Charles Hart.
;;
;; Revision 1.25  2000/07/13 07:18:24  paulk
;; * You can now specify a list of packages to exclude from import
;;   into a source file. See csde-wiz-import-excluded-packages for
;;   more information. Thanks to "Jim Loverde" <loverde@str.com>
;;   for this enhancement.
;;
;; * Changed name of csde-wiz-insert-excluded-packages-regexp to
;;   csde-wiz-import-excluded-packages.
;;
;; Revision 1.23  2000/06/22 02:50:25  paulk
;; The import wizard dialog now uses radio buttons rather than check boxes to select
;;  the class to import. Thanks to Mark Gibson for this enhancement.
;;
;; Revision 1.22  2000/06/01 06:01:14  paulk
;; Added csde-sort-imports command. Thanks to David Ponce <david_ponce@mail.schneider.fr>.
;;
;; Revision 1.21  2000/01/18 07:11:26  paulk
;; Added csde-show-class-source. Thanks to Phil Lord for the initial
;; implementation of this command.
;;
;; Revision 1.20  1999/12/19 07:02:30  paulk
;; Changed import wizard to use csde.util.CsdeUtilities.getQualifiedName
;; eliminated redundancy. Thanks to Len Trigg <len@intelligenesis.net>
;; for this improvement.
;;
;; Revision 1.19  1999/11/01 03:11:42  paulk
;; Added csde-browse-class contributed by Rohit Namjoshi <Rohit_Namjoshi@trilogy.com>.
;;
;; Revision 1.18  1999/10/17 04:35:05  paulk
;; Fixed a line in csde-wiz.el, where an int is concat'd with some
;; strings.  This is not allowed by XEmacs 21.1.
;;
;; Revision 1.17  1999/10/01 05:58:14  paulk
;; Added csde-wiz-update-class-list function contributed by Phillip Lord.
;;
;; Revision 1.16  1999/09/17 06:52:50  paulk
;; Fixed regression error where the interface wizard was putting quotes
;; around the code inserted into the source buffer.
;;
;; Revision 1.15  1999/08/29 04:29:18  paulk
;; Patches provided by Michael Ernst <mernst@alum.mit.edu>
;;
;; Revision 1.14  1999/08/23 02:13:43  paulk
;; Fixed regression bug in csde-wiz-implement-interface caused by recent
;; change in csde-wiz-insert-imports.
;;
;; Revision 1.13  1999/06/22 21:12:20  paulk
;; Added variable to filter out unwanted classes from the list of classes being
;; considered for import command by csde-find-and-import. The csde-find-and-import
;; command now prompts the user if more than one class matches the specified
;; import name. Thanks to Phillip Lord <plord@hgmp.mrc.ac.uk> for these enhancements.
;;
;; Revision 1.12  1999/05/07 20:42:25  paulk
;; Cosmetic change.
;;
;; Revision 1.11  1999/05/07 20:40:49  paulk
;; Added new command, csde-wiz-find-and-import, that, given an unqualified class
;; name, generates and inserts an import statement for that class.
;;
;; Revision 1.10  1999/02/17 19:16:07  paulk
;; Provided more robust error handling for the interface wizard. The wizard
;; no longer kills the bsh when it cannot create an interface and provides
;; meaningfull error messages.
;;
;; Revision 1.9  1999/02/15 01:12:54  paulk
;; Fixed bug in csde-wiz-get-method-class that caused it to fail when the open bracket
;; for the class was not on the same line as the class keyworkd. Thanks to
;; P.Lord@mdx.ac.uk (Phillip Lord) for diagnosing this bug.
;;
;; Revision 1.8  1999/02/12 15:13:00  paulk
;; Added csde-wiz-import function.
;;
;; Revision 1.7  1999/02/11 19:14:50  paulk
;; Fixed bug in csde-wiz-update-implements-clause.
;;
;; Revision 1.6  1999/02/11 18:28:40  paulk
;; Corrected missing parentheses.
;;
;; Revision 1.5  1998/11/22 22:03:43  paulk
;; Fixed bug in interface wizard.
;;
;; Revision 1.4  1998/11/22 21:55:33  paulk
;; Fixed bug in interface wizard.
;;
;; Revision 1.3  1998/11/21 02:41:34  paulk
;; Fixed bug.
;; Added implements clause update function to interface implementation wizard.
;;
;; Revision 1.2  1998/11/10 00:46:39  paulk
;; Added smart import insertion to interface wizard.
;;
;; Revision 1.1  1998/11/08 00:39:24  paulk
;; Initial revision
;;


;; End of csde-wiz.el
