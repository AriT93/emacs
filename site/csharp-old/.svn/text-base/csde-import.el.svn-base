;;; csde-import-org.el --- Organize Csharp imports

;; Adapted from the JDE by Matt Bruce <matt.bruce@morganstanley.com>
;; Maintainer:  Matt Bruce

;; Copyright (C) 2001 by Matt Bruce

;; JDE version Copyright (C) 2000 by David Ponce

;; JDE Authors:     David Ponce <david@dponce.com>
;;              Paul Kinnucan <paulk@mathworks.com>
;; JDE Maintainers: David Ponce <david@dponce.com>
;;              Paul Kinnucan <paulk@mathworks.com>
;; Created: 15 Nov 2000
;; Version: $Revision: 1.1 $
;; Keywords: csharp, tools
;; VC: $Id: csde-import.el,v 1.1 2001/02/12 05:42:50 paulk Exp $

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
;; 
;; This library adds commands to the CSDE to insert and organize Csharp import
;; statements.

;; The latest version of the CSDE is available at
;; <URL:http://www.sourceforge.com/>.

;; Please send any comments, bugs, or upgrade requests to
;; Matt Bruce (matt.bruce@morganstanley.com)

;;; History:
;;
;; $Log: csde-import.el,v $
;; Revision 1.1  2001/02/12 05:42:50  paulk
;; Initial XEmacs revision.
;;
;; Revision 1.2  2000/11/27 06:18:40  paulk
;; Miscellaneous bug fixes and minor enhancements.
;;
;; Revision 1.1  2000/11/20 05:15:15  paulk
;; Added csde-import-organize command. Moved all import-related code from
;; csde-wiz.el to a new package named csde-import.el.
;;
;; Revision 1.2  2000/11/17 11:52:54  david_ponce
;; - New `csde-import-group-function' option to specify the function used
;;   to associate import token to group. The default one is
;;   `csde-import-group-of'. This let the user to completely handle the
;;   way imports are grouped.
;;
;; - New `csde-import-sorted-groups' option to specify if groups will be
;;   sorted. Notice that the *default* group (the one that contains
;;   imports not belonging to any specific group) is allways the last
;;   group.
;;
;; - Improvement of the function `csde-import-group-of'. For consistency
;;   `csde-import-group-rules' is now `csde-import-group-of-rules' and it
;;   is now possible to associate a group regexp to a particular name.
;;
;; Revision 1.1  2000/11/17 11:48:31  david_ponce
;; Initial Revision.
;;

;;; Code:

;;;;
;;;; Customization
;;;;

;; begin JVL enhancement contributed by Jim LoVerde <loverde@str.com>
(defcustom csde-import-excluded-packages '("bsh.*")
  "*Specifies classes that should not be imported into a source file.
The value of this variable should be a regular expression. The
`csde-import-find-and-import' command does not import any classes whose
fully qualified names match the regular expression. If more than one
fully qualified class name matches the unqualified name that you specify,
the command prompts you to select only the classes that do not match the
regular expression."
  :group 'csde-project
  :type '(repeat (string :tag "Package")))
;; end JVL enhancement contributed by Jim LoVerde <loverde@str.com>

;; auto sorting of import statements
(defcustom csde-import-auto-sort nil
  "*Automatically resort import statements after a `csde-import-import'.
If non-nil, the CSDE automatically resorts the import statements when a new import statement is added using `csde-import-import' or `csde-import-find-and-import'."
  :group 'csde-project
  :type 'boolean
)

(defcustom csde-import-reverse-sort-group nil
  "*Non-nil to sort each import group's packages in reverse alphabetic
order.  See command `csde-import-organize'.  Note: For sorting the
groups, see variable `csde-import-sorted-groups'."
  :group 'csde-project
  :type 'boolean)

(defcustom csde-import-sorted-groups nil
  "*Non-nil to sort import groups in alphabetic order. See command
`csde-import-organize'. Note: For sorting the packages within each
group, see variable `csde-import-reverse-sort-group'."
  :group 'csde-project
  :type '(choice :tag "Order"
                 (const :tag "No sort"                  nil)
                 (const :tag "alphabetic order"         asc)
                 (const :tag "reverse alphabetic order" desc)))

(defcustom csde-import-group-function 'csde-import-group-of
  "*Function used to associate an import token to a group.
It receives one argument, the import token and must return a group
name string or nil if the import does not belong to any group.  The
function `csde-import-group-of' is the default value."
  :group 'csde-project
  :type 'function)

(defcustom csde-import-group-of-rules
  '(
    ("^csharpx?\\.")
    )
  "*Import group definitions used by `csde-import-group-of'.
Each group definition is a pair (REGEXP . GROUP) where:
- - REGEXP is a regexp that import names of this group must match.
- - GROUP is a group name or the index of the match data returned as
    group name or nil if REGEXP is the group name."
  :group 'csde-project
  :type '(repeat
          (cons :tag "Group Rule"
                regexp
                (choice :tag "Group Name"
                        (string  :tag "A String")
                        (integer :tag "Match data at")
                        (const   :tag "The Regexp" nil))))
  :set '(lambda (sym val)
          ;; delete empty entries!
          (set-default sym (delete '("") val))))

(defun csde-import-get-imports()
  (let ((import-re "import[ ]+\\(.*\\)[ ]*;")
	(imports nil))    
    (save-excursion
      (goto-char (point-max))
      (while (re-search-backward import-re (point-min) t)
	(looking-at import-re)
	(setq imports (nconc imports 
			     (list (buffer-substring-no-properties 
				    (match-beginning 1) 
				    (match-end 1)))))))
    imports))


(defun csde-import-get-import-insertion-point ()
  (let ((ip-re
	 (list (cons "import[ ]+\\(.*\\)[ ]*;" 'backward)
	       (cons "package[ \t]+\\(.*\\)[ \t]*;" 'backward)
	       (cons "^$" 'forward)))
	insertion-point n)
    (save-excursion
      (let ((i 0)
	    (n (length ip-re)))
	(while (and
		(not insertion-point)
		(< i n))
	  (let ((re (car (nth i ip-re)))
		(direction (cdr (nth i ip-re))))
	    (if (eq direction 'forward)
		(progn
		  (goto-char (point-min))
		  (setq insertion-point (re-search-forward re (point-max) t)))
	      (goto-char (point-max))
	      (setq insertion-point (re-search-backward re (point-min) t)))
	    (when insertion-point
	      (forward-line 1)
	      (setq insertion-point (point))))
	  (setq i (+ i 1)))))
    insertion-point))


(defun csde-import-import (class) 
  "*Insert an import statement for a class in the current buffer.
CLASS is the fully qualified name of the class to be imported. This
function allows you to enter an import at the head of your buffer
from any point in the buffer. The function does nothing if an import
statement for the specified class alrady exists."
  (interactive
   "sClass: ")
  (csde-import-insert-imports (list class)))

;; Contributed by David Ponce <david_ponce@mail.schneider.fr>
(defun csde-import-sort (&optional reverse)
  "Sort Csharp import statements alphabetically. In reverse order if
REVERSE is non-nil.

Usage:
  \\[csde-import-sort] sort import statements ascending.
  \\[universal-argument] \\[csde-import-sort] sort descending.

The the current buffer must be in `csde-mode'. This command uses the
semantic Csharp parser and requires CSDE 2.1.6-beta24 and above."
  (interactive "P")
  (or (eq major-mode 'csde-mode)
      (error "Invalid major mode found. Must be 'csde-mode'."))
  (or (and (local-variable-p 'semantic-toplevel-bovine-table (current-buffer))
           (symbol-value 'semantic-toplevel-bovine-table))
      (error "Semantic Csharp parser not found. CSDE 2.1.6-beta24+ needed."))
  (and (interactive-p)
       (consp current-prefix-arg)
       (setq reverse t))
  (let* ((tokens  (semantic-bovinate-toplevel))
         (depends (semantic-find-nonterminal-by-token 'include tokens)))
    (if depends
        (let* ((first-import-token (car depends))
               (last-import-token  (nth (1- (length depends)) depends))
               (start (semantic-token-start first-import-token))
               (end   (semantic-token-end   last-import-token)))
          (when (and start end)
	    (require 'sort)
	    (let (sort-fold-case)
                (sort-lines reverse start end)
                (goto-char start)))))))


;; begin JVL enhancement contributed by Jim LoVerde <loverde@str.com>
(defun csde-import-find-and-import (class)
  "*Insert an import statement for a class in the current buffer.
CLASS is an unqualified class name. This function searches
the classpath for a class (or classes) that match CLASS. If it
finds only one, it inserts an import statements for the class at the
head of the current buffer. If it finds more than one class that matches
CLASS, it prompts you to select which class to import. You can use
the variable `csde-import-excluded-packages' to prevent
specified classes from being imported or consider for import. This command uses
the CSDE's BeanShell interpreter. It starts the interpreter if it is not
already running so there may be a short delay generating the first
import statement in the session. Note that you must explicitly include
any directories or jars that you want the command to search in your
classpath, including jars implicitly included by the jvm, e.g.,
rt.jar."
  (interactive
   (list (read-from-minibuffer "Class: "
			       (thing-at-point 'symbol))))
  (let (existing-import)
    (setq existing-import (csde-import-get-existing-import class))
    (if (not (null existing-import))
	(message "Skipping: already imported %s" existing-import)
      (let ((imports
             (bsh-eval-r
              (concat "csde.util.CsdeUtilities.getQualifiedName(\""
                      class "\");"))))
        (if imports
            (csde-import-insert-imports imports)
          (message "Error: could not find %s." class))))))
;; end JVL enhancement contributed by Jim LoVerde <loverde@str.com>

;; begin JVL enhancement contributed by Jim LoVerde <loverde@str.com>
(defun csde-import-insert-imports (new-imports)
  (let* ((imports
	  (mapcar 'csde-import-strip-excluded-imports
	   (csde-import-strip-existing-imports new-imports 
					   (csde-import-get-imports)))))
    ;;Delete the nil which result from the excluded ones
    (setq imports (delq nil imports))
    ;; If more than one class matches the specified class name, 
    ;; prompt the user to select a class for import.
    (if (> (length imports) 1 )
	(csde-import-choose-imports imports)
      (csde-import-insert-imports-into-buffer imports))))
;; end JVL enhancement contributed by Jim LoVerde <loverde@str.com>

(defun csde-import-strip-excluded-imports (new-import)
  "Removes excluded imports from the list"
  ;; If the string matches the regexp, we want to ignore it.
  (if csde-import-excluded-packages
      (let (i n result)
        (setq i 0)
        (message "exclude-regexp=%s"
		 csde-import-excluded-packages)
        (setq n (length csde-import-excluded-packages))
        (setq result new-import)
        (while (< i n)
          (let ((exclude-regexp
                 (nth i csde-import-excluded-packages)))
            (message "exclude-regexp=%s" exclude-regexp)
            (message "new-import=%s" new-import)
	    (if (or (not (string-match "[.]" new-import))
		    (string-match exclude-regexp new-import))
                (progn
                  (message "Excluding import: %s" new-import)
                  (setq result nil)))
            (setq i (+ i 1))))
        result)
    new-import))

(defun csde-import-strip-excluded-import (exclude-regexp new-import)
  "Removes excluded imports from the list"
  ;;if the string matchs the regexp we want to ignore it.
  (if (string-match exclude-regexp (concat " " new-import))      
      (progn (message "Excluding import: %s" new-import)
             ())
    new-import))

(defun csde-import-insert-imports-into-buffer (new-imports)
  "Inserts imports into the correct place in the buffer."
  (let (i n)
    (save-excursion
      (goto-char (csde-import-get-import-insertion-point))
      (setq i 0)
      (setq n (length new-imports))
      (while (< i n)
	(let (deactivate-mark) t)
	(let ((new-import 
	       (nth i new-imports)))
	  (progn
	    (insert
	     (concat "import " new-import ";\n"))
	    (message "Imported %s" new-import))
	  (setq i (+ i 1)))
	(if csde-import-auto-sort
	    (csde-import-sort))))))


(defun csde-import-strip-existing-imports (new-imports existing-imports)
  "Exclude classes that have already been imported."
  (let (i n return-imports)
    (setq i 0)
    (setq n (length new-imports))
    (while (< i n)
      ;;iterate through the new imports
      (let((new-import
	    (nth i new-imports)))
	;;Strip out those alreay there
	(when (not (find new-import existing-imports :test 'string=))
	  (setq return-imports (nconc (list new-import)
				      return-imports))))
      (setq i(+ i 1)))
    ;;Return any that still exist
    return-imports))


;; begin JVL enhancement contributed by Jim LoVerde <loverde@str.com>
(defun csde-import-get-existing-import (class-name)
  ""
  (let ((import-re "import[ ]+\\(.*\\)[ ]*;")
	(imports nil)
        (existing-import)
        (result nil))
    (save-excursion
      (goto-char (point-max))
      (while (re-search-backward import-re (point-min) t)
	(looking-at import-re)
        (setq existing-import (buffer-substring-no-properties
                               (match-beginning 1)
                               (match-end 1)))
        (if (string-equal class-name
                          (csde-import-strip-package-from-class
			   existing-import))
            (setq result existing-import))))
    result))

(defun csde-import-already-imports-class (class-name)
  "Determine if a class is already being imported (ignoring packages)"
  (find class-name (csde-import-get-imports-no-package) :test 'string=))

(defun csde-import-strip-package-from-class (class-name)
  "Strips the package name from fully qualified csharp class"
  (let (i return-name)
    (setq return-name class-name)
    (setq i (string-match "[^.]*$" class-name))
    (if i
        (setq return-name (substring class-name i)))
    return-name))

(defun csde-import-get-imports-no-package()
  (let ((import-re "import[ ]+\\(.*\\)[ ]*;")
	(imports nil))
    (save-excursion
      (goto-char (point-max))
      (while (re-search-backward import-re (point-min) t)
	(looking-at import-re)
	(setq imports (nconc imports
			     (list (csde-import-strip-package-from-class
				    (buffer-substring-no-properties
				     (match-beginning 1)
				     (match-end 1))))))))
    imports))
;; end JVL enhancement contributed by Jim LoVerde <loverde@str.com>


(defun csde-import-choose-imports (new-imports)
  "Prompts the user to select a class to import from a list of similarly
 named candidates."
  (let ((buf (get-buffer-create "*Select Import Class*" )))
    (setq csde-import-import-window-config (current-window-configuration))
    (setq csde-import-selected-import (car new-imports))
    (set-buffer buf)
    (widget-insert "Several classes match the name you specified.\n")
    (widget-insert "Select the one you want to import.\n")
    (widget-insert "Then click the OK button.\n" )
    (let ((args (list
		 'radio-button-choice
		 :value (car new-imports)
		 :notify (lambda (widget &rest ignore)
			   (setq csde-import-selected-import (widget-value widget))
			   (message "You selected: %s"
				    (widget-value widget))))))
      (setq args (nconc
		  args
		  (mapcar (lambda (x) (list 'item x)) new-imports)))
      (apply 'widget-create args))
    (widget-insert "\n")
    (widget-create 'push-button
		   :notify (lambda (&rest ignore)
			     (let ((dialog-buffer
				    (current-buffer)))
			       (if pop-up-windows (delete-window))
			       (kill-buffer dialog-buffer)
			       (set-window-configuration csde-import-import-window-config)
			       (csde-import-insert-imports-into-buffer (cons csde-import-selected-import nil))
			       (message "Import complete.")))
		   "Ok")
    (use-local-map widget-keymap)
    (widget-setup)
    (pop-to-buffer buf)))


;; Contributed by David Ponce.
(defun csde-import-kill-extra-imports (&optional comment)
  "Delete extra Csharp import statements.
An import statement is considered extra if it is a duplicate,
imports a class from the package to which this file belongs,
or imports a class belonging to an already imported package, i.e.,
a package already imported by an import statement ending in .*.
If optional argument COMMENT is non-nil, the extra import statements
are commented out instead of deleted. 

Usage:
  \\[csde-import-kill-extra-imports]
  to kills extra imports.
  \\[universal-argument] \\[csde-import-kill-extra-imports]
  to comment out extra imports.

The current buffer must be in `csde-mode'."
  (interactive "P")
  (or (eq major-mode 'csde-mode)
      (error "Major mode must be 'csde-mode'"))
  (and (interactive-p)
       (consp current-prefix-arg)
       (setq comment t))
  (let* ((tokens   (semantic-bovinate-toplevel))
         (imports  (semantic-find-nonterminal-by-token 'include tokens)))
    (if (not imports)
        (message "No import found")
      (let* ((packages (semantic-find-nonterminal-by-token 'package tokens))
	     (package-imports
	      (append
	       (mapcar 
		     (lambda (package)
		       ;; Return a global import name from PACKAGE token.
		       ;; That is add ".*" at end of token name.
		       (concat (semantic-token-name package) ".*")) 
		     packages)
                    (delq nil 
			  (mapcar 
			   (lambda (import)
			     ;; Return token name if IMPORT is global or nil if not.
			     ;; IMPORT is global if its name ends with ".*".
			     (let ((name (semantic-token-name import)))
			       (and (string-match "[.][*]\\'" name)
				    name)))
			   imports))))
	     (first-import (car imports))
	     extra-imports 
	     required-imports)
      ;; Get the list of extra imports
      (while imports
	(let* ((import (car imports))
	       (name (semantic-token-name import)))
	  (if (or 
	       ;; If name is already listed in the set
               ;; of required imports...
	       (member name required-imports)
	       ;; or imports a class in the current package...
	       (and
		;; make sure name is not a package import, e.g., foo.bar.*
		(not (string-match "[.][*]\\'" name))
		(member 
		 ;; convert class import to equivalent package import
                 ;; e.g., foo.barClass to foo.*
		 (concat
		  (substring 
		   name 
		   0  (or (string-match "[.][^.]+\\'" name)
			  (length name)))
		  ".*")
		 package-imports)))
	      ;; add name to the list of extra imports...
	      (setq extra-imports (cons import extra-imports))
	    ;; otherwise add to the list or required  imports
	    (setq required-imports (cons name required-imports))))
	(setq imports (cdr imports)))
      (if (not extra-imports)
          (message "No extra imports found")
	(let ((count 0))
	  ;; Move the point at the beginning of the first import
	  (goto-char (semantic-token-start first-import))
	  (save-excursion
          ;; Kill or comment out extra imports
          (while extra-imports
	    (let* ((extra-import (car extra-imports))
		   (start (semantic-token-start extra-import))
		   (end (semantic-token-end extra-import)))
	      (setq count  (1+ count))
	      (if comment
		  (comment-region start end)
		;; The following assumes that there is only one import
		;; statement on the same line. Line end comments are deleted
		;; too.
		(kill-region start
			     (progn
			       (goto-char end)
			       (forward-line)
			       (point))))
	      (setq extra-imports (cdr extra-imports))))
	  (message "%d extra import%s removed"
                 count (if (= count 1) "" "s")))))))))

;;;;
;;;; Helper functions
;;;;

(defun csde-import-group-of (import-token)
  "Return the group IMPORT-TOKEN belongs to or nil if not found.
A group is found as soon as the import name matches a regexp in
`csde-import-group-of-rules'.  The returned group name depends on the
corresponding group definition in `csde-import-group-of-rules'."
  (let ((import-name (semantic-token-name import-token))
        (groups      csde-import-group-of-rules)
        match rule regexp group)
    (while (and groups (not match))
      (setq rule    (car groups)
            groups  (cdr groups)
            regexp  (car rule)
            group   (cdr rule)
            match   (and (string-match regexp import-name)
                         (cond ((stringp  group)
                                group)
                               ((integerp group)
                                (match-string group import-name))
                               (t
                                regexp)))))
    match))

(defun csde-import-bucketize (imports)
  "Bucketize IMPORTS tokens.
Return a vector of buckets.  Each bucket is sorted alphabetically by
import name or in reverse order if `csde-import-reverse-sort-group' is
non-nil.  There is a bucket for each different group the function
specified by `csde-import-group-function' returns.  The last extra
bucket contains imports that do not belong to any group."
  (let (import group others bins bin i n)
    ;; Sort imports into an alist of groups.  Build a separate list
    ;; for imports not in any group.
    (while imports
      (setq import  (car imports)
            imports (cdr imports)
            group   (funcall (or csde-import-group-function
                                 #'csde-import-group-of)
                             import))
      (if (not group)
          (setq others (cons import others))
        (setq bin (assoc group bins))
        (if bin
            (setcdr bin (cons import (cdr bin)))
          (setq bins (cons (cons group (list import)) bins)))))
    ;; If required sort the bins by group name
    ;; Remember that bins are in reverse order at this point.
    (cond ((eq csde-import-sorted-groups 'asc)
           (setq bins (sort bins
                            (function
                             (lambda (bin1 bin2)
                               (string-lessp (car bin2)
                                             (car bin1)))))))
          ((eq csde-import-sorted-groups 'desc)
           (setq bins (sort bins
                            (function
                             (lambda (bin1 bin2)
                               (string-lessp (car bin1)
                                             (car bin2))))))))
    ;; Build the vector of buckets.
    (setq bins (apply 'vector
                      (delq nil
                            (nreverse (cons others
                                            (mapcar #'cdr bins)))))
          n    (length bins)
          i    0)
    ;; Sort each bucket.
    (while (< i n)
      (setq bin (aref bins i))
      (aset bins i (if csde-import-reverse-sort-group
                       (semantic-sort-tokens-by-name-decreasing bin)
                     (semantic-sort-tokens-by-name-increasing bin)))
      (setq i (1+ i)))
    bins))

(defun csde-import-insert-group (group &optional skip-line)
  "Insert a GROUP of import texts in the current buffer.
If optional SKIP-LINE is non-nil skip a line before the group."
  (when group
    (if skip-line
        (newline 2))
    (insert (car group))
    (setq group (cdr group))
    (while group
      (newline)
      (insert (car group))
      (setq group (cdr group)))))

;;;;
;;;; Commands
;;;;

;;;###autoload
(defun csde-import-organize (&optional force)
  "Organize import statements of the current Csharp source buffer.
If optional FORCE is non-nil force reordering even if imports are
already organized.

Imports are organized into groups returned by the function specified
by `csde-import-group-function'.  Groups are inserted in the order they
are found unless `csde-import-sorted-groups' requires that they must be
alphabetically sorted.  In each group imports are sorted by name
alphabetically or in reverse order if `csde-import-reverse-sort-group'
is non-nil.  A blank line is inserted between groups.

Usage:
  \\[csde-import-organize] group and sort import statements.
  \\[universal-argument] \\[csde-import-organize] to force reordering.

The current buffer must be in `csde-mode'.  This command requires a
version of the CSDE with the semantic parser."
  (interactive "P")
  (or (eq major-mode 'csde-mode)
      (error "Major mode must be 'csde-mode'"))
  (and (interactive-p)
       (consp current-prefix-arg)
       (setq force t))
  (let* ((tokens  (semantic-bovinate-toplevel t))
         (imports (semantic-find-nonterminal-by-token 'include tokens)))
    (if imports
        (let* ((bins (csde-import-bucketize imports))
               (n    (length bins))
               i l sl changed group)
          (if force
              (setq changed t)
            ;; Check if imports already ordered
            (setq sl (aref bins 0)
                  i  1)
            (while (< i n)
              (setq sl (append sl (aref bins i))
                    i  (1+ i)))
            (setq l imports)
            (while (and l (not changed))
              (setq changed (not (string-equal
                                  (semantic-token-name (car l))
                                  (semantic-token-name (car sl))))
                    l  (cdr l)
                    sl (cdr sl))))
          (if (not changed)
              (message "Import statements already ordered")
            ;; Imports need to be reordered.
            ;; 1- Get ordered import texts
            (setq i 0)
            (while (< i n)
              (aset bins i
                    (mapcar (function
                             (lambda (import)
                               (buffer-substring-no-properties
                                (semantic-token-start import)
                                (progn
                                  (goto-char (semantic-token-end import))
                                  (end-of-line) ; keep any line comment
                                  (point)))))
                            (aref bins i)))
              (setq i (1+ i)))
            ;; 2- Keep the point at the beginning of the first import
            (goto-char (semantic-token-start (car imports)))
            ;; 3- Kill current imports
            (kill-region (point)
                         (progn
                           (goto-char (semantic-token-end
                                       (car (reverse imports))))
                           (end-of-line)
                           (point)))
            ;; 4- Insert ordered imports
            (save-excursion
              ;; Insert the first group found
              (setq i 0)
              (while (and (< i n) (not group))
                (setq group (aref bins i)
                      i     (1+ i)))
              (csde-import-insert-group group)
              ;; Insert the others with a blank line before each group
              (while (< i n)
                (csde-import-insert-group (aref bins i) 'skip-line)
                (setq i (1+ i)))))))))

(provide 'csde-import)

;;; csde-import.el ends here
