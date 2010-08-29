;;; csde-complete.el -- Smart completion for the CSDE
;; $Revision: 1.2 $ 

;; Adapted from the JDE by Matt Bruce <matt.bruce@morganstanley.com>
;; Maintainer:  Matt Bruce

;; JDE Author: Rodrigo Reyes <reyes@chez.com>
;; JDE Maintainers: Rodrigo Reyes, Paul Kinnucan, Howard Spector, 
;;              Stephane Nicolas <s.nicolas@videotron.ca>
;; Keywords: csharp, intellisense, completion

;; Copyright (C) 2001 Matt Bruce

;; JDE Code Copyright (C) 1999, 2000, 2001 Rodrigo Reyes, Paul Kinnucan, David Ponce,
;;                          Stephane Nicolas

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

;; This package follows the GNU General Public Licence (GPL), see the
;; COPYING file that comes along with GNU Emacs. This is free software,
;; you can redistribute it and/or modify it under the GNU GPL terms.
;;
;; Csharp is a registered trademark of Sun Microsystem, Inc.
;;
;;; Commentary:

;; This is one of a set of packages that make up the
;; Csharp Development Environment (CSDE) for Emacs. See the
;; CSDE User's Guide for more information.

;;
;; This package adds smart completion to the CSDE. How it works is
;; simple : put the cursor at the end of a statement "under
;; construction", eg. "myVariable.rem<CURSOR HERE> and call the
;; csde-complete-at-point emacs-lisp function (this is by default
;; C-.). A completion is then inserted. If multiple completions are
;; possible, calling the completion function again will cycle through
;; all the possibilities (as dabbrev-mode does).

;; To retrieve all the possible completions, it uses the csharp code in
;; csde.util.Completion.getClassInfo(), called by beanshell. That
;; need the class to be compiled (but that's not worst than an etag
;; call).

;; Known bugs/problems :

;; - Due to the way the JVM works, it is not possible to explicitly
;; unload a class. So, if major changes are done in a class, the
;; beanshell must be restarted in order to reload the class.

;;
;; TODO :
;;
;; - [EASY] Check for the variables,
;; - [NOT THAT EASY] Keep the completion information in the minibuffer
;; (it is currently erased after the user presses a key).
;; - [AVERAGE] Add a cache for the class informations.
;; - Check for fields declared at the end of the class.


;; The latest version of the CSDE is available at
;; <URL:http://www.sourceforge.com/>.

;; Please send any comments, bugs, or upgrade requests to
;; Matt Bruce (matt.bruce@morganstanley.com)


(defvar csde-complete-current-list nil
  "The list of all the completion. Each element of the list is a list
which car is the possible completion, and the cdr is an additional
information about this completion.")

(defvar csde-complete-current-list-index nil
  "An index to an element in csde-complete-current-list. This is used to
cycle the list.")

(defvar csde-complete-current-beginning (make-marker)
  "The beginning of the region where the last completion was inserted.")

(defvar csde-complete-current-end (make-marker)
  "The end of the region where the last completion was inserted.")

;; Modified `csde-complete-import-list' to use semantic parser table
(defun csde-split-import-token (token)
  "Helper function used by `csde-complete-import-list' which return a
list (PACKAGE-DOT CLASS-OR-STAR) from given semantic 'include (that
is Csharp import) TOKEN.
For example:
  : (csde-split-import-token \"csharp.util.Hashtable\")
  > (\"csharp.util.\" . \"Hashtable\")
  : (csde-split-import-token \"csharp.lang.*\")
  > (\"csharp.lang.\" . \"*\")
  : (csde-split-import-token \"test\")
  > (\"test.\" . \"*\")"
  (let* ((import      (semantic-token-name token))
         (match-point (string-match "\\." import))
        split-point)
    (while match-point
      (setq split-point (1+ match-point)
            match-point (string-match "\\." import split-point)))
    (if split-point
        (list (substring import 0 split-point)
              (substring import split-point))
      (list (concat import ".")
            "*"))))

(defun csde-complete-import-list ()
  "Return the list of Csharp packages declared in the current buffer.
It uses the semantic parser table to find the 'package' and 'import'
statements. It implicitly adds the csharp.lang.* package. See also
`csde-split-import-token'."
  (let* ((tokens   (semantic-bovinate-toplevel t))
         (packages (semantic-find-nonterminal-by-token 'package tokens))
         (imports  (semantic-find-nonterminal-by-token 'include tokens))
         lst)
    (setq lst (append
               (mapcar (function
                        (lambda (token)
                          (list
                           (concat (semantic-token-name token) ".")
                           "*")))
                       packages)
               (mapcar 'csde-split-import-token
                       imports)))
    (or (member "csharp.lang.*" lst)
        (setq lst (append lst '(("csharp.lang." "*")))))
    lst))



(defun csde-complete-valid-csharp-declaration-at (point varname)
  "Verify that a POINT starts a valid csharp declaration
for the VARNAME variable."
  (save-excursion
    (goto-char point)
    (if (looking-at 
	 (concat "\\([A-Za-z0-9_.\177-\377]+\\)[ \t\n\r]+" 
		 (csde-complete-double-backquotes varname) 
		 "[ \t\n\r]*[;=]"))
	(match-string 1)
      nil)))
  
(defun csde-complete-double-backquotes (varname)
  "Build a new string identical to VARNAME, except that every backquote
`\' is doubled, so that it can be used in a regex expression"
  (let (result (idx 0) (len (length varname)) curcar)
    (while (< idx len)
      (setq curcar (elt varname idx))
      (setq result (concat result (if (eq curcar ?\\)
				      "\\\\"
				    (make-string 1 curcar))))
      (setq idx (1+ idx)))
    result))

(defun csde-complete-declared-type-of (name)
  "Find in the current buffer the csharp type of the variable NAME.  The
function returns a string containing the name of the class, or nil
otherwise. This function does not give the fully-qualified csharp class
name, it just returns the type as it is declared."
  (save-excursion
    (let (found res pos orgpt resname)
      (while (and (not found)
		  (search-backward name nil t))
	(setq pos (point))
	(backward-word 1)
	(setq resname (csde-complete-valid-csharp-declaration-at (point) name))
	(goto-char pos)
	(forward-char -1)
	(if resname
	    (progn (setq res resname)
		   (setq found t))))
      res)))

(defun csde-complete-filter-fqn (importlist)
  "Filter all the fully-qualified classnames in the import list. It uses
the knowledge that those classnames are at the beginning of the list,
so that it can stops at the first package import (with a star `*' at
the end of the declaration)."
  (if importlist
      (if (string= "*" (car (cdr (car importlist))))
	  importlist
	(csde-complete-filter-fqn (cdr importlist)))))



(defun csde-complete-guess-type-of (name)
  "Guess the fully qualified name of the class NAME, using the import
list. It returns a string if the fqn was found, or a list of possible
packages otherwise."
  (let ((importlist (csde-complete-import-list)) shortname fullname tmp result)
    (while (and importlist (null result))
      (setq tmp (car importlist))
      (setq shortname (car (cdr tmp)))
      (setq fullname (concat (car tmp) name))
      (cond 
       ((string= "*" shortname)
	(setq result importlist))
       ((string= name shortname)
	(setq result fullname))
       (t 
	(setq importlist (cdr importlist)))))
    result))

;; Contributed by Charles Hart <cfhart@Z-TEL.com>
;; Returns t if the fully qualified class name can be found in the
;; classpath, nil otherwise
(defun csde-complete-class-exists (name)
  (bsh-eval-r (concat "csde.util.CsdeUtilities.classExists(\"" name "\");")))

;; Contributed by Charles Hart <cfhart@Z-TEL.com>
;; Get the fully qualified name of the class NAME, using the import
;; list. It returns a string if the fqn was found, or null otherwise.
;; This is more capable than csde-complete-guess-type-of because it
;; uses the beanshell to determine if an import statement with a
;; wildcard contains the unqualified class name passed to this
;; function.
(defun csde-complete-get-qualified-name (name)
  "Guess the fully qualified name of the class NAME, using the import
list. It returns a string if the fqn was found, or null otherwise."
  (if (csde-complete-class-exists name)
      name
    (let ((importlist (csde-complete-import-list)) shortname fullname tmp result)
      (while (and importlist (null result))
	(setq tmp (car importlist))
	(setq shortname (car (cdr tmp)))
	(setq fullname (concat (car tmp) name))
	(cond 
	 ((and (string= "*" shortname) (csde-complete-class-exists fullname))
	  (setq result fullname))
	 ((string= name shortname)
	  (setq result fullname))
	 (t 
	  (setq importlist (cdr importlist)))))
      result)))

(defvar csde-complete-classinfo-cache nil)

(defcustom csde-complete-classinfo-cache-size 50
  "The max size of completion's cache.")

(defun csde-complete-flush-classinfo-cache ()
  "Flushes all entries in the completion cache"
  (interactive)
  (setq csde-complete-classinfo-cache nil))

(defun csde-complete-flush-classes-in-cache (class-list)
  "Flushes all the classes in CLASS-LIST as entries of cache."
  (let ((temp (nth 0 csde-complete-classinfo-cache))
	(index -1) 
	(found nil)
	(class (car class-list)))
    (while class
      (while (and temp (not found))
	(setq index (1+ index))
	(setq temp (nth index csde-complete-classinfo-cache))
	(if (string= (car temp) class)
	    (setq found t)))
      (if found 
	  (setq csde-complete-classinfo-cache
		(nthcdr (1+ index) csde-complete-classinfo-cache)))
      (setq class-list (cdr class-list))
      (setq class (car class-list))
      (setq found nil))))

(defun csde-complete-add-to-classinfo-cache (name classinfo)
  (let (new-entry new-list)
    (if (nth csde-complete-classinfo-cache-size csde-complete-classinfo-cache)
	(progn
	  (setq new-entry (list name classinfo))
	  (setq new-list (list new-entry nil))
	  (setcdr new-list (cdr csde-complete-classinfo-cache))
	  (setq csde-complete-classinfo-cache new-list)  
	  (message "cache is full")   )
      ;;else
      (setq csde-complete-classinfo-cache 
	    (append 
	     csde-complete-classinfo-cache 
	     (list (list name classinfo)))))))

(defun csde-complete-get-from-cache (name)
  (let ((temp (nth 0 csde-complete-classinfo-cache)) (index -1) (found nil))
    (while (and temp (not found))
      (setq index (1+ index))
      (setq temp (nth index csde-complete-classinfo-cache))
      (if (string= (car temp) name)
	  (setq found t)))
    (if found
	(nth 1 temp)
      nil)))

(defun csde-complete-get-classinfo (name)
  "Return the class info list for the class NAME. This function first
checks to see if the class info is cached. If so, it returns the
cached class info. Otherwise, it creates the class info list. Each
element of the list returned by this function is itself a list whose
car is a possible completion and whose cdr gives additional
informations on the completion."
  (let ((class-info (csde-complete-get-from-cache name)))
    (when (not class-info)
      (setq class-info 
	    (bsh-eval-r (concat "csde.util.Completion.getClassInfo(\"" name "\");")))
      (if class-info
	  (csde-complete-add-to-classinfo-cache name class-info)))
    class-info))
 


(defun csde-complete-get-classinfo-csharpcode (name import access-level)
  "Return the csharp code that calls the
csde.util.Completion.getClassInfo function with the short csharp class
name NAME and the package list IMPORT where to look at."
  (save-excursion
    (concat 
     "{ " 
     "String[] lst = new String[" (number-to-string (length import)) "];\n"
     (let ((count -1))
       (mapconcat 
	(function 
	 (lambda (x) 
	   (setq count (+ 1 count))
	   (concat "lst[" (int-to-string count) "]=\"" 
		   (car (nth count import)) "\";\n")))
	import
	" "))
     "csde.util.Completion.getClassInfo(\"" name "\",lst," (number-to-string access-level) ");\n"
     "}")))

(defun csde-complete-isolate-to-complete (s)
  "Returns the right expression that needs completion in S." 
  (let* ((index (length s)) stop (paren 0) curcar)
    (while (and (> index 0)
		(not stop))     
      (setq index (- index 1))
      (setq curcar (aref s index))
      (cond
       ((eq ?\) curcar)
	(setq paren (1+ paren)))
       ((eq ?\( curcar)
	(setq paren (1- paren ))))
      (if (or (< paren 0)
	      (and (eq curcar ?\,) (<= paren 0)))
	  (setq stop t)))
    (if stop
	(setq index (1+ index)))
    (substring s index)))

(defun csde-complete-isolate-before-matching-of-last-car (s)
  "Returns the right expression that needs completion in S." 
  (let* ((index (length s)) stop (paren 0) (bracket 0) curcar)
    (while (and (> index 0)
		(not stop))     
      (setq index (- index 1))
      (setq curcar (aref s index))
      (cond
       ((eq ?\) curcar)
	(setq paren (1+ paren)))
       ((eq ?\( curcar)
	(setq paren (1- paren)))
       ((eq ?\] curcar)
	(setq bracket (1+ bracket)))
       ((eq ?\[ curcar)
	(setq bracket (1- bracket))))
      (if (and (= paren 0)
	       (= bracket 0)) 
	  (setq stop t))) 
    (substring s 0 index)))

(defun csde-complete-csharp-variable-at-point ()
  "Returns a list (VAR PARTIAL) where VAR.PARTIAL is the partially completed method or field
name at point. For example, suppose obj.f1.ge were the name at point. This function would return
the list (obj.f1 ge)."
  (save-excursion
    (let (start 
	  varname 
	  curcar 
	  found 
	  (original-point (point)) 
	  intermediate-point 
	  beg-point
	  first-part
	  second-part
          (bracket-count 0)
          (paren-count 0))
      (setq curcar (char-before))
      (while (null found)
	(cond 
	 ((or (and (>= curcar ?a) (<= curcar ?z))
	      (and (>= curcar ?A) (<= curcar ?Z))
	      (and (>= curcar ?0) (<= curcar ?9))
	      (>= curcar 127)
	      (member curcar '(?_ ?\\ )))
	  (forward-char -1))
	 ((eq ?. curcar)
	  (setq found (point)))
	 (t
	  (setq found t)))
	(setq curcar (char-before)))
      ;;
      (setq intermediate-point (point))
      (if (not (eq t found))
	  (progn 
	    (setq curcar (char-before))
	    (while (or (and (>= curcar ?a) (<= curcar ?z))
		       (and (>= curcar ?A) (<= curcar ?Z))
		       (and (>= curcar ?0) (<= curcar ?9))
		       (>= curcar 127)
                       (and (eq curcar ? ) (or (< 0 paren-count) (< 0 bracket-count)))
		       (member curcar '(?\. ?\_ ?\\ ?\( ?\) ?\, ?\[ ?\])))
              (cond 
               ((eq curcar ?\) )
                (setq paren-count (1+ paren-count)))
               ((eq curcar ?\( )
                (setq paren-count (1- paren-count)))
               ((eq curcar ?\] )
                (setq paren-count (1+ bracket-count)))
               ((eq curcar ?\[ )
                (setq paren-count (1- bracket-count))))
              (forward-char -1)
	      (setq curcar (char-before)))
	    (setq beg-point (point))
	    (set-marker csde-complete-current-beginning intermediate-point)
	    (set-marker csde-complete-current-end original-point)
            (setq first-part (buffer-substring-no-properties beg-point (- intermediate-point 1)))
            (setq first-part (csde-complete-isolate-to-complete first-part))
            (string-match " *\\(.*\\)" first-part)
            (setq first-part (substring first-part (match-beginning 1) (match-end 1)))
            (setq second-part (buffer-substring-no-properties intermediate-point original-point))
	    (list first-part second-part))
	nil))))

(defun csde-complete-build-completion-list (classinfo)
  "Build a completion list from the CLASSINFO list, as returned by the
csde.util.Completion.getClassInfo function."
  (let (result tmp)
    ;; get the variable fields
    (setq tmp (car classinfo))
    (while tmp
      (setq 
       result 
       (append 
	(list 
	 (list 
	  (car (car tmp)) 
	  (concat 
	   (nth 1 (car tmp)) 
	   " " 
	   (car (car tmp))))) 
	result))
      (setq tmp (cdr tmp)))
    ;; get the methods 
    (setq tmp (nth 2 classinfo))
    (while tmp
      (setq 
       result 
       (append 
	(list 
	 (list 
	  (concat (car (car tmp))"(")
	  (csde-complete-build-information-for-completion (car tmp)))) 
	result))
      (setq tmp (cdr tmp)))
    result))

(defun csde-complete-build-information-for-completion (lst)
  (let ((result (concat (car (cdr lst)) " " (car lst) "(")))
    (setq lst (cdr (cdr lst)))
    (while lst
      (setq result (concat result (car lst)))
      (setq lst (cdr lst))
      (if lst
	  (setq result (concat result ", "))))
    (setq result (concat result ")"))
    result))

(defun csde-complete-complete-cycle ()
  "Replace the previous completion by the next one in the list."
  (let (elem)
    (setq csde-complete-current-list-index (1+ csde-complete-current-list-index))
    (if (>= csde-complete-current-list-index (length csde-complete-current-list))
	(setq csde-complete-current-list-index 0))
    (setq elem (nth csde-complete-current-list-index csde-complete-current-list))
    (if (car elem)
	(progn
	  (delete-region csde-complete-current-beginning csde-complete-current-end)
	  (insert (car elem))
	  (set-marker csde-complete-current-end 
		      (+ (marker-position csde-complete-current-beginning) (length (car elem))))
	  (message (car (cdr elem))))
      (message (format "No completion at this point!(cycle)")))
    ;;  (goto-char (marker-position csde-complete-current-end))
    ))

(defun csde-complete-insert-completion (item)
  (if item 
      (let* ((chop-point
	      (if (string-match " : " item)
		  (string-match " : " item)
		(length item)))
	     (completion (substring item 0 chop-point)))
	(delete-region csde-complete-current-beginning csde-complete-current-end)
	(insert completion)
	(set-marker csde-complete-current-end 
		    (+ (marker-position csde-complete-current-beginning) 
		       (length completion))))))

;; (defun csde-complete-popup-xemacs-completion-menu (completion-list)
;;   (let* ((items
;; 	  (sort
;; 	   ;; Change each item in the completion list from the form
;; 	   ;;   return-value method-name(args)
;; 	   ;; to the form
;; 	   ;;   method-name(args) : return-value
;; 	   (mapcar
;; 	    (lambda (completion)
;; 	      (let ((completion-short (nth 0 completion))
;; 		    (completion-long (nth 1 completion)))
;; 		(if completion-long
;; 		    (let ((chop-pos (string-match " " completion-long)))
;; 		      (concat 
;; 		       (substring completion-long (1+ chop-pos)
;; 				  (length completion-long)) 
;; 		       " : " 
;; 		       (substring completion-long 0 chop-pos)))
;; 		  completion-short)))
;; 	    completion-list)
;; 	   'string<))
;; 	 (menu	
;; 	  (cons
;; 	   "Completions"
;; 	   (mapcar
;; 	    (lambda (item)
;; 	      (vector item (list 'csde-complete-insert-completion item)))
;; 	    items))))
;;     (popup-menu-and-execute-in-window menu (selected-window))))

(defun csde-complete-find-all-completions (pat lst &optional exact-match)
  (let ((result nil))
    (while lst
      (if (if exact-match 
	      (string= pat (car(car lst)))
	    (equal 0 (string-match pat (car (car lst)))))
	  (setq result (append (list (car lst)) result)))
      (setq lst (cdr lst)))
    result))

(defun csde-complete-split-by-dots (s)
  "Return a list containing the longest substring of S that ends with a dot, and the rest.But removes the intermediate(=last) dot."
  ;;we now isolate the last atom after a dot and the beginning
  (if (string-match "\\(.*\\)\\.\\(.*\\)" s)
      (list (match-string 1 s) (match-string 2 s))
    nil))

(defun csde-complete-get-component-type-of-array-class (name)
  (let (result)
    (setq result (bsh-eval (concat "System.out.println( Class.forName(\"" name "\").getComponentType().getName()) ;")));;removed \n
    (substring result 0 (- (length result) 1))))

(defun csde-complete-eval-type-of (expr)
  "Eval type of EXPR and returns either a csharp class name or a csharp type name."
					;(debug)
  (cond
   ;; If it's "this", we return the class name of the class we code in
   ((string= "this" expr)
    (csde-complete-get-qualified-name (csde-parse-get-class-at-point)))

   ;; If it's "super", we return the super class name of the class we code in
   ((string= "super" expr)
    (throw 'type (csde-complete-get-qualified-name (csde-parse-get-super-class-at-point))))
   ;;if it's a class name, done
   ((setq qualified-name (csde-complete-get-qualified-name expr))
    qualified-name)
   (t
    (let ((last-char 
	   (aref expr (- (length expr) 1 ))))

      ;; If it ends with a parenthesis
      (cond
       ((eq last-char ?\))
	(let* ((result (csde-complete-isolate-before-matching-of-last-car expr))
	       (temp (csde-complete-split-by-dots result))
	       to-complete)
	  (if temp
	      (csde-complete-find-completion-for-pair temp)
	    ;;we need exact completion here
	    (csde-complete-find-completion-for-pair (list "this" result)))
	  (if csde-complete-current-list
	      (progn
		(setq to-complete (nth 1 (car csde-complete-current-list)))
		(string-match "\\(.*?\\) " to-complete)
		(match-string 1 to-complete))
	    (error "Could not find type of %s." expr))))

       ;;if it's an array
       ((eq last-char ?\])
	(let ((temp (csde-complete-eval-type-of 
		     (csde-complete-isolate-before-matching-of-last-car expr))))
	  (csde-complete-get-component-type-of-array-class temp)))

       ;;we look for atoms if expr is splittable by dots
       ((setq temp (csde-complete-split-by-dots expr))
        ;;we need exact completion here
        (csde-complete-find-completion-for-pair temp t)
        (if csde-complete-current-list
	    (progn
	      (setq to-complete (nth 1 (car csde-complete-current-list)))
	      (string-match "\\(.*?\\) " to-complete)
	      (match-string 1 to-complete))
	  (error "Could not find type of %s." expr)))
        

       (t
	;; See if it's declared somewhere in this buffer.
	(let ((result (csde-parse-declared-type-of expr)))
	  (if result
	      (let ((count 0) type)
		(while (string-match ".*\\[\\]" result)
		  (setq result (substring result 0 (- (length result) 2 )))
		  (setq count (1+ count)))

		;; Handle primitive types, e.g., int
		(if (member result csde-complete-primitive-types)
		      (setq type result)
		  (setq type (csde-complete-get-qualified-name result)))

		(if type
		    (progn
		      (while (> count 0)
			(setq type (concat type "[]"))
			(setq count (1- count)))
		      (csde-complete-transform-array-classes-names type))
		  (if (y-or-n-p (format "Could not find type of %s. Attempt to import %s? " 
					expr result))
		      (progn
			;; import
			(csde-import-find-and-import result)
			;; recursive call of eval-type-of
			(csde-complete-eval-type-of expr))
		    (error "Could not find type of %s" result))))                        
	    (error "Could not find class %s." expr)))))))))

(defvar csde-complete-primitive-types '("byte" "char" "double" "float" 
				       "int" "long" "short" "boolean")
  "Primitive Csharp types.")

(defun csde-complete-find-completion-for-pair (pair &optional exact-completion )
  (let ((type (csde-complete-eval-type-of (car pair))))
    (if type
	(if (member type csde-complete-primitive-types)
	    (error "Cannot complete primitive type: %s." type)
	  (let ((classinfo  (csde-complete-get-classinfo type)))
	    (if classinfo
		(let ((fulllist (csde-complete-build-completion-list classinfo)))
		  (setq csde-complete-current-list 
			(csde-complete-find-all-completions (nth 1 pair) fulllist exact-completion)))))))))

(defun csde-complete-transform-array-classes-names (name)
  (let (result)
    (while (string-match ".*\\[\\]" name)
      (setq name (substring name 0 (- (length name) 2 ))) 
      (setq result (concat "[" result)))
    (if result
	(progn
	  (cond
	   ((string= name "byte")
	    (setq result (concat result "B")))
	   ((string= name "char")
	    (setq result (concat result "C")))
	   ((string= name "double")
	    (setq result (concat result "D")))
	   ((string= name "float")
	    (setq result (concat result "F")))
	   ((string= name "int")
	    (setq result (concat result "I")))
	   ((string= name "long")
	    (setq result (concat result "J")))
	   ((string= name "short")
	    (setq result (concat result "S")))
	   ((string= name "boolean")
	    (setq result (concat result "Z")))
	   (t
	    (setq result (concat result "L" name ";"))))
	  result)
      name)))


(defun csde-complete-at-point ()
  "Completes the method or field name at point.
Repeating the command cycles through all potential completions for the name.
This function displays the signature of a method completion in the minibuffer.
This command uses the Beanshell to run Csharp code that in turn uses Csharp
reflection to determine the methods and fields defined by the class of the
object at point. This command starts the Beanshell if necessary. Hence, you
may experience a slight delay when using this command for the first time in
a session or when completing a field or method of an object that has many
methods and fields. See `csde-complete-at-point-menu' for a version of this 
command that lets you select the desired completion from a popup menu."
  (interactive)
  (if (and
       csde-complete-current-list
       (markerp csde-complete-current-beginning)
       (markerp csde-complete-current-end)
       (marker-position csde-complete-current-beginning)
       (marker-position csde-complete-current-end)
       (>= (point) (marker-position csde-complete-current-beginning))
       (<= (point) (marker-position csde-complete-current-end))
       (eq last-command this-command))
      (csde-complete-complete-cycle) 
    ;;else
    (progn 
      (let ((pair (csde-complete-csharp-variable-at-point)))
        (if (string= (car pair) "" )
	    (progn
	      (setcar pair "this")
	      (goto-char (- csde-complete-current-beginning 1))
	      (insert "this")
	      (goto-char csde-complete-current-beginning)))
        (if (null pair)
	    (progn
	      (setq pair (list "this" "" ))
	      (set-marker csde-complete-current-beginning (point) ) 
	      (insert-before-markers "this.")
	      (set-marker csde-complete-current-end (point))))
        (csde-complete-find-completion-for-pair pair)
        (setq csde-complete-current-list-index -1)
        (csde-complete-complete-cycle)))))



(defun csde-complete-popup-completion-menu (completion-list &optional title)
  "Popup a completion menu for the object at point.
The popup menu displays all of the possible completions for the object
it was invoked on.  To automatically split large menus this function
use `imenu--mouse-menu' to handle the popup menu."
  (let*
      ;; Change each item in the completion list from the form
      ;; "return-value method-name(args)" to the form
      ;; "method-name(args) : return-value"
      ;; And sort the list alphabetically by method name 
      ((index
        (sort
         (mapcar (function
                  (lambda (completion)
                   (let ((completion-short (nth 0 completion))
                         (completion-long (nth 1 completion)))
                     (if completion-long
                         (let ((chop-pos (string-match " " completion-long)))
                           (concat 
                            (substring completion-long (1+ chop-pos)
                                       (length completion-long)) 
                            " : " 
                            (substring completion-long 0 chop-pos)))
                       completion-short))))
                 completion-list)
         'string<))
       (index-alist (mapcar (function
                             (lambda (elt) 
                               (cons elt elt)))
                             index))
       (name (cdr (if (= (length index-alist) 1)
                      ;; if only one item match, return it 
                      (car index-alist)
                    ;; delegates menu handling to imenu :-)
                    (imenu--mouse-menu index-alist
                                       (if csde-xemacsp
                                           nil
                                         t) ; popup the menu at mouse position
                                       (or title "Completion"))))))
    (csde-complete-insert-completion name)))

(defun csde-complete-at-point-menu ()
  "Completes the method or field name at point.
This command displays a popup menu listing the potential completions for the name
at point. Selecting a completion causes the command to use the completion to complete
the name at point. See `csde-complete-at-point' for a version of this 
command that lets you cycle throught the potential completions at point."
  (interactive)
  (let* ((pair (csde-complete-csharp-variable-at-point)) completion-list)
    (if (string= (car pair) "" )
	(progn
          (setcar pair "this")
          (goto-char (- csde-complete-current-beginning 1))
          (insert "this")
          (goto-char csde-complete-current-beginning)))
    (if (null pair)
	(progn
          (setq pair (list "this" "" ))
	  (set-marker csde-complete-current-beginning (point) ) 
	  (insert-before-markers "this.")
	  (set-marker csde-complete-current-end (point))))
    (setq completion-list (csde-complete-find-completion-for-pair pair))
    (if completion-list
        (let ((title (concat (car pair) "."
                             (car (cdr pair)) "[...]")))
          (csde-complete-popup-completion-menu completion-list title))
      (message "No completion at this point."))))


(provide 'csde-complete)

;; $Log: csde-complete.el,v $
;; Revision 1.2  2001/02/12 05:38:24  paulk
;; CSDE 2.2.7
;;
;; Revision 1.21  2001/01/25 04:31:01  paulk
;; Completion now asks user whether to import a class that it cannot find. Thanks to Phillip Lord.
;;
;; Revision 1.20  2000/12/19 04:33:34  paulk
;; Fixed popup completion menu to work on XEmacs. Thanks to David Ponce for providing this fix.
;;
;; Revision 1.19  2000/10/25 02:52:16  paulk
;; Fixed bug where the completion function was completing symbols that it could not find with the results of the previous completion.
;;
;; Revision 1.18  2000/10/20 04:02:10  paulk
;; Now uses semantic for some functions. Thanks to David Ponce.
;;
;; Revision 1.17  2000/10/08 12:55:39  paulk
;; *** empty log message ***
;;
;; Revision 1.16  2000/09/30 17:00:20  paulk
;; Use imenu to display completion choices. Thanks to David Ponce.
;;
;; Revision 1.15  2000/08/19 07:07:05  paulk
;; Flushes cache at end of compilation.
;;
;; Revision 1.14  2000/08/11 05:15:05  paulk
;; Now flushes the classinfo cache at the end of a compilation.
;;
;; Revision 1.13  2000/08/10 08:48:49  paulk
;; Now handles primitive arrays correctly.
;;
;; Revision 1.12  2000/08/09 02:04:26  paulk
;; Adds support for completion of array instances. Thanks to Steff.
;;
;; Revision 1.11  2000/08/01 07:37:40  paulk
;; Now caches methods and fields for each class referenced in a session. Now completes private and protected methods and fields. Thanks to Stephane <s.nicolas@videotron.ca>.
;;
;; Revision 1.10  2000/07/30 20:06:12  paulk
;; Updated doc for csde-complete-at-point and csde-complete-at-point-menu commands.
;;
;; Revision 1.9  2000/07/27 04:54:00  paulk
;; Now completes object fields to any depth and completes variables declared in method argument lists. Thanks to Stephane Nicolas <s.nicolas@videotron.ca>.
;;
;; Revision 1.8  2000/07/26 14:42:23  paulk
;; Adds support for static fields and methods and completion of fields and methods of this
;; and super objects. Thanks to  Stephane Nicolas <s.nicolas@videotron.ca> for this enhancement.
;;
;; Revision 1.7  2000/06/01 05:52:25  paulk
;; Completion menu now works on XEmacs.
;;
;; Revision 1.6  2000/05/16 04:41:28  paulk
;; *** empty log message ***
;;

;; end of csde-complete.el
