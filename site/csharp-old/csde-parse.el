;;; csde-parse.el
;; $Revision: 1.2 $ 

;; Adapted from the JDE by Matt Bruce <matt.bruce@morganstanley.com>

;; Copyright (C) 2001 by Matt Bruce
;; Maintainer:  Matt Bruce

;; JDE Author: Paul Kinnucan <paulk@mathworks.com>
;; JDE Maintainer: Paul Kinnucan

;; Keywords: csharp, tools

;; JDE version Copyright (C) 1997, 1998, 2000, 2001 Paul Kinnucan.

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

(require 'semantic)
(require 'semantic-sb)
(require 'semantic-bnf)
(require 'avltree)
(require 'eieio)
(require 'csde-csharp-grammar)

;;; david@dponce.com
(require 'csde-imenu)                    ; All the imenu stuff is here now!

(defcustom csde-auto-parse-enable t
  "Enables automatic reparsing of a Csharp source buffer after you makes changes to the buffer, but only if the buffer is less than 
`csde-auto-parse-max-buffer-size'."
  :group 'csde-project
  :type 'boolean)

(defcustom csde-auto-parse-buffer-interval 180
  "Time in seconds between the time you change a Csharp source buffer
and the time the CSDE reparses the buffer."
  :group 'csde-project
  :type 'number)

(defcustom csde-auto-parse-max-buffer-size 50000
  "Maximum size in bytes of buffers that the CSDE automatically reparses
when `csde-auto-parse-enable' is t. Setting the threshold to 0 causes the
CSDE to parse a buffer automatically regardless of its size."
  :group 'csde-project
  :type 'number)


(defvar csde-parse-buffer-needs-reparse-p nil
  "True if buffer changed since last parse.")
(make-variable-buffer-local 'csde-parse-buffer-needs-reparse-p)

(defvar csde-auto-parse-buffer-timer nil)
(make-variable-buffer-local 'csde-auto-parse-buffer-timer)

;;(defun t1 () (interactive) (semantic-bovinate-toplevel))
;;(defun t2 () (interactive) (semantic-bovinate-toplevel t))

;;(defun t1 () (interactive) (semantic-find-nonterminal-by-overlay))

(defun csde-parse-after-buffer-changed ()
  ;; This function should be called only in CSDE buffers
  ;; but for some reason it is called in every buffer
  ;; in some versions of XEmacs. Hence the following 
  ;; guard.
  (when (string= mode-name "CSDE")
    (semantic-clear-toplevel-cache)
    (semantic-bovinate-toplevel)))

(defun csde-parse-should-auto-parse-buffer-p ()
  "Return t if the CSDE should automatically reparse the buffer"
  (and csde-auto-parse-enable
       (or 
	(<= csde-auto-parse-max-buffer-size 0)
	(< (buffer-size) csde-auto-parse-max-buffer-size))))

(defun csde-parse-buffer-changed-hook (begin end length)
  ;; This function should be called only in CSDE buffers
  ;; but for some reason it is called in every buffer
  ;; in some versions of XEmacs. Hence the following 
  ;; guard.
  (when (string= mode-name "CSDE")
    (setq csde-parse-buffer-needs-reparse-p t)
    (if (and (csde-parse-should-auto-parse-buffer-p)
	     (not csde-auto-parse-buffer-timer))
	(setq csde-auto-parse-buffer-timer 
	      (run-with-timer 
	       csde-auto-parse-buffer-interval 
	       nil 'csde-parse-after-buffer-changed)))))

(defun csde-parse-buffer-contains-multiple-classes-p ()
  "Returns nonnil if buffer contains multiple class
definitions."
  (let* ((top-level-classes  
	  (semantic-find-nonterminal-by-token 
	   'type 
	   (semantic-bovinate-toplevel)))
	 (top-level-class-count (length top-level-classes)))
    (or 
     (>  top-level-class-count 1)
     (and
      (= top-level-class-count 1)
      (let* ((inner-class-parts (semantic-token-type-parts (car top-level-classes)))
	    (inner-classes 
	     (semantic-find-nonterminal-by-token 
	      'type inner-class-parts)))
	(>= (length inner-classes) 1))))))

; (defun test ()
;   (interactive)
;   (message 
;    (if (csde-parse-buffer-contains-multiple-classes-p)
;        "Yes"
;      "No")))
   
(defvar csde-parse-buffer-contains-multiple-classes-p nil
  "TRUE if buffer contains more than one class definition")
(make-variable-buffer-local 'csde-parse-buffer-contains-multiple-classes-p)


(defun csde-parse-update-after-parse ()
  (when (csde-parse-should-auto-parse-buffer-p)
    (setq csde-parse-buffer-needs-reparse-p nil)
    (if csde-auto-parse-buffer-timer
	(cancel-timer csde-auto-parse-buffer-timer))
    (setq csde-auto-parse-buffer-timer nil))
  (when (and
	 (boundp 'semantic-toplevel-bovine-cache)
	 (car semantic-toplevel-bovine-cache))
    (setq csde-parse-buffer-contains-multiple-classes-p
	  (csde-parse-buffer-contains-multiple-classes-p))
    (setq csde-parse-the-method-map (csde-parse-method-map "Method map"))))

(defun csde-get-csharp-source-buffers ()
  "Get a list of the Csharp source buffers open in the
current session."
  (mapcan (lambda (buffer)
	  (save-excursion
	    (set-buffer buffer)
	    (if (string= mode-name "CSDE")
		(list buffer))))
	  (buffer-list)))

(defun csde-get-visible-source-buffers ()
  "Returns a list of visible Csharp source buffers."
  (delq
   nil
   (mapcar
    (lambda (buffer) 
     (if (get-buffer-window buffer 'visible) buffer))
     (csde-get-csharp-source-buffers))))

(defun csde-get-selected-source-buffer ()
  (let ((selected-buffer (window-buffer (selected-window))))
    (save-excursion
      (set-buffer selected-buffer)
      (if (string= mode-name "CSDE") selected-buffer))))

(defun csde-parse-get-package-name ()
  "Gets the name of the package in which the Csharp source file in the
current buffer resides."
  (let ((package-re "package[ \t]+\\(.*\\)[ \t]*;"))
    (save-excursion
      (goto-char (point-max))
      (when (re-search-backward package-re (point-min) t)
	(looking-at package-re)
	(buffer-substring-no-properties
		       (match-beginning 1)
		       (match-end 1))))))

(defun csde-parse-get-package-from-name (class-name)
  "Gets the package portion of a qualified class name."
  (substring 
   class-name 0
   (let ((pos  (position ?. class-name :from-end t)))
     (if pos
	 pos
       0))))

(defun csde-parse-get-unqualified-name (name)
"Gets the last name in a qualified name." 
  (string-match "[^.]+$" name)
  (substring name (match-beginning 0) (match-end 0)))


(defun csde-parse-get-super-class-at-point ()
  (setq superClass "Object")
  (let ((class-re "extends[ \t]+\\([a-zA-z]+[a-zA-Z0-9._]*\\).*[ \n]*"))
    (save-excursion
      (let ((open-brace-pos
	     (scan-lists (point) -1 1)))
	(when open-brace-pos
	  (goto-char open-brace-pos)
	  (when (re-search-backward class-re (point-min) t)
	    (looking-at class-re)
	    (setq superClass (buffer-substring-no-properties
		     (match-beginning 1)
		     (match-end 1))))))))
  superClass
 )

(defun csde-parse-get-innermost-class-at-point ()
"Get the innermost class containing point.
If point is in a class, this function returns 
(CLASS_NAME . CLASS_POSITION), where CLASS_NAME is the 
name of the class and CLASS_POSITION is the position
of the first character of the class keyword. Otherwise,
this function returns nil."
  ;; (interactive)
  (let ((left-paren-pos (c-parse-state)))
    (if left-paren-pos
	(save-excursion
	  (catch 'class-found
	    (let ((left-paren-index 0)
		  (left-paren-count (length left-paren-pos)))
	      (while (< left-paren-index left-paren-count)
		(let ((paren-pos (nth left-paren-index left-paren-pos)))
		  (unless (consp paren-pos)
		    (goto-char paren-pos)
		    (if (looking-at "{")
			(let* ((search-end-pos
			       (if (< left-paren-index (1- left-paren-count))
				   (let ((pos (nth (1+ left-paren-index) left-paren-pos)))
				     (if (consp pos)
					 (cdr pos)
				       pos))
				 (point-min)))
                              (case-fold-search nil)
                              (class-re "^[ \t]*\\(\\(public\\|abstract\\|final\\|static\\|strictfp\\|protected\\)[ \t]+\\)*[ \t]*class[ \t]+\\([^ \t\n{]*\\).*")
                              (class-pos (re-search-backward class-re search-end-pos t)))      
                           (if class-pos
			      (progn
				(looking-at class-re)
				(throw
				 'class-found
				 (cons
				  (buffer-substring-no-properties
				   (match-beginning 3)
				   (match-end 3))
				  class-pos))))))))
		  (setq left-paren-index (1+ left-paren-index)))))))))


(defun csde-parse-get-class-at-point () 
  (let ((class-info (csde-parse-get-innermost-class-at-point))
	class-name)
    (while class-info
      (let ((name (car class-info))
	    (pos (cdr class-info)))
	(if (not class-name)
	    (setq class-name name)
	  (setq class-name (concat name "." class-name)))
	(save-excursion
	  (goto-char pos)
	  (setq class-info (csde-parse-get-innermost-class-at-point)))))
    class-name)) 

(defun csde-parse-get-classes-at-point ()
  (interactive)
  (let ((class (csde-parse-get-innermost-class-at-point)))
    (if class (message "%s %s" (car class) (cdr class) ) (message "no class")))
;; (goto-char (aref (c-search-uplist-for-classkey (c-parse-state)) 0))
)


(defun csde-parse-qualified-name-at-point ()
  "Returns (cons QUALIFIER NAME) where NAME is the symbol at point and
QUALIFIER is the symbol's qualifier. For example, suppose the name at
point is

     int i = error.msg.length()
                   ^
In this case, this function returns (cons \"error.msg\" \"length\").
This function works only for qualified names that do not contain
white space. It returns null if there is no qualified name at point."
  (let ((symbol-at-point (thing-at-point 'symbol)))
    (when symbol-at-point
      (thing-at-point-looking-at "[^ \n\t();,:+]+")
      (let ((qualified-name 
	      (buffer-substring-no-properties
	       (match-beginning 0)
	       (match-end 0))))
	(string-match "\\(.+[.]\\)*\\([^.]+\\)" qualified-name)
	(let ((qualifier (if (match-beginning 1)
			     (substring qualified-name 
					(match-beginning 1) (match-end 1))))
	      (name (substring qualified-name 
				    (match-beginning 2) (match-end 2))))
	  (if qualifier
	      (setq qualifier (substring qualifier 0 (1- (length qualifier)))))
	  (cons qualifier name))))))


(defun csde-parse-double-backslashes (name)
  (mapconcat (lambda (x) (if (eq x ?\\)
		 "\\\\"
	       (string x)))
	     name ""))

(defun csde-parse-valid-declaration-at (point varname)
  "Verify that a POINT starts a valid csharp declaration
for the VARNAME variable."
  (save-excursion
    (goto-char point)
    (if (looking-at (concat "\\([][A-Za-z0-9_.\177-\377]+\\)[ \t\n\r]+" 
			    (csde-parse-double-backslashes varname) 
			    "[ \t\n\r]*[),;=]"))
	(let ((type (match-string 1))
	      (type-pos (match-beginning 1)))
	  (goto-char type-pos)
	  ;;  Check for following case.
	  ;;     Object table
	  ;;    //representing objects after all updates.
          ;;    table = new Truc();
          ;;    table.
          ;;  Avoid false hit on updates.
	  (if (not (or 
		    (csde-parse-comment-or-quoted-p)
		    (string= type "instanceof")
                    (string= type "return")))
	      type))
      nil)))

(defun csde-parse-declared-type-of (name)
  "Find in the current buffer the csharp type of the variable NAME.  The
function returns a string containing the name of the class, or nil
otherwise. This function does not give the fully-qualified csharp class
name, it just returns the type as it is declared."
  (save-excursion
    (let (found res pos orgpt resname)
      (setq orgpt (point))
      (while (and (not found)
		  (search-backward name nil t))
	(setq pos (point))
	(backward-word 1)
	(setq resname (csde-parse-valid-declaration-at (point) name))
	(goto-char pos)
	(forward-char -1)
	(if (not (null resname))
	    (progn (setq res resname)
		   (setq found t))))
      
      (goto-char orgpt)

      (while (and (not found)
		  (search-forward name nil t))
	(setq pos (point))
	(backward-word 2)
	(setq resname (csde-parse-valid-declaration-at (point) name))
	(goto-char pos)
	(forward-char 1)
	(if (not (null resname))
	    (progn (setq res resname)
		   (setq found t))))
      res)))


(defun csde-display-parse-error (error)
  (let* ((parser-buffer-name "*Csharp Parser*")
	 (buf (get-buffer parser-buffer-name))) 
    (if (not buf)
	(setq buf (get-buffer-create parser-buffer-name)))
    (set-buffer buf)
    (erase-buffer)
    (insert error)
    (pop-to-buffer buf)))

(defun csde-parse ()
"*Parses the Csharp source file displayed in the current buffer.
If the source file parses successfully, this command displays
a success message in the minibuffer. Otherwise, it displays an error
message in the Csharp Parser buffer. If the Csharp Parser buffer does
not exist, this command creates it.

Note. This command uses an external Csharp parser implemented in
Csharp to parse Csharp source files. This command uses the CSDE's integrated
Csharp source interpreter, the BeanShell, to invoke the parser. If the
BeanShell is not running, this command starts the BeanShell. Thus,
the first time you invoke the parser you may notice a slight delay
before getting a response. Thereafter, the response should be very
fast."
  (interactive)
  (save-some-buffers (not compilation-ask-about-save) nil)
  (let ((parse-error
	 (bsh-eval-r (concat "csde.parser.ParserMain.parseFile(\"" (buffer-file-name) "\");"))))
    (if parse-error
	(csde-display-parse-error parse-error)
      (message "Parsed %s successfully" (buffer-name)))))


;; Thanks to Eric D. Friedman <friedman@lmi.net> for this function.
(defun csde-parse-comment-or-quoted-p ()
  "Returns t if point is in a comment or a quoted string. nil otherwise"
  (interactive "p")
  ;; limit our analysis to the current line.
  (let ((beg (save-excursion (beginning-of-line) (point))))
    (if
        (or
         ;; are we in a csharpdoc comment?
         (save-excursion
           (re-search-backward
            "^[ \t]*/?\\*"
            beg t))
         ;; are we in a '//' or a '/*' style comment?
         ;; note that /* or /** on a line with only leading whitespace
         ;; will have matched in the previous regex.  We check again here
         ;; because the above case needs to allow for a line with
         ;; the continuation of a comment (using only a '*') whereas this
         ;; requires the presence of a '/' in front of the '*' so as to
         ;; distinguish a comment from a '*' operator.
         ;; To make a long story short,
         ;; the first regex matches
         ;;   /* a comment */
         ;; and
         ;; /**
         ;;  * a comment
         ;;  */
         ;; while the second one matches
         ;; System.out.println(foo); /* a comment */
         ;; but not
         ;; i = 3 * 5;
         ;; if you do something like following, you're on your own:
         ;; i = 3
         ;;       * 5; 
         ;; Suggestions for improving the robustness of this algorithm
         ;; gratefully accepted.
         (save-excursion
           (re-search-backward
            "\\(//\\|/\\*\\)"
            beg t))
         ;; are we in a quoted string?
         (save-excursion
           (re-search-backward
            "\"" ;;
            beg t)))
        t ;; return true if we had any matches; nil otherwise
      nil)))

(defun csde-parse-get-method-at-point (&optional position)
  "Gets the method at POSITION, if specified, otherwise at point.
Returns (CLASS_NAME . METHOD_NAME) if the specified position is
in a method; otherwise, nil."
  ;; Define an internal function that recursively searches a class
  ;; and its subclasses for a method containing point.
  (flet ((search-class 
	  (class pos)
	  (let* ((class-name       (semantic-token-name class))
		 (class-parts      (semantic-token-type-parts class))
		 (class-subclasses (semantic-find-nonterminal-by-token 'type class-parts))
		 (class-methods    (semantic-find-nonterminal-by-token 'function class-parts)))

	      ;; Is point in a method of a subclass of this class?
	      (loop for subclass in class-subclasses do
		(search-class subclass pos))

	      ;; Is point in any of the methods of this class?
	      (loop for method in class-methods do
		    (let* ((method-name  (semantic-token-name method))
			 (method-start (semantic-token-start method))
			 (method-end   (semantic-token-end method)))
		    (when (and (>= pos method-start) 
			       (<= pos method-end))
		      (throw 'found (cons (cons class-name method-name)
					  (cons method-start method-end)))))))))  
		       
    (let* ((pos (if position position (point)))
	   (tokens (semantic-bovinate-toplevel))
	   (classes (semantic-find-nonterminal-by-token 'type tokens)))
      (catch 'found
	(loop for class in classes
	  do (search-class class pos))))))



(defclass csde-avl-tree ()
  ((tree        :initarg tree
	        :type list
	        :documentation
	        "The tree")
   (compare-fcn :initarg compare-fcn
		:type function
		;; :initform <
		:documentation    
		"Compare function."))
  "Balanced binary tree.")

(defmethod initialize-instance ((this csde-avl-tree) &rest fields)
  "Constructor for binary balanced tree."
  
  ;; Call parent initializer
  (call-next-method)

  (assert (typep  (oref this compare-fcn)  'function))

  (oset this  tree (avltree-create (oref this compare-fcn))))

(defmethod csde-avl-tree-add ((this csde-avl-tree) item)
  "Inserts ITEM in this tree."
  (avltree-enter (oref this tree) item))

(defmethod csde-avl-tree-delete ((this csde-avl-tree) item)
  "Deletes ITEM from THIS tree."
  (avltree-delete (oref this tree) item))

(defmethod csde-avl-tree-is-empty ((this csde-avl-tree))
  "Return t if THIS tree is empty, otherwise return nil."
  (avltree-empty (oref this tree)))

(defmethod csde-avl-tree-find ((this csde-avl-tree) item)
  "Return the element in THIS tree that matches item."
  (avltree-member (oref this tree) item))

(defmethod csde-avl-tree-map ((this csde-avl-tree) map-function)
  "Applies MAP-FUNCTION to all elements of THIS tree."
  (avltree-map (oref this tree) item))

(defmethod csde-avl-tree-first ((this csde-avl-tree))
  "Return the first item in THIS tree."
  (avltree-first (oref this tree)))
  
(defmethod csde-avl-tree-last ((this csde-avl-tree))
  "Return the last item in THIS tree."
  (avltree-last (oref this tree)))

(defmethod csde-avl-tree-copy ((this csde-avl-tree))
  "Return a copy of THIS tree."
  (avltree-copy (oref this tree)))

(defmethod csde-avl-tree-flatten ((this csde-avl-tree))
  "Return a sorted list containing all elements of THIS tree."
  (avltree-flatten (oref this tree)))

(defmethod csde-avl-tree-size ((this csde-avl-tree))
  "Return the number of elements in THIS tree."
  (avltree-size (oref this tree)))

(defmethod csde-avl-tree-clear ((this csde-avl-tree))
  "Delete all elements of THIS tree."
  (avltree-clear (oref this tree)))

(defclass csde-parse-method-map (csde-avl-tree) 
  ()
  "Map of the methods in the current buffer.")


(defun csde-parse-method-map-compare-fcn (m1 m2)
  (and 
   (< (car (cdr m1)) (car (cdr m2)))
   (< (cdr (cdr m1)) (car (cdr m2)))))

(defmethod initialize-instance ((this csde-parse-method-map) &rest fields)
  "Constructor for method map."

  (oset  this compare-fcn 'csde-parse-method-map-compare-fcn)

;   (oset 
;    this 
;    compare-fcn
;     (lambda (m1 m2)
;       (and 
;        (< (car (cdr m1)) (car (cdr m2)))
;        (< (cdr (cdr m1)) (car (cdr m2))))))

  ;; Call parent initializer.
  (call-next-method)

  (flet ((add-methods 
	  (class)
	  (let* ((class-name       (semantic-token-name class))
		 (class-parts      (semantic-token-type-parts class))
		 (class-subclasses (semantic-find-nonterminal-by-token 'type class-parts))
		 (class-methods    (semantic-find-nonterminal-by-token 'function class-parts)))

	      ;; Add methods of subclasses
	      (loop for subclass in class-subclasses do
		(add-methods subclass))

	      ;; Add methods of this class?
	      (loop for method in class-methods do
		    (let* ((method-name  (semantic-token-name method))
			   (method-start (semantic-token-start method))
			   (method-end   (semantic-token-end method)))
		      (csde-avl-tree-add 
		       this
		       (cons
			(cons class-name method-name)
			(cons method-start method-end))))))))
		       
    (let* ((tokens (semantic-bovinate-toplevel))
	   (classes (semantic-find-nonterminal-by-token 'type tokens)))
	(loop for class in classes do 
	      (add-methods class)))))

(defmethod csde-parse-method-map-get-method-at ((this csde-parse-method-map) &optional pos)
  "Get the method at POS, if specified, otherwise, at point."
  (let ((p (if pos pos (point))))
	  (csde-avl-tree-find this (cons (cons "" "") (cons p p)))))

(defvar csde-parse-the-method-map nil
  "Map of methods defined in this buffer sorted by location.")
(make-variable-buffer-local 'csde-parse-the-method-map)


(defun csde-current-buffer-exact-name-match-p (tag)
  (and (tag-exact-match-p tag)
       (equal (buffer-file-name (window-buffer (selected-window))) 
	      (file-of-tag))))

(defun csde-etags-recognize-tags-table () ; see etags-recognize-tags-table
  (let ((recognized (etags-recognize-tags-table)))
    (if recognized 
	;; prefer exact match in current buffer to other files
	(setq find-tag-tag-order '(csde-current-buffer-exact-name-match-p
				   tag-exact-file-name-match-p
				   tag-exact-match-p
				   ))
      recognized)))


(provide 'csde-parse)

;; $Log: csde-parse.el,v $
;; Revision 1.2  2001/02/12 05:38:26  paulk
;; CSDE 2.2.7
;;
;; Revision 1.29  2001/01/05 07:14:35  paulk
;; Removed old version of csde-parse-get-class-at-point.
;;
;; Revision 1.28  2001/01/03 06:10:48  paulk
;; Fixes infinite recursion bug in csde-parse-update-after-parse when creating a new file.
;;
;; Revision 1.27  2000/12/18 05:22:46  paulk
;; *** empty log message ***
;;
;; Revision 1.26  2000/11/27 06:18:40  paulk
;; Miscellaneous bug fixes and minor enhancements.
;;
;; Revision 1.25  2000/10/25 04:44:23  paulk
;; Changed csde-auto-parse-disable-threshold to csde-auto-parse-max-buffer-size.
;;
;; Revision 1.24  2000/10/25 04:32:58  paulk
;; Moved code generated by the semantic bovinator to csde-csharp-grammar.el.
;;
;; Revision 1.23  2000/10/20 04:09:31  paulk
;; Now uses generalized version of classes menu shipped with
;; semantic. Thanks to David Ponce and Eric Ludlam.
;;
;; Revision 1.22  2000/10/08 12:55:39  paulk
;; *** empty log message ***
;;
;; Revision 1.21  2000/09/05 04:55:28  paulk
;; Bug fixes
;;
;; Revision 1.20  2000/08/31 05:31:15  paulk
;; * Now creates a binary tree, csde-parse-method-map, listing the
;; locations of all methods in the source buffer.
;;
;; * Now parses the source buffer 30 seconds after a change.
;;
;; Revision 1.19  2000/08/16 05:30:35  paulk
;; Set case-fold-search to nil to ensure case sensitivity when parsing buffer.
;;
;; Revision 1.18  2000/08/07 05:06:35  paulk
;; Fixes a couple of bugs in csde-parse-valid-declaration-at. Thanks to Lou Aloia <xlxa@rims.com> and Stephane <s.nicolas@videotron.ca> for the fixes.
;;
;; Revision 1.17  2000/07/28 06:27:46  paulk
;; Committing all modified files.
;;
;; Revision 1.16  2000/07/13 05:22:48  paulk
;; *** empty log message ***
;;
;; Revision 1.15  2000/06/29 02:33:42  paulk
;; Added sort option to Classes index menu. Thanks to David Ponce for this contribution.
;;
;; Revision 1.14  2000/06/22 03:40:16  paulk
;; Index menu now shows variable types and class definitions. Thanks to David Ponce for these enhancments. Changed the name of csde-enable-index-menu to csde-imenu-enable and csde-enable-full-method-signatures-index-menu to csde-imenu-include signature.
;;
;; Revision 1.13  2000/06/09 05:07:06  paulk
;; Classes index menu now shows full signatures of methods. Thanks to Ittay Freiman <ittay@vigiltech.com> for suggesting this enhancement and to David Ponce <david@dponce.com> for implementing it.
;;
;; Revision 1.12  2000/05/26 09:14:10  paulk
;; Updated grammar to handle argument variables with modifiers and array arguments.
;;
;; Revision 1.11  2000/05/16 04:08:55  paulk
;; Adds a Classes index menu to the Emacs menubar.
;;
;; Revision 1.10  2000/05/11 03:07:17  paulk
;; Updated bovinator grammar.
;;
;; Revision 1.9  2000/05/11 01:24:40  paulk
;; Added support for Eric Ludlam's semantic bovinator. Moved regular expression-based imenu indexer to this file.
;;
;; Revision 1.8  2000/03/16 05:18:11  paulk
;; Miscellaneous small bug fixes and enhancements.
;;
;; Revision 1.7  2000/03/08 03:47:02  paulk
;; Fixed regular expression in csde-parse-get-innermost-class-at-point to handle more cases. Thanks to Steve Haflich <smh@franz.com> for reporting the problem and providing a starting point for the fix.
;;
;; Revision 1.6  2000/02/16 04:40:33  paulk
;; Implemented Cygwin/XEmacs compatiblity fixes provided by Fred Hart
;; <cfhart@Z-TEL.com> in bsh-internal.
;;
;; Revision 1.5  2000/02/14 06:21:32  paulk
;; Fixed find innermost class regular expression.
;;
;; Revision 1.4  2000/02/09 05:18:10  paulk
;; Added methods for parsing symbol at point.
;;
;; Revision 1.3  2000/02/01 04:10:48  paulk
;; Fixed regular expression for classes to handle case where point is in
;; a constructor. Thanks to Francois Cogne <cogne@col.bsf.alcatel.fr>.
;;
;; Revision 1.2  1999/08/20 00:52:14  paulk
;; Added csde-parse-get-package-from-name function.
;;
;; Revision 1.1  1999/04/27 16:25:46  paulk
;; Initial revision
;;
