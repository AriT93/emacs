;;; semantic-analyze-fcn.el --- Analyzer support functions.

;; Copyright (C) 2007, 2008 Eric M. Ludlam

;; Author: Eric M. Ludlam <eric@siege-engine.com>
;; X-RCS: $Id: semantic-analyze-fcn.el,v 1.15 2008/05/21 03:09:59 zappo Exp $

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
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:
;;
;; Analyzer support functions.

;;; Code:

;;; Small Mode Specific Options
;;
;; These queries allow a major mode to help the analyzer make decisions.
;;
(define-overload semantic-analyze-tag-prototype-p (tag)
  "Non-nil if TAG is a prototype."
  )

(defun semantic-analyze-tag-prototype-p-default (tag)
  "Non-nil if TAG is a prototype."
  (let ((p (semantic-tag-get-attribute tag :prototype-flag)))
    (cond
     ;; Trust the parser author.
     (p p)
     ;; Empty types might be a prototype.
     ((eq (semantic-tag-class tag) 'type)
      (not (semantic-tag-type-members tag)))
     ;; No other heuristics.
     (t nil))
    ))

;;------------------------------------------------------------

(define-overload semantic-analyze-split-name (name)
  "Split a tag NAME into a sequence.
Sometimes NAMES are gathered from the parser that are compounded,
such as in C++ where foo::bar means:
  \"The class BAR in the namespace FOO.\"
Return the string NAME for no change, or a list if it needs to be split.")

(defun semantic-analyze-split-name-default (name)
  "Don't split up NAME by default."
  name)

;;; SELECTING
;;
;; If you narrow things down to a list of tags that all mean
;; the same thing, how to you pick one?  Select or merge.
;;

(defun semantic-analyze-select-best-tag (sequence &optional tagclass)
  "For a SEQUENCE of tags, all with good names, pick the best one.
If SEQUENCE is made up of namespaces, merge the namespaces together.
If SEQUENCE has several prototypes, find the non-prototype.
If SEQUENCE has some items w/ no type information, find the one with a type.
If SEQUENCE is all prototypes, or has no prototypes, get the first one.
Optional TAGCLASS indicates to restrict the return to only
tags of TAGCLASS."

  ;; If there is a srew up and we get just one tag.. massage over it.
  (when (semantic-tag-p sequence)
    (setq sequence (list sequence)))    

  ;; Filter out anything not of TAGCLASS
  (when tagclass
    (setq sequence (semantic-find-tags-by-class tagclass sequence)))

  (if (< (length sequence) 2)
      ;; If the remaining sequence is 1 tag or less, just return it
      ;; and skip the rest of this mumbo-jumbo.
      (car sequence)

    ;; 1)
    ;; This step will eliminate a vast majority of the types,
    ;; in addition to merging namespaces together.
    ;;
    ;; 2)
    ;; It will also remove prototypes.
    (setq sequence (semanticdb-typecache-merge-streams sequence nil))

    (if (< (length sequence) 2)
	;; If the remaining sequence after the merge is 1 tag or less,
	;; just return it and skip the rest of this mumbo-jumbo.
	(car sequence)

      (let ((best nil)
	    (notypeinfo nil)
	    )
	(while (and (not best) sequence)
	
	  ;; 3) select a non-prototype.
	  (if (not (semantic-tag-type (car sequence)))
	      (setq notypeinfo (car sequence))

	    (setq best (car sequence))
	    )
	
	  (setq sequence (cdr sequence)))
      
	;; Select the best, or at least the prototype.
	(or best notypeinfo)))))

;;; Tag Finding
;;
;; Mechanism for lookup up tags by name.
;;
(defun semantic-analyze-find-tags-by-prefix (prefix)
  ;; @todo - only used in semantic-complete.  Find something better?
  "Attempt to find a tag with PREFIX.
This is a wrapper on top of semanticdb, and semantic search functions.
Almost all searches use the same arguments."
  (if (and (fboundp 'semanticdb-minor-mode-p)
           (semanticdb-minor-mode-p))
      ;; Search the database & concatenate all matches together.
      (semanticdb-strip-find-results
       (semanticdb-find-tags-for-completion prefix)
       'name)
    ;; Search just this file because there is no DB available.
    (semantic-find-tags-for-completion
     prefix (current-buffer))))
 
;;; Finding Datatypes
;;
;; Finding a data type by name within a project.
;;
(defun semantic-analyze-tag-type-to-name (tag)
  "Get the name of TAG's type.
The TYPE field in a tag can be nil (return nil)
or a string, or a non-positional tag."
  (let ((tt (semantic-tag-type tag)))
    (cond ((semantic-tag-p tt)
	   (semantic-tag-name tt))
	  ((stringp tt)
	   tt)
	  ((listp tt)
	   (car tt))
	  (t nil))))

(defun semantic-analyze-tag-type (tag &optional scope)
  "Return the semantic tag for a type within the type of TAG.
TAG can be a variable, function or other type of tag.
The type of tag (such as a class or struct) is a name.
Lookup this name in database, and return all slots/fields
within that types field.  Also handles anonymous types.
Optional SCOPE represents a calculated scope in which the
types might be found.  This can be nil."
  (let ((ttype (semantic-tag-type tag))
	(name nil)
	(typetag nil)
	)

    ;; Is it an anonymous type?
    (if (and ttype
	     (semantic-tag-p ttype)
	     (semantic-tag-of-class-p ttype 'type)
	     (not (semantic-analyze-tag-prototype-p ttype))
	     )
	;; We have an anonymous type for TAG with children.
	;; Use this type directly.
	(semantic-analyze-dereference-metatype-stack ttype scope)

      ;; Not an anonymous type.  Look up the name of this type
      ;; elsewhere, and report back.
      (setq name (semantic-analyze-tag-type-to-name tag))

      (if (and name (not (string= name "")))
	  (progn
	    ;; Find a type of that name in scope.
	    (setq typetag (and scope (semantic-scope-find name 'type scope)))
	    ;; If no typetag, try the typecache
	    (when (not typetag)
	      (setq typetag (semanticdb-typecache-find name))))

	;; No name to look stuff up with.
	(error "Semantic tag %S has no type information"
	       (semantic-tag-name ttype)))

      ;; Handle lists of tags.
      (when (and (consp typetag) (semantic-tag-p (car typetag)))
	(setq typetag (semantic-analyze-select-best-tag typetag 'type))
	)

      ;; We now have a tag associated with the type.
      (semantic-analyze-dereference-metatype-stack typetag scope))))

(defun semantic-analyze-dereference-metatype-stack (type scope)
  "Dereference metatypes repeatedly until we hit a real TYPE.
Uses `semantic-analyze-dereference-metatype'.
Argument SCOPE is the scope object with additional items in which to search."
  (let ((lasttype type)
	(nexttype (semantic-analyze-dereference-metatype type scope))
	(idx 0))
    (while (and nexttype (not (eq nexttype lasttype)))
      (setq lasttype nexttype)
      (setq nexttype (semantic-analyze-dereference-metatype lasttype scope))
      (setq idx (1+ idx))
      (when (> idx 20) (error "Possible metatype recursion?"))
      )
    lasttype))

(define-overload semantic-analyze-dereference-metatype (type scope)
  ;; todo - move into typecahe!!
  "Return a concrete type tag based on input TYPE tag.
A concrete type is an actual declaration of a memory description,
such as a structure, or class.  A meta type is an alias,
or a typedef in C or C++.  If TYPE is concrete, it
is returned.  If it is a meta type, it will return the concrete
type defined by TYPE.
The default behavior always returns TYPE.
Override functions need not return a real semantic tag.
Just a name, or short tag will be ok.  It will be expanded here.
SCOPE is the scope object with additional items in which to search for names."
  (catch 'default-behavior
    (let ((ans (:override
                ;; Nothing fancy, just return type be default.
                (throw 'default-behavior type))))
      ;; If ANS is a string, or if ANS is a short tag, we
      ;; need to do some more work to look it up.
      (if (stringp ans)
	  ;; The metatype is just a string... look it up.
	  (or (and scope (car-safe
			  ;; @todo - should this be `find the best one'?
			  (semantic-scope-find ans 'type scope)))
	      (semanticdb-typecache-find ans))
	(when (and (semantic-tag-p ans)
		   (eq (semantic-tag-class ans) 'type))
	  ;; We have a tag.
	  (if (semantic-analyze-tag-prototype-p ans)
	      ;; It is a prototype.. find the real one.
	      (or (and scope 
		       (car-safe
			(semantic-scope-find (semantic-tag-name ans)
					     'type scope)))
		  (semanticdb-typecache-find (semantic-tag-name ans)))
	    ;; We have a tag, and it is not a prototype.
	    ans))
	))))

(provide 'semantic-analyze-fcn)
;;; semantic-analyze-fcn.el ends here
