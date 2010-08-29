;;; csde-imenu.el --- imenu setup for the CSDE
;; $Revision: 1.1 $ 

;; Adapted from the JDE by Matt Bruce <matt.bruce@morganstanley.com>
;; Maintainer:  Matt Bruce

;; Copyright (C) 2001 by Matt Bruce

;; JDE Author: Paul Kinnucan <paulk@mathworks.com>,
;;         David Ponce <david@dponce.com>
;; JDE Maintainer: Paul Kinnucan, David Ponce

;; Keywords: csharp, tools

;; JDE version Copyright (C) 2000 Paul Kinnucan.

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

;;; Commentary:

;;; Code:

(require 'semantic-imenu)
(require 'semantic-mode)

;;;
;;; Global options
;;;

(defcustom csde-imenu-enable t
  "*Enables creation of a classes index menu in the Emacs menubar."
  :group 'csde-project
  :type 'boolean)

(defcustom csde-imenu-create-index-function 'semantic-create-imenu-index
  "*Function used to create the \"Classes\" imenu.
Files must be reopened to update the imenu when this option is
changed. The default is the generic `semantic-create-imenu-index'."
  :group 'csde-project
  :type 'function)

(defcustom csde-imenu-include-signature t
  "*If non-nil imenu displays full method signatures and field types.
Use *Rescan* to rebuild the imenu when you have changed this option."
  :group 'csde-project
  :type 'boolean)

(defcustom csde-imenu-include-modifiers nil
  "*If non-nil imenu shows abbreviations for Csharp modifiers.
Use *Rescan* to rebuild the imenu when you have changed this option.
See also `csde-imenu-modifier-abbrev-alist'."
  :group 'csde-project
  :type 'boolean)

(defconst csde-imenu-default-modifier-abbrev-alist
  '(
    ("public"        . ?+)              ; +
    ("protected"     . 177)             ; ±
    ("private"       . 172)             ; ¬

    ("static"        . ?§)              ; §
    ("transient"     . ?#)              ; #
    ("volatile"      . ?~)              ; ~

    ("abstract"      . 170)             ; ª
    ("final"         . 182)             ; ¶
    ("native"        . ?$)              ; $

    ("synchronized"  . ?@)              ; @
    ("strictfp"      . ?%)              ; %
    )
  "Default value of `csde-imenu-modifier-abbrev-alist'.")

(defconst csde-imenu-valid-modifiers-regexp
  "\\b\\(public\\|protected\\|private\\|static\\|abstract\\|final\\|native\\|synchronized\\|transient\\|volatile\\|strictfp\\)\\b"
  "Regexp of valid Csharp modifiers used by
`csde-imenu-modifier-field-validate'.")

(defun csde-imenu-modifier-field-validate (widget)
  "Validate a Csharp modifier value.
Used by `csde-imenu-modifier-abbrev-alist' customization."
  (save-excursion
    (let ((value (widget-value widget)))
      (if (and (stringp value)
               (string-match csde-imenu-valid-modifiers-regexp value))
          nil
        (widget-put widget :error (format "Invalid modifier: %S" value))
        widget))))

(defun csde-imenu-abbrev-field-validate (widget)
  "Validate a character abbreviation.
 Used by `csde-imenu-modifier-abbrev-alist' customization."
  (save-excursion
    (let ((value (widget-value widget)))
      (if (char-valid-p value)
          nil
        (widget-put widget :error (format "Invalid character value: %S" value))
        widget))))
      
(defcustom csde-imenu-modifier-abbrev-alist
  csde-imenu-default-modifier-abbrev-alist
  "*Alist of character abbreviations for Csharp modifiers.
Each association has the form (MODIFIER . CHARACTER) where MODIFIER is
a valid Csharp modifier string (see `csde-imenu-valid-modifiers-regexp')
and CHARACTER any valid character. Modifiers without any valid
association are not displayed (see also `csde-imenu-include-modifiers')."
  :group 'csde-project
  :type '(repeat
	  (cons :tag "Abbrev"
		(string :tag "Modifier"
                        :validate
                        (lambda (widget)
                          (csde-imenu-modifier-field-validate widget))
                        "")
                (choice :tag "Abbreviation"
                        (const     :tag "None" nil)
                        (character :tag "Character")
                        (integer   :tag "Character value"
                                   :validate
                                   (lambda (widget)
                                     (csde-imenu-abbrev-field-validate widget))
                                   ))
                )))

(defcustom csde-imenu-sort nil
  "*If non-nil sorts items in the index menu.
You can choose:

- - 'asc   to sort by token name ascending (ignoring case).
- - 'desc  to sort by token name descending (ignoring case).

Use *Rescan* to rebuild the imenu when you have changed this option."
  :group 'csde-project
  :type '(choice (const :tag "No sort"    nil )
                 (const :tag "Ascending"  asc )
                 (const :tag "Descending" desc))
  :set '(lambda (sym val)
          ;; setup sorting for `semantic-create-imenu-index'
          ;; buffer local
          (setq semantic-imenu-sort-bucket-function
                (cond ((eq val 'asc)
                       'semantic-sort-tokens-by-name-increasing-ci)
                      ((eq val 'desc)
                       'semantic-sort-tokens-by-name-decreasing-ci)
                      (t
                       nil)))
          ;; global
          (set-default 'semantic-imenu-sort-bucket-function
                              semantic-imenu-sort-bucket-function)
          (set-default sym val)))

;;;
;;; Helper functions
;;;

(defun csde-imenu-abbreviate-modifiers (modifiers)
  "Return a string of character abbreviations for MODIFIERS or \"\" if
not found. This string is prepended to each type, function and
variable prototype, giving a synthetic view of their modifiers (See
also `csde-imenu-include-modifiers')."
  (if (not csde-imenu-include-modifiers)
      ""
    (let ((alist csde-imenu-modifier-abbrev-alist)
          (abbrevs "")
          entry modifier)
      (while alist
        (setq entry (car alist)
              alist (cdr alist))
        (if (member (car entry) modifiers)
            (setq abbrevs
                  (concat abbrevs (if (char-valid-p (cdr entry))
                                      (char-to-string (cdr entry))
                                    "")))))
      (if (> (length abbrevs) 0)
          (concat abbrevs " ")          ; trailing whitespace separator
        abbrevs))))

;;;
;;; Universal `semantic-imenu' stuff adapted to CSDE's needs
;;;

(defun csde-imenu-prototype-nonterminal (token)
  "Return a prototype for TOKEN.
See also `semantic-prototype-nonterminal'."
  (let* ((token-cat  (semantic-token-token token))
         (token-name (semantic-token-name  token))
         (prototyper (intern (concat "csde-imenu-prototype-"
                                     (symbol-name token-cat)))))
    (if (fboundp prototyper)
        (funcall prototyper token)
      (message "Unknow token %S" token-cat))))

(defun csde-imenu-prototype-function (token)
  "Return a function (method) prototype for TOKEN.
See also `semantic-prototype-nonterminal'."
  (let ((name (semantic-token-name  token))
        (mods (semantic-token-function-modifiers token))
        sign)
    (if csde-imenu-include-signature
        (let ((type (semantic-token-type token))
              (args (semantic-token-function-args token)))
          (setq sign (if type
                        (format "%s %s(" type name)
                      (format "%s(" name)))
          (while args
            (let ((arg-token (car args))
                  arg-type)
              (when (semantic-token-p arg-token)
                (setq arg-type (semantic-token-type arg-token))
                (setq sign (concat sign arg-type ",")))
              (setq args (cdr args))))
          ;; remove the extra comma at end
          (if (char-equal ?, (aref sign (1- (length sign))))
              (setq sign (substring sign 0 -1)))
          (setq sign (concat sign ")")))
      (setq sign (format "%s()" name)))
    (format "%s%s" (csde-imenu-abbreviate-modifiers mods) sign)))

(defun csde-imenu-prototype-variable (token)
  "Return a variable (field) prototype for TOKEN.
See also `semantic-prototype-nonterminal'."
  (let ((name (semantic-token-name token))
        (mods (semantic-token-variable-modifiers token)))
    (if csde-imenu-include-signature
        (setq name (concat (semantic-token-type token)
                           " " name)))
    (format "%s%s" (csde-imenu-abbreviate-modifiers mods) name)))

(defun csde-imenu-prototype-type (token)
  "Return a type (class/interface) prototype for TOKEN.
See also `semantic-prototype-nonterminal'."
  (let ((name  (semantic-token-name token))
        (type  (semantic-token-type token))
        (mods  (semantic-token-type-modifiers token)))
    (format "%s%s %s" (csde-imenu-abbreviate-modifiers mods) type name)))

(defun csde-imenu-prototype-include (token)
  "Return an include (import) prototype for TOKEN.
See also `semantic-prototype-nonterminal'."
  (semantic-token-name token))

(defun csde-imenu-prototype-package (token)
  "Return a package prototype for TOKEN.
See also `semantic-prototype-nonterminal'."
  (semantic-token-name token))

;;;
;;; Specific CSDE's imenu (to be replaced by semantic-imenu stuff)
;;;

(defcustom csde-imenu-include-classdef t
  "*If non-nil `csde-imenu-index-class' adds *class def* items in imenu
index to go to class definition."
  :group 'csde-project
  :type 'boolean)

(defun csde-imenu-sort-tokens (tokens)
  "Sorts the token list TOKENS depending on `csde-imenu-sort' value."
  (cond ((eq csde-imenu-sort 'asc)
         (sort tokens
               (function
                (lambda (token1 token2)
                  (string-lessp (upcase (semantic-token-name token1))
                                (upcase (semantic-token-name token2)))))))
        ((eq csde-imenu-sort 'desc)
         (sort tokens
               (function
                (lambda (token1 token2)
                  (string-lessp (upcase (semantic-token-name token2))
                                (upcase (semantic-token-name token1)))))))
        (t
         tokens)))

(defun csde-imenu-index-class  (class-token)
  "Creates an imenu index for a class in CLASS-TOKEN."
  (let* ((class-name  (semantic-token-name       class-token))
         (class-type  (semantic-token-type       class-token))
         (class-start (semantic-token-start      class-token))
         (class-parts (semantic-token-type-parts class-token))
         (class-index (csde-imenu-index-class-parts class-parts)))

    (if csde-imenu-include-classdef
        ;; If requested adds a *class def* item to go to the class def.
        (setq class-index (cons (cons "*class def*" class-start)
                                class-index))
      ;; Else adds an *empty* item to go to the class def. only
      ;; when there is not parts
      (or class-index
          (setq class-index
                (list (cons "*empty*"
                            class-start)))))

    (list (cons (format "%s %s" class-type class-name)
                class-index))))

(defun csde-imenu-index-class-parts (tokens)
  "Creates an imenu index for class parts in TOKENS.
When`csde-imenu-include-signature' is non-nil the
index menu displays full method signatures and field types."
  (let ((methods (semantic-find-nonterminal-by-token 'function tokens))
        (fields  (semantic-find-nonterminal-by-token 'variable tokens))
        (classes (semantic-find-nonterminal-by-token 'type     tokens))
        index)

    (setq methods (csde-imenu-sort-tokens methods))
    (while methods
      (let* ((method-token (car methods))
             (method-name  (semantic-token-name method-token))
             (method-pos   (semantic-token-start method-token))
             method-sig)
        (if csde-imenu-include-signature
            (let ((method-type  (semantic-token-type method-token))
                  (method-args  (semantic-token-function-args method-token)))
              (setq method-sig (if method-type
                                   (format "%s %s(" method-type method-name)
                                 (format "%s(" method-name)))
              (while method-args
                (let ((method-arg-token (car method-args))
                      method-arg-type)
                  (when (semantic-token-p method-arg-token)
                    (setq method-arg-type (semantic-token-type method-arg-token))
                    (setq method-sig (concat method-sig method-arg-type ",")))
                  (setq method-args (cdr method-args))))
              ;; remove the extra comma at end
              (if (char-equal ?, (aref method-sig (1- (length method-sig))))
                  (setq method-sig (substring method-sig 0 -1)))
              (setq method-sig (concat method-sig ")")))
          (setq method-sig (format "%s()" method-name)))
        (setq index
              (append
               index (list (cons method-sig method-pos)))))
      (setq methods (cdr methods)))

    ;; Add a separator between method and field index
    (if fields
        (setq index (append index '(("-"))))) 
    
    (setq fields (csde-imenu-sort-tokens fields))
    (while fields
      (let* ((field-token (car fields))
             (field-name  (semantic-token-name  field-token))
             (field-pos   (semantic-token-start field-token)))
        (if csde-imenu-include-signature
            (setq field-name (concat (semantic-token-type field-token)
                                     " " field-name)))
        (setq index 
              (append 
               index (list (cons field-name field-pos)))))
      (setq fields (cdr fields)))

    (setq classes (csde-imenu-sort-tokens classes))
    (while classes
      (let* ((class-token  (car classes))
             (class-index  (csde-imenu-index-class class-token)))
        (setq index (append index class-index)))
      (setq classes (cdr classes)))
    index))

(defun csde-create-imenu-index ()
  "Creates an imenu index for a Csharp source buffer.
This function uses the semantic bovinator to index the buffer."

    (semantic-bovinate-toplevel t)
 
    (let* ((tokens   (semantic-bovinate-toplevel))
	   (packages (semantic-find-nonterminal-by-token 'package tokens))
	   (depends  (semantic-find-nonterminal-by-token 'include tokens))
	   (classes  (semantic-find-nonterminal-by-token 'type tokens))
	   depend-index
	   index)


      (setq classes (csde-imenu-sort-tokens classes))
      (while classes
	(let* ((class-token  (car classes))
	       (class-index  (csde-imenu-index-class class-token)))
	  (setq index (append index class-index)))
	(setq classes (cdr classes)))

      (setq depends (csde-imenu-sort-tokens depends))
      (while depends
	(let* ((depend-token (car depends))
	       (depend-name  (semantic-token-name  depend-token))
	       (depend-pos   (semantic-token-start depend-token)))
	  (setq depend-index (append depend-index (list (cons depend-name depend-pos)))))
	(setq depends (cdr depends)))
      (if depend-index
	  (setq index (append index (list (cons "imports" depend-index)))))

      (setq packages (csde-imenu-sort-tokens packages))
      (while packages
	(let* ((package-token (car packages))
	       (package-name  (semantic-token-name  package-token))
	       (package-pos   (semantic-token-start package-token)))
	  (setq index 
		(append 
		 index 
		 (list (cons (concat "package " package-name) package-pos)))))
	(setq packages (cdr packages)))
      index))

;;;
;;; CSDE's imenu setup
;;;

(defun csde-imenu-setup ()
  "Setup the CSDE's \"Classes\" imenu when entering csde-mode."
  (when csde-imenu-enable

    ;; function to use when creating items in imenu.
    (setq semantic-imenu-summary-function
          'semantic-prototype-nonterminal)

    ;; speedbar and imenu buckets name.
    (setq semantic-symbol->name-assoc-list
          '((type     . "Classes")
            (variable . "Variables")
            (function . "Methods")
            (include  . "Imports")
            (package  . "Package")))

    ;; semantic overloaded functions
    (setq semantic-override-table
          '((prototype-nonterminal . csde-imenu-prototype-nonterminal)))

    ;; function to use for creating the imenu
    (setq imenu-create-index-function
          (if (fboundp csde-imenu-create-index-function)
              csde-imenu-create-index-function
            'semantic-create-imenu-index))

    ;; add the imenu to the menu bar for the current buffer
    (imenu-add-to-menubar "Classes")

    ;; enable `semantic-minor-mode', so imenu configuration and others
    ;; semantic services will be available in the "Parse" menu.
    ;; (semantic-minor-mode 1)
    ))

;; Enable which-function-mode in csde-mode (using semantic parser)
(eval-after-load "which-func"
  '(add-to-list 'which-func-modes 'csde-mode))

(provide 'csde-imenu)

;; $Log: csde-imenu.el,v $
;; Revision 1.1  2001/02/12 05:40:58  paulk
;; Initial XEmacs version.
;;
;; Revision 1.2  2000/11/27 06:18:39  paulk
;; Miscellaneous bug fixes and minor enhancements.
;;
;; Revision 1.1  2000/10/20 04:04:20  paulk
;; Initial version.
;;

;;; csde-imenu.el ends here
