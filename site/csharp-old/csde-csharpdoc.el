;;; csde-csharpdoc.el --- CSDE csharpdoc autodoc
;; $Revision: 1.2 $

;; Adapted from the JDE by Matt Bruce <matt.bruce@morganstanley.com>
;; Maintainer:  Matt Bruce

;; Copyright (C) 2001 by Matt Bruce

;; JDE version Copyright (C) 1998, 2000, 2001 by David Ponce

;; JDE Author: David Ponce <david@dponce.com>
;; JDE Maintainer: David Ponce <david@dponce.com>
;;             Paul Kinnucan <paulk@mathworks.com>

;; Keywords: csharp, tools
;;
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


;; The latest version of the CSDE is available at
;; <URL:http://www.sourceforge.com/>.

;; Please send any comments, bugs, or upgrade requests to
;; Matt Bruce (matt.bruce@morganstanley.com)

;;; Commentary:
;;
;; This library provides a doc comment generator to help source
;; documentation

;;; History:
;;
;; See at end of this file.

;;; Code:
(require 'tempo)
(require 'semantic)

;;;; Customization
;;;; -------------

(defgroup csde-csharpdoc nil
  "CSDE csharpdoc utilities"
  :group 'csde
  :prefix "csde-csharpdoc-")

;; IMPORTANT: This function must be defined before the following defcustoms
;; because it is used in their :set clause.
(defun csde-csharpdoc-define-template (sym val)
  "Define a template (see `tempo-define-template').
The template name is the `symbol-name' of SYM from which the
'-template' suffix has been removed, prefixed by 'tempo-template-'.
VAL is the template value.  If VAL is a string it is converted to a
list of template elements."
  (let* ((name (symbol-name sym))
         (template-name
          (if (string-match "\\(.*\\)-template$" name)
              (match-string 1 name)
            (error "Invalid template variable name: %S" name)))
         (template-val
          (if (stringp val)
              (car (read-from-string (format "(%s)" val)))
            val)))
    (tempo-define-template template-name template-val nil name)
    (set-default sym val)))

(defcustom csde-csharpdoc-describe-class-template
  "\"///<summary>\n/// Describe class \" (csde-csharpdoc-code name) \" here.\n///</summary>\""
  "*Line template used to describe a class.
If nil the line is not inserted.
The variable 'name' is set to the class name.
See `csde-csharpdoc-autodoc-at-line' for usage.  Define the template
variable `tempo-template-csde-csharpdoc-describe-class'."
  :group 'csde-csharpdoc
  :type '(choice :tag "Template form"
                 (text :format "%t\n%v" :tag "String")
                 (repeat :tag "Lisp Expressions" (sexp :tag "")))
  :set 'csde-csharpdoc-define-template)

(defcustom csde-csharpdoc-describe-interface-template
  "\"///<summary>\n/// Describe interface \" (csde-csharpdoc-code name) \" here.\n///</summary>\""
  "*Line template used to describe an interface.
If nil the line is not inserted.
The variable 'name' is set to the interface name.
See `csde-csharpdoc-autodoc-at-line' for usage.  Define the template
variable `tempo-template-csde-csharpdoc-describe-interface'."
  :group 'csde-csharpdoc
  :type '(choice :tag "Template form"
                 (text :format "%t\n%v" :tag "String")
                 (repeat :tag "Lisp Expressions" (sexp :tag "")))
  :set 'csde-csharpdoc-define-template)

(defcustom csde-csharpdoc-describe-constructor-template
  "\"///<summary>\n/// Creates a new \" (csde-csharpdoc-code name) \" instance.\n///</summary>\""
  "*Line template used to describe a constructor.
If nil the line is not inserted.
The variable 'name' is set to the constructor name (that is the class name).
See `csde-csharpdoc-autodoc-at-line' for usage.  Define the template
variable `tempo-template-csde-csharpdoc-describe-constructor'."
  :group 'csde-csharpdoc
  :type '(choice :tag "Template form"
                 (text :format "%t\n%v" :tag "String")
                 (repeat :tag "Lisp Expressions" (sexp :tag "")))
  :set 'csde-csharpdoc-define-template)

(defcustom csde-csharpdoc-describe-method-template
  "\"///<summary>\n/// Describe \" (csde-csharpdoc-code name) \" method here.\n///</summary>\""
  "*Line template used to describe a method.
If nil the line is not inserted.
The variable 'name' is set to the method name.
See `csde-csharpdoc-autodoc-at-line' for usage.  Define the template
variable `tempo-template-csde-csharpdoc-describe-method'."
  :group 'csde-csharpdoc
  :type '(choice :tag "Template form"
                 (text :format "%t\n%v" :tag "String")
                 (repeat :tag "Lisp Expressions" (sexp :tag "")))
  :set 'csde-csharpdoc-define-template)

(defcustom csde-csharpdoc-describe-field-template
  "\"///<summary>\n/// Describe \" (csde-csharpdoc-field-type modifiers)
 \" \" (csde-csharpdoc-code name) \" here.\n///</summary>\""
  "*Line template used to describe a method.
If nil the line is not inserted.
The variable 'name' is set to the field name.
The variable 'type' is set to the field type.
The variable 'modifiers' is set to the field modifiers.
See `csde-csharpdoc-autodoc-at-line' for usage.  Define the template
variable `tempo-template-csde-csharpdoc-describe-field'."
  :group 'csde-csharpdoc
  :type '(choice :tag "Template form"
                 (text :format "%t\n%v" :tag "String")
                 (repeat :tag "Lisp Expressions" (sexp :tag "")))
  :set 'csde-csharpdoc-define-template)

(defcustom csde-csharpdoc-param-tag-template
  "\"/// <param name=\\\"\" name \"\\\">\" (csde-csharpdoc-a type)
 \" \" (csde-csharpdoc-code type) \" value</param>\""
  "*Line template used to describe a parameter.
If nil the line is not inserted.
The variable 'name' is set to the parameter name.
The variable 'type' is set to the parameter type.
A line is inserted for each parameter.
See `csde-csharpdoc-autodoc-at-line' for usage.  Define the template
variable `tempo-template-csde-csharpdoc-param-tag'."
  :group 'csde-csharpdoc
  :type '(choice :tag "Template form"
                 (text :format "%t\n%v" :tag "String")
                 (repeat :tag "Lisp Expressions" (sexp :tag "")))
  :set 'csde-csharpdoc-define-template)

(defcustom csde-csharpdoc-return-tag-template
  "\"/// <returns>\" (csde-csharpdoc-a type)
 \" \" (csde-csharpdoc-code type) \" value</returns>\""
  "*Line template used to describe a returned value.
If nil the line is not inserted.
The variable 'type' is set to the returned type.
See `csde-csharpdoc-autodoc-at-line' for usage.  Define the template
variable `tempo-template-csde-csharpdoc-return-tag'."
  :group 'csde-csharpdoc
  :type '(choice :tag "Template form"
                 (text :format "%t\n%v" :tag "String")
                 (repeat :tag "Lisp Expressions" (sexp :tag "")))
  :set 'csde-csharpdoc-define-template)

(defcustom csde-csharpdoc-exception-tag-template
  "\"/// WARNING:  CSHARP DOES NOT SUPPORT CHECKED EXCEPTIONS:  <exception> \" type \" if an error occurs</exception>\""
  "*Line template used to describe an exception.
If nil the line is not inserted.
The variable 'type' is set to the exception type.
A line is inserted for each exception in the 'throws' clause.
See `csde-csharpdoc-autodoc-at-line' for usage.  Define the template
variable `tempo-template-csde-csharpdoc-exception-tag'."
  :group 'csde-csharpdoc
  :type '(choice :tag "Template form"
                 (text :format "%t\n%v" :tag "String")
                 (repeat :tag "Lisp Expressions" (sexp :tag "")))
  :set 'csde-csharpdoc-define-template)

(defcustom csde-csharpdoc-author-tag-template
  "\"/// <author> <a href=\\\"mailto:\" user-mail-address
 \"\\\">\" user-full-name \"</a> </author>\""
  "*Line template used to give an author.
If nil the line is not inserted.
See `csde-csharpdoc-autodoc-at-line' for usage.  Define the template
variable `tempo-template-csde-csharpdoc-author-tag'."
  :group 'csde-csharpdoc
  :type '(choice :tag "Template form"
                 (text :format "%t\n%v" :tag "String")
                 (repeat :tag "Lisp Expressions" (sexp :tag "")))
  :set 'csde-csharpdoc-define-template)

;; (defcustom csde-csharpdoc-version-tag-template
;;   '("* @version 1.0")
;;   "*Line template used to give a version.
;; If nil the line is not inserted.
;; See `csde-csharpdoc-autodoc-at-line' for usage.  Define the template
;; variable `tempo-template-csde-csharpdoc-version-tag'."
;;   :group 'csde-csharpdoc
;;   :type '(choice :tag "Template form"
;;                  (text :format "%t\n%v" :tag "String")
;;                  (repeat :tag "Lisp Expressions" (sexp :tag "")))
;;   :set 'csde-csharpdoc-define-template)

;; (defcustom csde-csharpdoc-see-tag-template
;;   '("* @see " ref)
;;   "*Line template used to give a reference.
;; If nil the line is not inserted.
;; The variable 'ref' is set to the class or interface name.
;; A line is inserted for each name in the 'extends' then 'implements' clauses.
;; See `csde-csharpdoc-autodoc-at-line' for usage.  Define the template
;; variable `tempo-template-csde-csharpdoc-see-tag'."
;;   :group 'csde-csharpdoc
;;   :type '(choice :tag "Template form"
;;                  (text :format "%t\n%v" :tag "String")
;;                  (repeat :tag "Lisp Expressions" (sexp :tag "")))
;;   :set 'csde-csharpdoc-define-template)

;; (defcustom csde-csharpdoc-since-tag-template
;;   '("* @since 1.0")
;;   "*Line template used to give a since reference.
;; If nil the line is not inserted.
;; See `csde-csharpdoc-autodoc-at-line' for usage.  Define the template
;; variable `tempo-template-csde-csharpdoc-since-tag'."
;;   :group 'csde-csharpdoc
;;   :type '(choice :tag "Template form"
;;                  (text :format "%t\n%v" :tag "String")
;;                  (repeat :tag "Lisp Expressions" (sexp :tag "")))
;;   :set 'csde-csharpdoc-define-template)

(defcustom csde-csharpdoc-end-block-template
  nil
  "*Csharpdoc end comment block characters.
If nil \"*/\" is inserted.
See `csde-csharpdoc-autodoc-at-line' for usage.  Define the template
variable `tempo-template-csde-csharpdoc-end-block'."
  :group 'csde-csharpdoc
  :type '(choice :tag "Template form"
                 (text :format "%t\n%v" :tag "String")
                 (repeat :tag "Lisp Expressions" (sexp :tag "")))
  :set 'csde-csharpdoc-define-template)

;;;; Utilities
;;;; ---------
(defmacro csde-csharpdoc-status-forms (message donestr &rest forms)
  "Wrapper for `working-status-forms'.
See `working-status-forms' for details on MESSAGE, DONESTR and FORMS
arguments.  Does not override an outer `working-status-forms'
MESSAGE."
  `(working-status-forms (or working-message ,message) ,donestr
     ,@forms))

(defun csde-csharpdoc-dynamic-status (&rest args)
  "Wrapper for `working-dynamic-status'.
Does nothing if not called within the macro `working-status-forms'.
See `working-dynamic-status' for meaning of ARGS."
  (and working-message
       (apply #'working-dynamic-status args)))

(defmacro csde-csharpdoc-skip-spaces-backward ()
  "Move point backward, skipping Csharp whitespaces."
  `(skip-chars-backward " \n\r\t"))

(defmacro csde-csharpdoc-skip-spaces-forward ()
  "Move point forward, skipping Csharp whitespaces."
  `(skip-chars-forward " \n\r\t"))

(defalias 'csde-csharpdoc-indent-line 'c-indent-line)

(defun csde-csharpdoc-indent-region (start end)
  "Indent region between START and END."
  (save-excursion
    (goto-char end)
    (setq end (point-marker))
    (goto-char start)
    (while (< (point) end)
      (csde-csharpdoc-dynamic-status)
      (or (and (bolp) (eolp))
          (csde-csharpdoc-indent-line))
      (forward-line 1))
    (move-marker end nil)))

(defun csde-csharpdoc-map (f l &rest args)
  "Apply F to each element of L.
F receives optional ARGS after the current element of L."
  (while l
    (apply f (car l) args)
    (setq l (cdr l))))
                
(defun csde-csharpdoc-window-lines ()
  "Return the number of lines of the selected window.
This number may be greater than the number of actual lines in the
buffer if any wrap on the display due to their length."
  (let ((start (point-min))
        (end   (point-max)))
    (if (= start end)
        0
      (save-excursion
        (save-restriction
          (widen)
          (narrow-to-region start end)
          (goto-char start)
          (vertical-motion (buffer-size)))))))

(defun csde-csharpdoc-adjust-window (window)
  "Adjust WINDOW height to fit its buffer contents."
  (save-selected-window
    (select-window window)
    (let ((height (window-height))
          (lines  (+ 3 (csde-csharpdoc-window-lines))))
      ;; ensure window will not be deleted if too small
      (if (< lines window-min-height)
          (setq lines window-min-height))
      (enlarge-window (- lines height)))))

;;;; Text helpers
;;;; ------------

(defun csde-csharpdoc-field-type (modifiers)
  "Return field category.
That is \"constant\" if field MODIFIERS contains \"static\" and
\"final\" or \"variable\" otherwise.  Useful to generate field
description."
  (if (and (member "static" modifiers) (member "final" modifiers))
      "constant"
    "variable"))

(defun csde-csharpdoc-a (word)
  "Return \"an\" if WORD begin with a vowel or \"a\" otherwise.
Useful to generate description like \"an int value\" or \"a long value\"."
  (if (string-match "^[aeiouyAEIOUY]" word)
      "an" "a"))

(defun csde-csharpdoc-code (text)
  "Return \"<code>TEXT</code>\".
Useful to generate HTML code style."
  (concat "<code>" text "</code>"))

;;;; Csharpdoc comment parser
;;;; ----------------------

;; tags and matchers
;;
(defconst csde-csharpdoc-line-tags
  (list
   "author"
   "version"
   "param"
   "return"
   "exception"
   "throws"
   "see"
   "since"
   "serial"
   "serialData"
   "serialField"
   "deprecated"
   )
  "Valid csharpdoc line tags.
Ordered following Sun's Tag Convention at
<http://csharp.sun.com/products/jdk/csharpdoc/writingdoccomments/index.html>")

(defconst csde-csharpdoc-with-name-tags
  (list "exception"
        "param"
        "throws")
  "Csharpdoc tags which have a name.")

(defconst csde-csharpdoc-with-ref-tags
  (list "see")
  "Csharpdoc tags which have a reference.")

(defconst csde-csharpdoc-desc-tag
  "*DESCRIPTION*"
  "Special internal tag associated to descriptions.")

(defconst csde-csharpdoc-start-tag-regexp
  ;;  "[\r\n][ \t]*\\(/\\*\\*\\|\\*?\\)[ \t]*@"
  "///[ ]*<summary>"  
  "Regexp matching the beginning of a tag.")

(defconst csde-csharpdoc-end-tag-regexp
;;  (concat "\\(" csde-csharpdoc-start-tag-regexp
;;          "\\|[ \n\r\t]*\\*/\\)")
  "///[ ]*</summary>"
  "Regexp matching the end of a tag or description.")

;; Optional csharpdoc tags by token category
;;
(defconst csde-csharpdoc-extra-type-tags
  (list
   "version"
   "see"
   "since"
   "deprecated"
   )
  "Optional tags used in class/interface documentation.
Ordered following Sun's Tag Convention.")

(defconst csde-csharpdoc-extra-function-tags
  (list
   "see"
   "since"
   "serialData"
   "deprecated"
   )
  "Optional tags used in method/constructor documentation.
Ordered following Sun's Tag Convention.")

(defconst csde-csharpdoc-extra-variable-tags
  (list
   "see"
   "since"
   "serial"
   "serialField"
   "deprecated"
   )
  "Optional tags used in field documentation.
Ordered following Sun's Tag Convention.")

;; All csharpdoc tags by token category
;;
(defconst csde-csharpdoc-type-tags
  (append
   (list
    "author"
    )
   csde-csharpdoc-extra-type-tags)
  "Tags allowed in class/interface documentation.
Ordered following Sun's Tag Convention.")

(defconst csde-csharpdoc-function-tags
  (append
   (list
    "param"
    "return"
    "exception"
    "throws"
    )
   csde-csharpdoc-extra-function-tags)
  "Tags allowed in method/constructor documentation.
Ordered following Sun's Tag Convention.")

(defconst csde-csharpdoc-variable-tags
  (append
   (list
    ;; No tag required
    )
   csde-csharpdoc-extra-function-tags)
  "Tags allowed in field documentation.
Ordered following Sun's Tag Convention.")

;; Core comment parser
;;
(defun csde-csharpdoc-normalize-description (desc)
  "Ensure DESC text begins with '\\n* ' and ends with '\\n*\\n'."
  (let ((i (string-match "[^ *\n\r\t]" desc)))
    (if i
        (setq desc (concat "\n* " (substring desc i))))
    ;; TODO: ensure empty line at end
    desc))

(defun csde-csharpdoc-normalize-ref (val)
  "Strip any [* \\n\\r\\t] from VAL."
  (let* ((keep "[^* \n\r\t]+")
         (ref  "")
         (i    (string-match keep val))
         j)
    (while i
      (csde-csharpdoc-dynamic-status)
      (setq j   (match-end 0)
            ref (concat ref (substring val i j))
            i   (string-match keep val j)))
    ref))

(defun csde-csharpdoc-parse-description (docstring)
  "Return the description from DOCSTRING or nil if not found.
The returned value has the form ((DESC)).  See also
`csde-csharpdoc-parse-tag-values'."
  (let ((matcher "<summary>")
        (i 0)
        j tag-val)
    (when (string-match matcher docstring)
      (csde-csharpdoc-dynamic-status)
      (setq j (match-end 0))
      (setq i (string-match csde-csharpdoc-end-tag-regexp docstring j))
      (setq tag-val (if i
                        (substring docstring j i)
                      (substring docstring j)))
      ;; Ensure that a valid description exists
      (if (not (string-equal ""
                             (csde-csharpdoc-normalize-ref tag-val)))
          (list (list tag-val))))))

(defun csde-csharpdoc-parse-tag-values (docstring tag &optional with-key)
  "Return from DOCSTRING the list of TAG values or nil if not found.
Each value is a pair (VALUE-STRING . VALUE-KEY).  If optional WITH-KEY
is 'name VALUE-KEY is the first word of VALUE-STRING.  If optional
WITH-KEY is 'ref VALUE-KEY is a normalized VALUE-STRING reference (see
`csde-csharpdoc-normalize-ref').  Otherwise VALUE-KEY is nil."
  (let ((matcher (concat csde-csharpdoc-start-tag-regexp tag))
        (i 0)
        j tag-val key tag-list)
    (while (string-match matcher docstring i)
      (csde-csharpdoc-dynamic-status)
      (setq j (match-end 0))
      (setq i (or (string-match csde-csharpdoc-end-tag-regexp docstring j)
                  (length docstring)))
      (setq tag-val (substring docstring j i))
      (cond ((eq with-key 'name)
             (setq key (and (string-match "[* \n\r\t]*\\([^ \n\r\t]+\\)" tag-val)
                            (match-string 1 tag-val))))
            ((eq with-key 'ref)
             (setq key (csde-csharpdoc-normalize-ref tag-val))))
      (setq tag-list (cons (cons tag-val key) tag-list)))
    (nreverse tag-list)))

(defun csde-csharpdoc-parse-tag (tag docstring)
  "Return the TAG documentation from DOCSTRING or nil if not found.
Documentation has the form (TAG VALUE-LIST).  See also
`csde-csharpdoc-parse-tag-values'."
  (cond ((string-equal tag csde-csharpdoc-desc-tag)
         (csde-csharpdoc-parse-description docstring))
        ((member tag csde-csharpdoc-with-name-tags)
         (csde-csharpdoc-parse-tag-values docstring tag 'name))
        ((member tag csde-csharpdoc-with-ref-tags)
         (csde-csharpdoc-parse-tag-values docstring tag 'ref))
        (t
         (csde-csharpdoc-parse-tag-values docstring tag))
        ))

(defun csde-csharpdoc-parse-tag-list (docstring)
  "Return the list of tag found in DOCSTRING."
  (let* ((matcher (concat csde-csharpdoc-start-tag-regexp
                          "\\([^ \n\r\t]+\\)"))
         (depth (regexp-opt-depth matcher))
         (i (string-match matcher docstring))
         j tag-list)
    (while i
      (csde-csharpdoc-dynamic-status)
      (setq tag-list (cons (match-string depth docstring) tag-list))
      (setq i (string-match matcher docstring (match-end depth))))
    (nreverse tag-list)))

(defun csde-csharpdoc-parse-docstring (docstring)
  "Return the parsed documentation tree from DOCSTRING.
Result has the following form: (DOCSTRING TAG-LIST TAG-VALUE-ALIST)."
  (if docstring
      (let (tag-list tag-alist l tag throws-assoc except-assoc merged-values)
        (csde-csharpdoc-status-forms "Parsing" "done"
          (csde-csharpdoc-dynamic-status)
          (setq tag-list (csde-csharpdoc-parse-tag-list docstring))
          (setq l (cons csde-csharpdoc-desc-tag tag-list))
          (while l
            (csde-csharpdoc-dynamic-status)
            (setq tag (car l))
            (if (assoc tag tag-alist)
                nil                     ; tag already processed
              (setq tag-alist
                    (cons (cons tag
                                (csde-csharpdoc-parse-tag tag docstring))
                          tag-alist)))
            (setq l (cdr l)))
          ;; The 'throws' and 'exception' tags are equivalent, so their
          ;; values are merged to allow access to 'exception' tag using
          ;; 'throws' and vice versa.
          (csde-csharpdoc-dynamic-status)
          (setq throws-assoc (assoc "throws"    tag-alist))
          (csde-csharpdoc-dynamic-status)
          (setq except-assoc (assoc "exception" tag-alist))
          (when (or throws-assoc except-assoc)
            (csde-csharpdoc-dynamic-status)
            (setq merged-values (append (cdr throws-assoc) (cdr except-assoc)))
            (csde-csharpdoc-dynamic-status)
            (if throws-assoc
                (setcdr throws-assoc merged-values)
              (setq tag-alist (cons (cons "throws"    merged-values) tag-alist)))
            (csde-csharpdoc-dynamic-status)
            (if except-assoc
                (setcdr except-assoc merged-values)
              (setq tag-alist (cons (cons "exception" merged-values) tag-alist))))
          (csde-csharpdoc-dynamic-status t))
        (list docstring tag-list tag-alist))))

;; Handling of csharpdoc comment parsed tree
;;
(defmacro csde-csharpdoc-doctree-docstring (doctree)
  "Return the docstring part of DOCTREE."
  `(car ,doctree))

(defmacro csde-csharpdoc-doctree-tag-list (doctree)
  "Return the tag-list part of DOCTREE."
  `(car (cdr ,doctree)))

(defmacro csde-csharpdoc-doctree-tag-value-alist (doctree)
  "Return the tag-value-alist part of DOCTREE."
  `(car (cdr (cdr ,doctree))))

(defun csde-csharpdoc-doctree-tag (doctree tag &optional name)
  "Return from DOCTREE the list of TAG values.
If optional NAME is non-nil return its specific value."
  (let ((doc (cdr
              (assoc tag
                     (csde-csharpdoc-doctree-tag-value-alist doctree)))))
    (and doc
         name
         (setq doc (rassoc name doc))
         (setq doc (list doc)))
    doc))

(defun csde-csharpdoc-doctree-known-tag-list (doctree)
  "Return the list of known tags in DOCTREE .
That is tags in `csde-csharpdoc-line-tags'."
  (delq nil
        (mapcar (function
                 (lambda (tag)
                   (and (member tag csde-csharpdoc-line-tags)
                        tag)))
                (csde-csharpdoc-doctree-tag-list doctree))))

(defun csde-csharpdoc-doctree-unknown-tag-list (doctree)
  "Return the list of unknown tags in DOCTREE .
That is tags not in `csde-csharpdoc-line-tags'."
  (delq nil
        (mapcar (function
                 (lambda (tag)
                   (and (not (member tag csde-csharpdoc-line-tags))
                        tag)))
                (csde-csharpdoc-doctree-tag-list doctree))))

;;;; semantic tokens stuff
;;;; ---------------------

(defun csde-csharpdoc-find-documentation (&optional token nosnarf)
  "Find documentation from TOKEN and return it as a clean string.
Csharp have documentation set in a comment preceeding TOKEN's
definition.  Optional argument NOSNARF means to only return the flex
token for it.  If NOSNARF is 'flex, then only return the flex
token.  See also `semantic-find-documentation'."
  (if (or token (setq token (semantic-current-nonterminal)))
      (save-excursion
        (set-buffer (semantic-token-buffer token))
        ;; Move the point at token start
        (goto-char (semantic-token-start token))
        (csde-csharpdoc-skip-spaces-forward)
        ;; If the point already at "/**" (this occurs after a doc fix)
        (if (looking-at "///[ ]*<summary>")
            nil
          ;; Skip previous spaces...
          (csde-csharpdoc-skip-spaces-backward)
          ;; Verify the point is after "*/" (csharpdoc block comment end)
          (condition-case nil
              (backward-char 2)
            (error nil))
          (when (looking-at "\\*/")
          ;; Move the point backward across the comment
            (forward-char 2)              ; return just after "*/"
            (forward-comment -1)          ; to skip the entire block
            ))
        ;; Verify the point is at "/**" (csharpdoc block comment start)
        (if (looking-at "///[ ]*<summary>")
            (let ((p (point))
                  (c (semantic-find-doc-snarf-comment 'flex)))
              (when c
                ;; Verify that the token just following the doc comment is
                ;; the current one!
                (goto-char (semantic-flex-end c))
                (csde-csharpdoc-skip-spaces-forward)
                (if (eq token (semantic-current-nonterminal))
                    (if (eq nosnarf 'flex)
                        c
                      (goto-char p)
                      (semantic-find-doc-snarf-comment nosnarf)))))))))


(defun csde-csharpdoc-token-doctree (token)
  "Return the parsed documentation tree from TOKEN."
  (csde-csharpdoc-parse-docstring
   (csde-csharpdoc-find-documentation token t)))

(defun csde-csharpdoc-replace-documentation (token &optional docstring)
  "Replace TOKEN documentation with DOCSTRING.
If DOCSTRING is nil just delete the existing documentation."
  (let* ((comment (csde-csharpdoc-find-documentation token 'flex))
         start end)
    (when comment
      (set-buffer (semantic-token-buffer token))
      (setq start (semantic-flex-start comment))
      (setq end   (semantic-flex-end   comment))
      (goto-char start)
      (save-excursion
        (goto-char end)
        (csde-csharpdoc-skip-spaces-forward)
        (delete-region start (point))
        (when docstring
          (insert docstring)
          (csde-csharpdoc-indent-documentation token))))))
  
(defun csde-csharpdoc-delete-documentation (token &optional noconfirm)
  "Delete TOKEN documentation.
Require confirmation if optional NOCONFIRM is non-nil.  Return non-nil
if done."
  (if (or noconfirm
          (y-or-n-p (format "Delete '%s' previous documentation? "
                            (semantic-token-name token))))
      (progn
        (csde-csharpdoc-replace-documentation token)
        t)))

(defun csde-csharpdoc-recenter-documentation (token &optional arg)
  "Center TOKEN documentation in window and redisplay frame.
With ARG, put point on line ARG.  See also `recenter'."
  (let ((comment (csde-csharpdoc-find-documentation token 'flex))
        start)
    (if (not comment)
        (setq start (semantic-token-start token))
      (set-buffer (semantic-token-buffer token))
      (setq start (semantic-flex-start comment)))
    (goto-char start)
    (recenter arg)))

(defun csde-csharpdoc-indent-documentation (token)
  "Indent TOKEN documentation."
  (save-excursion
    (let ((comment (csde-csharpdoc-find-documentation token 'flex))
          start end)
      ;;(if (not comment)
	  ;; comment can't be found? indent whole file :(
      ;; broken.  always reindent file
	  (save-excursion
	    (goto-char (point-min))
	    (while (= (forward-line 1) 0)
	      (indent-according-to-mode)))
	;; otherwise, do the right thing and just indent the comment
	(when comment
	  (set-buffer (semantic-token-buffer token))
	  (setq start (semantic-flex-start comment))
	  (setq end   (semantic-flex-end   comment))
	  (goto-char end)
	  (csde-csharpdoc-skip-spaces-forward)
	  (csde-csharpdoc-indent-region start (point))))));;)

;;;; Doc checker
;;;; -----------

(defconst csde-csharpdoc-checker-report-buffer "*csde-csharpdoc-checker*"
  "Name of the checker report buffer.")

(defvar csde-csharpdoc-checker-token nil
  "Current checked token.
Local to checker report buffer.")

(defvar csde-csharpdoc-checker-buffer nil
  "Current checked buffer.
Local to checker report buffer.")

(defvar csde-csharpdoc-checker-report-font-lock-keywords
  (list
   ;; References
   (list "`\\(.*\\)'"
         1 'font-lock-variable-name-face)
   ;; Csharpdoc tags
   (list "\\(@[^ \n\r\t]+\\)"
         1 'font-lock-constant-face)
   (list "\\(summary\\)"
         1 'font-lock-constant-face)
   ;; Misc.
   (list "\\[\\([fnpq]\\)\\]"
         1 'font-lock-keyword-face)
   )
  "Keywords used to highlight the checker report buffer.")

(defvar csde-csharpdoc-checker-report-mode-map nil
  "Keymap used in `csde-csharpdoc-checker-report-mode'.")

(if csde-csharpdoc-checker-report-mode-map
    ()
  (let ((keymap (make-sparse-keymap)))
    (define-key keymap "q" 'csde-csharpdoc-checker-quit)
    (define-key keymap "p" 'csde-csharpdoc-checker-previous)
    (define-key keymap "n" 'csde-csharpdoc-checker-next)
    (setq csde-csharpdoc-checker-report-mode-map keymap)))

(defun csde-csharpdoc-checker-report-mode ()
  "Mode used in checker report buffer.
\\{csde-csharpdoc-checker-report-mode-map}"
  (kill-all-local-variables)
  (setq major-mode 'csde-csharpdoc-checker-report-mode)
  (setq mode-name "csde-csharpdoc-checker")
  (set (make-local-variable 'paragraph-start)
       "[ \t]*$")
  (set (make-local-variable 'paragraph-separate)
       paragraph-start)
  (set (make-local-variable 'font-lock-defaults)
       '((csde-csharpdoc-checker-report-font-lock-keywords)
         t t ((?_ . "w"))))
  (use-local-map csde-csharpdoc-checker-report-mode-map)
  (turn-on-font-lock))

(defun csde-csharpdoc-checker-show-report (report token)
  "Show the `csde-csharpdoc-checker' REPORT for TOKEN."
  (let ((buffer (semantic-token-buffer token)))
    (pop-to-buffer csde-csharpdoc-checker-report-buffer)
    (setq buffer-read-only nil)
    (erase-buffer)
    (csde-csharpdoc-checker-report-mode)
    (set (make-local-variable 'csde-csharpdoc-checker-buffer) buffer)
    (set (make-local-variable 'csde-csharpdoc-checker-token)  token)
    (cond
     (report
      (define-key (current-local-map) "f" 'csde-csharpdoc-checker-fix)
      (insert (car report))
      (newline 2)
      (mapcar (function
               (lambda (line)
                 (let* ((from (point))
                        (to (progn
                              (fill-region
                               (point)
                               (progn
                                 (insert "  " line)
                                 (newline)
                                 (point)))
                              (point))))
                   (goto-char from)
                   (delete-char 1)
                   (insert-char ?\* 1)
                   (goto-char to))))
              (cdr report))
      (newline)
      (insert "[f]-try to fix  ")
      )
     (t
      (define-key (current-local-map) "f" nil)
      (insert "Documentation is up-to-date")
      (newline 2)))
    (insert "[p]-check previous  [n]-check next  [q]-quit")
    (goto-char (point-min))
    (csde-csharpdoc-adjust-window
     (get-buffer-window (current-buffer)))
    (setq buffer-read-only t)
    (save-selected-window
      (let ((window (get-buffer-window buffer)))
        (if (not window)
            nil
          (select-window window)
          (goto-char (semantic-token-start token))
          (csde-csharpdoc-skip-spaces-forward)
          (when (looking-at "/\\*\\*")
            (forward-comment 1)
            (csde-csharpdoc-skip-spaces-forward))
          (recenter -4))))
    (semantic-momentary-highlight-token token)))

(defun csde-csharpdoc-check-add-summary (report type name)
  "Add a summary to REPORT error list, using token TYPE and NAME."
  (and (setq report (delq nil report))  ;; Clear empty entries
       (let* ((count (length report))
              (eword (if (= count 1) "error" "errors")))
         (cons (format "%s `%s' has %d documentation %s:"
                       type name count eword)
               (nreverse report)))))

(defvar csde-csharpdoc-tag-order-alist nil
  "Cached alist of (TAG . ORDER-NO).
See function `csde-csharpdoc-ref-tag-order-alist'.")

(defun csde-csharpdoc-ref-tag-order-alist ()
  "Associate each tag in `csde-csharpdoc-line-tags' with an order number.
The result is cached in variable `csde-csharpdoc-tag-order-alist'."
  (or csde-csharpdoc-tag-order-alist
      (setq csde-csharpdoc-tag-order-alist
            (let ((l csde-csharpdoc-line-tags)
                  (i 1)
                  alist)
              (while l
                (csde-csharpdoc-dynamic-status)
                (setq alist (cons (cons (car l) i) alist)
                      i     (1+  i)
                      l     (cdr l)))
              alist))))

(defmacro csde-csharpdoc-tag-order (tag)
  "Return TAG order number."
  `(or (cdr (assoc ,tag (csde-csharpdoc-ref-tag-order-alist)))
       0))

;; Basic doc checkers
;;
(defun csde-csharpdoc-check-description (doctree)
  "Return a message if DOCTREE does not contain a description."
  (if (csde-csharpdoc-doctree-tag doctree csde-csharpdoc-desc-tag)
      nil
    "Missing description"))

(defun csde-csharpdoc-check-tag-ordered (doctree reference)
  "Return a message if tags in DOCTREE are not correctly ordered.
REFERENCE is the list of allowed tags in correct order.  See variable
`csde-csharpdoc-line-tags'."
  (let* ((tag-list (csde-csharpdoc-doctree-known-tag-list doctree))
         (tag      (car tag-list))
         (l        (cdr tag-list))
         (ok       t))
    (while (and l ok)
      (csde-csharpdoc-dynamic-status)
      (setq ok  (<= (csde-csharpdoc-tag-order tag)
                    (csde-csharpdoc-tag-order (car l)))
            tag (car l)
            l   (cdr l)))
    (if ok
        nil
      (concat "Recommended tag order is @"
              (mapconcat 'identity
                         reference
                         ", @")))))

(defun csde-csharpdoc-check-tag-allowed (doctree allowed)
  "Return a message if some tags in DOCTREE are not in ALLOWED.
Third party tags (not in `csde-csharpdoc-line-tags') are allways
allowed."
  (let ((invalids
         (delq nil
               (mapcar (function
                        (lambda (tag)
                          (csde-csharpdoc-dynamic-status)
                          (and (member tag csde-csharpdoc-line-tags)
                               (not (member tag allowed))
                               tag)))
                       (csde-csharpdoc-doctree-tag-list doctree)))))
    (if (not invalids)
        nil
      (concat "Invalid tag"
              (if (> (length invalids) 1) "s @" " @")
              (mapconcat 'identity invalids ", @")))))

;; Token based doc checkers
;;
(defun csde-csharpdoc-check-type (token doctree)
  "Check doc of 'type' (class or interface) TOKEN.
DOCTREE is the current doctree of TOKEN.  Return a non-nil report if
errors were found."
  (let ((name (semantic-token-name token))
        (type (semantic-token-type token))
        report)
    ;; Check for missing description
    (csde-csharpdoc-dynamic-status)
    (setq report
          (cons
           (csde-csharpdoc-check-description doctree)
           report))
    ;; Check for incorrect tag order
    (csde-csharpdoc-dynamic-status)
    (setq report
          (cons
           (csde-csharpdoc-check-tag-ordered doctree csde-csharpdoc-type-tags)
           report))
    ;; Check for invalid tags
    (csde-csharpdoc-dynamic-status)
    (setq report
          (cons
           (csde-csharpdoc-check-tag-allowed doctree csde-csharpdoc-type-tags)
           report))
    ;; Setup the error summary
    (csde-csharpdoc-dynamic-status)
    (csde-csharpdoc-check-add-summary report type name)))

(defun csde-csharpdoc-check-variable (token doctree)
  "Check doc of 'variable' (field) TOKEN.
DOCTREE is the current doctree of TOKEN.  Return a non-nil report if
errors were found."
  (let ((name (semantic-token-name token))
        report)
    ;; Check for missing description
    (csde-csharpdoc-dynamic-status)
    (setq report
          (cons
           (csde-csharpdoc-check-description doctree)
           report))
    ;; Check for incorrect tag order
    (csde-csharpdoc-dynamic-status)
    (setq report
          (cons
           (csde-csharpdoc-check-tag-ordered doctree csde-csharpdoc-variable-tags)
           report))
    ;; Check for invalid tags
    (csde-csharpdoc-dynamic-status)
    (setq report
          (cons
           (csde-csharpdoc-check-tag-allowed doctree csde-csharpdoc-variable-tags)
           report))
    ;; Setup the error summary
    (csde-csharpdoc-dynamic-status)
    (csde-csharpdoc-check-add-summary report "variable" name)))

(defun csde-csharpdoc-check-function (token doctree)
  "Check doc of 'function' (method or constructor) TOKEN.
DOCTREE is the current doctree of TOKEN.  Return a non-nil report if
errors were found."
  (let ((name   (semantic-token-name               token))
        (type   (semantic-token-type               token))
        (args   (semantic-token-function-args      token))
        (throws (semantic-token-function-throws    token))
        report items item)
    ;; Check for missing description
    (csde-csharpdoc-dynamic-status)
    (setq report
          (cons
           (csde-csharpdoc-check-description doctree)
           report))
    ;; Check for missing @param tags
    (setq items nil)
    (while args
      (csde-csharpdoc-dynamic-status)
      (if (semantic-token-p (car args))
          (progn
            (setq item (semantic-token-name (car args)))
            (setq items (cons item items))
            (or (csde-csharpdoc-doctree-tag doctree "param" item)
                (setq report (cons (format "Missing @param tag for `%s'" item)
                                   report)))))
      (setq args (cdr args)))
    ;; Check for extra @param tags
    (setq args (csde-csharpdoc-doctree-tag doctree "param"))
    (while args
      (csde-csharpdoc-dynamic-status)
      (setq item (cdr (car args)))
      (or (member item items)
          (setq report (cons (format "Invalid @param tag `%s'" item)
                             report)))
      (setq args (cdr args)))
    ;; Check for missing @exception tags
    (setq items nil)
    (while throws
      (csde-csharpdoc-dynamic-status)
      (setq item (car throws))
      (setq items (cons item items))
      (setq throws (cdr throws))
      (or (csde-csharpdoc-doctree-tag doctree "exception" item)
          (setq report (cons (format "Missing @exception tag for `%s'" item)
                             report))))
    ;; Check for extra @exception tags
    (setq args (csde-csharpdoc-doctree-tag doctree "exception"))
    (while args
      (csde-csharpdoc-dynamic-status)
      (setq item (cdr (car args)))
      (or (member item items)
          (setq report
                (cons
                 (format "Extra @exception tag `%s' (maybe not an error)" item)
                 report)))
      (setq args (cdr args)))
    ;; Check for missing or extra @return tag
    (setq item (csde-csharpdoc-doctree-tag doctree "return"))
    (csde-csharpdoc-dynamic-status)
    (cond ((and (not type) item)
           (setq report (cons "Invalid @return tag for constructor"
                              report)))
          ((and type (string-equal type "void") item)
           (setq report (cons "Invalid @return tag for void method"
                              report)))
          ((and type (not (string-equal type "void")) (not item))
           (setq report (cons "Missing @return tag"
                              report))))
    ;; Check for incorrect tag order
    (csde-csharpdoc-dynamic-status)
    (setq report
          (cons
           (csde-csharpdoc-check-tag-ordered doctree csde-csharpdoc-function-tags)
           report))
    ;; Check for invalid tags
    (csde-csharpdoc-dynamic-status)
    (setq report
          (cons
           (csde-csharpdoc-check-tag-allowed doctree csde-csharpdoc-function-tags)
           report))
    ;; Setup the error summary
    (csde-csharpdoc-dynamic-status)
    (csde-csharpdoc-check-add-summary report
                                   (if type "method" "constructor")
                                   name)))

(defun csde-csharpdoc-checker (token &optional noreport)
  "Call the csharpdoc checker associated to TOKEN.
If optional NOREPORT is non-nil does not show error report.  Return a
non-nil report if errors were found."
  (let* ((type    (semantic-token-token token))
         (checker (intern (format "csde-csharpdoc-check-%s" type))))
    (or (fboundp checker)
        (error "No checker found to process '%S' token" type))
    (goto-char (semantic-token-start token))
    (recenter 1)
    (let (doctree report)
      (csde-csharpdoc-status-forms "Checking" "done"
        (csde-csharpdoc-dynamic-status)
        (setq doctree (csde-csharpdoc-token-doctree token))
        (setq report  (funcall checker token doctree))
        (or noreport
            (csde-csharpdoc-checker-show-report report token))
        (csde-csharpdoc-dynamic-status t))
      report)))

(defcustom csde-csharpdoc-checker-level 'protected
  "*Accessibility level to check.
Only 'type, 'function or 'variable tokens with this level will be
checked.  The level is defined to be consistent with the csharpdoc show
options.  That is:

- - public    - check only public classes and members.
- - protected - check only protected and public classes and
                members.  This is the default.
- - package   - check only package, protected, and public classes
                and members.
- - private   - check all classes and members."
  :group 'csde-csharpdoc
  :type  '(choice :tag "level"
                  (const :tag "public"    public)
                  (const :tag "protected" protected)
                  (const :tag "package"   package)
                  (const :tag "private"   private)))

(defconst csde-csharpdoc-access-level-weights
  '((public    . 8)
    (protected . 4)
    (package   . 2)
    (private   . 0))
  "Csharp access level weights.")

(defun csde-csharpdoc-access-level-lower-p (l1 l2)
  "Return non-nil if access level L1 is lower than L2."
  (< (cdr (assq l1 csde-csharpdoc-access-level-weights))
     (cdr (assq l2 csde-csharpdoc-access-level-weights))))
         
(defun csde-csharpdoc-token-access-level (token)
  "Return the access level of TOKEN.
That is 'public 'package 'protected or 'private.  If TOKEN is included
in other ones its access level is the lowest one found in the
hierarchy."
  (let ((deps (semantic-find-nonterminal-by-overlay
               (semantic-token-start token)
               (semantic-token-buffer token)))
        last-type token categ modifiers levels)
    (while deps
      (setq token (car deps))
      (setq deps  (cdr deps))
      (setq categ (semantic-token-token token))
      (setq modifiers
            (cond
             ((eq categ 'type)
              (setq last-type (semantic-token-type token))
              (semantic-token-type-modifiers token))
             ((eq categ 'function)
              (if (string-equal last-type "interface")
                  (list "public")       ; interface members are always public
                (semantic-token-function-modifiers token)))
             ((eq categ 'variable)
              (if (string-equal last-type "interface")
                  (list "public")       ; interface members are always public
                (semantic-token-variable-modifiers token)))
             (t                         ; must never occurs
              (error "Invalid %s token" categ))))
      (setq levels
            (cons (cond ((member "public" modifiers)
                         'public)
                        ((member "protected" modifiers)
                         'protected)
                        ((member "private" modifiers)
                         'private)
                        (t
                         'package))
                  levels)))
    (car (sort levels 'csde-csharpdoc-access-level-lower-p))))

(defun csde-csharpdoc-checker-at-level-p (token)
  "Return non-nil if checking is allowed for TOKEN.
That is TOKEN accessibility level is greater than or equal to the one
specified by `csde-csharpdoc-checker-level'.  TOKEN category must be
'type, 'function or 'variable."
  (let ((level (or csde-csharpdoc-checker-level 'protected)))
    ;; if level is 'private check all
    (or (eq level 'private)
        ;; else check if token access level >= checker level
        (not (csde-csharpdoc-access-level-lower-p
              (csde-csharpdoc-token-access-level token)
              level)))))

(defun csde-csharpdoc-checker-do-find-previous-token (tokens &optional token prev)
  "Visit TOKENS and return the token before TOKEN.
PREV is the last token visited or nil at start.  If TOKEN is nil
return the last token found.  Return only a 'type 'function or
'variable token."
  (let (current categ)
    (while tokens
      (setq current (car tokens))
      (setq tokens  (cdr tokens))
      (setq categ   (semantic-token-token current))
      (if (memq categ '(type function variable))
          (cond
           ((null token)
            (if (null tokens)
                (throw 'found prev)))
           ((>= (semantic-token-start current)
                (semantic-token-start token))
            (throw 'found prev))
           (t
            (setq prev
                  (if (eq categ 'type)
                      (csde-csharpdoc-checker-do-find-previous-token
                       (semantic-token-type-parts current)
                       token current)
                    current))))
        ))
    prev))

(defun csde-csharpdoc-checker-find-previous-token (tokens &optional token)
  "Visit TOKENS and return the token before TOKEN.
If TOKEN is nil return the last token found.  Return only a 'type
'function or 'variable token."
  (catch 'found
    (csde-csharpdoc-checker-do-find-previous-token tokens token)))

(defun csde-csharpdoc-checker-find-next-token (tokens &optional token)
  "Visit TOKENS and return the token following TOKEN.
If TOKEN is nil return the first token found.  Return only a 'type
'function or 'variable token."
  (let (current next categ)
    (while (and tokens (not next))
      (setq current (car tokens))
      (setq categ   (semantic-token-token current))
      (if (memq categ '(type function variable))
          (if (or (null token)
                  (> (semantic-token-start current)
                     (semantic-token-start token)))
              (setq next current)
            (if (eq categ 'type)
                (setq next (csde-csharpdoc-checker-find-next-token
                            (semantic-token-type-parts current) token)))))
      (setq tokens (cdr tokens)))
    next))

(defun csde-csharpdoc-checker-previous-token (buffer &optional token)
  "Report the previous token in BUFFER with documentation errors.
Start checking before TOKEN if non-nil or at the last token found."
  (pop-to-buffer buffer)
  (let* ((tokens (semantic-bovinate-toplevel))
         (prev   (csde-csharpdoc-checker-find-previous-token tokens token))
         (report (and prev
                      (csde-csharpdoc-checker-at-level-p prev)
                      (csde-csharpdoc-checker prev t))))
    (while (and prev (not report))
      (setq prev   (csde-csharpdoc-checker-find-previous-token tokens prev))
      (setq report (and prev
                        (csde-csharpdoc-checker-at-level-p prev)
                        (csde-csharpdoc-checker prev t))))
    (if report
        (csde-csharpdoc-checker-show-report report prev)
      (if token
          (if (y-or-n-p "No more doc error found.  Quit? ")
              (csde-csharpdoc-checker-quit)
            (csde-csharpdoc-checker token))
        (message "No doc errors found")
        (csde-csharpdoc-checker-quit)))))
                        
(defun csde-csharpdoc-checker-next-token (buffer &optional token)
  "Report the next token in BUFFER with documentation errors.
Start checking after TOKEN if non-nil or at the first token found."
  (pop-to-buffer buffer)
  (let* ((tokens (semantic-bovinate-toplevel))
         (next   (csde-csharpdoc-checker-find-next-token tokens token))
         (report (and next
                      (csde-csharpdoc-checker-at-level-p next)
                      (csde-csharpdoc-checker next t))))
    (while (and next (not report))
      (setq next   (csde-csharpdoc-checker-find-next-token tokens next))
      (setq report (and next
                        (csde-csharpdoc-checker-at-level-p next)
                        (csde-csharpdoc-checker next t))))
    (if report
        (csde-csharpdoc-checker-show-report report next)
      (if token
          (if (y-or-n-p "No more doc error found.  Quit? ")
              (csde-csharpdoc-checker-quit)
            (csde-csharpdoc-checker token))
        (message "No doc errors found")
        (csde-csharpdoc-checker-quit)))))
                        
;;;; Doc generator
;;;; -------------

(defun csde-csharpdoc-insert (*name* &rest *texts*)
  "Insert the template *NAME* or *TEXTS* and a newline.
If *NAME* value is nil *TEXTS* are inserted if non-nil.  If *NAME* and
*TEXTS* are nil the function does nothing.
The name of variables local to this function are enclosed with \"*\"
to avoid conflicts with variables used in templates."
  (cond ((and *name* (symbolp *name*) (symbol-value *name*))
         (tempo-insert-template *name* nil)
         (newline))
        (*texts*
         (apply #'insert *texts*)
         (newline))))

;; Basic generators
;;
(defun csde-csharpdoc-insert-start-block ()
  "Insert a csharpdoc comment block start '/**' at point."
;;  (delete-region (point) (progn (skip-chars-backward " \t") (point)))
;;  (csde-csharpdoc-indent-line)
  (csde-csharpdoc-insert nil "")) ;; Leave start blank
  
(defun csde-csharpdoc-insert-empty-line ()
  "Insert an empty csharpdoc line '*'."
  (csde-csharpdoc-insert nil "///"))
  
(defun csde-csharpdoc-insert-end-block ()
  "Insert a csharpdoc end comment block."
  (csde-csharpdoc-insert 'tempo-template-csde-csharpdoc-end-block "") ;; Leave end blank
  ;; dont forget to reindent the line following the end block
  (csde-csharpdoc-indent-line))
  
(defun csde-csharpdoc-insert-previous-description (doctree)
  "Insert a csharpdoc description if it already exists in DOCTREE."
  (let ((previous (csde-csharpdoc-doctree-tag doctree csde-csharpdoc-desc-tag)))
    (if previous
        (csde-csharpdoc-insert
         nil
         "/**"
         (csde-csharpdoc-normalize-description (car (car previous)))))
    previous))

(defun csde-csharpdoc-insert-previous-tag (doctree tag &optional key)
  "If it already exists in DOCTREE, insert csharpdoc TAG value(s).
If optional KEY is non-nil insert its specific value."
  (let ((previous (csde-csharpdoc-doctree-tag doctree tag key)))
    (if previous
        (csde-csharpdoc-map
         (function
          (lambda (item)
            (csde-csharpdoc-dynamic-status)
            (csde-csharpdoc-insert nil "* @" tag (car item))))
         previous))
    previous))

(defun csde-csharpdoc-insert-unknown-tags (doctree)
  "Insert unknown tags found in DOCTREE.
See `csde-csharpdoc-doctree-unknown-tag-list'."
  (csde-csharpdoc-map
   (function
    (lambda (tag)
      (csde-csharpdoc-dynamic-status)
      (csde-csharpdoc-insert-previous-tag doctree tag)))
   (csde-csharpdoc-doctree-unknown-tag-list doctree)))

(defun csde-csharpdoc-insert-author-tag (doctree)
  "Insert csharpdoc @author tags.
If tags already exist in DOCTREE keep them, else insert a new default
one."
  (or (csde-csharpdoc-insert-previous-tag doctree "author")
      (csde-csharpdoc-insert 'tempo-template-csde-csharpdoc-author-tag)))

;; (defun csde-csharpdoc-insert-since-tag (doctree)
;;   "Insert a csharpdoc @since tag.
;; If tag already exists in DOCTREE keep it, else insert a new default one."
;;   (or (csde-csharpdoc-insert-previous-tag doctree "since")
;;       (csde-csharpdoc-insert 'tempo-template-csde-csharpdoc-since-tag)))

;; (defun csde-csharpdoc-insert-version-tag (doctree)
;;   "Insert a csharpdoc @version tag.
;; If tag already exists in DOCTREE keep it, else insert a new default one."
;;   (or (csde-csharpdoc-insert-previous-tag doctree "version")
;;       (csde-csharpdoc-insert 'tempo-template-csde-csharpdoc-version-tag)))

;; (defun csde-csharpdoc-insert-see-tag (doctree refs)
;;   "Insert csharpdoc @see tags.
;; If tags already exist in DOCTREE keep them, else insert a new default one
;; for each item in REFS."
;;   (or (csde-csharpdoc-insert-previous-tag doctree "see")
;;       (csde-csharpdoc-map
;;        (function
;;         (lambda (ref)
;;           (and ref (not (string= ref ""))
;;                (csde-csharpdoc-insert 'tempo-template-csde-csharpdoc-see-tag))))
;;        refs)))

(defun csde-csharpdoc-insert-param-tag (doctree type name)
  "Insert a csharpdoc @param tag.
If tag already exists in DOCTREE keep it, else insert a new default one
using parameter TYPE and NAME."
  (or (csde-csharpdoc-insert-previous-tag doctree "param" name)
      (and type (not (string= type ""))
           name (not (string= name ""))
           (csde-csharpdoc-insert 'tempo-template-csde-csharpdoc-param-tag))))

(defun csde-csharpdoc-insert-exception-tag (doctree types)
  "Insert csharpdoc @exception tags.
If tags already exist in DOCTREE keep them, else insert a new default one
for each exception in TYPES."
  (csde-csharpdoc-map
   (function
    (lambda (type)
      (csde-csharpdoc-dynamic-status)
      (or (csde-csharpdoc-insert-previous-tag doctree "exception" type)
          (and type (not (string= type ""))
               (csde-csharpdoc-insert 'tempo-template-csde-csharpdoc-exception-tag)))))
   types)
  ;; Keep extra exception tags (maybe not invalid tags)
  (csde-csharpdoc-map
   (function
    (lambda (type)
      (csde-csharpdoc-dynamic-status)
      (setq type (cdr type))
      (or (member type types)
          (csde-csharpdoc-insert-previous-tag doctree "exception" type))))
   (csde-csharpdoc-doctree-tag doctree "exception")))

(defun csde-csharpdoc-insert-return-tag (doctree type)
  "Insert a csharpdoc @return tag.
If tag already exists in DOCTREE keep it, else insert a new default one
using return TYPE."
  (and type (not (string= type "void"))
       (or (csde-csharpdoc-insert-previous-tag doctree "return")
           (csde-csharpdoc-insert 'tempo-template-csde-csharpdoc-return-tag))))

(defun csde-csharpdoc-insert-field-desc (doctree modifiers type name)
  "Insert a field description.
If a description already exists in DOCTREE keep it, else insert a default
one using field MODIFIERS TYPE and NAME."
  (if (csde-csharpdoc-insert-previous-description doctree)
      nil
    (csde-csharpdoc-insert-start-block)
    (csde-csharpdoc-insert 'tempo-template-csde-csharpdoc-describe-field)
    (csde-csharpdoc-insert-empty-line)))

(defun csde-csharpdoc-insert-function-desc (doctree type name)
  "Insert a method or constructor description.
If a description already exists in DOCTREE keep it, else insert a default
one using method TYPE and NAME.  If TYPE is nil insert a default
constructor description."
  (if (csde-csharpdoc-insert-previous-description doctree)
      nil
    (csde-csharpdoc-insert-start-block)
    (if (and name (not (string= name "")))
        (csde-csharpdoc-insert
         (if (and type (not (string= type "")))
             'tempo-template-csde-csharpdoc-describe-method
           'tempo-template-csde-csharpdoc-describe-constructor)))
    (csde-csharpdoc-insert-empty-line)))

(defun csde-csharpdoc-insert-class-desc (doctree name)
  "Insert a class description.
If a description already exists in DOCTREE keep it, else insert a default
one using class NAME."
  (if (csde-csharpdoc-insert-previous-description doctree)
      nil
    (csde-csharpdoc-insert-start-block)
    (csde-csharpdoc-insert 'tempo-template-csde-csharpdoc-describe-class)
    (csde-csharpdoc-insert-empty-line)))

(defun csde-csharpdoc-insert-interface-desc (doctree name)
  "Insert an interface description.
If a description already exists in DOCTREE keep it, else insert a default
one using interface NAME."
  (if (csde-csharpdoc-insert-previous-description doctree)
      nil
    (csde-csharpdoc-insert-start-block)
    (csde-csharpdoc-insert 'tempo-template-csde-csharpdoc-describe-interface)
    (csde-csharpdoc-insert-empty-line)))

;; Main generators
;;
(defun csde-csharpdoc-type-doc (modifiers type name parents &optional doctree)
  "Document a class or interface using MODIFIERS TYPE NAME PARENTS.
If description and tags already exist in DOCTREE keep them."
  ;; description
  (csde-csharpdoc-dynamic-status)
  (if (string-equal type "interface")
      (csde-csharpdoc-insert-interface-desc doctree name)
    (csde-csharpdoc-insert-class-desc doctree name))
  ;; author
  (csde-csharpdoc-dynamic-status)
  (csde-csharpdoc-insert-author-tag doctree)
  ;; Keep extra optional tags if they already exist
  (csde-csharpdoc-dynamic-status)
  (csde-csharpdoc-map (function
                    (lambda (tag)
                      (csde-csharpdoc-insert-previous-tag doctree tag)))
                   csde-csharpdoc-extra-type-tags)
  ;; Keep unknown (not Sun's) tags
  (csde-csharpdoc-insert-unknown-tags doctree)
  ;; The end of comment block
  (csde-csharpdoc-dynamic-status)
  (csde-csharpdoc-insert-end-block))

(defun csde-csharpdoc-variable-doc (modifiers type name &optional doctree)
  "Document a field using MODIFIERS TYPE NAME.
If description and tags already exist in DOCTREE keep them."
  ;; description
  (csde-csharpdoc-insert-field-desc doctree modifiers type name)
  ;; Keep extra optional tags if they already exist
  (csde-csharpdoc-map (function
                    (lambda (tag)
                      (csde-csharpdoc-insert-previous-tag doctree tag)))
                   csde-csharpdoc-extra-variable-tags)
  ;; Keep unknown (not Sun's) tags
  (csde-csharpdoc-insert-unknown-tags doctree)
  ;; The end of comment block
  (csde-csharpdoc-insert-end-block))

(defun csde-csharpdoc-function-args-doc (tokens &optional doctree)
  "Document function arguments in TOKENS.
If tags already exist in DOCTREE keep them."
  (csde-csharpdoc-map
   (function
    (lambda (token)
      (if (semantic-token-p token)      ; because TOKENS can be (nil)
          (let ((modifiers (semantic-token-variable-modifiers token))
                (type      (semantic-token-type               token))
                (name      (semantic-token-name               token)))
            (csde-csharpdoc-insert-param-tag doctree type name)))))
   tokens))
      
(defun csde-csharpdoc-function-doc (modifiers type name args throws &optional doctree)
  "Document a function using MODIFIERS TYPE NAME ARGS THROWS.
If description and tags already exist in DOCTREE keep them."
  ;; description
  (csde-csharpdoc-insert-function-desc doctree type name)
  ;; param
  (csde-csharpdoc-function-args-doc args doctree)
  ;; return
  (csde-csharpdoc-insert-return-tag doctree type)
  ;; exception
  (csde-csharpdoc-insert-exception-tag doctree throws)
  ;; Keep extra optional tags if they already exist
  (csde-csharpdoc-map (function
                    (lambda (tag)
                      (csde-csharpdoc-insert-previous-tag doctree tag)))
                   csde-csharpdoc-extra-function-tags)
  ;; Keep unknown (not Sun's) tags
  (csde-csharpdoc-insert-unknown-tags doctree)
  ;; The end of comment block
  (csde-csharpdoc-insert-end-block))

;; Main token based generators
;;
(defun csde-csharpdoc-type (token doctree)
  "Document a 'type' (class or interface) TOKEN.
DOCTREE is the current doctree of TOKEN."
  (let ((modifiers  (semantic-token-type-modifiers token))
        (type       (semantic-token-type           token))
        (name       (semantic-token-name           token))
        (parents    (semantic-token-type-parent    token)))
    (csde-csharpdoc-dynamic-status)
    (csde-csharpdoc-type-doc modifiers type name parents doctree)))

(defun csde-csharpdoc-variable (token doctree)
  "Document a 'variable' (field) TOKEN.
DOCTREE is the current doctree of TOKEN."
  (let ((modifiers  (semantic-token-variable-modifiers token))
        (type       (semantic-token-type               token))
        (name       (semantic-token-name               token)))
    (csde-csharpdoc-dynamic-status)
    (csde-csharpdoc-variable-doc modifiers type name doctree)))

(defun csde-csharpdoc-function (token doctree)
  "Document a 'function' (method or constructor) TOKEN.
DOCTREE is the current doctree of TOKEN."
  (let ((modifiers  (semantic-token-function-modifiers token))
        (type       (semantic-token-type               token))
        (name       (semantic-token-name               token))
        (args       (semantic-token-function-args      token))
        (throws     (semantic-token-function-throws    token)))
    (csde-csharpdoc-dynamic-status)
    (csde-csharpdoc-function-doc modifiers type name args throws doctree)))

(defun csde-csharpdoc-generator (token &optional noconfirm)
  "Call the csharpdoc generator associated to TOKEN.
Require confirmation to delete existing documentation if optional
NOCONFIRM is non-nil."
  (let* ((type      (semantic-token-token token))
         (generator (intern (format "csde-csharpdoc-%s" type)))
         (checker   (intern (format "csde-csharpdoc-check-%s" type))))
    (or (fboundp generator)
        (error "No generator found to process '%S' token" type))
    (goto-char (semantic-token-start token))
    (let ((report t) doctree)
      (csde-csharpdoc-status-forms "Updating" "done"
        (csde-csharpdoc-dynamic-status)
        (setq doctree (csde-csharpdoc-token-doctree token))
        (if (and (fboundp checker)
                 (not (funcall checker token doctree)))
            (setq report nil)
          (when (or (not doctree)
                    (csde-csharpdoc-delete-documentation token noconfirm))
            (funcall generator token doctree)
            (csde-csharpdoc-indent-documentation token)))
        (csde-csharpdoc-dynamic-status t))
      (or report
          (message "Documentation is up-to-date")))))

;;;; Misc command auxiliaries
;;;; ------------------------

(defun csde-csharpdoc-nonterminal-at-line (&optional checkcache)
  "Search the bovine table for the token at the current line.
If the optional argument CHECKCACHE is non-nil, then reparse the
buffer if needed."
  (semantic-bovinate-toplevel checkcache)
  (save-excursion
    ;; Move the point to the first non-blank character found.  Skip
    ;; spaces, tabs and newlines.
    (beginning-of-line)
    (csde-csharpdoc-skip-spaces-forward)
    (semantic-current-nonterminal)))

(defvar csde-csharpdoc-checkdoc-excursion nil
  "Buffer and position before running `csde-csharpdoc-checkdoc'.")

(defun csde-csharpdoc-checkdoc-clear-excursion ()
  "Clear saved buffer, window start and point.
See `csde-csharpdoc-checkdoc-save-excursion'."
  (setq csde-csharpdoc-checkdoc-excursion nil))

(defun csde-csharpdoc-checkdoc-save-excursion ()
  "Save current buffer, window start and point."
  (setq csde-csharpdoc-checkdoc-excursion
        (vector (current-buffer)
                (window-start)
                (point))))

(defun csde-csharpdoc-checkdoc-restore-excursion ()
  "Restore window start and point of the current buffer.
Does nothing if the current buffer is not the one saved by previous
`csde-csharpdoc-checkdoc-save-excursion'."
  (when (and (vectorp csde-csharpdoc-checkdoc-excursion)
             (= 3 (length csde-csharpdoc-checkdoc-excursion))
             (eq (current-buffer) (aref csde-csharpdoc-checkdoc-excursion 0)))
    (set-window-start (selected-window)
                      (aref csde-csharpdoc-checkdoc-excursion 1))
    (goto-char (aref csde-csharpdoc-checkdoc-excursion 2))
    (csde-csharpdoc-checkdoc-clear-excursion)))
                    
(defun csde-csharpdoc-checker-previous ()
  "Goto the previous token with doc errors."
  (interactive)
  (and (eq major-mode 'csde-csharpdoc-checker-report-mode)
       (csde-csharpdoc-checker-previous-token csde-csharpdoc-checker-buffer
                                           csde-csharpdoc-checker-token)))

(defun csde-csharpdoc-checker-next ()
  "Goto the next token with doc errors."
  (interactive)
  (and (eq major-mode 'csde-csharpdoc-checker-report-mode)
       (csde-csharpdoc-checker-next-token csde-csharpdoc-checker-buffer
                                       csde-csharpdoc-checker-token)))

(defun csde-csharpdoc-checker-fix ()
  "Fix documentation of checked token.
Used in `csde-csharpdoc-checker-report-mode'."
  (interactive)
  (and (eq major-mode 'csde-csharpdoc-checker-report-mode)
       (let ((token  csde-csharpdoc-checker-token)
             (buffer csde-csharpdoc-checker-buffer))
         (when (and token buffer)
           (pop-to-buffer buffer)
           (goto-char (semantic-token-start token))
           ;; do the fix (THE POINT STAY AT TOKEN START POSITION)
           (csde-csharpdoc-generator token t)
           ;; recheck the token
           (csde-csharpdoc-checker token)))))

(defun csde-csharpdoc-checker-quit ()
  "Quit the `csde-csharpdoc-checker' report buffer.
Used in `csde-csharpdoc-checker-report-mode'."
  (interactive)
  (let ((buffer (get-buffer csde-csharpdoc-checker-report-buffer)))
    (if (not buffer)
        (csde-csharpdoc-checkdoc-restore-excursion)
      (set-buffer buffer)
      (when (bufferp csde-csharpdoc-checker-buffer)
        (pop-to-buffer csde-csharpdoc-checker-buffer)
        (csde-csharpdoc-checkdoc-restore-excursion))
      (delete-windows-on buffer t)
      (kill-buffer buffer))))

;;;; Commands
;;;; --------

;;;###autoload
(defun csde-csharpdoc-customize ()
  "Show the csde-csharpdoc options panel."
  (interactive)
  (customize-group "csde-csharpdoc"))

;;;###autoload
(defun csde-csharpdoc-autodoc-at-line ()
  "Update csharpdoc comment block for declaration at current line.

Uses the semantic bovinator parser table to find declarations (see
`csde-csharpdoc-nonterminal-at-line').

BEFORE EXECUTING THE COMMAND, THE POINT MUST BE LOCATED AT THE FIRST
LINE OF THE CLASS OR METHOD DECLARATION.  IF NOT RESULT IS UNCERTAIN.

In the following examples, point is located at the beginning of the
line, before the word 'public' (but it could be anywhere on this
line):

1- Class example:
   -------------

-|-  public class MyClass
       extends MySuperClass implements Runnable, csharp.io.Serializable
     {
       ...

\\[csde-csharpdoc-autodoc-at-line] inserts:

+    /**
+     * Describe class <code>MyClass</code> here.
+     *
+     * @author \"David Ponce\" <david.ponce@wanadoo.fr>
+     * @version 1.0
+     * @since 1.0
+     * @see MySuperClass
+     * @see Runnable
+     * @see csharp.io.Serializable
+     */
     public class MyClass
       extends MySuperClass implements Runnable, csharp.io.Serializable
     {
       ...

2- Method example:
   --------------

-|-  public
     void   myMethod( int  x,  int y )
       throws Exception
     {
       ...

\\[csde-csharpdoc-autodoc-at-line] inserts:

+    /**
+     * Describe <code>myMethod</code> method here.
+     *
+     * @param x an <code>int</code> value
+     * @param y a <code>long</code> value
+     * @exception Exception if an error occurs
+     */
     public
     void   myMethod( int  x,  long y )
       throws Exception
     {
       ...

3- Field example:
   --------------

-|-  private static final int SIZE = 10;

\\[csde-csharpdoc-autodoc-at-line] inserts:

+    /**
+     * Describe constant <code>SIZE</code> here.
+     */
     private static final int SIZE = 10;


`tempo' templates are used for each category of csharpdoc line.  The
following templates are currently defined and fully customizable (see
`tempo-define-template' for the different items that can be used in a
tempo template):

- - `csde-csharpdoc-author-tag-template'
- - `csde-csharpdoc-describe-class-template'
- - `csde-csharpdoc-describe-constructor-template'
- - `csde-csharpdoc-describe-interface-template'
- - `csde-csharpdoc-describe-method-template'
- - `csde-csharpdoc-describe-field-template'
- - `csde-csharpdoc-exception-tag-template'
- - `csde-csharpdoc-param-tag-template'
- - `csde-csharpdoc-return-tag-template'
- - `csde-csharpdoc-see-tag-template'
- - `csde-csharpdoc-since-tag-template'
- - `csde-csharpdoc-version-tag-template'

For example if you customize `csde-csharpdoc-describe-class-template'
with the following value:

'(\"* \" (P \"Class description: \"))

you will be asked to enter the class description in the minibuffer.
See also the `csde-csharpdoc-field-type', `csde-csharpdoc-a' and
`csde-csharpdoc-code' helper functions."
  (interactive)
  (or (eq major-mode 'csde-mode)
      (error "Major mode must be 'csde-mode'"))
  (let ((found (csde-csharpdoc-nonterminal-at-line t)))
    (if found
        (csde-csharpdoc-generator found)
      (error "No token found at point"))))

;;;###autoload
(defun csde-csharpdoc-checkdoc-at-line ()
  "Check csharpdoc comment block of declaration at current line.

Uses the semantic bovinator parser table to find declarations (see
`csde-csharpdoc-nonterminal-at-line').

BEFORE EXECUTING THE COMMAND, THE POINT MUST BE LOCATED AT THE FIRST
LINE OF THE CLASS OR METHOD DECLARATION.  IF NOT RESULT IS UNCERTAIN."
  (interactive)
  (or (eq major-mode 'csde-mode)
      (error "Major mode must be 'csde-mode'"))
  (let ((found (csde-csharpdoc-nonterminal-at-line t)))
    (if (not found)
        (error "No token found at point")
      (csde-csharpdoc-checkdoc-save-excursion)
      (csde-csharpdoc-checker found))))

;;;###autoload
(defun csde-csharpdoc-checkdoc ()
  "Check doc comments of tokens in the current buffer.
Report the next token with documentation errors."
  (interactive)
  (or (eq major-mode 'csde-mode)
      (error "Major mode must be 'csde-mode'"))
  (semantic-bovinate-toplevel t)
  (csde-csharpdoc-checkdoc-save-excursion)
  (csde-csharpdoc-checker-next-token (current-buffer)))

(defalias 'csde-csharpdoc-autodoc 'csde-csharpdoc-checkdoc)

;;; Compatibility
;;
(defalias 'csde-csharpdoc-generate-csharpdoc-template 'csde-csharpdoc-autodoc-at-line)
(require 'csde-csharpdoc-gen)              ; Load the csharpdoc builder

(defun csde-csharpdoc-enable-menu-p ()
  "Return non-nil if corresponding doc menu item is enabled.
That is point is on the first line of a class, method, or field
definition."
  (let ((p (save-excursion (beginning-of-line) (point)))
        (token-at-line (csde-csharpdoc-nonterminal-at-line))
        start end)
    (and token-at-line
         (memq (semantic-token-token token-at-line)
               '(function type variable))
         (save-excursion
           (setq start (progn
                         (goto-char (semantic-token-start token-at-line))
                         (beginning-of-line)
                         (point)))
           (setq end (or (re-search-forward "[;={]")
                         (progn
                           (goto-char p)
                           (end-of-line)
                           (point))))
           (and (>= p start) (<= p end))))))

(provide 'csde-csharpdoc)

;;
;; $Log: csde-csharpdoc.el,v $
;; Revision 1.2  2001/02/12 05:38:25  paulk
;; CSDE 2.2.7
;;
;; Revision 1.17  2001/01/27 05:42:07  paulk
;; Enhancements from David Ponce:
;;
;; - Improved `csde-csharpdoc-start-tag-regexp' and
;;   `csde-csharpdoc-end-tag-regexp' regular expressions which fix little
;;   problems when parsing csharpdoc tags.  These regexps enforce the
;;   following rule (from JDK 1.3 documentation "csharpdoc - The Csharp API
;;   Documentation Generator" - Chapter "DOCUMENTATION COMMENTS" -
;;   "Standard and in-line tags":
;;
;;
;;   "[...] a standard tag must appear at the beginning of a line,
;;   ignoring leading asterisks, white space and comment separator
;;   (/**). This means you can use the @ character elsewhere in the text
;;   and it will not be interpreted as the start of a tag. If you want to
;;   start a line with the @ character and not have it be interpreted,
;;   use the HTML entity @. [...]"
;;
;;
;; - `csde-csharpdoc-checker-report-mode' now turns on `font-lock-mode'.
;;   This is useful in XEmacs which don't have a `global-font-lock-mode'.
;;
;;
;; - Filling of messages in `csde-csharpdoc-checker-show-report' now works
;;   with XEmacs too.
;;
;; Revision 1.16  2000/12/18 05:22:45  paulk
;; *** empty log message ***
;;
;; Revision 1.15  2000/11/27 06:18:40  paulk
;; Miscellaneous bug fixes and minor enhancements.
;;
;; Revision 1.14  2000/10/25 03:11:57  paulk
;; Enhancements from David Ponce:
;;
;; * It is now possible to check csharpdoc comments only for tokens with a
;;   specified access level. The new `csde-csharpdoc-checker-level' option
;;   defines the accessibility level to check.
;;
;;   Only 'type, 'function or 'variable tokens with this level will be
;;   checked. The level is defined to be consistent with the csharpdoc show
;;   options. That is:
;;
;;   - - public    - check only public classes and members.
;;   - - protected - check only protected and public classes and
;;                   members. This is the default.
;;   - - package   - check only package, protected, and public classes
;;                   and members.
;;   - - private   - check all classes and members.
;;
;;   If a token is included in other ones its access level is the lowest
;;   one found in the hierarchy.
;;
;;   Using `csde-csharpdoc-checkdoc-at-line' it is always possible to check
;;   any token regardless of its access level.
;;
;; * Changed '[u]-update' action by more appropriate '[f]-try to fix' in
;;   the checker report dialog.
;;
;; * Changed message "Invalid tag order, must be ..." by
;;   "Recommended tag order is ...".
;;
;; Revision 1.13  2000/10/22 07:54:04  paulk
;; Add menu enabler for csharpdoc commands.
;;
;; Revision 1.12  2000/10/20 04:07:44  paulk
;; Enhancements from David Ponce.
;;
;; Revision 1.11  2000/10/08 12:55:39  paulk
;; *** empty log message ***
;;
;; Revision 1.10  2000/09/23 04:26:13  paulk
;; Adds csde-csharpdoc-command-path and csde-csharpdoc-display-doc variables. Thanks to "Jason Stell" <Jason.Stell@globalone.net> for providing the initial version of these changes.
;;
;; Revision 1.9  2000/08/19 06:44:02  paulk
;; Don't quote class names in csharpdoc command.
;;
;; Revision 1.8  2000/08/08 04:49:32  paulk
;; Fixed the doc for csde-csharpdoc-make.
;;
;; Revision 1.7  2000/08/07 06:09:57  paulk
;; csde-csharpdoc-make now uses csde-db-source-directories as the sourcepath for generating doc.
;;
;; Revision 1.6  2000/08/07 05:16:10  paulk
;; Adds csde-csharpdoc-make command. Thanks to Sergey A Klibanov <sakliban@cs.wustl.edu>.
;;
;; Revision 1.5  2000/07/13 05:22:48  paulk
;; *** empty log message ***
;;
;; Revision 1.4  2000/07/08 07:15:41  paulk
;; Latest updates from David.
;;
;; Revision 1.2  2000/06/22 03:36:40  paulk
;; Fix submitted by David Ponce.
;;
;; Revision 1.1  2000/06/12 08:19:03  paulk
;; Initial revision.
;;
;;

;;; csde-csharpdoc.el ends here
