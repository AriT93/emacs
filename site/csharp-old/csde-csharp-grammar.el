;;; csde-csharp-grammar.el
;; $Revision: 1.2 $

;; Adapted from the JDE by Matt Bruce <matt.bruce@morganstanley.com>
;; Maintainer:  Matt Bruce

;; JDE Author: Paul Kinnucan <paulk@mathworks.com>
;; JDE Maintainer: Paul Kinnucan

;; Keywords: csharp, tools

;; Copyright (C) 2001 Matt Bruce

;; The JDE is Copyright (C) 1997, 1998, 1999, 2000, 2001 Paul Kinnucan.

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
(defvar csde-parse-bovine-csharp-grammar
  `((bovine-toplevel
     ( type_declaration)
     )					; end bovine-toplevel
    (number
     ( symbol "[0-9]" punctuation "\\." symbol "[0-9Ee]" punctuation "[-+]" symbol "[0-9fFdD]")
     ( symbol "[0-9]" punctuation "\\." symbol "[0-9EefFdD]")
     ( symbol "[0-9fFdD]")
     )					; end number
    (literal
     ( number)
     ( qualified_name)
     ( string)
     )					; end literal
    (type
     ( reference_type
       ,(semantic-lambda
	 (nth 0 vals)))
     ( primitive_type
       ,(semantic-lambda
	 (nth 0 vals)))
     )					; end type
    (primitive_type
     ( BOOLEAN)
     ( BYTE)
     ( SHORT)
     ( INT)
     ( LONG)
     ( CHAR)
     ( FLOAT)
     ( DOUBLE)
     )					; end primitive_type
    (reference_type
     ( array_type
       ,(semantic-lambda
	 (nth 0 vals)))
     ( qualified_name
       ,(semantic-lambda
	 (nth 0 vals)))
     )					; end reference_type
    (array_type
     ( primitive_type dims
		      ,(semantic-lambda
			(list ( concat ( car (nth 0 vals)) ( car (nth 1 vals))))))
     ( qualified_name dims
		      ,(semantic-lambda
			(list ( concat ( car (nth 0 vals)) ( car (nth 1 vals))))))
     )					; end array_type
    (qualified_name
     ( symbol punctuation "\\." qualified_name
	      ,(semantic-lambda
		(list ( concat (nth 0 vals) (nth 1 vals) ( car (nth 2 vals))))))
     ( symbol
       ,(semantic-lambda
	 (list (nth 0 vals))))
     )					; end qualified_name
    (namespace_declaration
     ( NAMESPACE qualified_name namespace_body
		 ,(semantic-lambda
		   (list (nth 1 vals) 'type "namespace" (nth 2 vals) nil)))
     )  					; end namespace_declaration
    (namespace_body
     ( semantic-list
       ,(semantic-lambda

	 (semantic-bovinate-from-nonterminal-full (car (nth 0 vals)) (cdr (nth 0 vals)) 'type_declaration)
	 ))
     )					; end namespace_body
    (using_declaration
     ( USING qualified_name punctuation ";"
	      ,(semantic-lambda
		(nth 1 vals) (list 'include nil)))
     )					; end using_declaration
    (type_declarations
     ( type_declaration type_declarations
			,(semantic-lambda
			  ( cons ( car (nth 0 vals)) (nth 1 vals))))
     ( type_declaration
       ,(semantic-lambda
	 (nth 0 vals)))
     )                                  ; end type_declarations
    (type_declaration
     ( punctuation ";")
     ( class_declaration
       ,(semantic-lambda
	 (nth 0 vals)))
     ( interface_declaration
       ,(semantic-lambda
	 (nth 0 vals)))
     ( namespace_declaration
       ,(semantic-lambda
	 (nth 0 vals)))
     ( using_declaration
       ,(semantic-lambda
	 (nth 0 vals)))
     )					; end type_declaration
    (modifiers_opt
     ( modifiers
       ,(semantic-lambda
	 (nth 0 vals)))
     ()
     )					; end modifiers_opt
    (modifiers
     ( modifier modifiers
		,(semantic-lambda
		  ( cons ( car (nth 0 vals)) (nth 1 vals))))
     ( modifier
       ,(semantic-lambda
	 (nth 0 vals)))
     )					; end modifiers
    (modifier
     ( PUBLIC)
     ( PROTECTED)
     ( PRIVATE)
     ( STATIC)
     ( ABSTRACT)
     ( FINAL)
     ( NATIVE)
     ( SYNCHRONIZED)
     ( TRANSIENT)
     ( VOLATILE)
     ( STRICTFP)
     ( OVERRIDE)
     )					; end modifier
    (class_declarations
     ( class_declaration class_declarations)
     ,(semantic-lambda
       ( cons ( car (nth 0 vals)) (nth 1 vals))))
    (interface_declarations
     ( interface_declaration interface_declarations)
     ,(semantic-lambda
       ( cons ( car (nth 0 vals)) (nth 1 vals))))
    (class_declaration
     ( modifiers_opt CLASS qualified_name class_parents class_body
		     ,(semantic-lambda
		       (nth 2 vals) (list 'type "class" (nth 4 vals) (nth 3 vals) (nth 0 vals) nil)))
     )					; end class_declaration
    (class_parents
     ( inherits
       ,(semantic-lambda
	 (nth 0 vals)))
     ( super interfaces
	     ,(semantic-lambda
	       ( append (nth 0 vals) (nth 1 vals))))
     ( interfaces super
		  ,(semantic-lambda
		    ( append (nth 1 vals) (nth 0 vals))))
     ( super
       ,(semantic-lambda
	 (nth 0 vals)))
     ( interfaces
       ,(semantic-lambda
	 ( cons nil (nth 0 vals))))
     ()
     )					; end class_parents
    (super
     ( EXTENDS qualified_name
	       ,(semantic-lambda
		 (nth 1 vals)))
     )					; end super
    (interfaces
     ( IMPLEMENTS qualified_name_list
		  ,(semantic-lambda
		    (nth 1 vals)))
     )					; end interfaces
    (inherits
     ( punctuation ":" qualified_name_list
		   ,(semantic-lambda
		     (nth 1 vals)))
     )                                  ; end inherits
    (qualified_name_list
     ( qualified_name punctuation "," qualified_name_list
		      ,(semantic-lambda
			( cons ( car (nth 0 vals)) (nth 2 vals))))
     ( qualified_name
       ,(semantic-lambda
	 (nth 0 vals)))
     )					; end qualified_name_list
    (class_body
     ( semantic-list
       ,(semantic-lambda

	 (semantic-bovinate-from-nonterminal-full (car (nth 0 vals)) (cdr (nth 0 vals)) 'class_body_declarations)
	 ))
     )					; end class_body
    (class_body_declarations
     ( class_declaration
       ,(semantic-lambda
	 (nth 0 vals)))
     ( interface_declaration
       ,(semantic-lambda
	 (nth 0 vals)))
     ( field_declaration
       ,(semantic-lambda
	 (nth 0 vals)))
     ( method_declaration
       ,(semantic-lambda
	 (nth 0 vals)))
     ( constructor_declaration
       ,(semantic-lambda
	 (nth 0 vals)))
     )					; end class_body_declarations
    (field_declaration
     ( modifiers_opt type variable_declarators punctuation ";"
		     ,(semantic-lambda
		       (nth 2 vals) (list 'variable) (nth 1 vals) (list nil nil (nth 0 vals) nil)))
     )					; end field_declaration
    (variable_declarators
     ( variable_declarator variable_declarators_opt
			   ,(semantic-lambda
			     (nth 0 vals)))
     )					; end variable_declarators
    (variable_declarators_opt
     ( punctuation "," variable_declarators)
     ()
     )					; end variable_declarators_opt
    (variable_declarator
     ( variable_declarator_id variable_assign_opt
			      ,(semantic-lambda
				(nth 0 vals)))
     )					; end variable_declarator
    (variable_assign_opt
     ( punctuation "=" variable_initializer)
     ()
     )					; end variable_assign_opt
    (variable_declarator_id
     ( symbol dims
	      ,(semantic-lambda
		(list ( concat (nth 0 vals) ( car (nth 1 vals))))))
     ( symbol
       ,(semantic-lambda
	 (list (nth 0 vals))))
     )					; end variable_declarator_id
    (variable_initializer
     ( array_initializer)
     ( expression)
     )					; end variable_initializer
    (method_declaration
     ( method_header method_body
		     ,(semantic-lambda
		       (nth 0 vals)))
     )					; end method_declaration
    (method_header
     ( modifiers_opt method_type symbol formal_parameter_list_opt throws_opt
		     ,(semantic-lambda
		       (list (nth 2 vals) 'function) (nth 1 vals) (list (nth 3 vals) (nth 0 vals) (nth 4 vals) nil)))
     )					; end method_header
    (method_type
     ( VOID
       ,(semantic-lambda
	 (list (nth 0 vals))))
     ( type
       ,(semantic-lambda
	 (nth 0 vals)))
     )					; end method_type
    (formal_parameter_list_opt
     ( semantic-list
       ,(lambda (vals start end)

	  (semantic-bovinate-from-nonterminal (car (nth 0 vals)) (cdr (nth 0 vals)) 'formal_parameter_list)
	  ))
     )					; end formal_parameter_list_opt
    (formal_parameter_list
     ( open-paren "(" close-paren ")"
		  ,(semantic-lambda
		    (list nil)))
     ( open-paren "(" formal_parameter formal_parameter_list_next
		  ,(semantic-lambda
		    ( cons (nth 1 vals) (nth 2 vals))))
     )					; end formal_parameter_list
    (formal_parameter_list_next
     ( close-paren ")"
		   ,(semantic-lambda
		     (list nil)))
     ( punctuation "," formal_parameter formal_parameter_list_next
		   ,(semantic-lambda
		     ( cons (nth 1 vals) (nth 2 vals))))
     )					; end formal_parameter_list_next
    (formal_parameter-modifier
     ( FINAL)
     ()
     )					; end formal_parameter-modifier
    (formal_parameter
     ( formal_parameter-modifier type variable_declarator_id
				 ,(semantic-lambda
				   (list ( car (nth 2 vals)) 'variable ( car (nth 1 vals)) nil nil (nth 0 vals) nil)))
     )					; end formal_parameter
    (throws_opt
     ( throws
       ,(semantic-lambda
	 (nth 0 vals)))
     ()
     )					; end throws_opt
    (throws
     ( THROWS qualified_name_list
	      ,(semantic-lambda
		(nth 1 vals)))
     )					; end throws
    (method_body
     ( punctuation ";"
		   ,(semantic-lambda
		     (list nil)))
     ( block
	 ,(semantic-lambda
	   (list nil)))
     )					; end method_body
    (constructor_declaration
     ( modifiers_opt symbol formal_parameter_list_opt throws_opt constructor_body
		     ,(semantic-lambda
		       (list (nth 1 vals) 'function nil (nth 2 vals) (nth 0 vals) (nth 3 vals) nil)))
     )					; end constructor_declaration
    (constructor_body
     ( block
	 ,(semantic-lambda
	   (list nil)))
     )					; end constructor_body
    (interface_declaration
     ( modifiers_opt INTERFACE symbol interface_parents interface_body
		     ,(semantic-lambda
		       (list (nth 2 vals) 'type "interface" (nth 4 vals) (nth 3 vals) (nth 0 vals) nil)))
     )					; end interface_declaration
    (interface_parents
     ( EXTENDS qualified_name_list
	       ,(semantic-lambda
		 (nth 1 vals)))
     ()
     )					; end interface_parents
    (interface_body
     ( semantic-list
       ,(semantic-lambda

	 (semantic-bovinate-from-nonterminal-full (car (nth 0 vals)) (cdr (nth 0 vals)) 'interface_body_declarations)
	 ))
     )					; end interface_body
    (interface_body_declarations
     ( class_declaration
       ,(semantic-lambda
	 (nth 0 vals)))
     ( interface_declaration
       ,(semantic-lambda
	 (nth 0 vals)))
     ( method_header punctuation ";"
		     ,(semantic-lambda
		       (nth 0 vals)))
     ( field_declaration
       ,(semantic-lambda
	 (nth 0 vals)))
     )					; end interface_body_declarations
    (array_initializer
     ( semantic-list "\\`{")
     )					; end array_initializer
    (block
	( semantic-list "\\`{")
      )					; end block
    (primary
     ( array_creation_expression)
     ( primary_no_new_array primary_dim_opt)
     )					; end primary
    (primary_dim_opt
     ( semantic-list "\\`\\[")
     ()
     )					; end primary_dim_opt
    (primary_no_new_array
     ( qualified_name semantic-list "\\`(")
     ( class_instance_creation_expression)
     ( semantic-list "\\`(")
     ( array_type punctuation "\\." CLASS)
     ( literal)
     )					; end primary_no_new_array
    (class_instance_creation_expression
     ( NEW qualified_name semantic-list "\\`(" semantic-list "\\`{")
     ( NEW qualified_name semantic-list "\\`(")
     )					; end class_instance_creation_expression
    (array_creation_expression
     ( NEW qualified_name dims array_initializer)
     ( NEW qualified_name dims)
     )					; end array_creation_expression
    (dims_opt
     ( dims
       ,(semantic-lambda
	 (nth 0 vals)))
     (
      ,(semantic-lambda
	(list nil)))
     )					; end dims_opt
    (dims
     ( semantic-list "\\`\\[" dims_opt
		     ,(semantic-lambda
		       (list ( concat "[]" ( car (nth 1 vals))))))
     )					; end dims
    (field_access
     ( primary punctuation "\\." symbol)
     ( qualified_name)
     )					; end field_access
    (postfix_expression
     ( primary postfix_operator_opt)
     )					; end postfix_expression
    (postfix_operator_opt
     ( punctuation "[-+]" punctuation "[-+]")
     ()
     )					; end postfix_operator_opt
    (unary_expression
     ( punctuation "[-+^!]" unary_expression)
     ( punctuation "[-+]" punctuation "[-+]" unary_expression)
     ( semantic-list "\\`(" unary_expression)
     ( postfix_expression)
     )					; end unary_expression
    (operator
     ( punctuation "[-+*/%=<>^~&|!?:.]")
     ( INSTANCEOF)
     )					; end operator
    (operators
     ( operator operators)
     ( operator)
     )					; end operators
    (operators_expression_opt
     ( operators expression)
     ()
     )					; end operators_expression_opt
    (expression
     ( unary_expression operators_expression_opt)
     )					; end expression
    )
  "Grammar used by the semantic library to parse Csharp source
buffers. This grammar is a Lisp version of the BNF grammar stored in
csharp.bnf. The Lisp grammar is generated automatically from the BNF
source. For this reason, you should never modify this variable
directly.  See `bovinate' for more information.")

;;; david@dponce.com
(defvar csde-parse-bovine-csharp-keywords
  (semantic-flex-make-keyword-table
   `( ("abstract" . ABSTRACT)
      ("bool" . BOOLEAN)
      ("break" . BREAK)
      ("byte" . BYTE)
      ("case" . CASE)
      ("catch" . CATCH)
      ("char" . CHAR)
      ("class" . CLASS)
      ("const" . CONST)
      ("continue" . CONTINUE)
      ("default" . DEFAULT)
      ("do" . DO)
      ("double" . DOUBLE)
      ("else" . ELSE)
      ("extends" . EXTENDS)
      ("final" . FINAL)
      ("finally" . FINALLY)
      ("float" . FLOAT)
      ("for" . FOR)
      ("goto" . GOTO)
      ("if" . IF)
      ("implements" . IMPLEMENTS)
      ("using" . USING)
      ("instanceof" . INSTANCEOF)
      ("int" . INT)
      ("interface" . INTERFACE)
      ("long" . LONG)
      ("native" . NATIVE)
      ("new" . NEW)
      ("namespace" . NAMESPACE)
      ("override" . OVERRIDE)
      ("private" . PRIVATE)
      ("protected" . PROTECTED)
      ("public" . PUBLIC)
      ("return" . RETURN)
      ("short" . SHORT)
      ("static" . STATIC)
      ("strictfp" . STRICTFP)
      ("super" . SUPER)
      ("switch" . SWITCH)
      ("synchronized" . SYNCHRONIZED)
      ("this" . THIS)
      ("throw" . THROW)
      ("throws" . THROWS)
      ("transient" . TRANSIENT)
      ("try" . TRY)
      ("void" . VOID)
      ("volatile" . VOLATILE)
      ("while" . WHILE)
      ))
  "Table of Csharp grammar keywords. Generated automatically from the
BNF source.")

;;; david@dponce.com
(defun csde-parse-semantic-default-setup ()
  "Setup the semantic bovinator for the CSDE."
;;; WARNING: the following code is automatically generated from the
;;; BNF source. Do not modify it here! Update the BNF source
;;; instead. See `bovinate' for more information.
  ;; Code generated from csharp-with-tokens.bnf
  (setq semantic-toplevel-bovine-table csde-parse-bovine-csharp-grammar)
  (setq semantic-flex-keywords-obarray csde-parse-bovine-csharp-keywords)
  (progn
    ;; Csharp is case sensitive
    (setq semantic-case-fold nil)
    (setq document-comment-start "///[ ]*<summary>")
    (setq document-comment-end   "^[\s]*[\w]+")
    ;; imenu & speedbar setup
    (csde-imenu-setup)
    ;; initial parsing of the current buffer
    (semantic-bovinate-toplevel)
    (csde-parse-update-after-parse)
    )

;; End code generated from csharp-with-tokens.bnf
)

(provide 'csde-csharp-grammar)

;; $Log: csde-csharp-grammar.el,v $
;; Revision 1.2  2001/02/22 12:45:16  youngs
;; Fix problem with building CSDE and xslt-process
;;
;; Revision 1.1  2001/02/12 05:48:27  paulk
;; Initial XEmacs revision.
;;
;; Revision 1.2  2001/01/25 05:38:39  paulk
;; Changed the definition of formal_parameter_list to improve performance (less
;; backtracking) when parsing parameters in method and constructor
;; declarations. Thanks to David Ponce.
;;
;; Revision 1.1  2000/10/25 04:30:31  paulk
;; Initial revision.
;;

;; End of csde-csharp-grammar.el

