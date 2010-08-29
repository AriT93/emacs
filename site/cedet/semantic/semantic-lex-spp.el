;;; semantic-lex-spp.el --- Semantic Lexical Pre-processor

;;; Copyright (C) 2006, 2007, 2008 Eric M. Ludlam

;; X-CVS: $Id: semantic-lex-spp.el,v 1.19 2008/05/03 14:20:36 zappo Exp $

;; This file is not part of GNU Emacs.

;; Semantic is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This software is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:
;;
;; The Semantic Preprocessor works with semantic-lex to provide a phase
;; during lexical analysis to do the work of a pre-processor.
;;
;; A pre-processor identifies lexical syntax mixed in with another language
;; and replaces some keyword tokens with streams of alternate tokens.
;; 
;; If you use SPP in your language, be sure to specify this in your
;; semantic language setup function:
;;
;; (add-hook 'semantic-lex-reset-hooks 'semantic-lex-spp-reset-hook nil t)
;;
;;; TODO:
;;
;; Use `semantic-push-parser-warning' for situations where there are likely
;; macros that are undefined unexpectedly, or other problem.

(require 'semantic-lex)

;;; Code:
(defvar semantic-lex-spp-macro-symbol-obarray nil
  "Table of macro keywords used by the Semantic Macro.")
(make-variable-buffer-local 'semantic-lex-spp-macro-symbol-obarray)

(defvar semantic-lex-spp-project-macro-symbol-obarray nil
  "Table of macro keywords for this project used by the Semantic Macro.")
(make-variable-buffer-local 'semantic-lex-spp-project-macro-symbol-obarray)

(defvar semantic-lex-spp-dynamic-macro-symbol-obarray nil
  "Table of macro keywords found during lexical analysis.
This table is then used by the macro during the lexical analysis
step.")
(make-variable-buffer-local 'semantic-lex-spp-dynamic-macro-symbol-obarray)

;;; MACRO TABLE UTILS
;;
(defsubst semantic-lex-spp-symbol (name)
  "Return spp symbol with NAME or nil if not found."
  (and
   (stringp name)
   (or
    ;; DYNAMIC
    (and (arrayp semantic-lex-spp-dynamic-macro-symbol-obarray)
	 (intern-soft name semantic-lex-spp-dynamic-macro-symbol-obarray))
    ;; PROJECT
    (and (arrayp semantic-lex-spp-project-macro-symbol-obarray)
	 (intern-soft name semantic-lex-spp-project-macro-symbol-obarray))
    ;; SYSTEM
    (and (arrayp semantic-lex-spp-macro-symbol-obarray)
	 (intern-soft name semantic-lex-spp-macro-symbol-obarray))
    ;; ...
    )))

(defsubst semantic-lex-spp-symbol-p (name)
  "Return non-nil if a keyword with NAME exists in any keyword table."
  (if (semantic-lex-spp-symbol name)
      t))

(defsubst semantic-lex-spp-dynamic-map ()
  "Return the dynamic macro map for the current buffer."
  (or semantic-lex-spp-dynamic-macro-symbol-obarray
      (setq semantic-lex-spp-dynamic-macro-symbol-obarray
	    (make-vector 13 0))))

(defun semantic-lex-spp-symbol-set (name value &optional obarray)
  "Set value of spp symbol with NAME to VALUE and return VALUE.
If optional OBARRAY is non-nil, then use that obarray instead of
the dynamic map."
  (if (and (stringp value) (string= value "")) (setq value nil))
  (set (intern name (or obarray
			(semantic-lex-spp-dynamic-map)))
       value))

(defsubst semantic-lex-spp-symbol-remove (name &optional obarray)
  "Remove the spp symbol with NAME.
If optional OBARRAY is non-nil, then use that obarray instead of
the dynamic map."
  (unintern name (or obarray
		     (semantic-lex-spp-dynamic-map))))

(defsubst semantic-lex-spp-symbol-stream (name)
  "Return replacement stream of macro with NAME."
  (let ((spp (semantic-lex-spp-symbol name)))
    (if spp
        (symbol-value spp))))

(defun semantic-lex-make-spp-table (specs)
  "Convert spp macro list SPECS into an obarray and return it.
SPECS must be a list of (NAME . REPLACEMENT) elements, where:

NAME is the name of the spp macro symbol to define.
REPLACEMENT a string that would be substituted in for NAME."

  ;; Create the symbol hash table
  (let ((semantic-lex-spp-macro-symbol-obarray (make-vector 13 0))
        spec)
    ;; fill it with stuff
    (while specs
      (setq spec  (car specs)
            specs (cdr specs))
      (semantic-lex-spp-symbol-set
       (car spec) 
       (cdr spec)
       semantic-lex-spp-macro-symbol-obarray))
    semantic-lex-spp-macro-symbol-obarray))

(defun semantic-lex-spp-save-table ()
  "Return a list of spp macros and values.
The return list is meant to be saved in a semanticdb table."
  (let (macros)
    (when (arrayp semantic-lex-spp-dynamic-macro-symbol-obarray)
      (mapatoms
       #'(lambda (symbol)
	   (setq macros (cons (cons (symbol-name symbol)
				    (symbol-value symbol))
			      macros)))
       semantic-lex-spp-dynamic-macro-symbol-obarray))
    macros))

(defun semantic-lex-spp-macros ()
  "Return a list of spp macros as Lisp symbols.
The value of each symbol is the replacement stream."
  (let (macros)
    (when (arrayp semantic-lex-spp-macro-symbol-obarray)
      (mapatoms
       #'(lambda (symbol)
	   (setq macros (cons symbol macros)))
       semantic-lex-spp-macro-symbol-obarray))
    (when (arrayp semantic-lex-spp-project-macro-symbol-obarray)
      (mapatoms
       #'(lambda (symbol)
	   (setq macros (cons symbol macros)))
       semantic-lex-spp-project-macro-symbol-obarray))
    (when (arrayp semantic-lex-spp-dynamic-macro-symbol-obarray)
      (mapatoms
       #'(lambda (symbol)
	   (setq macros (cons symbol macros)))
       semantic-lex-spp-dynamic-macro-symbol-obarray))
    macros))

(defun semantic-lex-spp-set-dynamic-table (new-entries)
  "Set the dynamic symbol table to NEW-ENTRIES.
Fore use with semanticdb restoration of state."
  (dolist (e new-entries)
    ;; Default obarray for below is the dynamic map.
    (semantic-lex-spp-symbol-set (car e) (cdr e))))

(defun semantic-lex-spp-reset-dynamic-table ()
  "Reset the dynamic spp symbol table.
This should be done before any new parsing step."
  (setq semantic-lex-spp-dynamic-macro-symbol-obarray nil))

(defun semantic-lex-spp-reset-hook (start end)
  "Reset anything needed by SPP for parsing.
In this case, reset the dynamic macro symbol table if
START recons the entire buffer.
END is not used."
  (if (= start (point-min))
      (setq semantic-lex-spp-dynamic-macro-symbol-obarray nil))
  )

;;; MACRO EXPANSION PARSING
;;
(defun semantic-lex-spp-extract-regex-and-compare (analyzer value)
  "Extract a regexp from an ANALYZER and use to match VALUE.
Return non-nil if it matches"
  (let* ((condition (car analyzer))
	 (regex (cond ((eq (car condition) 'looking-at)
		       (nth 1 condition))
		      (t
		       nil))))
    (when regex
      (string-match regex value))
    ))

(defun semantic-lex-spp-macro-with-args (val)
  "If the macro value VAL has an arglist, return the arglist."
  (when (and val (consp val) (consp (car val))
	     (eq 'spp-arg-list (car (car val))))
    (car (cdr (car val)))))

(defun semantic-lex-spp-macro-to-macro-stream (val beg end argvalues)
  "Convert lexical macro contents VAL into a macro expansion stream.
Argument VAL is the value of some macro to be converted into a stream.
BEG and END are the token bounds of the macro to be expanded
that will somehow gain a much longer token stream.
ARGVALUES are values for any arg list, or nil."
  (cond
   ;; If val is nil, then just skip it.
   ((null val)
    nil)
   ;; If it is a token, then return that token rebuilt.
   ((and (consp val) (car val) (symbolp (car val)))
    (semantic-lex-push-token
     (semantic-lex-token (car val) beg end (semantic-lex-token-text val)))
    )
   ;; If it is a list of tokens, then push each token one at a time.
   ((and (consp val) (consp (car val)) (car (car val))
	 (symbolp (car (car val))))
    (let ((arglist (semantic-lex-spp-macro-with-args val))
	  (argalist nil)
	  )
      ;; Skip the arg list.
      (when arglist (setq val (cdr val)))

      ;; Push args into the replacement list.
      (dolist (A arglist)
	(semantic-lex-spp-symbol-set A (car argvalues))
	(setq argalist (cons (cons A (car argvalues)) argalist))
	(setq argvalues (cdr argvalues)))

      ;; Push everything else onto the list.
      (dolist (v val)
	(let ((txt (car (cdr v)))
	      (sym nil)
	      )
	  (cond
	   ((and (eq (car v) 'symbol) (setq sym (semantic-lex-spp-symbol txt)))
	    ;; Special arg symbol
	    (semantic-lex-spp-macro-to-macro-stream
	     (symbol-value sym)
	     beg end nil)
	    )
	   ((eq (car v) 'semantic-list)
	    ;; Push our arg list onto the semantic list.
	    (when argalist
	      (setq txt (concat txt))
	      (put-text-property 0 1 'macros argalist txt))
	    (semantic-lex-push-token
	     (semantic-lex-token (car v) beg end txt))
	    )
	   (t
	    ;; Nothing new.
	    (semantic-lex-push-token
	     (semantic-lex-token (car v) beg end txt))
	    )
	   )))
      
      (dolist (A arglist)
	(semantic-lex-spp-symbol-remove A))

      ))
   ;; We perform a replacement.  Technically, this should
   ;; be a full lexical step over the "val" string, but take
   ;; a guess that its just a keyword or existing symbol.
   ;;
   ;; Probably a really bad idea.  See how it goes.
   ((semantic-lex-spp-extract-regex-and-compare
     semantic-lex-symbol-or-keyword val)
    (semantic-lex-push-token
     (semantic-lex-token (or (semantic-lex-keyword-p val) 'symbol)
			 beg end
			 val)))

   ;; Ok, the rest of these are various types of syntax.
   ;; This is a poor solution.  We should really have some sort of
   ;; stream merging.
   ((semantic-lex-spp-extract-regex-and-compare
     semantic-lex-punctuation val)
    (semantic-lex-token 'punctuation beg end val))
   ((semantic-lex-spp-extract-regex-and-compare
     semantic-lex-number val)
    (semantic-lex-token 'number beg end val))
   ((semantic-lex-spp-extract-regex-and-compare
     semantic-lex-paren-or-list val)
    (semantic-lex-token 'semantic-list beg end val))
   ((semantic-lex-spp-extract-regex-and-compare
     semantic-lex-string val)
    (semantic-lex-token 'string beg end val))
   (t nil)
   ))


;;; MACRO TABLE DEBUG
;;
(defun semantic-lex-spp-describe (&optional buffer)
  "Describe the current list of spp macros for BUFFER.
If BUFFER is not provided, use the current buffer."
  (interactive)
  (let ((syms (save-excursion
		(if buffer (set-buffer buffer))
		(semantic-lex-spp-macros)))
	(sym nil))
    (with-output-to-temp-buffer "*SPP MACROS*"
      (princ "Macro\t\tValue\n")
      (while syms
	(setq sym (car syms)
	      syms (cdr syms))
	(princ (symbol-name sym))
	(princ "\t")
	(if (< (length (symbol-name sym)) 8)
	    (princ "\t"))
	(prin1 (symbol-value sym))
	(princ "\n")
	))))


;;; Analyzers
;;
(defun semantic-lex-spp-anlyzer-do-replace (sym val beg end)
  "Do the lexical replacement for SYM with VAL.
Argument BEG and END specify the bounds of SYM in the buffer."
  (if (not val)
      (setq semantic-lex-end-point end)
    (let ((arg-in nil)
	  (arg-parsed nil)
	  (arg-split nil)
	  (val-rest val))

      ;; Check for arguments.
      (setq arg-in (semantic-lex-spp-macro-with-args val))

      (when arg-in
	(let ((new-end nil))
	  (setq val-rest (cdr val))
	  (save-excursion
	    (goto-char end)
	    (setq arg-parsed
		  (semantic-lex-spp-one-token-and-move-for-macro
		   (point-at-eol)))
	    (setq end (semantic-lex-token-end arg-parsed))

	    (when (and (listp arg-parsed) (eq (car arg-parsed) 'semantic-list))
	      (setq arg-split
		    ;; Use lex to split up the contents of the argument list.
		    (semantic-lex-spp-stream-for-arglist arg-parsed)
		    ))
	    )))

      ;; if we have something to sub in, then do it.
      (semantic-lex-spp-macro-to-macro-stream val beg end arg-split)
      (setq semantic-lex-end-point end)
      )
    ))

(defvar semantic-lex-spp-replacements-enabled t
  "Non-nil means do replacements when finding keywords.
Disable this only to prevent recursive expansion issues.")

(defun semantix-lex-spp-analyzer-push-tokens-for-symbol (str beg end)
  "Push lexical tokens for the symbol or keyword STR.
STR occurs in the current buffer between BEG and END."
  (let (sym val)
    (cond
     ;;
     ;; It is a macro.  Prepare for a replacement.
     ((and semantic-lex-spp-replacements-enabled
	   (semantic-lex-spp-symbol-p str))
      (setq sym (semantic-lex-spp-symbol str)
	    val (symbol-value sym))
      (semantic-lex-spp-anlyzer-do-replace sym val beg end))
     ;;
     ;; A macro that needs a raw replacement, not a full textual replacemnet.
     ((and (not semantic-lex-spp-replacements-enabled)
	   (semantic-lex-spp-symbol-p str)
	   ;; Get sym values
	   (setq sym (semantic-lex-spp-symbol str))
	   (setq val (symbol-value sym))
	   ;; Is it a list of lex tags?
	   (consp val) (consp (car val))
	   )
      ;; We are inside a stream for assignment into another lexical
      ;; symbol.  Push a magic keyword.
      ;;(message "push %S" val)
      (semantic-lex-push-token
       (semantic-lex-token 'spp-replace-replace start end val))
      )
     ;; Anything else.
     (t
      ;; A regular keyword.
      (semantic-lex-push-token
       (semantic-lex-token (or (semantic-lex-keyword-p str) 'symbol)
			   beg end))))
    ))

(define-lex-regex-analyzer semantic-lex-spp-replace-or-symbol-or-keyword
  "Like 'semantic-lex-symbol-or-keyword' plus preprocessor macro replacement."
  "\\(\\sw\\|\\s_\\)+"
  (let ((str (match-string 0))
	(beg (match-beginning 0))
	(end (match-end 0)))
    (semantix-lex-spp-analyzer-push-tokens-for-symbol str beg end)))

(defun semantic-lex-spp-first-token-arg-list (token)
  "If TOKEN is a semantic-list, turn it into a an SPP ARG LIST."
  (when (and (consp token)
	     (symbolp (car token))
	     (eq 'semantic-list (car token)))
    ;; Convert TOKEN in place.
    (let ((argsplit (split-string (semantic-lex-token-text token)
				  "[(), ]" t)))
      (setcar token 'spp-arg-list)
      (setcar (nthcdr 1 token) argsplit))
    ))

(defun semantic-lex-spp-one-token-and-move-for-macro (max)
  "Lex up one token, and move to end of that token.
Don't go past MAX."
  (let ((ans (semantic-lex (point) max 0 0)))
    (if (not ans)
	(progn (goto-char max)
	       nil)
      (goto-char (semantic-lex-token-end (car ans)))
      (car ans))
    ))

(defun semantic-lex-spp-stream-for-arglist (token)
  "Lex up the contents of the arglist TOKEN.
Parsing starts inside the parens, and ends at the end of TOKEN."
  (save-excursion
    (let ((end (semantic-lex-token-end token))
	  (fresh-toks nil)
	  (toks nil))
      (goto-char (semantic-lex-token-start token))
      ;; A cheat for going into the semantic list.
      (forward-char 1)
      (setq fresh-toks (semantic-lex-spp-stream-for-macro (1- end)))
      (dolist (tok fresh-toks)
	(when (memq (semantic-lex-token-class tok) '(symbol semantic-list))
	  (setq toks (cons tok toks))))
      (nreverse toks))
    ))

(defun semantic-lex-spp-stream-for-macro (eos)
  "Lex up a stream of tokens for a #define statement.
Parsing starts at the current point location.
EOS is the end of the stream to lex for this macro."
  (let ((stream nil))
    (while (< (point) eos)
      (let* ((tok (semantic-lex-spp-one-token-and-move-for-macro eos))
	     (str (when tok
		    (cond
		     ((eq (semantic-lex-token-class tok) 'spp-replace-replace)
		      (car (cdr tok))
		      )
		     (t
		      (semantic-lex-token-text tok)))))
	     )
	(if str
	    (push (semantic-lex-token (semantic-lex-token-class tok)
				      (semantic-lex-token-start tok)
				      (semantic-lex-token-end tok)
				      str)
		  stream)
	  ;; Nothing to push.
	  nil)))
    (goto-char eos)
    ;; Fix the order
    (nreverse stream)
    ))

(defmacro define-lex-spp-macro-declaration-analyzer (name doc regexp tokidx
							  &rest valform)
  "Define a lexical analyzer for defining new MACROS.
NAME is the name of the analyzer.
DOC is the documentation for the analyzer.
REGEXP is a regular expression for the analyzer to match.
See `define-lex-regex-analyzer' for more on regexp.
TOKIDX is an index into REGEXP for which a new lexical token
of type `spp-macro-def' is to be created.
VALFORM are forms that return the value to be saved for this macro, or nil.
When implementing a macro, you can use `semantic-lex-spp-stream-for-macro'
to convert text into a lexical stream for storage in the macro."
  (let ((start (make-symbol "start"))
	(end (make-symbol "end"))
	(val (make-symbol "val"))
	(startpnt (make-symbol "startpnt"))
	(endpnt (make-symbol "endpnt")))
    `(define-lex-regex-analyzer ,name
       ,doc
       ,regexp
       (let ((,start (match-beginning ,tokidx))
	     (,end (match-end ,tokidx))
	     (,startpnt semantic-lex-end-point)
	     (,val (save-match-data ,@valform))
	     (,endpnt semantic-lex-end-point))
	 (semantic-lex-spp-symbol-set
	  (buffer-substring-no-properties ,start ,end)
	  ,val)
	 (semantic-lex-push-token
	  (semantic-lex-token 'spp-macro-def
			      ,start ,end))
	 ;; Preserve setting of the end point from the calling macro.
	 (when (and (/= ,startpnt ,endpnt)
		    (/= ,endpnt semantic-lex-end-point))
	   (setq semantic-lex-end-point ,endpnt))
	 ))))

(defmacro define-lex-spp-macro-undeclaration-analyzer (name doc regexp tokidx)
  "Undefine a lexical analyzer for defining new MACROS.
NAME is the name of the analyzer.
DOC is the documentation for the analyzer.
REGEXP is a regular expression for the analyzer to match.
See `define-lex-regex-analyzer' for more on regexp.
TOKIDX is an index into REGEXP for which a new lexical token
of type `spp-macro-undef' is to be created."
  (let ((start (make-symbol "start"))
	(end (make-symbol "end")))
    `(define-lex-regex-analyzer ,name
       ,doc
       ,regexp
       (let ((,start (match-beginning ,tokidx))
	     (,end (match-end ,tokidx))
	     )
	 (semantic-lex-spp-symbol-remove
	  (buffer-substring-no-properties ,start ,end))
	 (semantic-lex-push-token
	  (semantic-lex-token 'spp-macro-undef
			      ,start ,end))
	 ))))

;;; INCLUDEs
;;
;; Bringing pre-processor macros in from included headers.
(defcustom semantic-lex-spp-use-headers-flag nil
  "*Non-nil means to pre-parse headers as we go.
For languages that use the Semantic pre-processor, this can
improve the accuracy of parsed files where include files
can change the state of what's parsed in the current file."
  :group 'semantic
  :type 'boolean)

(defun semantic-lex-spp-merge-header (name)
  "Extract and merge any macros from the header with NAME.
Finds the header file belonging to NAME, gets the macros
from that file, and then merge the macros with our current
symbol table."
  (when semantic-lex-spp-use-headers-flag
    ;; @todo - do this someday, ok?
    ))

(defmacro define-lex-spp-include-analyzer (name doc regexp tokidx
						&rest valform)
  "Define a lexical analyzer for defining a new INCLUDE lexical token.
Macros defined in the found include will be added to our running table
at the time the include statement is found.
NAME is the name of the analyzer.
DOC is the documentation for the analyzer.
REGEXP is a regular expression for the analyzer to match.
See `define-lex-regex-analyzer' for more on regexp.
TOKIDX is an index into REGEXP for which a new lexical token
of type `spp-macro-include' is to be created.
VALFORM are forms that return the name of the thing being included, and the
type of include.  The return value should be of the form:
  (NAME . TYPE)
where NAME is the name of the include, and TYPE is the type of the include,
where a valid symbol is 'system, or nil."
  (let ((start (make-symbol "start"))
	(end (make-symbol "end"))
	(val (make-symbol "val"))
	(startpnt (make-symbol "startpnt"))
	(endpnt (make-symbol "endpnt")))
    `(define-lex-regex-analyzer ,name
       ,doc
       ,regexp
       (let ((,start (match-beginning ,tokidx))
	     (,end (match-end ,tokidx))
	     (,startpnt semantic-lex-end-point)
	     (,val (save-match-data ,@valform))
	     (,endpnt semantic-lex-end-point))
	 ;;(message "(car ,val) -> %S" (car ,val))
	 (semantic-lex-spp-merge-header (car ,val))
	 (semantic-lex-push-token
	  (semantic-lex-token (if (eq (cdr ,val) 'system)
				  'spp-system-include
				'spp-include)
			      ,start ,end
			      (car ,val)))
	 ;; Preserve setting of the end point from the calling macro.
	 (when (and (/= ,startpnt ,endpnt)
		    (/= ,endpnt semantic-lex-end-point))
	   (setq semantic-lex-end-point ,endpnt))
	 ))))

;;; EDEBUG Handlers
;;
(add-hook
 'edebug-setup-hook
 #'(lambda ()
     
     (def-edebug-spec define-lex-spp-macro-declaration-analyzer
       (&define name stringp stringp form def-body)
       )

     (def-edebug-spec define-lex-spp-macro-undeclaration-analyzer
       (&define name stringp stringp form)
       )

     (def-edebug-spec define-lex-spp-include-analyzer
       (&define name stringp stringp form def-body)
       )
     ))

  
(provide 'semantic-lex-spp)

;;; semantic-lex-spp.el ends here
