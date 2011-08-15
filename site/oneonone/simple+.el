;;; simple+.el --- Extensions to standard library `simple.el'.
;;
;; Filename: simple+.el
;; Description: Extensions to standard library `simple.el'.
;; Author: Drew Adams
;; Maintainer: Drew Adams
;; Copyright (C) 1996-2007, Drew Adams, all rights reserved.
;; Created: Fri Apr 12 10:56:45 1996
;; Version: 21.0
;; Last-Updated: Fri Jan 19 21:25:46 2007 (-28800 Pacific Standard Time)
;;           By: dradams
;;     Update #: 354
;; URL: http://www.emacswiki.org/cgi-bin/wiki/simple+.el
;; Keywords: internal, lisp, extensions, abbrev
;; Compatibility: GNU Emacs 20.x, GNU Emacs 21.x, GNU Emacs 22.x
;;
;; Features that might be required by this library:
;;
;;   `misc-fns', `strings', `thingatpt', `thingatpt+'.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;;    Extensions to standard library `simple.el'.
;;
;;  See also library `icicles.el' for extensions to `simple.el' that
;;  concern input completion in the minibuffer.  Those extensions used
;;  to be in this library, but they are used by `icicles.el', so they
;;  have been moved there.
;;
;;
;;  Variables defined here:
;;
;;    `set-any-variable-value-history'.
;;
;;  Functions defined here:
;;
;;    `read-var-and-value', `set-any-variable'.
;;
;;
;;  ***** NOTE: The following functions defined in `simple.el' have
;;              been REDEFINED HERE:
;;
;;    `edit-and-eval-command' - Bug fix for < Emacs 21: Adds COMMAND
;;                              as a command to `command-history', not
;;                              as a string.
;;
;;  ***** NOTE: This EMACS PRIMITIVE has been REDEFINED HERE:
;;
;;    `set-variable' - Uses `read-var-and-value' to get args.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Change log:
;;
;; 2006/09/15 dadams
;;     set-variable: Protect custom-variable-p if not defined.
;; 2006/08/03 dadams
;;     No longer initialize kill-ring to ("") - no longer needed, and can cause problems.
;; 2005-10-31 dadams
;;     Added: set-any-variable-value-history, read-var-and-value, set*-variable.
;; 2005/10/03 dadams
;;     Removed require of icomplete+.el (no longer redefines read-from-minibuffer).
;; 2005/07/28 dadams
;;     Moved all completion stuff to icicles.el:
;;       choose-completion-string, completion-setup-function, switch-to-completions.
;;     No longer require icicles.el.
;;     Removed: completion-reference-buffer.
;;     completion-setup-function:
;;       Renamed icicle-completing-read-prompt-suffix to icicle-prompt-suffix.
;; 2005/07/15 dadams
;;     choose-completion-string, completion-setup-function: Updated for Emacs 21+.
;; 2005/07/10 dadams
;;     Renamed: command-calling-for-completion -> icicle-cmd-calling-for-completion.
;; 2004/09/21 dadams
;;     Only redefine edit-and-eval-command & choose-completion-string if prior to Emacs 21.
;; 1999/03/17 dadams
;;     1. choose-completion-string: Added doc string.  Updated to correspond to
;;        Emacs 34.1 version.
;;     2. completion-setup-function: diff prompt setups.  face1 & face2 tests.
;;     3. Added: switch-to-completions.
;; 1996/06/14 dadams
;;     kill-ring: Bug fix: `mouse-save-then-kill' expects a consp, so ensure this.
;; 1996/04/26 dadams
;;     Put escaped newlines on long-line strings.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Code:

;; Cannot do (require 'simple), because `simple.el' does no `provide'.
;; Don't want to do a (load-library "simple") either, because it wouldn't
;; allow doing (eval-after-load "simple" '(require 'simple+))

(and (< emacs-major-version 21)
     (eval-when-compile (require 'cl))) ;; push, pop (plus, for Emacs <20: when, unless)

(require 'strings) ;; read-any-variable

;;;;;;;;;;;;;;;;;;;;




;; REPLACES ORIGINAL in `simple.el':
;; Original was bugged: it added COMMAND as a string to
;; `command-history'.  This version adds it as a command.
;; This was fixed in Emacs 21.
(when (< emacs-major-version 21)
  (defun edit-and-eval-command (prompt command)
    "Prompting with PROMPT, let user edit COMMAND and eval result.
COMMAND is a Lisp expression.  Let user edit that expression in
the minibuffer, then read and evaluate the result."
    (let* ((minibuffer-history-sexp-flag t)
           (command (read-from-minibuffer prompt (prin1-to-string command)
                                          read-expression-map t
                                          '(command-history . 1))))
      ;; If command was added to `command-history' as a string,
      ;; get rid of that.  We want only evaluable expressions there.
      (when (stringp (car command-history)) (pop command-history))
      ;; If command to be redone does not match front of `command-history',
      ;; add it to `command-history'.
      (unless (equal command (car command-history))
        (push command command-history))
      (eval command))))


(defvar set-any-variable-value-history nil
  "History of values entered with `set-any-variable'.")


;; Helper function for `set*-variable'.
;; Inspired from original `set-variable', with these changes:
;; Use READ-VAR-FN and SET-VAR-HIST-VAR.  Use current value as default value.
;;
(defun read-var-and-value (read-var-fn set-var-hist-var make-local-p)
  "Read a variable name and value.
READ-VAR-FN is a function to read the variable name.
SET-VAR-HIST-VAR is a variable holding a history of variable values.
MAKE-LOCAL-P non-nil means the variable is to be local."
  (let* ((var (funcall read-var-fn "Set variable: "))
         (current-val (format "%S" (symbol-value var)))
         (minibuffer-help-form '(describe-variable var))
         (prompt (format "Set %s%s to value: " var
                         (cond ((local-variable-p var) " (buffer-local)")
                               ((or make-local-p (local-variable-if-set-p var))
                                " buffer-locally")
                               (t " globally"))))
         (prop (get var 'variable-interactive))
         (val (if prop
                  ;; Use VAR's `variable-interactive' property
                  ;; as an interactive spec for prompting.
                  (call-interactively `(lambda (arg) (interactive ,prop) arg))
                (read (read-string prompt current-val set-var-hist-var current-val)))))
    (list var val make-local-p)))



;; REPLACES ORIGINAL (built-in):
;; Uses `read-var-and-value' to get args interactively.
;;
;;;###autoload
(defun set-variable (var val &optional make-local)
  "Set VARIABLE to VALUE.  VALUE is a Lisp object.
When using this interactively, enter a Lisp object for VALUE.
If you want VALUE to be a string, you must surround it with doublequotes.
VALUE is used literally, not evaluated.

If VARIABLE has a `variable-interactive' property, that is used as if
it were the arg to `interactive' (which see) to interactively read VALUE.

If VARIABLE has been defined with `defcustom', then the type information
in the definition is used to check that VALUE is valid.

With a prefix argument, set VARIABLE to VALUE buffer-locally."
  (interactive (read-var-and-value 'read-variable
                                   'set-variable-value-history
                                   current-prefix-arg))
  (and (or (not (fboundp 'custom-variable-p)) (custom-variable-p var))
       (not (get var 'custom-type))
       (custom-load-symbol var))
  (let ((type (get var 'custom-type)))
    (when type
      ;; Match with custom type.
      (require 'cus-edit)
      (setq type (widget-convert type))
      (unless (widget-apply type :match val)
	(error "Value `%S' does not match type %S of %S"
	       val (car type) var))))
  (if make-local
      (make-local-variable var))
  (set var val)
  ;; Force a thorough redisplay for the case that the variable
  ;; has an effect on the display, like `tab-width' has.
  (force-mode-line-update))

(defun set-any-variable (variable value &optional make-local)
  "Set VARIABLE to VALUE.  VALUE is a Lisp object.
VARIABLE can be any Lisp variable, unlike `set-variable', where it
must be a user option.  Enter VALUE in Lisp syntax.  For example, if
you want VALUE to be a string, you must surround it with doublequotes.
VALUE is used literally, not evaluated.

If VARIABLE has a `variable-interactive' property, that is used as if
it were the arg to `interactive' (which see) to interactively read value.

If VARIABLE has been defined with `defcustom', then the type information
in the definition is used to check that VALUE is valid.

With a prefix argument, set VARIABLE to VALUE buffer-locally."
  (interactive (read-var-and-value 'read-any-variable
                                   'set-any-variable-value-history
                                   current-prefix-arg))
  (and (custom-variable-p variable)
       (not (get variable 'custom-type))
       (custom-load-symbol variable))
  (let ((type (get variable 'custom-type)))
    (when type
      ;; Match with custom type.
      (require 'cus-edit)
      (setq type (widget-convert type))
      (unless (widget-apply type :match value)
	(error "Value `%S' does not match type %S of %S"
	       value (car type) variable))))
  (if make-local (make-local-variable variable))
  (set variable value)
  ;; Force a thorough redisplay for the case that the variable
  ;; has an effect on the display, like `tab-width' has.
  (force-mode-line-update))


;; REPLACES ORIGINAL in `simple.el':
;; Just updates 20.3 with version from 20.6.1: corrects deletion of multiple.
(when (string< emacs-version "20.6.1")
  (defun comment-region (beg end &optional arg)
    "Comment or uncomment each line in the region.
With just C-u prefix arg, uncomment each line in region.
Numeric prefix arg ARG means use ARG comment characters.
If ARG is negative, delete that many comment characters instead.
Comments are terminated on each line, even for syntax in which newline does
not end the comment.  Blank lines do not get comments."
    ;; if someone wants it to only put a comment-start at the beginning and
    ;; comment-end at the end then typing it, C-x C-x, closing it, C-x C-x
    ;; is easy enough.  No option is made here for other than commenting
    ;; every line.
    (interactive "r\nP")
    (or comment-start (error "No comment syntax is defined"))
    (if (> beg end) (let (mid) (setq mid beg beg end end mid)))
    (save-excursion
      (save-restriction
        (let ((cs comment-start) (ce comment-end)
              (cp (when comment-padding
                    (make-string comment-padding ? )))
              numarg)
          (if (consp arg) (setq numarg t)
            (setq numarg (prefix-numeric-value arg))
            ;; For positive arg > 1, replicate the comment delims now,
            ;; then insert the replicated strings just once.
            (while (> numarg 1)
              (setq cs (concat cs comment-start)
                    ce (concat ce comment-end))
              (setq numarg (1- numarg))))
          ;; Loop over all lines from BEG to END.
          (narrow-to-region beg end)
          (goto-char beg)
          (if (or (eq numarg t) (< numarg 0))
              (while (not (eobp))
                (let (found-comment)
                  ;; Delete comment start from beginning of line.
                  (if (eq numarg t)
                      (while (looking-at (regexp-quote cs))
                        (setq found-comment t)
                        (delete-char (length cs)))
                    (let ((count numarg))
                      (while (and (> 1 (setq count (1+ count)))
                                  (looking-at (regexp-quote cs)))
                        (setq found-comment t)
                        (delete-char (length cs)))))
                  ;; Delete comment padding from beginning of line
                  (when (and found-comment comment-padding
                             (looking-at (regexp-quote cp)))
                    (delete-char comment-padding))
                  ;; Delete comment end from end of line.
                  (if (string= "" ce)
                      nil
                    (if (eq numarg t)
                        (progn
                          (end-of-line)
                          ;; This is questionable if comment-end ends in
                          ;; whitespace.  That is pretty brain-damaged,
                          ;; though.
                          (while (progn (skip-chars-backward " \t")
                                        (and (>= (- (point) (point-min)) (length ce))
                                             (save-excursion
                                               (backward-char (length ce))
                                               (looking-at (regexp-quote ce)))))
                            (delete-char (- (length ce)))))
                      (let ((count numarg))
                        (while (> 1 (setq count (1+ count)))
                          (end-of-line)
                          ;; this is questionable if comment-end ends in whitespace
                          ;; that is pretty brain-damaged though
                          (skip-chars-backward " \t")
                          (if (>= (- (point) (point-min)) (length ce))
                              (save-excursion
                                (backward-char (length ce))
                                (if (looking-at (regexp-quote ce))
                                    (delete-char (length ce)))))))))
                  (forward-line 1)))

            (when comment-padding
              (setq cs (concat cs cp)))
            (while (not (eobp))
              ;; Insert at beginning and at end.
              (if (looking-at "[ \t]*$") ()
                (insert cs)
                (if (string= "" ce) ()
                  (end-of-line)
                  (insert ce)))
              (search-forward "\n" nil 'move)))))))
  )

;;;;;;;;;;;;;;;;;;;;;;;

(provide 'simple+)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; simple+.el ends here
