;;; csde-csharp-font-lock.el -- Extra level font locking for csharp

;; Adapted from the JDE by Matt Bruce <matt.bruce@morganstanley.com>
;; Maintainer:  Matt Bruce

;; Copyright (C) 2001 by Matt Bruce

;; JDE version is Copyright (C) 1998, 1999, 2000, 2001 by David Ponce

;; JDE Author: David Ponce <david@dponce.com>
;; JDE Maintainer: David Ponce <david@dponce.com>
;;             Paul Kinnucan <paulk@mathworks.com>

;; Keywords: csharp, tools
;; VC: $Id: csde-csharp-font-lock.el,v 1.1 2001/02/12 05:45:05 paulk Exp $

;; This file is not part of Emacs

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:
;;
;; Adds some extra level font locking for csharp in `csde-mode'.
;;
;; * Numbers are fontified with `csde-csharp-font-lock-number-face'.
;;
;; * Modifiers are fontified with `font-lock-builtin-face'.  This face
;;   is based on XEmacs `font-lock-preprocessor-face' if available.
;;
;; * Keywords const and goto are fontified with
;;   `font-lock-warning-face'.  These keywords are reserved, even
;;   though they are not currently used.
;;
;; * The keyword default is fontified with `font-lock-keyword-face'.
;;
;; * User's defined identifiers (see variable
;;   `csde-csharp-font-lock-api-file') are fontified with
;;   `csde-csharp-font-lock-api-face'.
;;
;; * Capitalized identifiers, text between `' in comments and csharpdoc
;;   tags (including non official csharpdoc tags) are fontified with
;;   `font-lock-constant-face'.  This face is based on XEmacs
;;   `font-lock-reference-face' if available.
;;
;; * Csharpdoc links (following @link tags or enclosed in HTML <a> tags)
;;   are fontified with `csde-csharp-font-lock-link-face'
;;
;; * Csharpdoc code samples (enclosed in HTML <code> tags or following
;;   @see tags) are fontified with `csde-csharp-font-lock-code-face'.  By
;;   default, this face is based on `font-lock-builtin-face'.
;;  
;; * Csharpdoc HTML bold style is fontified with
;;   `csde-csharp-font-lock-bold-face'.  By default, this face is based
;;   on `bold'.
;;
;; * Csharpdoc HTML italic style is fontified with
;;   `csde-csharp-font-lock-italic-face'.  By default, this face is based
;;   on `italic'.
;;
;; * Csharpdoc HTML underlined style is fontified with
;;   `csde-csharp-font-lock-underline-face'.  By default, this face is
;;   based on `underline'.
;;
;; * Csharpdoc HTML preformatted style is fontified with
;;   `csde-csharp-font-lock-pre-face'.  By default, this face is based on
;;   `default'.
;;
;; All font-lock and csde-csharp-font-lock faces are individually
;; customizable.

;; This code has been tested with FSF Emacs 20.7, 21.0 and XEmacs
;; 21.1.  Any comments, suggestions, bug reports or upgrade requests
;; are welcome.  Please send them to the maintainers.

;; The latest version of the CSDE is available at
;; <URL:http://www.sourceforge.com/>.

;; Please send any comments, bugs, or upgrade requests to
;; Matt Bruce (matt.bruce@morganstanley.com)

;;; History:
;;
;; See at end of this file.

;;; Code:

(defcustom csde-use-font-lock t
  "*Turn on font-locking if non-nil.
Set to nil to disable the use of font-locking."
  :group 'csde-project
  :type 'boolean)

;;;;
;;;; Define the faces
;;;;

;; Create a specific face for numbers
(defface csde-csharp-font-lock-number-face
  '((((class grayscale) (background light)) (:foreground "DimGray" :italic t))
    (((class grayscale) (background dark)) (:foreground "LightGray" :italic t))
    (((class color) (background light)) (:foreground "RosyBrown"))
    (((class color) (background dark)) (:foreground "LightSalmon"))
    (t (:italic t)))
  "Font Lock mode face used to highlight numbers."
  :group 'font-lock-highlighting-faces)

;; Create a specific face for user's defined names
(defface csde-csharp-font-lock-api-face
  '((((class grayscale) (background light)) (:foreground "DimGray"))
    (((class grayscale) (background dark)) (:foreground "LightGray"))
    (((class color) (background light)) (:foreground "dark goldenrod"))
    (((class color) (background dark)) (:foreground "light goldenrod")))
  "Font Lock mode face used to highlight user's defined names."
  :group 'font-lock-highlighting-faces)

;; Create a specific face for links
(defface csde-csharp-font-lock-link-face
  '((t (:foreground "blue" :italic nil :underline t)))
  "Font Lock mode face used to highlight links."
  :group 'font-lock-highlighting-faces)

;;; Compatibility
(if csde-xemacsp
    (progn
      
      (defvar font-lock-builtin-face 'font-lock-builtin-face
	"Face name to use for builtins.")

      ;; For consistency try to define the builtin face as the XEmacs
      ;; preprocessor face
      (condition-case nil
          (copy-face 'font-lock-preprocessor-face 'font-lock-builtin-face)
        (error
         (defface font-lock-builtin-face
           '((t (:foreground "blue" :italic nil :underline t)))
           "Font Lock mode face used to highlight builtins."
           :group 'font-lock-highlighting-faces)))

      (defvar font-lock-constant-face 'font-lock-constant-face
        "Face name to use for constant and label names.")
      
      ;; For consistency try to define the constant face as the XEmacs
      ;; reference face
      (condition-case nil
          (copy-face 'font-lock-reference-face 'font-lock-constant-face)
        (error
         (defface font-lock-constant-face
           '((((class grayscale) (background light))
              (:foreground "LightGray" :bold t :underline t))
             (((class grayscale) (background dark))
              (:foreground "Gray50" :bold t :underline t))
             (((class color) (background light)) (:foreground "CadetBlue"))
             (((class color) (background dark)) (:foreground "Aquamarine"))
             (t (:bold t :underline t)))
           "Font Lock mode face used to highlight constants and labels."
           :group 'font-lock-highlighting-faces)))

      ))

;; Make new faces based on existing ones
(copy-face 'bold                   'csde-csharp-font-lock-bold-face)
(copy-face 'italic                 'csde-csharp-font-lock-italic-face)
(copy-face 'underline              'csde-csharp-font-lock-underline-face)
(copy-face 'default                'csde-csharp-font-lock-pre-face)
(copy-face 'font-lock-builtin-face 'csde-csharp-font-lock-code-face)

;; Define the extra font lock faces
(defvar csde-csharp-font-lock-number-face    'csde-csharp-font-lock-number-face
  "Face name to use for numbers.")
(defvar csde-csharp-font-lock-api-face       'csde-csharp-font-lock-api-face
  "Face name to use for user's defined names.")
(defvar csde-csharp-font-lock-link-face      'csde-csharp-font-lock-link-face
  "Face name to use for links.")
(defvar csde-csharp-font-lock-bold-face      'csde-csharp-font-lock-bold-face
  "Face name to use for HTML bold text style.")
(defvar csde-csharp-font-lock-italic-face    'csde-csharp-font-lock-italic-face
  "Face name to use for HTML italic text style.")
(defvar csde-csharp-font-lock-underline-face 'csde-csharp-font-lock-underline-face
  "Face name to use for HTML underlined text style.")
(defvar csde-csharp-font-lock-pre-face       'csde-csharp-font-lock-pre-face
  "Face name to use for HTML preformatted text style.")
(defvar csde-csharp-font-lock-code-face      'csde-csharp-font-lock-code-face
  "Face name to use for HTML program code style.")

;;;;
;;;; Useful constants
;;;;

(defconst csde-csharp-font-lock-capital-letter
  "A-Z\300-\326\330-\337_$"
  "Csharp identifier capital letter.")

(defconst csde-csharp-font-lock-letter
  (concat csde-csharp-font-lock-capital-letter "a-z")
  "Csharp identifier letter.")

(defconst csde-csharp-font-lock-capital-letter-or-digit
  (concat csde-csharp-font-lock-capital-letter "0-9")
  "Csharp identifier capital letter or digit.")

(defconst csde-csharp-font-lock-letter-or-digit
  (concat csde-csharp-font-lock-letter "0-9")
  "Csharp identifier letter or digit.")

;;;;
;;;; Support for fontification inside csharpdocs and comments.
;;;;

;; Define font lock keywords for comments and csharpdocs only
(defun csde-csharp-font-lock-remove-csharpdoc-keywords (keywords)
  "Remove existing csharpdoc font lock keywords from KEYWORDS.
That is those with \"@\" in their matcher regexp."
  (let (kw matcher match)
    (while keywords
      (setq matcher  (car keywords)
            keywords (cdr keywords))
      (if (not (and (consp matcher)
                    (stringp (car matcher))
                    (string-match "@" (car matcher))))
          (setq kw (cons matcher kw))))
    (nreverse kw)))

(defun csde-csharp-font-lock-in-csharpdoc-p ()
  "Return non-nil if point is in a csharpdoc comment."
  (let* ((p (point))
         (in-csharpdoc-p
          (save-match-data
            (and (re-search-backward "^[ \t]*/\\*\\*" nil t)
                 (skip-chars-forward " \t" p)
                 (eq (get-text-property (point) 'face)
                     'font-lock-comment-face)
                 (forward-comment 1)
                 (< p (point))))))
    (goto-char p)
    in-csharpdoc-p))

(defun csde-csharp-font-lock-search-in-comment (regexp end)
  "Search forward from point for regular expression REGEXP.
Ensure matching occurs in a csharp comment.  Buffer position END bounds
the search.  The match found must not extend after that position."
  (let (in-comment-p)
    (while (and (not in-comment-p)
                (re-search-forward regexp end t))
      (setq in-comment-p
            (eq (get-text-property (match-beginning 0) 'face)
                'font-lock-comment-face)))
    in-comment-p))

(defun csde-csharp-font-lock-search-in-csharpdoc (regexp end)
  "Search forward from point for regular expression REGEXP.
Ensure matching occurs in a csharpdoc comment.  Buffer position END
bounds the search.  The match found must not extend after that
position."
  (let (in-csharpdoc-p)
    (while (and (not in-csharpdoc-p)
                (re-search-forward regexp end t))
      (setq in-csharpdoc-p (csde-csharp-font-lock-in-csharpdoc-p)))
    in-csharpdoc-p))

(defun csde-csharp-font-lock-quote-keyword ()
  "Return a font lock keyword for comment enclosed in \`\'."
  `((lambda (end)
      (csde-csharp-font-lock-search-in-comment
       "`\\(.*\\)'"
       end))
    1 font-lock-constant-face t))

(defun csde-csharp-font-lock-html-ahref-keyword ()
  "Return a font lock keyword for HTML A HREF anchor.
Only fontify csharpdoc comments."
  `((lambda (end)
      (csde-csharp-font-lock-search-in-csharpdoc
       "<[Aa]\\s-+[Hh][Rr][Ee][Ff][^>]*>\\([^>]+\\)</[Aa]>"
       end))
    1 csde-csharp-font-lock-link-face t))

(defun csde-csharp-font-lock-html-strong-keyword ()
  "Return a font lock keyword for HTML STRONG style.
Only fontify csharpdoc comments."
  `((lambda (end)
      (csde-csharp-font-lock-search-in-csharpdoc
       "<[Ss][Tt][Rr][Oo][Nn][Gg]>\\([^<]*\\)</[Ss][Tt][Rr][Oo][Nn][Gg]>"
       end))
    1 csde-csharp-font-lock-bold-face t))

(defun csde-csharp-font-lock-html-bold-keyword ()
  "Return a font lock keyword for HTML B style.
Only fontify csharpdoc comments."
  `((lambda (end)
      (csde-csharp-font-lock-search-in-csharpdoc
       "<[Bb]>\\([^<]*\\)</[Bb]>"
       end))
    1 csde-csharp-font-lock-bold-face t))

(defun csde-csharp-font-lock-html-italic-keyword ()
  "Return a font lock keyword for HTML I style.
Only fontify csharpdoc comments."
  `((lambda (end)
      (csde-csharp-font-lock-search-in-csharpdoc
       "<[Ii]>\\([^<]*\\)</[Ii]>"
       end))
    1 csde-csharp-font-lock-italic-face t))

(defun csde-csharp-font-lock-html-underline-keyword ()
  "Return a font lock keyword for HTML U style.
Only fontify csharpdoc comments."
  `((lambda (end)
      (csde-csharp-font-lock-search-in-csharpdoc
       "<[Uu]>\\([^<]*\\)</[Uu]>"
       end))
    1 csde-csharp-font-lock-underline-face t))

(defun csde-csharp-font-lock-html-code-keyword ()
  "Return a font lock keyword for HTML CODE style.
Only fontify csharpdoc comments."
  `((lambda (end)
      (csde-csharp-font-lock-search-in-csharpdoc
       "<[Cc][Oo][Dd][Ee]>\\([^<]*\\)</[Cc][Oo][Dd][Ee]>"
       end))
    1 csde-csharp-font-lock-code-face t))

(defun csde-csharp-font-lock-html-pre-keyword ()
  "Return a font lock keyword for HTML PRE style.
Only fontify csharpdoc comments."
  `((lambda (end)
      (csde-csharp-font-lock-search-in-csharpdoc
       "<[Pp][Rr][Ee]>\\([^<]*\\)</[Pp][Rr][Ee]>"
       end))
    1 csde-csharp-font-lock-pre-face t))

(defun csde-csharp-font-lock-csharpdoc-tag-keyword ()
  "Return a font lock keyword for csharpdoc tags.
Only fontify csharpdoc comments."
  `((lambda (end)
      (csde-csharp-font-lock-search-in-csharpdoc
       ,(concat "^[ \t]*\\(/\\*\\*\\|\\*?\\)[ \t]*"
                "\\(@[" csde-csharp-font-lock-letter-or-digit "]+\\)")
       end))
    2 font-lock-constant-face t))

(defun csde-csharp-font-lock-csharpdoc-docroot-keyword ()
  "Return a font lock keyword for csharpdoc @docRoot tags.
Only fontify csharpdoc comments."
  `((lambda (end)
      (csde-csharp-font-lock-search-in-csharpdoc
       "{\\(@docRoot\\)}"
       end))
    1 font-lock-constant-face t))

(defun csde-csharp-font-lock-csharpdoc-link-keyword ()
  "Return a font lock keyword for csharpdoc @link tags.
Only fontify csharpdoc comments."
  `((lambda (end)
      (csde-csharp-font-lock-search-in-csharpdoc
       "{\\(@link\\)\\>[ \t]+\\([^}]*\\)}"
       end))
    (1 font-lock-constant-face t)
    (2 csde-csharp-font-lock-link-face t)))

(defun csde-csharp-font-lock-csharpdoc-see-ref-keyword ()
  "Return a font lock keyword for csharpdoc @see references.
Only fontify csharpdoc comments."
  `((lambda (end)
      (csde-csharp-font-lock-search-in-csharpdoc
       ,(concat "^[ \t]*\\(/\\*\\*\\|\\*?\\)[ \t]*"
                "@see\\>[ \t]*"
                "\\([.#" csde-csharp-font-lock-letter-or-digit "]+\\)")
       end))
    2 csde-csharp-font-lock-code-face t))

(defun csde-csharp-font-lock-csharpdoc-param-name-keyword ()
  "Return a font lock keyword for csharpdoc @param names.
Only fontify csharpdoc comments."
  `((lambda (end)
      (csde-csharp-font-lock-search-in-csharpdoc
       ,(concat "^[ \t]*\\(/\\*\\*\\|\\*?\\)[ \t]*"
                "@param\\>[ \t]*\\(\\sw+\\)?")
       end))
    2 font-lock-variable-name-face prepend t))

(defun csde-csharp-font-lock-csharpdoc-exception-type-keyword ()
  "Return a font lock keyword for csharpdoc exception types.
Only fontify csharpdoc comments."
  `((lambda (end)
      (csde-csharp-font-lock-search-in-csharpdoc
       ,(concat "^[ \t]*\\(/\\*\\*\\|\\*?\\)[ \t]*"
                "@\\(exception\\|throws\\)\\>[ \t]*\\(\\S-+\\)?")
       end))
    3 font-lock-type-face prepend t))

;;;;
;;;; Support for fontification of user's defined names.
;;;;

(defcustom csde-csharp-font-lock-api-file
  (expand-file-name "~/csde-csharp-font-lock.api")
  "*File which contains a list of user's defined names to fontify.
If nil no name fontification occurs.  Otherwise the specified file must
contain one name by line.  Lines not beginning with a letter are
ignored.  When you change this file or modify its content a new cache
of font lock regular expressions will be rebuilt when restarting
Emacs.  Also, you can manually rebuild the cache and update font lock
keywords by entering the command:

\\[universal-argument] \\[csde-csharp-font-lock-setup-keywords]."
  :group 'csde-project
  :type '(choice :tag "Names"
                 (const :tag "No" nil)
                 (file  :tag "In file" :format "%t\n%v")))

(defcustom csde-csharp-font-lock-api-name-filter nil
  "*Function used to filter a name."
  :group 'csde-project
  :type 'function)

(defconst csde-csharp-font-lock-api-entry-regexp
  (concat "^[" csde-csharp-font-lock-letter "]"
          "[" csde-csharp-font-lock-letter-or-digit "]+$")
  "Regexp to match a valid entry in `csde-csharp-font-lock-api-file'.")

(defconst csde-csharp-font-lock-api-entry-match 0
  "Index of the match data in `csde-csharp-font-lock-api-entry-regexp'.")

(defun csde-csharp-font-lock-api-names (&optional filter)
  "Return the list of names in `csde-csharp-font-lock-api-file'.
If optional FILTER function is non-nil it is called for each name
found and must return non-nil to include it in the result list."
  (let (k kl)
    (if (and csde-csharp-font-lock-api-file
             (file-readable-p csde-csharp-font-lock-api-file))
        (with-temp-buffer
          (erase-buffer)
          (insert-file-contents csde-csharp-font-lock-api-file)
          (goto-char (point-min))
          (while (re-search-forward csde-csharp-font-lock-api-entry-regexp nil t)
            (setq k (match-string csde-csharp-font-lock-api-entry-match))
            ;; Allow filtering of names
            (if (or (null filter) (funcall filter k))
                (setq kl (cons k kl))))))
    kl))

(defun csde-csharp-font-lock-api-split-list (l n)
  "Split list L in sub listes of N elements.
If L is nil return nil.  If N is less than 1 all elements will be in
one sub list."
  (if l
      (if (<= n 0)
          (list l)
        (let (split-list sub-list i)
          (while l
            (setq i 0 sub-list nil)
            (while (and l (< i n))
              (setq sub-list (cons (car l) sub-list)
                    i        (1+ i)
                    l        (cdr l)))
            (if sub-list
                (setq split-list (cons sub-list split-list))))
          split-list))))

(defun csde-csharp-font-lock-api-build-regexps (max-matches)
  "Build regular expressions matching names to fontify.
MAX-MATCHES is the maximum number of names that one regular expression
will match.  If MAX-MATCHES is less than 1 one regular expression will
match all the names."
  (let ((max-specpdl-size 2000)) ;; Prevent errors in `regexp-opt'
				 ;; when processing long string listes
    (mapcar (function
             (lambda (k)
               (concat "\\<" (regexp-opt k t) "\\>")))
            (csde-csharp-font-lock-api-split-list
             (csde-csharp-font-lock-api-names
              csde-csharp-font-lock-api-name-filter)
             max-matches))))

(defvar csde-csharp-font-lock-api-cache nil
  "Cache of regular expressions matching names to fontify..")

(defun csde-csharp-font-lock-api-cache-file ()
  "Return the filename of the regular expressions cache.
There is a different cache file for each major version of (X)Emacs
because of incompatible regular expressions returned by `regexp-opt'."
  (and csde-csharp-font-lock-api-file
       (format "%s.%semacs-%d.apicache"
               csde-csharp-font-lock-api-file
               (if csde-xemacsp "x" "")
               emacs-major-version)))

(defconst csde-csharp-font-lock-api-cache-file-header
  ";;; Regular expressions matching names to fontify.
;;; Automatically generated by `csde-csharp-font-lock' on %s.
"
  "Header to be written into the cache file.")

(defun csde-csharp-font-lock-api-regexps (&optional rebuild)
  "Return regular expressions matching names to fontify.
The list is cached in variable `csde-csharp-font-lock-api-cache'.  If it
is nil try to initialize it from the cache file (see function
`csde-csharp-font-lock-api-cache-file').  If optional REBUILD flag is
non-nil or there is no cache file or the cache file is older than the
names file (see variable `csde-csharp-font-lock-api-file'), a new cache
is created."
  (let ((cache (csde-csharp-font-lock-api-cache-file)))
    (cond

     ;; Inconditionnal rebuild
     (rebuild
      ;; Clear the cache to rebuild
      (setq csde-csharp-font-lock-api-cache nil))

     ;; No names file exists
     ((null cache)
      ;; Clear the cache (no fontification)
      (setq csde-csharp-font-lock-api-cache nil))
     
     ;; A cache file exists
     ((file-readable-p cache)
      (if (file-newer-than-file-p csde-csharp-font-lock-api-file cache)
          (progn
            (message
             "csde-csharp-font-lock: names file %s newer than cache file %s"
             csde-csharp-font-lock-api-file cache)
            ;; The api file has been modified since the cache was
            ;; created, so clear the cache to rebuild
            (setq csde-csharp-font-lock-api-cache nil))
        ;; Try to load the existing cache if needed
        (or csde-csharp-font-lock-api-cache
            (condition-case nil
                (load-file cache)
              ;; If load fails clear the cache to rebuild
              (error
               (setq csde-csharp-font-lock-api-cache nil)))))))

    (or csde-csharp-font-lock-api-cache
        (not cache)
        ;; Build a new cache if it is empty and available
        (progn
          (message "csde-csharp-font-lock: building names cache...")
          (when (setq csde-csharp-font-lock-api-cache
                      (csde-csharp-font-lock-api-build-regexps
                       ;; WARNING: It seems XEmacs search fails with a
                       ;; very long regexp.  So get regexps for groups
                       ;; of up to 200 names.
                       (if csde-xemacsp 200 0)))
            ;; Save regexps in cache
            (with-current-buffer (find-file-noselect cache)
              (erase-buffer)
              (insert
               (format csde-csharp-font-lock-api-cache-file-header
                       (current-time-string))
               (format "(setq csde-csharp-font-lock-api-cache '%S)"
                       csde-csharp-font-lock-api-cache))
              (save-buffer)
              (kill-buffer (current-buffer))))
          (message "csde-csharp-font-lock: building names cache...%s"
                   (if csde-csharp-font-lock-api-cache "done" "empty"))))
          csde-csharp-font-lock-api-cache))

(defun csde-csharp-font-lock-api-keywords (&optional rebuild)
  "Return a list of font lock keywords for user's defined names.
If optional REBUILD flag is non-nil create a new cache of regular
expressions."
  (mapcar (function
	   (lambda (k)
	     (cons k 'csde-csharp-font-lock-api-face)))
	  (csde-csharp-font-lock-api-regexps rebuild)))

;;;;
;;;; Font lock setup.
;;;;

(defvar csharp-font-lock-keywords-4 nil
  "Extra level fontification keywords for CSDE mode.")

;;;###autoload
(defun csde-csharp-font-lock-setup-keywords (&optional rebuild)
  "Setup font lock keywords in `csharp-font-lock-keywords-4'.
If optional REBUILD flag is non-nil create a new cache of regular
expressions."
  (interactive "P")
  (and (interactive-p)
       (consp current-prefix-arg)
       (setq rebuild t))
  (setq
   csharp-font-lock-keywords-4
   (append

    ;; Feature scoping: These must come first or the Special
    ;; constants, Modifiers and Packages from keywords-1 will catch
    ;; them.
;;; Compatibility
    (if csde-xemacsp
        (list
            
         ;; Special keywords and constants
         '("\\<\\(this\\|super\\)\\>" (1 font-lock-keyword-face))
         '("\\<\\(false\\|null\\|true\\)\\>" (1 font-lock-constant-face))
         ))
       
    (list

     ;; Fontify default as keyword
     ;;'("\\<\\(default\\)\\>" (1 font-lock-keyword-face))

     ;; Fontify foreach as keyword
     ;;'("\\<\\(foreach\\)\\>" (1 font-lock-keyword-face))

     ;; Fontify in as keyword
     ;;'("\\<\\(in\\)\\>" (1 font-lock-keyword-face))

     ;; Fontify get as keyword
     ;;'("\\<\\(get\\)\\>" (1 font-lock-keyword-face))

     ;; Fontify set as keyword
     ;;'("\\<\\(set\\)\\>" (1 font-lock-keyword-face))

     ;; Fontify value as keyword
     ;;'("\\<\\(value\\)\\>" (1 font-lock-keyword-face))

     ;; Fontify struct as keyword
     ;;'("\\<\\(struct\\)\\>" (1 font-lock-keyword-face))

     ;; Fontify typeof as keyword
     ;;'("\\<\\(typeof\\)\\>" (1 font-lock-keyword-face))

     ;; Fontify lock as keyword
     ;;'("\\<\\(lock\\)\\>" (1 font-lock-keyword-face))

     ;; Fontify all keywords
     '("\\<\\(abstract\\|add\\|as\\|base\\|bool\\|break\\|byte\\|case\\|catch\\|char\\|checked\\|contniue\\|decimal\\|default\\|delegate\\|do\\|double\\|else\\|enum\\|event\\|explicit\\|extern\\|finally\\|fixed\\|for\\|foreach\\|if\\|get\\|implicit\\|in\\|int\\|interface\\|internal\\|is\\|lock\\|long\\|object\\|operator\\|out\\|params\\|ref\\|remove\\|sbyte\\|sealed\\|set\\|short\\|sizeof\\|stackalloc\\|string\\|String\\|struct\\|switch\\|throw\\|try\\|typeof\\|uint\\|ulong\\|ushort\\|value\\|void\\|while\\)\\>" (1 font-lock-keyword-face))

     ;; fontify java words that are not used in warning face
     ;; this should prevent some users from using the wrong thing!
     '("\\<\\(unchecked\\|unsafe\\|goto\\|boolean\\|package\\|import\\)\\>" (1 font-lock-warning-face))

     ;; Fontify modifiers.
     (cons (concat "\\<\\("
                   (eval-when-compile
                     (regexp-opt
                      '(
                        "abstract"
                        "const"
                        "final"
                        "native"
                        "private"
                        "protected"
                        "public"
                        "static"
                        "strictfp"
                        "synchronized"
                        "transient"
                        "volatile"
			"override"
			"readonly"
                        )))
                   "\\)\\>")
           'font-lock-builtin-face)
        
     ;; Fontify package directives
     '("\\<\\(namespace\\)\\>[ \t]*\\(\\sw+\\)?"
       (1 font-lock-keyword-face)
       (2 font-lock-constant-face nil t)
       ("\\=\\.\\(\\sw+\\)" nil nil
        (1 font-lock-constant-face nil t)))
        
     ;; Fontify import directives
     '("\\<\\(using\\)\\>[ \t]*\\(\\sw+\\)?"
       (1 font-lock-keyword-face)
       (2 font-lock-constant-face nil t)
       ("\\=\\.\\(\\*\\|\\sw+\\)" nil nil
        (1 (if (equal (char-after (match-end 0)) ?\.)
               'font-lock-constant-face
             (if (equal (char-before (match-end 0)) ?\*)
                 'csde-csharp-font-lock-number-face
               'font-lock-type-face)))))
     )

    ;; Fontify user's defined names
    (csde-csharp-font-lock-api-keywords rebuild)
       
;;; Compatibility
    (if csde-xemacsp
        java-font-lock-keywords-2
      ;; Remove existing java font lock keywords from FSF Emacs
      ;; `csharp-font-lock-keywords-3'
      (csde-csharp-font-lock-remove-csharpdoc-keywords
       csharp-font-lock-keywords-3))

;;; Compatibility
    (if csde-xemacsp
        nil
      ;; FSF Emacs don't fontify capitalized types so do it
      (list
       `(eval .
              (list
               (concat "\\<\\([" csde-csharp-font-lock-capital-letter "]\\sw*\\)\\>"
                       "\\([ \t]*\\[[ \t]*\\]\\)*"
                       "\\([ \t]*\\sw\\)")
               '(font-lock-match-c-style-declaration-item-and-skip-to-next
                 (goto-char (match-beginning 3))
                 (goto-char (match-beginning 3))
                 (1 (if (match-beginning 2)
                        font-lock-function-name-face
                      font-lock-variable-name-face)))))
       (cons
        (concat "\\<\\([" csde-csharp-font-lock-capital-letter "]\\sw*\\)\\>"
                "\\([ \t]*\\[[ \t]*\\]\\)*"
                "\\([ \t]*\\sw\\)")
        '(1 font-lock-type-face))
            
       '("\\<\\(new\\|instanceof\\)\\>[ \t]+\\(\\sw+\\)"
         2 font-lock-type-face)))

    ;; Some extra fontification
    (list
        
     ;; Fontify numbers
     (cons
      (concat "\\b\\(0[xX][0-9a-fA-F]+[lL]?\\|[0-9]+\\.?[0-9]*"
              "\\([eE][-+]?[0-9]+\\)?\\([lL]\\|[fF]\\|[dD]\\)?\\)\\b")
      'csde-csharp-font-lock-number-face)
     (cons
      (concat "\\b\\(\\.[0-9]+"
              "\\([eE][-+]?[0-9]+\\)?\\([lL]\\|[fF]\\|[dD]\\)?\\)\\b")
      'csde-csharp-font-lock-number-face)
     
     ;; Fontify capitalised identifiers as constant
     (cons
      (concat "\\(\\b[" csde-csharp-font-lock-capital-letter
              "]+[" csde-csharp-font-lock-capital-letter-or-digit
              "]*\\b\\)")
      '(1 font-lock-constant-face))

     ;; Fontify text between `' in comments
     (csde-csharp-font-lock-quote-keyword))

    ;; Fontify csharpdoc comments
    (list
       
     ;; Fontify csharpdoc tags (including non official ones)
     (csde-csharp-font-lock-csharpdoc-tag-keyword)
     ;; Fontify @param variable name
     (csde-csharp-font-lock-csharpdoc-param-name-keyword)
     ;; Fontify @exception or @throws exception type
     (csde-csharp-font-lock-csharpdoc-exception-type-keyword)
     ;; Fontify @docRoot
     (csde-csharp-font-lock-csharpdoc-docroot-keyword)
     ;; Fontify @link
     (csde-csharp-font-lock-csharpdoc-link-keyword)
     ;; Fontify @see reference
     (csde-csharp-font-lock-csharpdoc-see-ref-keyword)
     ;; Fontify the text of a HREF anchor
     (csde-csharp-font-lock-html-ahref-keyword)
     ;; Fontify <strong> style text
     (csde-csharp-font-lock-html-strong-keyword)
     ;; Fontify <b> style text
     (csde-csharp-font-lock-html-bold-keyword)
     ;; Fontify <i> style text
     (csde-csharp-font-lock-html-italic-keyword)
     ;; Fontify <u> style text
     (csde-csharp-font-lock-html-underline-keyword)
     ;; Fontify <code> style text
     (csde-csharp-font-lock-html-code-keyword)
     ;; Fontify <pre> style text
     (csde-csharp-font-lock-html-pre-keyword))
       
    )))

(csde-csharp-font-lock-setup-keywords)

;; Setup CSDE mode for font locking.  Copy `font-lock-defaults' from
;; `java-mode' and add the new `csharp-font-lock-keywords-4' level in
;; `csde-mode'.

(defun csde-csharp-font-lock-defaults (csharp-defaults)
  "Return a new `font-lock-defaults' value from CSHARP-DEFAULTS.
That is add the new `csharp-font-lock-keywords-4' level."
  (cons (append (car csharp-defaults) '(csharp-font-lock-keywords-4))
        (cdr csharp-defaults)))

;;; Compatibility
;; XEmacs
(if csde-xemacsp
    (put 'csde-mode 'font-lock-defaults
         (csde-csharp-font-lock-defaults
          (get 'java-mode 'font-lock-defaults)))
  
  ;; FSF Emacs
  (add-to-list
   'font-lock-defaults-alist
   (cons 'csde-mode
         (csde-csharp-font-lock-defaults
          (cdr (assq 'java-mode font-lock-defaults-alist)))))
  )
                 
(defun csde-setup-syntax-coloring()
  "Set up CSDE mode syntax coloring."
  (cond (window-system
         
	 ;; If not XEmacs 20.1 turn on font lock.
	 ;; (XEmacs 21 has font-lock on by default.)
	 (if (or
	      (not csde-xemacsp)
	      (not
	       (and
		(eq emacs-major-version 21)
		(eq emacs-minor-version 0))))
	     (turn-on-font-lock))

	 (setq font-lock-maximum-decoration t))))


(provide 'csde-csharp-font-lock)

;;; Change History:

;;
;; $Log: csde-csharp-font-lock.el,v $
;; Revision 1.1  2001/02/12 05:45:05  paulk
;; Initial XEmacs revision.
;;
;; Revision 1.4  2001/01/17 18:59:03  paulk
;; Font-locking improvements from David Ponce
;;
;;    - You can now fontify user-defined identifiers with the new
;;     csde-csharp-font-lock-api-face.  These identifiers are read in the
;;     file specified by the `csde-csharp-font-lock-api-file' option.
;;
;;     The CSDE provides a default file "csde-csharp-font-lock.api" to fontify class
;;     names of the core 'csharp' and 'csharpx' packages (JDK 1.3) and servlet
;;     API (JSDK 2.0).  To enable this fontification just put this file on
;;     your home directory, maybe modify it to suit your needs and execute
;;     the command:
;;
;;        M-x csde-csharp-font-lock-setup-keywords (or restart Emacs).
;;
;;     To improve reloading a cache file of regular expressions matching
;;     these names is created in the same directory (see the source for
;;     more details).
;;
;;   - Because the 'const' and 'goto' keywords are reserved, but not
;;     currently used they are now fontified with `font-lock-warning-face'.
;;
;;   - The 'default' keyword is now fontified with
;;     `font-lock-keyword-face'.  This was suggested by Stephane Nicolas
;;     s.nicolas@videotron.ca>.
;;
;; Revision 1.3  2000/12/18 05:22:45  paulk
;; *** empty log message ***
;;
;; Revision 1.2  2000/10/10 06:41:47  paulk
;; Fixed some XEmacs compatibility problems.
;;
;; Revision 1.1  2000/10/08 12:53:22  paulk
;; Initial revision.
;;

;;; csde-csharp-font-lock.el ends here
