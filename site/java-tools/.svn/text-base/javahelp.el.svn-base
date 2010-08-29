;;; javahelp.el --- provides online help for javadoc documents

;; Copyright (C) 1998, Rodrigo Reyes
;; Authors:    1998 Rodrigo Reyes
;; Maintainer: rodrigo@talana.linguist.jussieu.fr
;; Keywords:   java javadoc help
;;
;; This package follows the GNU General Public Licence (GPL), see the 
;; COPYING file that comes along with GNU Emacs. This is free software,
;; you can redistribute it and/or modify it under the GNU GPL terms.
;;
;; Java is a registered trademark of Sun Microsystem, Inc.
;;

;;
;; This is version 2.0 of the javahelp package. It provides online
;; help for javadoc-generated documents. Because the database used
;; by the package is dynamically generated, it is possible to use it
;; to provide help for user-defined classes, methods, and variables.
;; 
;; Although it was originally designed to mimic the online help found
;; on most Windows' IDE (the F1-key trick), it is also possible to use
;; it in an emacs-consistent manner. 
;;
;; If you bind a key to the javahelp-search-emacs-flavour, this will
;; let you select the word you want help for by using the minibuffer's
;; completion facilities.
;;
;; If you bind a key to the javahelp-search, this will use the full
;; functionnalities of javahelp: once you ask for a precise word
;; (this is done by positionning the cursor on the word and hitting
;; your help key), javahelp will generate an html document that
;; offers links to the exact or closest match (according to your
;; settings) for the given word. Your favorite browser is called
;; (using the browse-url package) to display the help results. The
;; package offers 5 levels of verbosity for the results (ranging from
;; 1 for the lowest to 5 for the highest). 
;;
;; Because only the short-name of each class is stored (ie. 
;; "java.io.File" is stored as "File") javahelp implements a 
;; long-to-short name translation. So, if you set the cursor anywhere
;; on "java.io.File", the package will identify it as "File", and
;; search only for "File". A side effect is that if multiple packages
;; own classes with identical short names, the package will not be
;; able to recognize the correct (although this may probably be a
;; future improvement) package, and will offer help for all the class
;; names. This is however only a minor inconvenience.
;;
;; You can modify the package verbosity with the
;; javahelp-verbosity-level variable, wich modify the javahelp
;; behaviour as follows:
;;
;;      ------------------------------------------------
;;       level          Exact  Alternative  Approximate
;;      ------------------------------------------------
;;          1             +          -           -
;;          2             +        depends       -
;;          3 (default)   +          +           -
;;          4             +          +         depends
;;          5             +          +           +
;;      ------------------------------------------------
;;       + = display if exists       - = never display
;;
;; The approximate results are those with the longest prefix matching
;; the searched word. The alternative results are those whose the
;; searched word is a prefix, eg. if "FontMetrics" is an alternative
;; result for "Font", because the latter is a prefix of the first,
;; while "FontMetrics" and "Font" are the approximate result of
;; "FonX", because they share the longest common prefix with the word.
;;
;; If you set the javahelp-verbosity-level to 1, only exact matches
;; (if any) will be displayed, and nothing else otherwise. If you set
;; it to 3, exact matches and alternative results (if any) only will
;; be displayed. At level 5, everything found will be displayed (which
;; is mostly noise for approximate results).
;; When it "depends", the javahelp will look at the size of the
;; matched word found. If it contains the same (or more) number of
;; letters than the javahelp-word-size-level-of-meaning value, then it
;; will be displayed as a possible results, and it will be filtered
;; otherwise. This avoids the approximate results to contain lots of
;; word which only have a two or three-letter long prefix in common with the
;; searched word.
;;
;; If you don't understand the preceding paragraph, just let the
;; defaults as is.
;;
;;
;; SETTING THE THING UP:
;; 
;; * First, set the javahelp-directories  variable according to your
;; java installation. This must contain a list of all your API
;; documentation directories. 
;; ie. (setq javahelp-directories '("/usr/doc/jdk-docs-1.1.4-1/api")) 
;;  NOTE: the directories shall point to the "/api/" sub-directories
;;  of the docs, where all the html-files are. You can add any
;;  directory that contains javadoc-generated html documents.
;;
;; * Add to your .emacs the following lines:
;;           (setq max-specpdl-size 50000)
;;           (setq max-lisp-eval-depth 50000)
;;           (load "javahelp")
;;   It is necessary to set max-specpdl-size and max-lisp-eval-depth
;;   to high values because the alist the stores the database is quite
;;   huge (5313 entries for me, which comprises java1.1 + swing docs).
;;
;; * Build the database: eval the "(javahelp-build)" command. This
;; will build the database and save it (the database file name is set
;; in the javahelp-database-file variable).
;;
;; * Bind a key to either javahelp-search or
;; javahelp-search-emacs-flavour. 
;; ie in your .emacs:
;;      (global-set-key '[f12] 'javahelp-search)
;;  or  (global-set-key '[f12] 'javahelp-search-emacs-flavour)
;;
;; * Javahelp requires the "browse-url" package, so be sure it is
;; present on your system, and configured to load your preferred
;; browser.
;;

(defconst javahelp-version "2.0" ) ; version of Javahelp

(require 'browse-url)    

;;; ie. for me:
;;; (setq javahelp-directories '( "/usr/doc/jdk-docs-1.1.4-1/api" 
;;;                               "/usr/doc/jfc-doc-1.1/api/" ))
(defvar javahelp-directories '("/usr/doc/jdk-docs-1.1.4-1/api")
  "The list of directories where the APIs (written by javadoc) are deposited. 
However, this information is only used when the javahelp-build function makes
a new database. The real set of directories is stored internally as a
javahelp-database-dirs. This is to prevent from the user to changing it on
the fly without re-building, and thus making the database incoherent.")

(defvar javahelp-temp-dir "/tmp/"
  "The directory where the javahelp puts its temporary HTML files.
Typically set to /tmp/ under unix, and in any temp dir under NT/95")

(defvar javahelp-database nil
  "Contains the BIG database of javahelp.
Don't initialize it yourself."  )

(defvar javahelp-database-dirs nil
  "A vector of directories. Don't use it.")

(defvar javahelp-verbosity-level 3
  "The level of verbosity for the javahelp results, ranging from 1 (lowest) to 5 (highest)."
)        ; 1-5 

(defvar javahelp-word-size-level-of-meaning 4
  "The minimum size of the words that are meaningful. Default is 4.")

(defvar javahelp-database-file "~/.javahelp"
  "The filename where the package stores its database.")

(defvar javahelp-temp-name-strict-html t
  "Non-nil means temporary files are strictly .html-suffixed.
By default, the javahelp builds temporary files with names that
do not conflict by calling make-temp-name. However, the make-temp-name
adds a suffix to the name, so that the file does not ends with .html.
This is not a problem for netscape, but can cause trouble in other
browsers (such as lynx of w3-mode). If this is the case, the javahelp
uses a temp file that ends with .html, but that is not guaranted to be
unique (and can be overwritten)." )

(defun javahelp-alist-add-last (a b)
  "Add a value to the end of an alist element."
  (let ( (lst a) )
    (while (and (not (null lst))
		(not (null (cdr lst)))		
		(not (equal (car lst) b)))
      (setq lst (cdr lst)))
    (if (null (cdr lst))
	(setcdr lst (list b))
      (if (null lst)
	  (setcar lst b)))))
;  (if (null (cdr a))
;      (setcdr a  (list b))    
;    (if (null a)
;	(setcar a b)
;      (if (equal (car a) b)
;	  t
;	(javahelp-alist-add-last (cdr a) b)))))


(defun javahelp-alist-add (alist key value)
  "Add a key/value pair to an alist, if it is not already present.
If the key already exists, add the value to the list of values of
the key."
  (if (null alist)
      (list (list key value))
    (let* ( (pair (javahelp-assoc key alist)) )
      (if (null pair)
	  (append (list (list key value))
		  alist)
	(progn (if (not (member value pair))
		   (javahelp-alist-add-last pair value))
	       alist)))))

;;;(javahelp-alist-add '((a b))  'a 'b)


(defun javahelp-html-tag-skip ()
  "If the cursor position is on a html starting tag '<', skip the while tag."
  (while (eq ?\< (char-after (point)))
    (search-forward ">"))
  t)

(defun javahelp-html-tag-next ()
  "Parse the current buffer, looking for specific information.
It return a cons cell with the tagname (the string between 
the two <a></a>) in the car, and the reference found in 
href= in the cdr" 
  (if (null (re-search-forward "<[Aa][ \t]" nil t 1))
      nil
    (progn
      (search-forward "href=" nil t 1)
      (let*  ( (quo  (search-forward "\"" nil t 1))
	       (te  (search-forward "\"" nil t 1)))
	(search-forward ">" nil t 1)
	(javahelp-html-tag-skip)
	(let* (  (refe (point))
		 (rence (search-forward "</" nil t 1)))
	  (cons (buffer-substring refe (- rence 2))
		(buffer-substring quo (- te 1))))))))


(defun javahelp-html-tag-parse (alist fname  prefix)
  "Parse a buffer full of javadoc-generated HTML.
Then, add information in the given alist. The prefix stands for the
index in the list of directories where the javahelp package finds 
documentation"
  (let ( (val (javahelp-html-tag-next)) (ref nil) )
    (while (not (null val))
      (progn (setq ref (javahelp-valid-reference (cdr val) fname))
	     (if (not (null ref))
		 (setq alist (javahelp-alist-add alist
						 (javahelp-short-class-name (javahelp-trim-string (car val)))
						 (cons prefix ref))))
	     (setq val (javahelp-html-tag-next))))
    alist))
;  (let ( (val (javahelp-html-tag-next)) )
;    (if (null val)
;	alist
;      (let ( (ref (javahelp-valid-reference (cdr val) fname)) )
;	(if (null ref)
;	    (javahelp-html-tag-parse alist fname prefix)
;	  (javahelp-html-tag-parse (javahelp-alist-add alist 
;						       (javahelp-short-class-name (javahelp-trim-string (car val)))
;						       (cons prefix ref))
;				   fname
;				   prefix))))))

(defun javahelp-valid-reference (ref fname)
  "Test weither REF is a valid reference for the database, and return either a valid reference, or nil.
Actually, only verify that the reference doesn't start with '#', and prefixes it with the
filename if this is the case."
  (if (equal ?# (elt ref 0))
      (concat (file-name-nondirectory fname) ref)
    ref))

(defun javahelp-expand-filename (pair)
  "Expand a ( index . relative-file-name ) pair to an absolute filename.
Check what the index is for in the javahelp-database-dirs, and 
concatenate the directory and the filename."
  (expand-file-name (cdr pair) (elt javahelp-database-dirs (car pair))))

(defun javahelp-html-tag-parse-file (alist pair perc)
  "Load an html document, and call javahelp-html-tag-parse to retrieve the information.
Add the information of the file PAIR to the ALIST list, displaying
the percentage in the minibuffer. The PAIR is the standard 
(index . relative-filename)."
  (if (null pair)
      alist
    (let* ( (fname (javahelp-expand-filename pair))
	    (buff (find-file-literally fname)))
      (set-buffer buff)
      (goto-char 1)
      (message "processing (%d%%) %s" perc (cdr pair) )
      (prog1
	  (javahelp-html-tag-parse alist fname (car pair))
	(kill-buffer buff)))))

(defun javahelp-html-parse-list (alist liste tot cur)
  "Add to ALIST all the information found in the list LISTE of files.
TOT is the total number of files, and CUR is the number of files
already processed. LISTE if a pair (index . filename)."
  (if (null liste)
      alist
    (javahelp-html-parse-list (javahelp-html-tag-parse-file alist (car liste) (/ (* cur 100) tot))
			      (cdr liste)
			      tot
			      (+ 1 cur))))

(defun javahelp-build-filelist-convert (list prefix)
  "Convert the LIST list of files to the list of pairs ( PREFIX . filename).
LIST is something like '( \"aa\" \"bb\" \"cc\"), PREFIX is a number, and
the function returns something like '( (0 . \"aa\") (0 . \"bb\") (0 . \"cc\"))."
  (if (null list)
      nil
    (append (list (cons prefix (car list)))
	    (javahelp-build-filelist-convert (cdr list) prefix))))

(defun javahelp-build-filelist (dirlist index)
  "Return a list of absolute filenames (ie. javahelp-directories) and expand it to a list of html documents filenames.
The returned list contains pairs(index . filename)."
  (if (null dirlist)
      nil
    (append  (javahelp-build-filelist-convert (directory-files (car dirlist) nil ".*[.]html") index)
	     (javahelp-build-filelist (cdr dirlist) (+ 1 index)))))

(defun javahelp-build ()
  "Build the database and save it.
The filename used to save is the value of javahelp-database-file."
  (interactive) 
  (setq javahelp-database-dirs (vconcat javahelp-directories))
  (let ( (flist (javahelp-build-filelist javahelp-directories 0)))
    (setq javahelp-database  (javahelp-html-parse-list nil flist (length flist) 0))
    (message "saving database...")
    (javahelp-save)))


(defun javahelp-load ()
  "Load the javahelp database."
  (if (file-exists-p javahelp-database-file)
      (let ((buff (find-file javahelp-database-file)))
	(set-buffer buff)
	(goto-char 0)
	(setq javahelp-database-dirs (read buff))
	(setq javahelp-database (read buff))
	(kill-buffer buff)
	t)
    (progn (setq javahelp-database-dirs '[])
	   (setq javahelp-database nil))))

;;(javahelp-load)

(defun javahelp-save ()
  "Print the javahelp-database in a buffer and save it."
  (let ( (buf (generate-new-buffer "##javahelp-database##")) )
    (set-buffer buf)
    (print (vconcat javahelp-database-dirs) buf)
    (print javahelp-database buf)
    (write-file javahelp-database-file)
    (kill-buffer buf)))


(defun javahelp-class-name-last-point (name index max cur)
  "Find the last '.' char in a string, starting at INDEX, until MAX. If no '.' is found, return CUR as default."
  (cond ( (>= index max) cur)
	( (equal ?. (elt name index)) (javahelp-class-name-last-point name (+ 1 index) max (+ 1 index)))
	( t (javahelp-class-name-last-point name (+ 1 index) max cur))))

(defun javahelp-short-class-name (name)
  "Return the short name of a java name.
Classes, methods, and variables in java can be accessed using either
longt ('java.io.File') or short ('File') names. This function
converts long names to short."
  (substring name (javahelp-class-name-last-point name 0 (length name) 0)))


(defun javahelp-search-in-string (str e pos &optional neg)
  "Return t is E is found in STR, starting at position POS.
E can be either an element or a list of elements, and if NEG 
is non-nil, the results is negated, ie. return t is E is NOT
found in STR, nil if it is."
  (if (>=  pos (length str))
      nil
    (if (null neg)
	(if (if (listp e)
		(member (elt str pos) e)
	    (equal (elt str pos) e))
	    pos
	  (javahelp-search-in-string str e (+ 1 pos) neg))
    (if (if (listp e)
	    (member (elt str pos) e)
	  (equal (elt str pos) e))
	(javahelp-search-in-string str e (+ 1 pos) neg)
      pos))))


(defun javahelp-trim-string (str)
  "Trim the string STR, ie. return a string with no white spaces around.
The behaviour of this function if to return only the first word
of the string, with no white spaces around."
  (let* ( (pos1 (javahelp-search-in-string str '(32 9) 0 t)) ;; first occurrence of non-WS
	  (pos2 (javahelp-search-in-string str '(32 9) (+ 1 pos1))) ;; second of non-WS
	  )
    (if (null pos2)
	(substring str pos1)
      (substring str pos1 pos2))))

(defun javahelp-common-prefix-size (str1 str2 count len1 len2)
  "Return the size of the common prefix for STR1 and STR2.
COUNT is the starting count, and should be 0 when called, while
LEN1 and LEN2 are respectively the lengthes of STR1 and STR2."
  (if (or (>= count len1) (>= count len2))
      count
    (if (equal (downcase (elt str1 count)) (downcase (elt str2 count)))
	(javahelp-common-prefix-size str1 str2 (+ 1 count) len1 len2)
      count)))

;(defun javahelp-assoc-bis (name alist)
;  (if (null alist)
;;      nil
;    (if (string= name (downcase (car (car alist))))
;	(car alist)
;      (javahelp-assoc-bis name (cdr alist)))))

(defun javahelp-assoc-bis (name alist)
  (let ( (lst alist) )
    (while (and (not (null lst))
		(not (string= name (downcase (car (car lst))))))
      (setq lst (cdr lst)))
    (if (null lst)
	nil
      (if (string= name (downcase (car (car lst))))
	  (car lst)
	nil))))

(defun javahelp-assoc (name alist)
  (javahelp-assoc-bis (downcase name) alist))
;;(assoc "Font" javahelp-database)

(defun javahelp-scan-list (alist str exact over prefix)
  "Search in the alist ALIST for the string STR. 
The EXACT, OVER, and PREFIX lists store the exact, alternative,
and approximate results, respectively."
  (if (null alist)
      (append (if (null exact)
		  '(nil)
		(list exact))
	      (if (null over)
		  '(nil)
		(list over))
	      (if (null prefix)
		  '(nil)
		(list prefix)))
    (let* ( (ref (car (car alist)))
	    (prefsize (javahelp-common-prefix-size str ref 0 (length str) (length ref))))
      (cond ( (= prefsize (length str))
	      (if (= (length str) (length ref))
		  (javahelp-scan-list (cdr alist)
				      str
				      (append (cdr (car alist))
					      exact)
				      over
				      prefix)
		(javahelp-scan-list (cdr alist)
				    str
				    exact
				    (append (cdr (car alist)) over)
				    prefix)))
	    ( (not (null prefix))
	      (cond ((> prefsize (car prefix)) 
		      (javahelp-scan-list (cdr alist) str exact over
					  (append (list prefsize) (cdr (car alist)))))
		    ((= prefsize (car prefix))
		     (javahelp-scan-list (cdr alist) str exact over
					 (append prefix (cdr (car alist)))))
		    (t
		     (javahelp-scan-list (cdr alist) str exact over prefix))))
	    ( (or (= javahelp-verbosity-level 5) 
		      (and (= javahelp-verbosity-level 4) (>= prefsize javahelp-word-size-level-of-meaning)))
	      (javahelp-scan-list (cdr alist) str exact over (append (list prefsize) (cdr (car alist)))))
	    ( t
	      (javahelp-scan-list (cdr alist) str exact over prefix))))))




(defun javahelp-expand-result (li)
  "Take a list of results and expands the filenames.
Beacause the results look like (INDEX FILENAME), the function
retrieves the path corresponding to INDEX (in javahelp-database-dirs),
and concats it with the filename, so that the filename is turned
into an absolute path+filename."
  (if (null li)
      nil
    (append (list (expand-file-name (cdr (car li))
				    (elt javahelp-database-dirs (car (car li)))))
		  (javahelp-expand-result (cdr li)))))

(defun javahelp-guess-name (str)
  "Guess the real filename of the string STR, and return it.
Mainly remove the '#...' stuff at the end of most html links."
  (let* ((fstr (file-name-nondirectory str)) (idx (string-match "\#" fstr)))
    (if (null idx)
	(file-name-sans-extension fstr)
      (let* ( (marq (substring fstr (+ idx 1))) )
	  (if (or (string= marq "_top_") (string= marq "_bottom_"))
	      (file-name-sans-extension (substring fstr 0 idx))
	    (concat (file-name-sans-extension (substring fstr 0 idx))
		" : "
		marq))))))


(defun javahelp-htmlize-list (lst buf)
  "Take a list LST of filenames, and print them as html links in the buffer BUF."
  (if (not (null lst))
      (progn
	(princ "<a href=\"file://" buf)
	(princ (car lst) buf)
	(princ "\">" buf)
	(princ (javahelp-guess-name (car lst)) buf)
	(princ "</a><br>\n" buf)
	(javahelp-htmlize-list (cdr lst) buf))))


(defun javahelp-get-temp-name ()
  "Return a valid temp name. 
Check the javahelp-temp-name-strict-html variable for more 
details."
  (if (null javahelp-temp-name-strict-html)
      (make-temp-name (concat (file-name-directory javahelp-temp-dir) "JavaHelp-Results.html."))
    (concat (make-temp-name (concat (file-name-directory javahelp-temp-dir) "JavaHelp-Results.")) ".html")))

(defun javahelp-create-html-results (orgstr lst)
  "Given a list LST of results for the search of the string ORGSTR, create an html document and call a browser."
  (let* ((tmpname (javahelp-get-temp-name)) 
	 (tmpbuf (find-file tmpname))
	 (exact (javahelp-expand-result (car lst)))
	 (over  (javahelp-expand-result (car (cdr lst))))
	 (nearlevel (car (car (cdr (cdr lst)))))
	 (near  (javahelp-expand-result (cdr (car (cdr (cdr lst)))))))
    (set-buffer tmpbuf)
    (erase-buffer)
    (princ (concat "<!DOCTYPE HTML PUBLIC \"-//IETF//DTD HTML//EN\"><html><head><title>JavaHelp results for " orgstr "</title></head><body>") tmpbuf)
    ;;
    ;; now the real choices
    ;;
    (princ (concat "<h1>JavaHelp results for \"" orgstr "\" :</h1><br>") tmpbuf)
    (if  (not (null exact))
	(javahelp-htmlize-list exact tmpbuf)
      (princ (concat "No result matching the request string \"" orgstr "\" !<br>") tmpbuf))
    (if (> javahelp-verbosity-level javahelp-word-size-level-of-meaning)
	(progn  (if (not (null over))  (progn (princ (concat "<br><hr><h1>Alternative results :</h1><br>") tmpbuf)
					      (javahelp-htmlize-list over tmpbuf)))
		(if (not (null near))  (progn (princ (concat "<br><hr><h1>Approximate results :</h1><br>") tmpbuf)
					      (javahelp-htmlize-list near tmpbuf))))
      (if (= javahelp-verbosity-level 4)
	  (progn (if (not (null over))  (progn (princ (concat "<br><hr><h1>Alternative results :</h1><br>") tmpbuf)
					       (javahelp-htmlize-list over tmpbuf)))
		 (if (and (not (null near)) (>= nearlevel javahelp-word-size-level-of-meaning)) 
		     (progn (princ (concat "<br><hr><h1>Approximate results :</h1><br>") tmpbuf)
			    (javahelp-htmlize-list near tmpbuf))))
	
	(if (= javahelp-verbosity-level 3)
	  (if (not (null over))  (progn (princ (concat "<br><hr><h1>Alternative results :</h1><br>") tmpbuf)
					(javahelp-htmlize-list over tmpbuf)))
	  (if (= javahelp-verbosity-level 2)
	      (if (and (not (null over)) (> (length orgstr) 3))
		  (progn (princ (concat "<br><hr><h1>Alternative results :</h1><br>") tmpbuf)
			 (javahelp-htmlize-list over tmpbuf)))))))
    ;;
    (princ (concat "<br><hr>Javahelp " javahelp-version " (on " (emacs-version) ")") tmpbuf)
    (princ (concat "<br>Verbosity level: " javahelp-verbosity-level " (range 1-5, default is 3)<br>") tmpbuf)
    (princ "Send bug reports to <a href=\"mailto:rodrigo@talana.linguist.jussieu.fr\">JavaHelp support</a>" tmpbuf)
    ;;
    (princ "</address></body> </html>" tmpbuf)
    ;;
    (save-buffer tmpbuf)
    ;;
    (browse-url (concat "file://" tmpname))
    (kill-buffer tmpbuf)))


(defun javahelp-search-string (str)
  "Search for the string STR and display the results."
  (javahelp-create-html-results str 
				(javahelp-scan-list javahelp-database str nil nil nil)))

(defun javahelp-strict-file-name (fname)
  "Remove the part on the right of the '#' of an html link."
  (let ( (pos (javahelp-search-in-string fname ?\# 0)) )
    (if (null pos)
	fname
      (substring fname 0 pos))))

(defun javahelp-help-for-word (word)
  "Display quickly a help for the string WORD. 
The first reference found is called, no Javahelp-Results document is created."
  (let ( (res (javahelp-assoc word javahelp-database)) )
    (if (not (null res))
	(progn
	  (browse-url (concat "file://" (car (javahelp-expand-result (list (car (cdr res)))))))
	  t)
      nil)))

(defun javahelp-search-string-emacs-flavour (str)
  "Call the online help with emacs flavour, using the minibuffer to complete word STR."
  (let ((res (completing-read "Javahelp describe word: " javahelp-database nil t str)))
    (javahelp-help-for-word res)))

(defun javahelp-search-emacs-flavour ()
  "Call the contextual online help with emacs flavour to find the word at cursor position."
  (interactive)
  (javahelp-search-string-emacs-flavour (javahelp-short-class-name (javahelp-current-java-word))))

(defun javahelp-current-java-word ()
  "Return the current word, according to java syntax.
The main thing is that a '.' is  part of a name."
  (save-excursion
    (while (or (and (>= (preceding-char) ?a) (<= (preceding-char) ?z))
	       (and (>= (preceding-char) ?A) (<= (preceding-char) ?Z))
	       (member (preceding-char) '(?. ?_)))
      (forward-char -1))
    (let ((pos1 (point)))
      (while (or (and (>= (following-char) ?a) (<= (following-char) ?z))
		 (and (>= (following-char) ?A) (<= (following-char) ?Z))
		 (member (following-char) '(?. ?_)))
	(forward-char 1))

      (let ((pos2 (point)))
	(buffer-substring pos1 pos2)))))

(defun javahelp-search ()
  "Call the standard contextual javahelp to describe the word at the current position in the current buffer."
  (interactive)
  (let ((wd (javahelp-short-class-name (javahelp-current-java-word))))
    (message (concat "Searching " wd "..."))
    (javahelp-search-string wd)))

;;;
;;; Load the javahelp database
(javahelp-load)

(provide 'javahelp)

;;; end of javahelp.el


