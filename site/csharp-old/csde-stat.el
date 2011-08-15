;;; csde-stat.el -- Integrated Development Environment for Csharp.
;; $Revision: 1.1 $ $Date: 2001/02/12 05:54:53 $ 

;; Adapted from the JDE by Matt Bruce <matt.bruce@morganstanley.com>

;; Copyright (C) 2001 by Matt Bruce
;; Maintainer:  Matt Bruce

;; JDE Author: Stephane Nicolas <s.nicolas@videotron.ca>
;; JDE Maintainer: Paul Kinnucan

;; Keywords: csharp, tools

;; JDE version Copyright (C) 2000 Stephane Nicolas, Paul Kinnucan

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

;;; Commentary:


;; The latest version of the CSDE is available at
;; <URL:http://www.sourceforge.com/>.

;; Please send any comments, bugs, or upgrade requests to
;; Matt Bruce (matt.bruce@morganstanley.com)

;;; Code:
 
(defun csde-stat-loc-report ()
  "Generates a report showing the number of code, comment,
csharpdoc, and blank lines in the current Csharp source buffer."
  (interactive)
  (flet ((perc2x2 (a b)
		  (format "%.1f" (* 100(/ (float a) (float b))))))
    (let* ((fname (buffer-file-name))
	   (result (csde-stat-count-loc))
	   (total (nth 0 result))
	   (comment (nth 1 result)) 
	   (csharpdoc (nth 2 result))
	   (blank (nth 3 result))
	   (code (- total (+ comment csharpdoc blank)))
	   (code-perc (perc2x2 code total))
	   (doc-perc (perc2x2 comment total))
	   (jdoc-perc (perc2x2 csharpdoc total))
	   (blank-perc (perc2x2 blank total)))
      (with-output-to-temp-buffer "LOC Report"
	(princ "Lines of Code Report\n\n")
	(princ (format "File name: %s\n" fname))
	(princ (format "File date: %s\n" (format-time-string "%D" (nth 5 (file-attributes fname)))))
	(princ "------------------- \n")
	(princ (format "Code lines:    %d (%s%%)\n" code code-perc))
	(princ (format "Csharpdoc lines: %d (%s%%)\n" csharpdoc jdoc-perc))
	(princ (format "Comment lines: %d (%s%%)\n" comment doc-perc))
	(princ (format "Blank lines:   %d (%s%%)\n" blank blank-perc))
	(princ (format "Total lines:   %d  \n" total))
	(princ "")))))


(defun csde-stat-parse-token-out-of-quote (token line)
  (let (result)
     ;;;Does the line contain '//'?
    (if (string-match token line)
         ;;;if so, does it contain '"'
	(if (not (string-match "\"" line ))
             ;;;no! Ok, it's a comment
	    (setq result t)
           ;;;yes!? so, we've got to parse to see if '//' exists without enclosing quote
	  (let (
                ;;;we split the line in '"' delimited parts
		(to-parse (split-string line "\""))
		(temp "")
		(count-even 0))
            ;;;we consider the even numbered parts of split
            ;;;to see if the contain a'//'
            ;;;if so, this is a doc line
	    (while temp
	      (setq temp (nth count-even to-parse))
	      (if temp
		  (if (string-match token temp)
		      (progn 
			(setq result t)
			(setq temp nil))))
	      (setq count-even (+ 2  count-even))))))
    result))

(defun csde-stat-count-loc ()
  "Counts the code, comments, csharpdoc, and blank lines in the current buffer.
Returns the counts in a list: (TOTAL-LINES COMMENT-LINES CSHARPDOC-LINES BLANK-LINES)."
  (let ((count 0)
	(line "")
	(csharpdoc-count 0)
	(comment-count 0)
	(blank-count 0)
	in-csharpdoc
	in-comment
        (test-b t)
	start
	end)
    (save-excursion
      (goto-char (point-min))
      (while test-b
	(beginning-of-line 1)
	(setq start (point))
	(end-of-line 1)
	(setq end (point))
	(setq line (buffer-substring start end))
	(setq count (+ 1 count))

      ;;;To match a blank line, we search the pattern representing an empty line 
      ;;;or a line that just contains spaces
	(if (string-match "^ *$" line)
	    (setq blank-count (+ 1 blank-count) ))

      ;;;To match a comment line, we search the pattern '//' 
      ;;;but we must disgard the '//' patterns enclosed in a pair of quote '"'
	(if (csde-stat-parse-token-out-of-quote "//" line)
	    (setq comment-count (+ 1 comment-count))) 

      ;;;To match a comment block start, we search the pattern '/*' and exclude those of type '/**' 
      ;;;but we must disgard the '/*' patterns enclosed in a pair of quote '"'
	(if (and 
	     (csde-stat-parse-token-out-of-quote "/\\*" line)
	     (not (csde-stat-parse-token-out-of-quote "/\\*\\*" line)))
	    (setq in-comment t)) 

      ;;;To match a csharpdoc block start, we search the pattern '/**'
      ;;;but we must disgard the '/**' patterns enclosed in a pair of quote '"'
	(if (csde-stat-parse-token-out-of-quote "/\\*\\*" line)
	    (setq in-csharpdoc t)) 

      ;;;To match a block end, we search the pattern '*/'
      ;;;but we must disgard the '*/' patterns enclosed in a pair of quote '"'
	(if (csde-stat-parse-token-out-of-quote "\\*/" line)
	    (progn  
	      (if in-csharpdoc
		  (setq csharpdoc-count (+ 1 csharpdoc-count)))
	      (if in-comment
		  (setq comment-count (+ 1 comment-count)))
	      (setq in-csharpdoc nil)
	      (setq in-comment nil)))
	(if in-csharpdoc
	    (setq csharpdoc-count (+ 1 csharpdoc-count)))
	(if in-comment 
	    (setq comment-count (+ 1 comment-count)))
	(if (not (= (forward-line 1) 0))
	    (setq test-b nil))))
    (list count comment-count csharpdoc-count blank-count))) 
 
 
(provide 'csde-stat)

;; Change History
;;
;; $Log: csde-stat.el,v $
;; Revision 1.1  2001/02/12 05:54:53  paulk
;; Initial XEmacs revision.
;;
;; Revision 1.1  2000/07/28 05:59:43  paulk
;; Initial revision. Thanks to Stephane Nicolas <s.nicolas@videotron.ca>
;; for contributing the initial version of this package.
;;
;;
;; End of csde-stat.el
