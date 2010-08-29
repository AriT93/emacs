
;; Adapted from the JDE by Matt Bruce <matt.bruce@morganstanley.com>
;; Maintainer:  Matt Bruce

;; Copyright (C) 2001 by Matt Bruce


;; The latest version of the CSDE is available at
;; <URL:http://www.sourceforge.com/>.

;; Please send any comments, bugs, or upgrade requests to
;; Matt Bruce (matt.bruce@morganstanley.com)

;; Display line-numbers, but has problems with filling (Emacs hangs) and
;; killing lines (confusing the number display).
(require 'setnu)

(defcustom csde-turn-on-setnu-mode-threshold 20000
 "Maximum number of bytes in a file (buffer) that can result in
automatic line numbering.")

(defvar csde-setnu-deletion-check t "deletion check")
(make-variable-buffer-local 'csde-setnu-deletion-check)

(add-hook 
 'after-change-functions 
 ;; when in setnu-mode toggles setnu-mode off and on.
 (lambda (start end length)
   (if setnu-mode
       (if (or
	    (and
	     (> length 0)
	     csde-setnu-deletion-check)
	    (string-match 
		  "[\n\r]" 
		  (buffer-substring-no-properties start end)))
	   (run-with-timer 
	    0.001 nil
	    ;; setnu toggler      
	   (lambda () (setnu-mode) (setnu-mode))))
     (setq csde-setnu-deletion-check nil))))

(add-hook 
 'before-change-functions 
 ;; Determines whether any newlines were deleted
 (lambda (start end) 
   (if setnu-mode
       (if (> end start) 
	   (setq csde-setnu-deletion-check 
		 (string-match "[\n\r]" (buffer-substring-no-properties start end)))))))

(provide 'csde-setnu)
