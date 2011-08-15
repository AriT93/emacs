;;; dgnushack.el --- a hack to set the load path for byte-compiling
;; Copyright (C) 1994, 1995, 1996, 1997, 1998, 1999, 2000, 2001, 2003
;;        Free Software Foundation, Inc.

;; Author: Lars Magne Ingebrigtsen <larsi@gnus.org>
;; Version: 4.19
;; Keywords: news, path

;; This file is part of GNU Emacs.

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

;;; Code:

(defalias 'facep 'ignore)

(require 'cl)

(defvar srcdir (or (getenv "srcdir") "."))

(defun my-getenv (str)
  (let ((val (getenv str)))
    (if (equal val "no") nil val)))

(if (my-getenv "lispdir")
    (push (my-getenv "lispdir") load-path))

(push (or (my-getenv "URLDIR") (expand-file-name "../../url/lisp/" srcdir))
      load-path)

(push (or (my-getenv "W3DIR") (expand-file-name "../../w3/lisp/" srcdir))
      load-path)

;(push "/usr/share/emacs/site-lisp" load-path)

;; Define compiler macros for the functions provided by cl in old Emacsen.
(unless (featurep 'xemacs)
  (define-compiler-macro butlast (&whole form x &optional n)
    (if (>= emacs-major-version 21)
	form
      (if n
	  `(let ((x ,x)
		 (n ,n))
	     (if (and n (<= n 0))
		 x
	       (let ((m (length x)))
		 (or n (setq n 1))
		 (and (< n m)
		      (progn
			(if (> n 0)
			    (progn
			      (setq x (copy-sequence x))
			      (setcdr (nthcdr (- (1- m) n) x) nil)))
			x)))))
	`(let* ((x ,x)
		(m (length x)))
	   (and (< 1 m)
		(progn
		  (setq x (copy-sequence x))
		  (setcdr (nthcdr (- m 2) x) nil)
		  x))))))

  (define-compiler-macro remove (&whole form item seq)
    (if (>= emacs-major-version 21)
	form
      `(delete ,item (copy-sequence ,seq))))

  (define-compiler-macro mapc (&whole form fn seq &rest rest)
    (if (>= emacs-major-version 21)
	form
      (if rest
	  `(let* ((fn ,fn)
		  (seq ,seq)
		  (args (list seq ,@rest))
		  (m (apply (function min) (mapcar (function length) args)))
		  (n 0))
	     (while (< n m)
	       (apply fn (mapcar (function (lambda (arg) (nth n arg))) args))
	       (setq n (1+ n)))
	     seq)
	`(let ((seq ,seq))
	   (mapcar ,fn seq)
	   seq)))))

;; If we are building w3 in a different directory than the source
;; directory, we must read *.el from source directory and write *.elc
;; into the building directory.  For that, we define this function
;; before loading bytecomp.  Bytecomp doesn't overwrite this function.
(defun byte-compile-dest-file (filename)
  "Convert an Emacs Lisp source file name to a compiled file name.
 In addition, remove directory name part from FILENAME."
  (setq filename (byte-compiler-base-file-name filename))
  (setq filename (file-name-sans-versions filename))
  (setq filename (file-name-nondirectory filename))
  (if (memq system-type '(win32 w32 mswindows windows-nt))
      (setq filename (downcase filename)))
  (cond ((eq system-type 'vax-vms)
	 (concat (substring filename 0 (string-match ";" filename)) "c"))
	((string-match emacs-lisp-file-regexp filename)
	 (concat (substring filename 0 (match-beginning 0)) ".elc"))
	(t (concat filename ".elc"))))

(require 'bytecomp)
;; To avoid having defsubsts and inlines happen.
;(if (featurep 'xemacs)
;    (require 'byte-optimize)
;  (require 'byte-opt))
;(defun byte-optimize-inline-handler (form)
;  "byte-optimize-handler for the `inline' special-form."
;  (cons 'progn (cdr form)))
;(defalias 'byte-compile-file-form-defsubst 'byte-compile-file-form-defun)

(when (and (not (featurep 'xemacs))
	   (= emacs-major-version 21)
	   (= emacs-minor-version 3)
	   (condition-case code
	       (let ((byte-compile-error-on-warn t))
		 (byte-optimize-form (quote (pop x)) t)
		 nil)
	     (error (string-match "called for effect"
				  (error-message-string code)))))
  (defadvice byte-optimize-form-code-walker (around silence-warn-for-pop
						    (form for-effect)
						    activate)
    "Silence the warning \"...called for effect\" for the `pop' form.
It is effective only when the `pop' macro is defined by cl.el rather
than subr.el."
    (let (tmp)
      (if (and (eq (car-safe form) 'car)
	       for-effect
	       (setq tmp (get 'car 'side-effect-free))
	       (not byte-compile-delete-errors)
	       (not (eq tmp 'error-free))
	       (eq (car-safe (cadr form)) 'prog1)
	       (let ((var (cadr (cadr form)))
		     (last (nth 2 (cadr form))))
		 (and (symbolp var)
		      (null (nthcdr 3 (cadr form)))
		      (eq (car-safe last) 'setq)
		      (eq (cadr last) var)
		      (eq (car-safe (nth 2 last)) 'cdr)
		      (eq (cadr (nth 2 last)) var))))
	  (progn
	    (put 'car 'side-effect-free 'error-free)
	    (unwind-protect
		ad-do-it
	      (put 'car 'side-effect-free tmp)))
	ad-do-it))))

(push srcdir load-path)
(load (expand-file-name "lpath.el" srcdir) nil t)

(defalias 'device-sound-enabled-p 'ignore)
(defalias 'play-sound-file 'ignore)
(defalias 'nndb-request-article 'ignore)
(defalias 'efs-re-read-dir 'ignore)
(defalias 'ange-ftp-re-read-dir 'ignore)
(defalias 'define-mail-user-agent 'ignore)

(eval-and-compile
  (unless (featurep 'xemacs)
    (defalias 'get-popup-menu-response 'ignore)
    (defalias 'event-object 'ignore)
    (defalias 'x-defined-colors 'ignore)
    (defalias 'read-color 'ignore)))

(eval-and-compile
  (when (featurep 'xemacs)
    ;; XEmacs 21.1 needs some extra hand holding
    (when (eq emacs-minor-version 1)
      (autoload 'custom-declare-face "cus-face" nil t)
      (autoload 'cl-compile-time-init "cl-macs" nil t)
      (autoload 'defadvice "advice" nil nil 'macro))
    (unless (fboundp 'defadvice)
      (autoload 'defadvice "advice" nil nil 'macro))
    (autoload 'Info-directory "info" nil t)
    (autoload 'Info-menu "info" nil t)
    (autoload 'annotations-at "annotations")
    (autoload 'apropos "apropos" nil t)
    (autoload 'apropos-command "apropos" nil t)
    (autoload 'bbdb-complete-name "bbdb-com" nil t)
    (autoload 'browse-url "browse-url" nil t)
    (autoload 'customize-apropos "cus-edit" nil t)
    (autoload 'customize-save-variable "cus-edit" nil t)
    (autoload 'customize-variable "cus-edit" nil t)
    (autoload 'delete-annotation "annotations")
    (autoload 'dolist "cl-macs" nil nil 'macro)
    (autoload 'enriched-decode "enriched")
    (autoload 'info "info" nil t)
    (autoload 'make-annotation "annotations")
    (autoload 'make-display-table "disp-table")
    (autoload 'pp "pp")
    (autoload 'ps-despool "ps-print" nil t)
    (autoload 'ps-spool-buffer "ps-print" nil t)
    (autoload 'ps-spool-buffer-with-faces "ps-print" nil t)
    (autoload 'read-passwd "passwd")
    (autoload 'regexp-opt "regexp-opt")
    (autoload 'reporter-submit-bug-report "reporter")
    (if (emacs-version>= 21 5)
	(autoload 'setenv "process" nil t)
      (autoload 'setenv "env" nil t))
    (autoload 'smtpmail-send-it "smtpmail")
    (autoload 'sort-numeric-fields "sort" nil t)
    (autoload 'sort-subr "sort")
    (autoload 'trace-function-background "trace" nil t)
    (autoload 'w3-do-setup "w3")
    (autoload 'w3-prepare-buffer "w3-display")
    (autoload 'w3-region "w3-display" nil t)
    (defalias 'frame-char-height 'frame-height)
    (defalias 'frame-char-width 'frame-width)
    (defalias 'frame-parameter 'frame-property)
    (defalias 'make-overlay 'ignore)
    (defalias 'overlay-end 'ignore)
    (defalias 'overlay-get 'ignore)
    (defalias 'overlay-put 'ignore)
    (defalias 'overlay-start 'ignore)
    (defalias 'overlays-in 'ignore)
    (defalias 'replace-dehighlight 'ignore)
    (defalias 'replace-highlight 'ignore)
    (defalias 'run-with-idle-timer 'ignore)
    (defalias 'w3-coding-system-for-mime-charset 'ignore)))

(defun dgnushack-compile-verbosely ()
  "Call dgnushack-compile with warnings ENABLED.  If you are compiling
patches to gnus, you should consider modifying make.bat to call
dgnushack-compile-verbosely.  All other users should continue to use
dgnushack-compile."
  (dgnushack-compile t))

(defun dgnushack-compile (&optional warn)
  ;;(setq byte-compile-dynamic t)
  (when (and (not (featurep 'xemacs))
	     (< emacs-major-version 21))
    (setq max-specpdl-size 1200))
  (unless warn
    (setq byte-compile-warnings
	  '(free-vars unresolved callargs redefine)))
  (unless (locate-library "cus-edit")
    (error "You do not seem to have Custom installed.
Fetch it from <URL:http://www.dina.kvl.dk/~abraham/custom/>.
You also then need to add the following to the lisp/dgnushack.el file:

     (push \"~/lisp/custom\" load-path)

Modify to suit your needs."))
  (let ((files (directory-files srcdir nil "^[^=].*\\.el$"))
	;;(byte-compile-generate-call-tree t)
	file elc)
    ;; Avoid barfing (from gnus-xmas) because the etc directory is not yet
    ;; installed.
    (when (featurep 'xemacs)
      (setq gnus-xmas-glyph-directory "dummy"))
    (dolist (file '("dgnushack.el" "lpath.el"))
      (setq files (delete file files)))
    (when (featurep 'base64)
      (setq files (delete "base64.el" files)))
    (condition-case code
	(require 'w3-parse)
      (error
       (message "No w3: %s %s" (cadr code) (or (locate-library "w3-parse") ""))
       (dolist (file '("nnultimate.el" "webmail.el" "nnwfm.el"))
	 (setq files (delete file files)))))
    (condition-case code
	(require 'mh-e)
      (error
       (message "No mh-e: %s %s" (cadr code) (or (locate-library "mh-e") ""))
       (setq files (delete "gnus-mh.el" files))))
    (condition-case code
	(require 'xml)
      (error
       (message "No xml: %s %s" (cadr code) (or (locate-library "xml") ""))
       (setq files (delete "nnrss.el" files))))
    (dolist (file
	     (if (featurep 'xemacs)
		 '("md5.el")
	       '("gnus-xmas.el" "messagexmas.el" "nnheaderxm.el")))
      (setq files (delete file files)))

    (dolist (file files)
      (setq file (expand-file-name file srcdir))
      (when (and (file-exists-p
		  (setq elc (concat (file-name-nondirectory file) "c")))
		 (file-newer-than-file-p file elc))
	(delete-file elc)))

    (while (setq file (pop files))
      (setq file (expand-file-name file srcdir))
      (when (or (not (file-exists-p
		      (setq elc (concat (file-name-nondirectory file) "c"))))
		(file-newer-than-file-p file elc))
	(ignore-errors
	  (byte-compile-file file))))))

(defun dgnushack-recompile ()
  (require 'gnus)
  (byte-recompile-directory "." 0))

(defvar dgnushack-gnus-load-file
  (if (featurep 'xemacs)
      (expand-file-name "auto-autoloads.el")
    (expand-file-name "gnus-load.el")))

(defvar	dgnushack-cus-load-file 
  (if (featurep 'xemacs)
      (expand-file-name "custom-load.el")
    (expand-file-name "cus-load.el")))

(defun dgnushack-make-cus-load ()
  (load "cus-dep")
  (let ((cusload-base-file dgnushack-cus-load-file))
    (if (fboundp 'custom-make-dependencies)
	(custom-make-dependencies)
      (Custom-make-dependencies))
    (when (featurep 'xemacs)
      (message "Compiling %s..." dgnushack-cus-load-file)
      (byte-compile-file dgnushack-cus-load-file))))

(defun dgnushack-make-auto-load ()
  (require 'autoload)
  (unless (make-autoload '(define-derived-mode child parent name
			    "docstring" body)
			 "file")
    (defadvice make-autoload (around handle-define-derived-mode activate)
      "Handle `define-derived-mode'."
      (if (eq (car-safe (ad-get-arg 0)) 'define-derived-mode)
	  (setq ad-return-value
		(list 'autoload
		      (list 'quote (nth 1 (ad-get-arg 0)))
		      (ad-get-arg 1)
		      (nth 4 (ad-get-arg 0))
		      t nil))
	ad-do-it))
    (put 'define-derived-mode 'doc-string-elt 3))
  (let ((generated-autoload-file dgnushack-gnus-load-file)
	(make-backup-files nil)
	(autoload-package-name "gnus"))
    (if (featurep 'xemacs)
	(if (file-exists-p generated-autoload-file)
	    (delete-file generated-autoload-file))
      (with-temp-file generated-autoload-file
	(insert ?\014)))
    (batch-update-autoloads)))

(defun dgnushack-make-load ()
  (unless (featurep 'xemacs)
    (message "Generating %s..." dgnushack-gnus-load-file)
    (with-temp-file dgnushack-gnus-load-file
      (insert-file-contents dgnushack-cus-load-file)
      (delete-file dgnushack-cus-load-file)
      (goto-char (point-min))
      (search-forward ";;; Code:")
      (forward-line)
      (delete-region (point-min) (point))
      (insert "\
;;; gnus-load.el --- automatically extracted custom dependencies and autoload
;;
;;; Code:
")
      (goto-char (point-max))
      (if (search-backward "custom-versions-load-alist" nil t)
	  (forward-line -1)
	(forward-line -1)
	(while (eq (char-after) ?\;)
	  (forward-line -1))
	(forward-line))
      (delete-region (point) (point-max))
      (insert "\n")
      ;; smiley-* are duplicated. Remove them all.
      (let ((point (point)))
	(insert-file-contents dgnushack-gnus-load-file)
	(goto-char point)
	(while (search-forward "smiley-" nil t)
	  (beginning-of-line)
	  (if (looking-at "(autoload ")
	      (delete-region (point) (progn (forward-sexp) (point)))
	    (forward-line))))
      ;;
      (goto-char (point-max))
      (when (search-backward "\n(provide " nil t)
	(forward-line -1)
	(delete-region (point) (point-max)))
      (insert "\

\(provide 'gnus-load)

;;; Local Variables:
;;; version-control: never
;;; no-byte-compile: t
;;; no-update-autoloads: t
;;; End:
;;; gnus-load.el ends here
")
      ))
  (message "Compiling %s..." dgnushack-gnus-load-file)
  (byte-compile-file dgnushack-gnus-load-file)
  (when (featurep 'xemacs)
    (message "Creating dummy gnus-load.el...")
    (with-temp-file (expand-file-name "gnus-load.el")
      (insert "\

\(provide 'gnus-load)

;;; Local Variables:
;;; version-control: never
;;; no-byte-compile: t
;;; no-update-autoloads: t
;;; End:
;;; gnus-load.el ends here"))))


;;; dgnushack.el ends here
