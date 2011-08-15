;; vbp-mode.el --- A mode for editing Visual Basic project files.

;; Very little specialised editing here.  The real point is to load
;; all the files named in the vbp file.
;; Author : Kevin Whitefoot <kevin.whitefoot@nopow.abb.no>
;; Version: 0.1 (11th October 1996)
;; Keywords: languages, basic, Evil
;;
;; This file is NOT part of GNU Emacs but the same permissions apply.
;;
;; GNU Emacs  is free software;  you can redistribute it and/or modify
;; it under the terms of  the GNU General  Public License as published
;; by  the Free Software  Foundation;  either version  2, or (at  your
;; option) any later version.
;;
;; GNU  Emacs is distributed  in the hope that  it will be useful, but
;; WITHOUT    ANY  WARRANTY;  without even the     implied warranty of
;; MERCHANTABILITY or FITNESS FOR A  PARTICULAR PURPOSE.  See the  GNU
;; General Public License for more details.
;;
;; You should have received  a copy of  the GNU General Public License
;; along with GNU Emacs; see  the file COPYING.  If  not, write to the
;; Free Software Foundation, 675  Mass Ave, Cambridge, MA 02139,  USA.
;; This  program  is free  software;  you  can  redistribute it and/or
;; modify it  under  the terms of the  GNU  General Public License  as
;; published by the Free Software  Foundation; either version 2 of the
;; License, or (at your option) any later version.

;;; Commentary:
 
;; Purpose of this package: 
;;  This is a mode for editing project files for programs written in
;;  The World's Most Successful Programming Language.  Its principal
;;  feature is that loading a VBP file causes all the source files
;;  named in it to be loaded too.

;;  At the moment it has a number of functions that are, apart from
;;  their names, identical to those in visual-basic-mode.  Later these
;;  should be abstracted into a support file and used for both.

;; Installation instructions
;;  Put vbp-mode.el somewhere in your path, compile it, and add the
;;  following to your init file:

;;  (autoload 'vbp-mode "vbp-mode" "VBP mode." t)
;;  (setq auto-mode-alist (append '(("\\.vbp$" . 
;;                                  vbp-mode)) auto-mode-alist))
;; Revisions:
;; 0.1 11th October 1998: Initial version


;; Known bugs: 
;;  None.
 
;; todo:


;;; Code:
 
(provide 'vbp-mode)

(defvar vbp-xemacs-p (string-match "XEmacs\\|Lucid" (emacs-version)))
(defvar vbp-winemacs-p (string-match "Win-Emacs" (emacs-version)))
(defvar vbp-win32-p (eq window-system 'win32))

;; Variables you may want to customize.
(defvar vbp-fontify-p t "*Whether to fontify Basic buffers.")
(defvar vbp-capitalize-keywords-p t
  "*Whether to capitalize BASIC keywords.")

(defvar visual-basic-ide-pathname nil
  "*The full pathname of your Visual Basic exe file, if any.
Note that this is the same variable that is created by visual-basic-mode.")


(defvar vbp-mode-syntax-table nil)
;;; At the moment I don't know of any reason for using a separate
;;; syntax table so it is just a copy of the default.
(if vbp-mode-syntax-table
    ()
  (setq vbp-mode-syntax-table (make-syntax-table)))


 
;;; Use a mode map similar to that used by visual-basic-mode so that
;;; the IDE connection works the same (if anyone ever uses it that
;;; is).
(defvar vbp-mode-map nil)
(if vbp-mode-map
    ()
  (setq vbp-mode-map (make-sparse-keymap))
  (cond (vbp-winemacs-p
	 (define-key vbp-mode-map '(control C) 'vbp-start-ide))
	(vbp-win32-p
	 (define-key vbp-mode-map (read "[?\\S-\\C-c]") 'vbp-start-ide)))
  (if vbp-xemacs-p
      (progn
	(define-key vbp-mode-map "\M-G" 'vbp-grep)
	(define-key vbp-mode-map '(meta backspace) 'backward-kill-word))))


;; These abbrevs are valid only in a code context.
(defvar vbp-mode-abbrev-table nil)

(defvar vbp-mode-hook ())


;; This is some approximation of the set of reserved words in a vbp file.

(eval-and-compile
  (defvar vbp-all-keywords '("AutoIncrementVer"
			     "BoundsCheck"
			     "Class"
			     "CodeViewDebugInfo"
			     "Command32"
			     "CompatibleMode"
			     "CompilationType"
			     "ExeName32"
			     "FDIVCheck"
			     "FavorPentiumPro"
			     "FavorPentiumPro(tm)"
			     "FlPointCheck"
			     "Form"
			     "HelpContextID"
			     "HelpFile"
			     "MajorVer"
			     "MaxNumberOfThreads"
			     "MinorVer"
			     "Module"
			     "Name"
			     "NoAliasing"
			     "OptimizationType"
			     "OverflowCheck"
			     "Path32"
			     "Project"
			     "Reference"
			     "RelatedDoc"
			     "RevisionVer"
			     "ServerSupportFiles"
			     "StartMode"
			     "Startup"
			     "StartupProject"
			     "ThreadPerObject"
			     "Title"
			     "Unattended"
			     "UnroundedFP"
			     "UserControl"
			     "VersionCompanyName" )))


(defvar vbp-font-lock-keywords
  (eval-when-compile
    `((,(concat "\\<" (regexp-opt vbp-all-keywords t) "\\>")
       1 font-lock-keyword-face))))

(put 'vbp-mode 'font-lock-keywords 'vbp-font-lock-keywords)

(defun vbp-mode ()
  "A mode for editing Microsoft Visual Basic project files.
Really has little to do with editing but exists, principally, to automatically load the files belonging to the project.
Commands:
\\{vbp-mode-map}"
  (interactive)
  (kill-all-local-variables)
  (use-local-map vbp-mode-map)
  (setq major-mode 'vbp-mode)
  (setq mode-name "VB Project")
  (set-syntax-table vbp-mode-syntax-table)

  (add-hook 'local-write-file-hooks 'vbp-untabify)

  (setq local-abbrev-table vbp-mode-abbrev-table)
  (if vbp-capitalize-keywords-p
      (progn
	(make-local-variable 'pre-abbrev-expand-hook)
	(add-hook 'pre-abbrev-expand-hook 'vbp-pre-abbrev-expand-hook)
	(abbrev-mode 1)))

  (if vbp-fontify-p
      (vbp-enable-font-lock))
  (add-hook 'find-file-hooks 'vbp-load-associated-files)

  (run-hooks 'vbp-mode-hook))


(defun vbp-enable-font-lock ()
  ;; Emacs 19.29 requires a window-system else font-lock-mode errs out.
  (cond ((or vbp-xemacs-p window-system)
	 ;; In win-emacs this sets font-lock-keywords back to nil!
	 (if vbp-winemacs-p
	     (font-lock-mode 1))

	 ;; Accomodate emacs 19.29+
	 ;; From: Simon Marshall <Simon.Marshall@esrin.esa.it>
	 (cond ((boundp 'font-lock-defaults)
		(make-local-variable 'font-lock-defaults)
		(setq font-lock-defaults
		      '((vbp-font-lock-keywords)
			nil t (("_" . "w")))))
	       (t
		(make-local-variable 'font-lock-keywords)
		(setq font-lock-keywords vbp-font-lock-keywords)))

	 (if vbp-winemacs-p
	     (font-lock-fontify-buffer)
	   (font-lock-mode 1)))))


(defun vbp-construct-keyword-abbrev-table ()
  (if vbp-mode-abbrev-table
      nil
    (let ((words vbp-all-keywords)
	  (word nil)
	  (list nil))
      (while words
	(setq word (car words)
	      words (cdr words))
	(setq list (cons (list (downcase word) word) list)))
      (define-abbrev-table 'vbp-mode-abbrev-table list))))

;; Would like to do this at compile-time.
(vbp-construct-keyword-abbrev-table)


(defun vbp-pre-abbrev-expand-hook ()
  ;; Allow our abbrevs only in a code context.
  (setq local-abbrev-table
	(if (vbp-in-code-context-p)
	    vbp-mode-abbrev-table)))




(defun vbp-untabify ()
  "Do not allow any tabs into the file."
  (if (eq major-mode 'vbp-mode)
      (untabify (point-min) (point-max)))
  nil)


(defun vbp-default-tag ()
  (if (and (not (bobp))
	   (save-excursion
	     (backward-sexp)
	     (looking-at "\\w")))
      (backward-word 1))
  (let ((s (point))
	(e (save-excursion
	     (forward-sexp)
	     (point))))
    (buffer-substring s e)))


(defun vbp-grep (tag)
  "Search BASIC source files in current directory for TAG."
  (interactive
   (list (let* ((def (vbp-default-tag))
		(tag (read-string
		      (format "Grep for [%s]: " def))))
	   (if (string= tag "") def tag))))
  (grep (format "grep -n %s %s" tag vbp-wild-files)))


;;; IDE Connection.

(defun vbp-buffer-project-file ()
  "Return the project file name associated with the current buffer."
  (buffer-file-name))

(defun vbp-start-ide ()
  "Start Visual Basic (or your favorite IDE, (after Emacs, of course))
on the first project file in the current directory.
Note: it's not a good idea to leave Visual Basic running while you
are editing in Emacs, since Visual Basic has no provision for reloading
changed files."
  (interactive)
  (let (file)
    (cond ((null vbp-ide-pathname)
	   (error "No pathname set for Visual Basic.  See vbp-ide-pathname"))
	  ((null (setq file (vbp-buffer-project-file)))
	   (error "No project file found"))
	  ((fboundp 'win-exec)
	   (iconify-emacs)
	   (win-exec vbp-ide-pathname 'win-show-normal file))
	  ((fboundp 'start-process)
	   (iconify-frame (selected-frame))
	   (start-process "*VisualBasic*" nil vbp-ide-pathname file))
	  (t
	   (error "No way to spawn process!")))))



;;; Load associated files listed in class, module etc. lines
;;; Also load project files of vbg files

(defvar vbp-associated-projects-reg-exp
  "\\(Project\\|StartupProject\\) *= *\\(.*\\)$")

(defvar vbp-associated-files-reg-exp
  "\\(class\\|module\\)=\\w+ *; *\\(.*\\)$")

(defvar vbp-associated-forms-reg-exp
  "^ *form *= *\\(.*\\)$")

(defun vbp-load-associated-files ()
  "Load files named in module and class lines of vbp files and project lines of vbg files."
  (interactive)
  (message "Load files named in project file: %s" buffer-file-name)
  (vbp-load-files-by-reg-exp vbp-associated-forms-reg-exp 1)
  (vbp-load-files-by-reg-exp vbp-associated-files-reg-exp 2)
  (vbp-load-files-by-reg-exp vbp-associated-projects-reg-exp 2))




(defun vbp-load-files-by-reg-exp (reg-exp match)
  "Load files by searching the current buffer for lines matching reg-exp. 
Match tells which element to use as the file name.  If the file name
is relative it is relative to the directory containing the current
buffer.  If the file is already loaded nothing happens, this prevents
circular references causing trouble.  Modifies point so use inside
save-excursion."
  (goto-char (point-min))  
  (while (re-search-forward reg-exp (point-max) t)
    (vbp-load-file-ifnotloaded (match-string match) default-directory)))



(defun vbp-load-file-ifnotloaded (file default-directory)
  "Load file if not already loaded.  
If file is relative then default-directory provides the path"
  (let((file-absolute (expand-file-name file default-directory)))
    (if (get-file-buffer file-absolute); don't do anything if the buffer is already loaded
	()
      (find-file-noselect file-absolute ))))




;;; vbp-mode.el ends here
