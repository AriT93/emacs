;; @(#) jpack.el -- Csharp package statement generator
;; @(#) $Id: csde-package.el,v 1.1 2001/02/25 05:36:05 paulk Exp $

;; Adapted from the JDE by Matt Bruce <matt.bruce@morganstanley.com>

;; Copyright (C) 2001 by Matt Bruce
;; Maintainer:  Matt Bruce

;; This file is not part of Emacs

;; JDE Copyright (C) 1998, 2000, 2001 by David Ponce
;; JDE Author:       David Ponce <david@dponce.com>
;; JDE Maintainer:   David Ponce <david@dponce.com>
;;               Paul Kinnucan <paulk@mediaone.net>
;; Created:      September 28 1998

;; COPYRIGHT NOTICE
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
;; along with this program; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Description:
;;
;;  This package automatically generates a Csharp package statement.  
;;  The package name is deducted from the current classpath setting of CSDE. When
;;  a directory found in classpath is a root of the current buffer default
;;  directory, the relative path of the default directory from the classpath one
;;  becomes the package name by substituting directory separators by '.'.
;;
;;  For example:
;;      My CSDE classpath setting is ("~/csharp" "/jdk1.1.6/lib/classes.zip")
;;      I edit the file ~/csharp/FR/test/MyClass.cs
;;      The package name generated will be `FR.test'.
;;
;;      My CSDE classpath setting is ("~/csharp" "~/csharp/test" "/jdk1.1.6/lib/classes.zip")
;;      I edit the file ~/csharp/test/MyClass.cs
;;      "// Default package used." will be generated because the default package can be used.
;;
;;      My CSDE classpath setting is ("~/csharp" "/jdk1.1.6/lib/classes.zip")
;;      I edit the file /usr/csharp/MyClass.cs
;;      No package name will be generated because the directory /usr/csharp is not
;;      accessible from classpath.

;;; Usage:
;;
;;  M-x `csde-package-update' to update the Csharp package statement or insert a new
;;                             at top of buffer.
;;  The function `csde-package-generate-package-statement' could be used in template to
;;  automatically generate the package statement.

;;; Customization:
;;
;;  M-x `csde-package-customize' to customize all the csde-package options.
;;
;;  The following variables could be set:
;;
;;  o `csde-package-load-hook'
;;         hook run when package has been loaded.
;;
;;  o `csde-package-package-comment'
;;         Csharp line comment appended to the generated package statement.
;;         Default to " // Generated package name"
;;
;;  o `csde-package-default-package-comment'
;;         Csharp line comment generated when the default package is used.
;;         Default to "// Default package used"
;;
;;  o `csde-package-search-classpath-variables'
;;         The list of variables where to search the current classpath list.
;;         Default to '(csde-compile-option-classpath csde-global-classpath)

;;; Support:

;; The latest version of the CSDE is available at
;; <URL:http://www.sourceforge.com/>.

;; Please send any comments, bugs, or upgrade requests to
;; Matt Bruce (matt.bruce@morganstanley.com)


;;; Code:


(defconst csde-package-unknown-package-name
  "*unknown*"
  "The string returned when a package name can't be generated.")

(defconst csde-package-package-regexp
  "package .*;.*$"
  "The regexp used to find the Csharp package statement.")

(defgroup csde-package nil
  "csde-package package customization"
  :group 'tools
  :prefix "csde-package-")

(defcustom csde-package-load-hook nil
  "*Hook run when package has been loaded."
  :group 'csde-package
  :type 'hook
  )

(defcustom csde-package-package-comment " // Generated package name"
  "*Csharp line comment appended to the generated package statement.
An empty string suppress the generation of this comment."
  :group 'csde-package
  :type 'string
  )

(defcustom csde-package-default-package-comment "// Default package used"
  "*Csharp line comment generated when the default package is used.
An empty string suppress the generation of this comment."
  :group 'csde-package
  :type 'string
  )

(defcustom csde-package-search-classpath-variables
  '(csde-compile-option-classpath csde-global-classpath)
  "*Specify the variables where to search the current classpath list.
The first one which has a non nil value will be used by csde-package."
  :group 'csde-package
  :type '(repeat variable)
  )

;;;###autoload
(defun csde-package-customize ()
  "Customization of the group csde-package."
  (interactive)
  (customize-group "csde-package"))


(defun csde-package-fullpath (path)
  "Returns the full path of the given path.
~ (HOME) and environment variables references are expanded."
  (expand-file-name (substitute-in-file-name path)))

(defun csde-package-get-classpath ()
  "Returns the current classpath list. That is to say the first non-nil value
found in the variables given by `csde-package-search-classpath-variables'."
  (let ((search-in csde-package-search-classpath-variables)
        (classpath))
    (catch 'csde-package-classpath
      (while search-in
        (setq classpath (symbol-value (car search-in)))
        (if classpath
            (throw 'csde-package-classpath classpath))
        (setq search-in (cdr search-in))))))

(defun csde-package-get-directories-in-classpath ()
  "Returns the list of directories found in classpath."
  (mapcan '(lambda (path)
             (unless (string= path ".") ; "." is ignored in classpath
               (let ((path (csde-package-fullpath path)))
                 (when (file-directory-p path)
                   (list (file-name-as-directory path))))))
          (csde-package-get-classpath)))

(defun csde-package-get-current-directory ()
  "Returns the full path of the current directory."
  (csde-package-fullpath default-directory))

(defun csde-package-seach-package-directories ()
  "Returns a list of package directory candidates or nil if none found."
  (let ((dir (csde-package-get-current-directory))
        (case-fold-search (eq system-type 'windows-nt))) ; case-insensitive for Windows
    (mapcan '(lambda (root)
               (let ((root (regexp-quote root)))
                 (message "Seaching %S in %S..." root dir)
                 (and (string-match root dir)
                      (list (substring dir (match-end 0))))))
            (csde-package-get-directories-in-classpath))))

(defun csde-package-get-package-directory ()
  "Returns the package directory found or `csde-package-unknown-package-name'
if none found."
  (or (csde-package-best-package-candidate (csde-package-seach-package-directories))
      csde-package-unknown-package-name))

(defun csde-package-best-package-candidate (candidates)
  "Returns the best package directory candidate from the given list of
package directories. The best is the shortest one that could be found."
  (car (sort candidates '(lambda (dir1 dir2) (string-match (regexp-quote dir1) dir2)))))

(defun csde-package-convert-directory-to-package (dir)
  "Converts the given directory path to a valid Csharp package name
by replacing `directory-sep-char' by '.' and removing extra
`directory-sep-char' at end."
  (if (string= dir "")
      ""
    (subst-char-in-string directory-sep-char ?.
                          (substring (file-name-as-directory dir) 0 -1)
                          t)))

;;;###autoload
(defun csde-package-generate-package-statement ()
  "Generates a Csharp package statement. If the package name cant be generated
this function returns an empty string."
  (let ((package-name (csde-package-get-package-directory)))
    (cond ((string= package-name csde-package-unknown-package-name)
           (message "The current directory is not accessible from classpath.")
           "")
          ((string= package-name "")
           (message "Default package used.")
           csde-package-default-package-comment)
          (t (message "package %s;%s"
                      (csde-package-convert-directory-to-package package-name)
                      csde-package-package-comment)))))

;;;###autoload
(defun csde-package-update ()
  "Updates or replaces the Csharp package statement in the current buffer.
If the package statement does not exist a new one is inserted at the
top of the buffer. If the package name cant be generated nothing is done.
The major mode of current buffer must be 'csde-mode'."
  (interactive)
  (if (eq major-mode 'csde-mode)
      (let ((package (csde-package-generate-package-statement)))
        (unless (string= package "")
          (goto-char (point-min))
          (if (re-search-forward csde-package-package-regexp nil t)
              (replace-match package)
            (progn
              (insert package)
              (newline)))))
    (message "Invalid major mode found. Must be 'csde-mode'.")))

(provide 'csde-package)
(run-hooks 'csde-package-load-hook)

;;; Change History:

;; $Log: csde-package.el,v $
;; Revision 1.1  2001/02/25 05:36:05  paulk
;; Initial XEmacs version.
;;
;; Revision 1.2  2001/02/22 03:36:30  paulk
;; Removed require for csde to fix infinite recursion loading bug.
;;
;; Revision 1.1  2001/02/21 05:52:02  paulk
;; Initial revision.
;;

;;
;; $Log: csde-package.el,v $
;; Revision 1.1  2001/02/25 05:36:05  paulk
;; Initial XEmacs version.
;;
;; Revision 1.2  2001/02/22 03:36:30  paulk
;; Removed require for csde to fix infinite recursion loading bug.
;;
;; Revision 1.1  2001/02/21 05:52:02  paulk
;; Initial revision.
;;

;;; csde-package.el ends here.
