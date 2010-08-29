;; @(#) jsee.el -- Javadoc viewer
;; @(#) $Id: jsee.el,v 1.2 1999-03-01 14:56:25+01 ebat311 Exp ebat311 $

;; This file is not part of Emacs

;; Copyright (C) 1998 by David Ponce
;; Author:       David Ponce david.ponce@wanadoo.fr
;; Maintainer:   David Ponce david.ponce@wanadoo.fr
;; Created:      December 3 1998

;; LCD Archive Entry:
;; <el>|David Ponce|david.ponce@wanadoo.fr|
;; <docum>|
;; <date>|$Revision: 1.2 $|~/misc/|

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
;;  This package is a JDE add-on which automatically generates and view
;;  the API HTML documentation of the java file in the current buffer.

;;; Installation:
;;
;;  Put this file on your Emacs-Lisp load path and add following into your
;;  ~/.emacs startup file
;;
;;      (require 'jsee)

;;; Usage:
;;
;;  M-x `jsee-browse-api-doc' or `C-f1'
;;     generates and view the API documentation of the java file in the current
;;     buffer.
;;

;;; Customization:
;;
;;  M-x `jsee-customize' to customize all the jsee options.
;;
;;  The following variables could be set:
;;
;;  o `jde-jsee-load-hook'
;;        hook run when package has been loaded. The provided hook
;;        `jsee-default-load-hook' defines the default key mapping.
;;
;;  o `jde-jsee-doc-generator'
;;        Specifies the path to the tool to be used to generate API
;;        documentation for the program in the current buffer.
;;        The default is the JDK tool (javadoc).
;;
;;  o `jde-jsee-javadoc--d-directory'
;;        The working directory in which javadoc will generate HTML files.
;;        This directory must exists. For better results you could copy in it
;;        the `images' sub-directory of the JDK API documentation directory.
;;
;;  o `jde-jsee-javadoc-others-options'
;;        Specify javadoc options as a string of command-line arguments.
;;        The value of this variable should be a string of switches
;;        understood by javadoc, for example, "-author -version". This
;;        variable is intended to be used to set javadoc options not
;;        otherwise defined by jsee.
;;
;;  o `jde-jsee-javadoc-version-option'
;;        If on javadoc will include @version paragraphs.
;;
;;  o `jde-jsee-javadoc-nodeprecated-option'
;;        If on javadoc will exclude @deprecated paragraphs.
;;
;;  o `jde-jsee-javadoc-author-option'
;;        If on javadoc will include @author paragraphs.
;;
;;  o `jde-jsee-javadoc-noindex-option'
;;        If on javadoc will not generate method and field index.
;;
;;  o `jde-jsee-javadoc-notree-option'
;;        If on javadoc will not generate method and field index.
;;
;;  o `jde-jsee-javadoc-public-option'
;;        If on javadoc will show only public classes and members.
;;
;;  o `jde-jsee-javadoc-protected-option'
;;        If on javadoc will show protected/public classes and members.
;;
;;  o `jde-jsee-javadoc-package-option'
;;        If on javadoc will show package/protected/public classes and members.
;;
;;  o `jde-jsee-javadoc-private-option'
;;        If on javadoc will show all classes and members.
;;
;;  o `jde-jsee-get-doc-generator-options-function' 'jsee-get-javadoc-options
;;     Specifes the function used to get the documentation generator command
;;     line options. The default function provided `jsee-get-javadoc-options'
;;     builds command line options for the JDK javadoc tool.
;;
;;  o `jde-jsee-get-doc-url-function' 'jsee-get-javadoc-url
;;     Specifes the function used to get the generated HTML file URL.
;;     The default function provided `jsee-get-javadoc-url' builds an URL
;;     according to the JDK javadoc conventions. See the description of
;;     `jsee-get-javadoc-url' for details. For the JDK 1.2 javadoc tool
;;     you could use `jsee-get-javadoc1.2-url' or add "-1.1" to the variable
;;     `jde-jsee-javadoc-others-options'.

;;; Support:
;;
;;  Any comments, suggestions, bug reports or upgrade requests are welcome.
;;  Please send them to David Ponce at david.ponce@wanadoo.fr.
;;
;;  This version of jsee was developed with NTEmacs 20.3.1 under MS Windows
;;  NT 4 WKS SP3 and also tested with Emacs 20.3 under Sun Solaris 2.5.
;;  Please, let me know if it works with other OS and versions of Emacs.

;;; Code:
  
(require 'jde)
(require 'browse-url)

(defconst jsee-version "$Revision: 1.2 $"
  "jsee version number.")

;; I USE A SUBGROUP OF `jde' AND PREFIXED ALL VARIABLES WITH `jde-jsee-'
;; TO ALLOW THEIR SAVING IN A JDE PROJECT FILE.
(defgroup jde-jsee nil
  "jsee package customization."
  :group 'jde
  :prefix "jde-jsee-")

(defcustom jde-jsee-doc-generator "javadoc"
  "*The Java API Documentation Generator.
Specifies the path to the tool to be used to generate API
documentation for the program in the current buffer.
The default is the JDK tool (javadoc)."
  :group 'jde-jsee
  :type 'string
  )

(defcustom jde-jsee-javadoc--d-directory "$TEMP/jsee"
  "*The working directory in which javadoc will generate HTML files.
This directory must exists. For better results you could copy in it
the `images' sub-directory of the JDK API documentation directory."
  :group 'jde-jsee
  :type 'string
  )

(defcustom jde-jsee-javadoc-others-options ""
  "*Specify javadoc options as a string of command-line arguments.
The value of this variable should be a string of switches understood by javadoc,
for example, \"-author -version\". This variable is intended to be used to set
javadoc options not otherwise defined by jsee."
  :group 'jde-jsee
  :type 'string)

(defcustom jde-jsee-javadoc-version-option t
  "*If on javadoc will include @version paragraphs."
  :group 'jde-jsee
  :type 'boolean
  )

(defcustom jde-jsee-javadoc-nodeprecated-option nil
  "*If on javadoc will exclude @deprecated paragraphs."
  :group 'jde-jsee
  :type 'boolean
  )

(defcustom jde-jsee-javadoc-author-option t
  "*If on javadoc will include @author paragraphs."
  :group 'jde-jsee
  :type 'boolean
  )

(defcustom jde-jsee-javadoc-noindex-option t
  "*If on javadoc will not generate method and field index."
  :group 'jde-jsee
  :type 'boolean
  )

(defcustom jde-jsee-javadoc-notree-option t
  "*If on javadoc will not generate method and field index."
  :group 'jde-jsee
  :type 'boolean
  )

(defcustom jde-jsee-javadoc-public-option nil
  "*If on javadoc will show only public classes and members."
  :group 'jde-jsee
  :type 'boolean
  )

(defcustom jde-jsee-javadoc-protected-option t
  "*If on javadoc will show protected/public classes and members."
  :group 'jde-jsee
  :type 'boolean
  )

(defcustom jde-jsee-javadoc-package-option nil
  "*If on javadoc will show package/protected/public classes and members."
  :group 'jde-jsee
  :type 'boolean
  )

(defcustom jde-jsee-javadoc-private-option nil
  "*If on javadoc will show all classes and members."
  :group 'jde-jsee
  :type 'boolean
  )

(defcustom jde-jsee-get-doc-generator-options-function 'jsee-get-javadoc-options
"*Specifes the function used to get the documentation generator command line options.
The default function provided builds command line options for the JDK javadoc tool."
  :group 'jde-jsee
  :type 'function
  )

(defcustom jde-jsee-get-doc-url-function 'jsee-get-javadoc-url
"*Specifes the function used to get the generated HTML file URL.
The default function provided builds an URL according to the JDK javadoc
conventions. See `jsee-get-javadoc-url' for details."
  :group 'jde-jsee
  :type 'function
  )

(defcustom jde-jsee-load-hook '(jsee-default-load-hook)
   "*Hook run when package has been loaded. The default hook provided
`jsee-default-load-hook' maps the java-mode key `C-f1' to the
`jsee-browse-api-doc' command."
  :group 'jde-jsee
  :type 'hook
  )

(defun jsee-customize ()
  "Customization of the group jde-jsee."
  (interactive)
  (customize-group "jde-jsee"))

(defun jsee-version-number ()
  "Returns jsee version number."
  (string-match "[0123456789.]+" jsee-version)
  (match-string 0 jsee-version))

(defun jsee-display-version ()
  "Displays jsee version."
  (interactive)
  (message "Using 'jsee' version %s." (jsee-version-number)))

(defun jsee-fullpath (path)
  "Returns the full path of the given path.
~ (HOME) and environment variables references are expanded."
  (expand-file-name (substitute-in-file-name path))
  )

(defun jsee-get-package-name ()
  "Straight copied from `jde-wiz-get-package-name'."
  (let ((package-re "package[ \t]+\\(.*\\)[ \t]*;"))
    (save-excursion
      (goto-char (point-max))
      (when (re-search-backward package-re (point-min) t)
        (looking-at package-re)
        (buffer-substring-no-properties
         (match-beginning 1)
         (match-end 1)))))
  )

(defun jsee-get-javadoc--d-directory ()
  (if jde-jsee-javadoc--d-directory
      (file-name-as-directory (jsee-fullpath jde-jsee-javadoc--d-directory))
    "")
  )

(defun jsee-build-javadoc-classpath-option ()
  (if jde-compile-option-classpath
      (jde-build-classpath-arg
       jde-compile-option-classpath jde-quote-classpath)
    (if jde-global-classpath
        (jde-build-classpath-arg
         jde-global-classpath jde-quote-classpath)))
  )

(defun jsee-get-javadoc-options ()
  ""
  (let ((options (jsee-build-javadoc-classpath-option)))
    (if jde-jsee-javadoc-version-option
        (setq options (concat options " -version")))
    (if jde-jsee-javadoc-nodeprecated-option
        (setq options (concat options " -nodeprecated")))
    (if jde-jsee-javadoc-author-option
        (setq options (concat options " -author")))
    (if jde-jsee-javadoc-noindex-option
        (setq options (concat options " -noindex")))
    (if jde-jsee-javadoc-notree-option
        (setq options (concat options " -notree")))
    (if jde-jsee-javadoc-public-option
        (setq options (concat options " -public")))
    (if jde-jsee-javadoc-protected-option
        (setq options (concat options " -protected")))
    (if jde-jsee-javadoc-package-option
        (setq options (concat options " -package")))
    (if jde-jsee-javadoc-private-option
        (setq options (concat options " -private")))
    (if jde-jsee-javadoc--d-directory
        (setq options (concat options " -d " (jsee-get-javadoc--d-directory))))
    (if (not (string-equal jde-jsee-javadoc-others-options ""))
        (setq options (concat options " " jde-jsee-javadoc-others-options)))
    options)
  )

(defun jsee-make-doc-generator-command ()
  (concat jde-jsee-doc-generator " " 
          (funcall jde-jsee-get-doc-generator-options-function) 
          " "
          (file-name-nondirectory buffer-file-name))
  )

(defun jsee-get-javadoc-url ()
  "Builds the URL of the generated HTML file to browse according to the JDK
javadoc conventions. For example, javadoc -d /tmp/apidoc MyProgram.java,
produces an HTML file /tmp/apidoc/my.package.MyProgram.html."
  (let ((package-name(jsee-get-package-name)))
    (if package-name
        (setq package-name (concat package-name "."))
      (setq package-name ""))
    (concat (jsee-get-javadoc--d-directory)
            package-name
            (file-name-sans-extension (file-name-nondirectory buffer-file-name))
            ".html"
            ))
  )


(defun jsee-get-javadoc1.2-url ()
  "Builds the URL of the generated HTML file to browse according to the JDK 1.2
javadoc conventions. For example, javadoc -d /tmp/apidoc MyProgram.java,
produces an HTML file /tmp/apidoc/my/package/MyProgram.html."
  (let ((sep (char-to-string directory-sep-char))
        (package-name(jsee-get-package-name)))
    (if package-name
        (setq package-name
              (concat (mapconcat 'identity
                                 (split-string package-name "\\.")
                                 sep)
                      sep))
      (setq package-name ""))
    (concat (jsee-get-javadoc--d-directory)
            package-name
            (file-name-sans-extension (file-name-nondirectory buffer-file-name))
            ".html"
            ))
  )

(defvar jsee-doc-url nil
  "URL of the generated HTML file to browse."
  )

(defun jsee-run-doc-generator ()
  "Runs the Java API Documentation Generator."
  (let ((compile-command (jsee-make-doc-generator-command)))
    ;; Force save-some-buffers to use the minibuffer
    ;; to query user about whether to save modified buffers.
    ;; Otherwise, when user invokes jde-compile from
    ;; menu, save-some-buffers tries to popup a menu
    ;; which seems not to be supported--at least on
    ;; the PC.
    (if (eq system-type 'windows-nt)
        (let ((temp last-nonmenu-event))
          ;; The next line makes emacs think that jde-compile
          ;; was invoked from the minibuffer, even when it
          ;; is actually invoked from the menu-bar.
          (setq last-nonmenu-event t)
          (save-some-buffers (not compilation-ask-about-save) nil)
          (setq last-nonmenu-event temp))
      (save-some-buffers (not compilation-ask-about-save) nil))
    (let ((compilation-process-setup-function 'jsee-doc-generator-process-setup))
      (setq jsee-doc-url (funcall jde-jsee-get-doc-url-function))
      (compile-internal compile-command "No more errors")
      )
    )
  )

(defun jsee-doc-generator-process-setup ()
  "Sets up `compilation-finish-function' for the documentation generator."
  (make-local-variable 'jsee-doc-url)
  (set (make-local-variable 'compilation-finish-function)
       'jsee-compilation-finish-function)
  )

(defun jsee-compilation-finish-function (buffer msg)
  "The `compilation-finish-function' for the documentation generator."
  (if (string-equal msg "finished\n")
      (progn
        (message "browse-url-of-file %s" jsee-doc-url)
        (browse-url-of-file jsee-doc-url)
        ))
  )

(defun jsee-browse-api-doc ()
  "Runs the Java API Documentation Generator for the Java source file
in the current buffer and browses the resulting HTML file."
  (interactive)
  (if (or (eq major-mode 'jde-mode) (eq major-mode 'java-mode))
      (jsee-run-doc-generator)
    (message "Invalid major mode found. Must be 'java-mode' or 'jde-mode'."))
  )

(defun jsee-default-load-hook ()
  "Hook run when package has been loaded. It maps the keys `C-f1' to
`jsee-browse-api-doc'."
  (define-key java-mode-map [C-f1] 'jsee-browse-api-doc)
  )

(provide 'jsee)
(run-hooks 'jde-jsee-load-hook)

;;; Change History:

;;
;; $Log: jsee.el,v $
;; Revision 1.2  1999-03-01 14:56:25+01  ebat311
;; FIXED - when a java file doesn't have a package name the
;; `jsee-get-javadoc-url' and `jsee-get-javadoc1.2-url' functions
;; built invalid URL.
;; Thanks to Leif Jonsson <albedo@hem2.passagen.se> who
;; has reported this bug and suggested the correction.
;;
;; Revision 1.1  1998/12/08 09:43:55  ebat311
;; Initial revision
;;
;;

;;; jsee.el ends here.
