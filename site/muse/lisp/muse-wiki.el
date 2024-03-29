;;; muse-wiki.el --- wiki features for Muse

;; Copyright (C) 2005, 2006 Free Software Foundation, Inc.

;; Author: Yann Hodique <Yann.Hodique@lifl.fr>
;; Keywords:

;; This file is part of Emacs Muse.  It is not part of GNU Emacs.

;; Emacs Muse is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published
;; by the Free Software Foundation; either version 2, or (at your
;; option) any later version.

;; Emacs Muse is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with Emacs Muse; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;;; Contributors:

;; Per B. Sederberg (per AT med DOT upenn DOT edu) made it so that all
;; files in a Muse project can become implicit links.

;;; Code:

(require 'muse-regexps)
(require 'muse-mode)

(eval-when-compile
  (require 'muse-colors))

(defgroup muse-wiki nil
  "Options controlling the behavior of Emacs Muse Wiki features."
  :group 'muse-mode)

(defcustom muse-wiki-use-wikiword t
  "Whether to use color and publish bare WikiNames."
  :type 'boolean
  :group 'muse-wiki)

(defcustom muse-wiki-allow-nonexistent-wikiword nil
  "Whether to color bare WikiNames that don't have an existing file."
  :type 'boolean
  :group 'muse-wiki)

(defcustom muse-wiki-match-all-project-files nil
  "If non-nil, Muse will color and publish implicit links to any
file in your project, regardless of whether its name is a WikiWord."
  :type 'boolean
  :group 'muse-wiki)

(defcustom muse-wiki-ignore-implicit-links-to-current-page nil
  "If non-nil, Muse will not recognize implicit links to the current
page, both when formatting and publishing."
  :type 'boolean
  :group 'muse-wiki)

(defvar muse-wiki-project-file-regexp nil
  "Regexp used to match the files in the current project.

This is set by `muse-wiki-update-project-file-regexp' automatically
when `muse-wiki-martch-all-project-files' is non-nil.")
(make-variable-buffer-local 'muse-wiki-project-file-regexp)

(defun muse-wiki-update-project-file-regexp ()
  "Update a local copy of `muse-wiki-project-file-regexp' to include
all the files in the project."
  ;; see if the user wants to match project files
  (when muse-wiki-match-all-project-files
    (let ((files (mapcar #'car (muse-project-file-alist (muse-project)))))
      (setq muse-wiki-project-file-regexp
            (when files
              (concat "\\("
                      ;; include all files from the project
                      (regexp-opt files 'words)
                      "\\)"))))
    ;; update coloring setup
    (when (featurep 'muse-colors)
      (muse-configure-highlighting
       'muse-colors-markup muse-colors-markup))))

(add-hook 'muse-update-values-hook
          'muse-wiki-update-project-file-regexp)
(add-hook 'muse-project-file-alist-hook
          'muse-wiki-update-project-file-regexp)

(defcustom muse-wiki-wikiword-regexp
  (concat "\\<\\(\\(?:[" muse-regexp-upper
          "]+[" muse-regexp-lower "]+\\)\\(?:["
          muse-regexp-upper "]+[" muse-regexp-lower "]+\\)+\\)")
  "Regexp used to match WikiWords."
  :set (function
        (lambda (sym value)
          (set sym value)
          (when (featurep 'muse-colors)
            (muse-configure-highlighting
             'muse-colors-markup muse-colors-markup))))
  :type 'regexp
  :group 'muse-wiki)

(defcustom muse-wiki-ignore-bare-project-names nil
  "Determine whether project names without a page specifer are links.

If non-nil, project names without a page specifier will not be
considered links.

When nil, project names without a specifier are highlighted and
they link to the default page of the project that they name."
  :type 'boolean
  :group 'muse-wiki)

(defvar muse-wiki-interwiki-regexp ""
  "Regexp that matches all interwiki links.

This is automatically generated by setting `muse-wiki-interwiki-alist'.
It can also be set by calling `muse-wiki-update-interwiki-regexp'.")

(defcustom muse-wiki-interwiki-delimiter "#\\|::"
  "Delimiter regexp used for InterWiki links.

If you use groups, use only shy groups."
  :type 'regexp
  :group 'muse-wiki)

(defcustom muse-wiki-interwiki-replacement ": "
  "Regexp used for replacing `muse-wiki-interwiki-delimiter' in
InterWiki link descriptions.

If you want this replacement to happen, you must add
`muse-wiki-publish-pretty-interwiki' to
`muse-publish-desc-transforms'."
  :type 'regexp
  :group 'muse-wiki)

(eval-when-compile
  (defvar muse-wiki-interwiki-alist))

(defun muse-wiki-project-files-with-spaces (&optional project)
  "Return a list of files in PROJECT that have spaces."
  (setq project (muse-project project))
  (let ((flist nil))
    (save-match-data
      (mapcar (function (lambda (file)
                          (when (string-match " " (car file))
                            (setq flist (cons (car file) flist)))))
              (muse-project-file-alist project)))
    flist))

(defun muse-wiki-update-interwiki-regexp ()
  "Update the value of `muse-wiki-interwiki-regexp' based on
`muse-wiki-interwiki-alist' and `muse-project-alist'."
  (when muse-project-alist
    (setq muse-wiki-interwiki-regexp
          (concat "\\<\\(" (regexp-opt (mapcar #'car muse-project-alist))
                  (when muse-wiki-interwiki-alist
                    (let ((interwiki-rules (mapcar #'car
                                                   muse-wiki-interwiki-alist)))
                      (when interwiki-rules
                        (concat "\\|" (regexp-opt interwiki-rules)))))
                  "\\)\\(?:\\(" muse-wiki-interwiki-delimiter
                  "\\)\\("
                  (when muse-wiki-match-all-project-files
                  ;; append the files from the project
                    (let ((files nil))
                      (dolist (proj muse-project-alist)
                        (setq files
                              (nconc (muse-wiki-project-files-with-spaces
                                      (car proj))
                                     files)))
                      (when files
                        (concat (regexp-opt files) "\\|"))))
                  "\\sw+\\)?\\)?\\>"))
    (when (featurep 'muse-colors)
      (muse-configure-highlighting 'muse-colors-markup muse-colors-markup))))

(defcustom muse-wiki-interwiki-alist
  '(("EmacsWiki" . "http://www.emacswiki.org/cgi-bin/wiki/"))
  "A table of WikiNames that refer to external entities.

The format of this table is an alist, or series of cons cells.
Each cons cell must be of the form:

  (WIKINAME . STRING-OR-FUNCTION)

The second part of the cons cell may either be a STRING, which in most
cases should be a URL, or a FUNCTION.  If a function, it will be
called with one argument: the tag applied to the Interwiki name, or
nil if no tag was used.  If the cdr was a STRING and a tag is used,
the tag is simply appended.

Here are some examples:

  (\"JohnWiki\" . \"http://alice.dynodns.net/wiki?\")

Referring to [[JohnWiki::EmacsModules]] then really means:

  http://alice.dynodns.net/wiki?EmacsModules

If a function is used for the replacement text, you can get creative
depending on what the tag is.  Tags may contain any alphabetic
character, any number, % or _.  If you need other special characters,
use % to specify the hex code, as in %2E.  All browsers should support
this."
  :type '(repeat (cons (string :tag "WikiName")
                       (choice (string :tag "URL") function)))
  :set (function
        (lambda (sym value)
          (set sym value)
          (muse-wiki-update-interwiki-regexp)))
  :group 'muse-wiki)

(add-hook 'muse-update-values-hook
          'muse-wiki-update-interwiki-regexp)

(defun muse-wiki-resolve-project-page (&optional project page)
  "Return the published path from the current page to PAGE of PROJECT.

If PAGE is not specified, use the value of :default in PROJECT.

If PROJECT is not specified, default to first project of
`muse-projects-alist'.

Note that PAGE can have several output directories.  If this is
the case, we will use the first one that matches our current
style and has the same link suffix, ignoring the others.  If no
style has the same link suffix as the current publishing style,
use the first style we find."
  (setq project (or (and project
                         (muse-project project))
                    (car muse-project-alist))
        page (or page (muse-get-keyword :default
                                        (cadr project))))
  (let* ((page-path (muse-project-page-file page project))
         (remote-styles (when page-path (muse-project-applicable-styles
                                         page-path (cddr project))))
         (local-style (muse-project-current-output-style)))
    (cond ((and remote-styles local-style muse-publishing-p)
           (muse-project-resolve-link page local-style remote-styles))
          ((not muse-publishing-p)
           (if page-path
               page-path
             (when muse-wiki-allow-nonexistent-wikiword
               ;; make a path to a nonexistent file in project
               (setq page-path (expand-file-name
                                page (car (cadr project))))
               (if (and muse-file-extension
                        (not (string= muse-file-extension "")))
                   (concat page-path "." muse-file-extension)
                 page-path)))))))

(defun muse-wiki-handle-implicit-interwiki (&optional string)
  "If STRING or point has an interwiki link, resolve it and
return the first match.

Match 1 is set to the link.
Match 2 is set to the description."
  (when (if string (string-match muse-wiki-interwiki-regexp string)
          (looking-at muse-wiki-interwiki-regexp))
    (let* ((project (match-string 1 string))
           (subst (cdr (assoc project muse-wiki-interwiki-alist)))
           (word (if string
                     (and (match-beginning 3)
                          (substring string (match-beginning 3)))
                   (match-string 3 string))))
      (if subst
          (if (functionp subst)
              (funcall subst word)
            (concat subst word))
        (and (assoc project muse-project-alist)
             (or word (not muse-wiki-ignore-bare-project-names))
             (muse-wiki-resolve-project-page project word))))))

(defun muse-wiki-handle-explicit-interwiki (&optional string)
  "If STRING or point has an interwiki link, resolve it and
return the first match.

Match 1 is set to the link.
Match 2 is set to the description."
  (let ((right-pos (if string (length string) (match-end 1))))
    (when (if string (string-match muse-wiki-interwiki-regexp string)
            (looking-at muse-wiki-interwiki-regexp))
      (let* ((project (match-string 1 string))
             (subst (cdr (assoc project muse-wiki-interwiki-alist)))
             (word (when (match-end 2)
                     (if string
                         (substring string (match-end 2))
                       (if right-pos
                           (buffer-substring (match-end 2)
                                             right-pos))))))
        (if (and (null word)
                 right-pos
                 (not (= right-pos (match-end 1))))
            ;; if only a project name was found, it must take up the
            ;; entire string or link
            nil
          (if subst
              (if (functionp subst)
                  (funcall subst word)
                (concat subst word))
            (and (assoc project muse-project-alist)
                 (or word (not muse-wiki-ignore-bare-project-names))
                 (muse-wiki-resolve-project-page project word))))))))

(defun muse-wiki-handle-wikiword (&optional string)
  "If STRING or point has a WikiWord, return it.

Match 1 is set to the WikiWord."
  (when (and (or (and muse-wiki-match-all-project-files
                      muse-wiki-project-file-regexp
                      (if string
                          (string-match muse-wiki-project-file-regexp string)
                        (looking-at muse-wiki-project-file-regexp)))
                 (and muse-wiki-use-wikiword
                      (if string
                          (string-match muse-wiki-wikiword-regexp string)
                        (looking-at muse-wiki-wikiword-regexp))))
             (cond
              (muse-wiki-allow-nonexistent-wikiword
               t)
              ((and muse-wiki-ignore-implicit-links-to-current-page
                    (string= (match-string 1 string) (muse-page-name)))
               nil)
              ((and (muse-project-of-file)
                    (muse-project-page-file
                     (match-string 1 string) muse-current-project t))
               t)
              ((file-exists-p (match-string 1 string))
               t)
              (t nil)))
    (match-string 1 string)))

;; Prettifications

(defcustom muse-wiki-publish-small-title-words
  '("the" "and" "at" "on" "of" "for" "in" "an" "a")
  "Strings that should be downcased in a page title.

This is used by `muse-wiki-publish-pretty-title', which must be
called manually."
  :type '(repeat string)
  :group 'muse-wiki)

(defcustom muse-wiki-hide-nop-tag t
  "If non-nil, hide <nop> tags when coloring a Muse buffer."
  :type 'boolean
  :group 'muse-wiki)

(defun muse-wiki-publish-pretty-title (&optional title explicit)
  "Return a pretty version of the given TITLE.

If EXPLICIT is non-nil, TITLE will be returned unmodified."
  (unless title (setq title (or (muse-publishing-directive "title") "")))
  (if (or explicit
          (save-match-data (string-match muse-url-regexp title)))
      title
    (save-match-data
      (let ((case-fold-search nil))
        (while (string-match (concat "\\([" muse-regexp-lower
                                     "]\\)\\([" muse-regexp-upper
                                     "0-9]\\)")
                             title)
          (setq title (replace-match "\\1 \\2" t nil title)))
        (let* ((words (split-string title))
               (w (cdr words)))
          (while w
            (if (member (downcase (car w))
                        muse-wiki-publish-small-title-words)
                (setcar w (downcase (car w))))
            (setq w (cdr w)))
          (mapconcat 'identity words " "))))))

(defun muse-wiki-publish-pretty-interwiki (desc &optional explicit)
  "Replace instances of `muse-wiki-interwiki-delimiter' with
`muse-wiki-interwiki-replacement'."
  (if (or explicit
          (save-match-data (string-match muse-url-regexp desc)))
      desc
    (muse-replace-regexp-in-string muse-wiki-interwiki-delimiter
                                   muse-wiki-interwiki-replacement
                                   desc)))

;; Coloring setup

(eval-after-load "muse-colors"
  '(progn
     (defun muse-wiki-colors-nop-tag (beg end)
       (when muse-wiki-hide-nop-tag
         (add-text-properties beg (+ beg 5)
                              '(invisible muse intangible t))))
     (defun muse-colors-wikiword-separate ()
       (add-text-properties (match-beginning 0) (match-end 0)
                            '(invisible muse intangible t)))

     (add-to-list 'muse-colors-tags
                  '("nop" nil nil nil muse-wiki-colors-nop-tag)
                  t)

     (add-to-list 'muse-colors-markup
                  '(muse-wiki-interwiki-regexp t muse-colors-implicit-link)
                  t)
     (add-to-list 'muse-colors-markup
                  '(muse-wiki-wikiword-regexp t muse-colors-implicit-link)
                  t)
     (add-to-list 'muse-colors-markup
                  '(muse-wiki-project-file-regexp t muse-colors-implicit-link)
                  t)
     (add-to-list 'muse-colors-markup
                  '("''''" ?\' muse-colors-wikiword-separate)
                  nil)

     (muse-configure-highlighting 'muse-colors-markup muse-colors-markup)))

;; Publishing setup

(eval-after-load "muse-publish"
  '(progn
     (add-to-list 'muse-publish-markup-regexps
                  '(3100 muse-wiki-interwiki-regexp 0 link)
                  t)
     (add-to-list 'muse-publish-markup-regexps
                  '(3200 muse-wiki-wikiword-regexp 0 link)
                  t)
     (add-to-list 'muse-publish-markup-regexps
                  '(3250 muse-wiki-project-file-regexp 0 link)
                  t)
     (add-to-list 'muse-publish-markup-regexps
                  '(3300 "''''" 0 "")
                  t)

     (custom-add-option 'muse-publish-desc-transforms
                        'muse-wiki-publish-pretty-interwiki)
     (custom-add-option 'muse-publish-desc-transforms
                        'muse-wiki-publish-pretty-title)))

;; Insinuate link handling

(custom-add-option 'muse-implicit-link-functions
                   'muse-wiki-handle-implicit-interwiki)
(custom-add-option 'muse-implicit-link-functions
                   'muse-wiki-handle-wikiword)

(custom-add-option 'muse-explicit-link-functions
                   'muse-wiki-handle-explicit-interwiki)

(add-to-list 'muse-implicit-link-functions
             'muse-wiki-handle-implicit-interwiki t)
(add-to-list 'muse-implicit-link-functions
             'muse-wiki-handle-wikiword t)

(add-to-list 'muse-explicit-link-functions
             'muse-wiki-handle-explicit-interwiki t)

;; Obsolete functions

(defun muse-wiki-update-custom-values ()
  (muse-display-warning
   (concat "Please remove `muse-wiki-update-custom-values' from"
           " `muse-mode-hook'.  Its use is now deprecated.")))

(provide 'muse-wiki)
;;; muse-wiki.el ends here
