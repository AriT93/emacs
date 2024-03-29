;;; org-find-file.el --- select org files from title and tags. -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2023 Howard X. Abrams
;;
;; Author: Howard X. Abrams <http://gitlab.com/howardabrams>
;; Maintainer: Howard X. Abrams <howard.abrams@gmail.com>
;; Created: January 8, 2023
;;
;; This file is not part of GNU Emacs.
;;
;; *NB:* Do not edit this file. Instead, edit the original literate file at:
;;            ~/website/Technical/Emacs/org-find-file-tags-2.org
;;       And tangle the file to recreate this one.
;;
;;; Code:

(defun org-find-file (file &optional directory)
  "Load file from FILE-TUPLE like `find-file'.

  If called interactively, first ask for a TAG and then limit the
  files displayed based on if they have a headline that contains
  that TAG."
  (interactive (list (org-find-file--choose-file directory)))
  (find-file file))

(defun org-find-file--choose-file (&optional directory)
  "docstring"
  (let* ((default-directory (if (project-current)
                                (project-root (project-current))
                              (or directory default-directory)))
         (file-choices (org-find-file--file-choices))
         (chosen-file  (completing-read "File: " file-choices)))
    (alist-get chosen-file file-choices nil nil 'equal)))

(defun org-find-file--file-choices ()
  "Return alist of file _labels_ and the file references."
  (let ((titles  (org-find-file--gather-titles))
        (tags    (org-find-file--gather-tags)))
    (seq-map (lambda (entry)
               (seq-let (file title) entry
                 (cons (org-find-file--file-format file title (gethash file tags))
                       file)))
             titles)))

(defun org-find-file--file-format (file title tags)
  "Return a nicely format string containing the parameters."
  (let* ((title-color `(:foreground ,(face-attribute 'org-document-title :foreground)))
         (title-str    (string-trim title))
         (title-pretty (propertize title-str 'face title-color))
         (tag-str      (string-join " ")))
    (format "%s : %s %s" file title-pretty tag-str)))

(defun org-find-file--gather-titles ()
  "Return list "
  (thread-last "rg --ignore-case --no-heading --no-line-number '^#\\+title:'"
               (shell-command-to-list)
               (--map (split-string it ":"))
               (--map (list (nth 0 it) (nth 2 it)))))

(defun shell-command-to-list (command)
  "Return call to COMMAND as a list of strings for each line."
  (thread-first command
                shell-command-to-string
                (string-lines t)))

(defvar org-find-files-tag-line-re
  (rx line-start
      (or
       (seq (one-or-more "*") " " (+? any) ":"
            (group (one-or-more (any alnum "@_#%:"))) ":")
       (seq "#+tags:" (one-or-more space)
            (group (one-or-more (any alnum "@_#%" space)))))
      line-end)
  "Regular expression that matches either headline or global file tags.")

(defun org-find-file--gather-tags ()
  "Return hash-table of key as filename, and values are tags.
Note that the tags are _all_ tags in the file."
  (let ((results  (make-hash-table :test 'equal))
        (tag-list (thread-last (format "rg --ignore-case --no-heading --no-line-number '%s'"
                                       (rxt-elisp-to-pcre org-find-files-tag-line-re))
                               (shell-command-to-list)
                               (--map (split-string it ":")))))
    (dolist (entry tag-list)
      (seq-let (file ignored tags) entry
        (let ((prev-tags (gethash file results))
              (new-tags  (org-find-file--massage-tags tags)))
          (puthash file (seq-union prev-tags new-tags) results))))
    results))

(defun org-find-file--massage-tags (tag-string)
  "Return TAG-STRING as a list of tags.
For instance, the string: foo:bar -> '(\"foo\" \"bar\")"
  (let* ((tag-separators (rx (1+ (any space ":"))))
         (tag-list       (split-string tag-string tag-separators t)))
    (--map (concat ":" it) tag-list)))

(provide 'org-find-file)
;;; org-find-file.el ends here
