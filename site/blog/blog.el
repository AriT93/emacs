
;;; blog.el --- major mode for adding entries to a blosxom blog

;; Copyright (C) 2006 by Ari Turetzky

;; Author: Ari Turetzky <arit93@yahoo.com>
;; Maintainer: Ari Turetzky <arit93@yahoo.com>
;; Version: see `blog-mode-version'
;; Keywords: blog blosxom

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2 of
;; the License, or (at your option) any later version.

;; This program is distributed in the hope that it will be
;; useful, but WITHOUT ANY WARRANTY; without even the implied
;; warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
;; PURPOSE.  See the GNU General Public License for more details.

;; You should have received a copy of the GNU General Public
;; License along with this program; if not, write to the Free
;; Software Foundation, Inc., 59 Temple Place, Suite 330, Boston,
;; MA 02111-1307 USA

;;; Commentary:

;; to use this include the following in your .emacs
;; (add-to-list 'load-path (expand-file-name "/path/to/blog.el"))
;; (require 'blog)

;; This package provides some very basic functions:
;; write-blog:  prompts for a title and opens a buffer
;;  to write the blog entry
;; write-blog-finish:  saves the blog entry to
;; blog-root as timestamp.  blog-file-extension
;; blog-make-para:  inserts <p></p> tag
;; write-blog-daily:  prompts for title, weight and workout
;;  then calls write-blog and passes those values.  this will
;;  add the <div> section for displaying the workout info
;;  This is completely gratuitous and probably only needed by me
;;  but if you want to use your blog to track your progress in weight
;; loss it is nice.

;; TO DO
;; =====

;; add blog-buffer -- allow you to save a buffer as a blog entry --done
;; add blog-region -- allow you to save a region as a blog entry --done
;; add htmlize -- allow you to run htmlize on a region/bugger and
;;  write that to a blog entry --80%
;; add ability to retrieve a past blog entry for editing
;; add ability to save to other blogging engines, ie. Moveable Type,
;;  Wordpress, blogger etc

;; Warning
;; =======

;; My grasp on Lisp and emacs-lisp is tenous at best.
;; please be kind


;; This file is NOT part of GNU Emacs


;;; History:
;;

;;; Code:
(defgroup blog nil
  "Blogging mode for use with blosxom"
  :group 'blog)

(defvar blog-mode-version "0.1"
  "Version reported by blog mode.")

(defvar blog-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "\C-c\C-c" 'write-blog-finish)
    (define-key map "\C-c\C-p" 'blog-make-para)
    map))
(defvar blog-file-name nil)


(defcustom blog-root nil
  "Root directory of the blog when saving."
  :type '(string)
  :group 'blog)

(defcustom blog-file-extension ".txt"
  "The extension to add for blog entries."
  :type '(string)
  :group 'blog)

(defun blog-mode-version ()
  "Reports the version of `blog-mode'."
  (interactive)
  (message "Blogging with Version: %s" blog-mode-version)
)
(defun blog-mode ()
  "Mode for blogging with blosxom."
  (interactive)
  (kill-all-local-variables)
  (use-local-map blog-mode-map)
  (setq mode-name "blog")
  (setq major-mode 'blog-mode)
  (run-hooks 'blog-mode-hook))


(defcustom blog-mode-hook nil
  "Hook run when entering blog mode."
  :type 'hook
  :group 'blog)

(defcustom blog-mode-write-blog-hook nil
  "Hook run after a blog has been written"
  :type 'hook
  :group 'blog)

(defun blog-mode-hook-identify()
  "show that the mode has run"
  (make-local-variable 'blog-title))

(add-hook 'blog-mode-hook 'blog-mode-hook-identify)
(add-hook 'blog-mode-hook 'turn-on-auto-fill)

(setq blog-imenu-generic-expression
      '(("Blog" "^<blog>.*" 1)))
(add-hook 'blog-mode-hook
          (lambda ()
                (setq imenu-generic-expression blog-imenu-generic-expression)))

(defun write-blog (blog-name &optional blog-weight blog-workout)
  "Create a blog entry.
Argument BLOG-NAME name of the blog.
Optional argument BLOG-WEIGHT weight for the day.
Optional argument BLOG-WORKOUT workout for the day."
  (interactive "sName: ")
  (setq blog-file-name blog-name)
  (setq blog-title blog-name)
  (pop-to-buffer (get-buffer-create (concat "*" blog-name "*")))
  (blog-mode)
  (insert blog-name "\n\n")
  (if blog-weight
      (insert (concat "<div class=\"workout\">\nWeight:" blog-weight "<br/>\n"
                      "Workout:" blog-workout "<br/>\n</div>\n\n")))
  (blog-make-para)
;;  (wrap-blog-mmm-mode)
  (setq blog-file-name (format-time-string "%Y%m%d%H%M%S"))
  (message "press C-c C-c when done editing %s" blog-name ))

(defun write-blog-daily(blog-name blog-weight blog-workout)
  "Write a daily blog this will have the section with weight and workout"
  (interactive "sTitle: \nsWeight: \nsWorkout: ")
  (write-blog blog-name blog-weight blog-workout))

(defun wrap-blog-mmm-mode()
  "Wrap the blog entry in <blog></blog> tags so mmm-mode will font-lock the
entry "
  (interactive)
  (save-excursion
  (goto-char (point-min))
  (goto-line 2)
  ;;(insert "<blog>\n")
  (goto-char (point-max))
;;  (insert "\n</blog>"))
))

(defun write-blog-finish ()
  "Save the temporary buffer as a blog entry."
  (interactive)
  (message blog-title)
  (write-file (concat blog-root blog-file-name blog-file-extension))
  (message "write-blog-finish %s " blog-file-name )
  (kill-buffer (current-buffer))
  (setq blog-file-name nil)
  (other-window 1)
  (delete-other-windows)
  (run-hook-with-args 'blog-mode-write-blog-hook blog-title)
  (tramp-cleanup-all-connections))

(defun blog-make-para ()
 "Insert a paragraph tag."
 (interactive)
 (save-excursion)
 (goto-char (point-max))
 (insert "<p></p>")
 (backward-char 4)
 (insert" ")
 (backward-char 1))

(defun blog-buffer (&optional name)
  "Blog the current buffer.
Optional argument NAME name for the blog entry."
  (interactive)
  (let ((p (point-min)) (m (point-max)))
    (kill-ring-save p m)
    ( if name
        (write-blog name)
      (write-blog (substring (buffer-name) 0 (string-match "[.]" (buffer-name)))))
    (yank)))

(defun blog-region (name beg end)
  "Blog the current region.
Argument NAME name for the blog entry.
Argument BEG point pos.
Argument END mark pos."
  (interactive "sName: \nr")
  (let ((p (point)) (m (mark)))
    (kill-ring-save p m)
    (write-blog name)
    (yank)))

(defun blog-buffer-html(name)
  "htmlize the buffer then blog it"
  (interactive "sName: ")
  (setq htmlize-output-type "font")
  (let ((buffer (htmlize-buffer)))
    (set-buffer buffer)
    (blog-buffer name)
    (kill-buffer buffer))
  (setq htmlize-output-type "css"))
;;testing

(defun blog-region-html(name beg end)
  "htmlize the region then blog it"
  (interactive "sName: \nr")
  (setq htmlize-output-type "font")
  (let ((buffer (htmlize-region beg end)))
    (set-buffer buffer)
    (blog-buffer name)
    (kill-buffer buffer ))
  (setq htmlize-output-type "css"))

(defun blog-grep-edit-blog(regexp)
  "use find-grep-dired to edit a blog"
  (interactive "sregexp:")
  (find-grep-dired blog-root regexp))

(provide 'blog)

;;; blog.el ends here
