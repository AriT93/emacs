;;; blog.el --- write and edit entries for a Hugo blog kept in git  -*- lexical-binding: t; -*-

;; Copyright (C) 2006 by Ari Turetzky

;; Author: Ari Turetzky <arit93@yahoo.com>
;; Maintainer: Ari Turetzky <arit93@yahoo.com>
;; Version: see `blog-mode-version'
;; Keywords: blog hugo

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
;; (setq blog-repo "~/path/to/clone/of/turetzky.org/")

;; Version 0.2 writes for the Hugo blog (it used to feed blosxom over
;; TRAMP).  A post is a Markdown file with front matter in a local git
;; clone of the blog repository (hugo/content/posts/).  Finishing a
;; post commits it, rebases onto the remote (so posts written in the
;; web editor are picked up) and pushes; the site rebuilds from git
;; about a minute later.  There is no ssh to the web server any more.

;; Commands:
;; write-blog:  prompts for a title and opens a buffer holding the new
;;  post, front matter first.  Edit it freely, including the front
;;  matter.
;; write-blog-finish (C-c C-c):  saves the post, commits, pulls
;;  --rebase and pushes.  The buffer is only killed once that worked.
;; write-blog-daily:  prompts for title, weight and workout; these go
;;  into the front matter (body_weight, workout) and the site draws the
;;  Weight/Workout box and the weight graph from them.  Blank values
;;  are left out.
;; blog-edit-post:  pick a past entry (newest first) and edit it; C-c
;;  C-c commits and pushes the change.
;; blog-buffer, blog-region:  make a new post from a buffer or region.
;; blog-make-para:  inserts <p></p> tag
;; blog-grep-edit-blog:  grep the posts, in dired.

;; Authoring needs write access to the repository: git push over
;; ssh (LAN) or https with a Forgejo access token kept by a git
;; credential helper.  The clone must be on `blog-git-branch'.

;; TO DO
;; =====

;; add htmlize -- allow you to run htmlize on a region/buffer and
;;  write that to a blog entry --80%
;; image upload (the web editor has it; here put files in hugo/static/)

;; Warning
;; =======

;; My grasp on Lisp and emacs-lisp is tenous at best.
;; please be kind


;; This file is NOT part of GNU Emacs


;;; History:
;; 0.2 Hugo + git instead of blosxom + TRAMP.
;; 0.1 blosxom, entries written to blog-root over TRAMP.

;;; Code:
(require 'subr-x)

(defgroup blog nil
  "Blogging mode for use with a Hugo blog kept in git."
  :group 'blog)

(defvar blog-mode-version "0.2"
  "Version reported by blog mode.")

(defvar blog-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "\C-c\C-c" 'write-blog-finish)
    (define-key map "\C-c\C-p" 'blog-make-para)
    map))

(defcustom blog-repo nil
  "Local clone of the blog repository (the one with hugo/ in it).
Must be checked out on `blog-git-branch'.  May be a TRAMP path."
  :type '(choice (const nil) directory)
  :group 'blog)

(defcustom blog-posts-dir "hugo/content/posts/"
  "Where posts live, relative to `blog-repo'."
  :type '(string)
  :group 'blog)

(defcustom blog-file-extension ".md"
  "The extension to add for new blog entries."
  :type '(string)
  :group 'blog)

(defcustom blog-git-remote "origin"
  "Git remote that holds the published branch (Forgejo)."
  :type '(string)
  :group 'blog)

(defcustom blog-git-branch "master"
  "Branch the site is published from."
  :type '(string)
  :group 'blog)

(defcustom blog-push t
  "Non-nil: finishing a post also pulls --rebase and pushes it.
Nil: only commit locally."
  :type 'boolean
  :group 'blog)

(defcustom blog-site-url "https://turetzky.org/blog/"
  "Base URL, only used for the message shown after publishing."
  :type '(string)
  :group 'blog)

(defvar-local blog-title nil
  "Title of the post being written in this buffer.")

(defun blog-mode-version ()
  "Reports the version of `blog-mode'."
  (interactive)
  (message "Blogging with Version: %s" blog-mode-version))

(define-derived-mode blog-mode text-mode "blog"
  "Mode for writing posts for the Hugo blog.
\\{blog-mode-map}"
  (setq-local blog-title nil)
  (setq imenu-generic-expression '(("Title" "^title: \\(.*\\)" 1))))

(defcustom blog-mode-hook nil
  "Hook run when entering blog mode."
  :type 'hook
  :group 'blog)

(defcustom blog-mode-write-blog-hook nil
  "Hook run after a blog has been written (and pushed).
Called with the post title."
  :type 'hook
  :group 'blog)

(add-hook 'blog-mode-hook 'turn-on-auto-fill)

;;;; git plumbing

(defconst blog--git-buffer "*blog git*")

(defun blog--repo ()
  "Return `blog-repo' as an expanded directory name, or signal."
  (unless blog-repo
    (user-error "Set `blog-repo' to your clone of the blog repository"))
  (let ((dir (file-name-as-directory (expand-file-name blog-repo))))
    (unless (file-directory-p (expand-file-name ".git" dir))
      (user-error "%s is not a git clone (no .git)" dir))
    (unless (file-directory-p (expand-file-name blog-posts-dir dir))
      (user-error "%s has no %s" dir blog-posts-dir))
    dir))

(defun blog--git (&rest args)
  "Run git with ARGS in `blog-repo'; return (EXIT . OUTPUT)."
  (let ((default-directory (blog--repo)))
    (with-temp-buffer
      (let ((code (apply #'process-file "git" nil t nil args)))
        (with-current-buffer (get-buffer-create blog--git-buffer)
          (goto-char (point-max))
          (insert (format "$ git %s\n" (string-join args " "))))
        (cons code (buffer-string))))))

(defun blog--git! (&rest args)
  "Run git with ARGS; on failure show the output and signal an error."
  (let ((res (apply #'blog--git args)))
    (with-current-buffer (get-buffer-create blog--git-buffer)
      (goto-char (point-max))
      (insert (cdr res) "\n"))
    (unless (eq (car res) 0)
      (display-buffer blog--git-buffer)
      (user-error "git %s failed (%s): %s" (car args) (car res)
                  (string-trim (cdr res))))
    (cdr res)))

(defun blog--check-branch ()
  "Refuse to publish from a clone that is not on `blog-git-branch'."
  (let ((branch (string-trim (blog--git! "rev-parse" "--abbrev-ref" "HEAD"))))
    (unless (equal branch blog-git-branch)
      (user-error "%s is on branch %s, not %s: switch it, or use a dedicated clone"
                  (blog--repo) branch blog-git-branch))))

;;;; building a post

(defun blog-slug (title)
  "File name part for TITLE: lower case letters, digits and dashes."
  (let* ((s (downcase title))
         (s (replace-regexp-in-string "[^a-z0-9]+" "-" s))
         (s (replace-regexp-in-string "\\`-+\\|-+\\'" "" s)))
    (if (string= s "") "post" (substring s 0 (min 60 (length s))))))

(defun blog--yaml-string (s)
  "S as a double quoted YAML string."
  (concat "\""
          (replace-regexp-in-string "\"" "\\\\\"" (replace-regexp-in-string "\\\\" "\\\\\\\\" s))
          "\""))

(defun blog--now ()
  "Current time the way Hugo wants it: seconds and a UTC offset."
  (format-time-string "%Y-%m-%dT%H:%M:%S%:z"))

(defun blog--front-matter (title weight workout)
  "Front matter for TITLE, with WEIGHT and WORKOUT when they are not blank."
  (concat "---\n"
          "title: " (blog--yaml-string title) "\n"
          "date: " (blog--now) "\n"
          (unless (string-blank-p (or weight ""))
            (concat "body_weight: " (blog--yaml-string (string-trim weight)) "\n"))
          (unless (string-blank-p (or workout ""))
            (concat "workout: " (blog--yaml-string (string-trim workout)) "\n"))
          "---\n\n"))

(defun blog--field (name)
  "Value of front matter line NAME in the current buffer, unquoted, or nil."
  (save-excursion
    (goto-char (point-min))
    (when (looking-at-p "---[ \t]*$")
      (forward-line 1)
      (let ((end (save-excursion (when (re-search-forward "^---[ \t]*$" nil t) (point)))))
        (when (and end (re-search-forward (format "^%s:[ \t]*\\(.*?\\)[ \t]*$" name) end t))
          (let ((v (match-string 1)))
            (cond ((string-match "\\`\"\\(.*\\)\"\\'" v)
                   (replace-regexp-in-string "\\\\\\(.\\)" "\\1" (match-string 1 v)))
                  ((string-match "\\`'\\(.*\\)'\\'" v)
                   (replace-regexp-in-string "''" "'" (match-string 1 v)))
                  (t v))))))))

(defun blog--check-front-matter ()
  "Signal unless the buffer has a title and a date Hugo can parse."
  (let ((title (blog--field "title")) (date (blog--field "date")))
    (when (or (null title) (string-blank-p title))
      (user-error "No `title:' in the front matter"))
    (when date
      (unless (string-match-p
               "\\`[0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}\\(T[0-9]\\{2\\}:[0-9]\\{2\\}:[0-9]\\{2\\}\\(Z\\|[+-][0-9]\\{2\\}:[0-9]\\{2\\}\\)\\)?\\'"
               date)
        (user-error "`date: %s' needs seconds and an offset, e.g. 2022-12-14T08:50:06-08:00" date)))
    title))

(defun blog--new-file ()
  "Path for the new post in this buffer, from its date and title."
  (let* ((dir (expand-file-name blog-posts-dir (blog--repo)))
         (date (or (blog--field "date") (blog--now)))
         (day (substring date 0 10))
         (base (concat day "-" (blog-slug (blog--field "title"))))
         (n 1) (file (concat dir base blog-file-extension)))
    (while (file-exists-p file)
      (setq n (1+ n) file (format "%s%s-%d%s" dir base n blog-file-extension)))
    file))

;;;; commands

(defun write-blog (blog-name &optional blog-weight blog-workout)
  "Create a blog entry.
Argument BLOG-NAME name (title) of the blog.
Optional argument BLOG-WEIGHT weight for the day.
Optional argument BLOG-WORKOUT workout for the day."
  (interactive "sName: ")
  (blog--check-branch)
  (pop-to-buffer (generate-new-buffer (concat "*" blog-name "*")))
  (blog-mode)
  (setq blog-title blog-name)
  (insert (blog--front-matter blog-name blog-weight blog-workout))
  (message "press C-c C-c when done editing %s" blog-name))

(defun write-blog-daily (blog-name blog-weight blog-workout)
  "Write a daily blog; this will have the weight and workout box."
  (interactive "sTitle: \nsWeight: \nsWorkout: ")
  (write-blog blog-name blog-weight blog-workout))

(defun blog-edit-post ()
  "Pick a past entry (newest first) and edit it; C-c C-c publishes the change."
  (interactive)
  (blog--check-branch)
  (let* ((dir (expand-file-name blog-posts-dir (blog--repo)))
         (names (sort (directory-files dir nil "\\`[^.]") (lambda (a b) (string> a b))))
         (pick (completing-read "Edit post: " names nil t)))
    (find-file (expand-file-name pick dir))
    (blog-mode)
    (setq blog-title (or (blog--field "title") pick))
    (message "press C-c C-c to publish your changes to %s" pick)))

(defun write-blog-finish ()
  "Save the post, commit it, rebase onto the remote and push."
  (interactive)
  (unless (derived-mode-p 'blog-mode)
    (user-error "Not a blog buffer"))
  (blog--check-branch)
  (let* ((title (blog--check-front-matter))
         (new (null buffer-file-name))
         (file (or buffer-file-name (blog--new-file)))
         (repo (blog--repo))
         (rel (file-relative-name file repo)))
    (if new (write-file file) (save-buffer))
    (blog--git! "add" "--" rel)
    ;; commit only this file, whatever else is staged in the clone
    (let ((res (blog--git "diff" "--cached" "--quiet" "--" rel)))
      (if (eq (car res) 0)
          (message "Nothing changed in %s" rel)
        (blog--git! "commit" "-m" (format "%s post \"%s\"" (if new "Create" "Edit") title) "--" rel)
        (when blog-push
          (let ((pull (blog--git "pull" "--rebase" "--autostash" blog-git-remote blog-git-branch)))
            (unless (eq (car pull) 0)
              (blog--git "rebase" "--abort")
              (display-buffer blog--git-buffer)
              (user-error "Saved and committed %s, but pull --rebase failed: %s (see *blog git*)"
                          rel (string-trim (cdr pull)))))
          (blog--git! "push" blog-git-remote blog-git-branch)
          (message "Published %s: %s%s.html (live in about a minute)"
                   rel blog-site-url (file-name-base file)))))
    (kill-buffer (current-buffer))
    (delete-other-windows)
    (run-hook-with-args 'blog-mode-write-blog-hook title)))

(defun blog-make-para ()
  "Insert a paragraph tag."
  (interactive)
  (goto-char (point-max))
  (insert "<p></p>")
  (backward-char 4)
  (insert " ")
  (backward-char 1))

(defun blog-buffer (&optional name)
  "Blog the current buffer.
Optional argument NAME name for the blog entry."
  (interactive)
  (let ((text (buffer-substring-no-properties (point-min) (point-max)))
        (title (or name (read-string "Name: "
                                     (substring (buffer-name) 0 (string-match "[.]" (buffer-name)))))))
    (write-blog title)
    (goto-char (point-max))
    (insert text)))

(defun blog-region (name beg end)
  "Blog the current region.
Argument NAME name for the blog entry.
Argument BEG point pos.
Argument END mark pos."
  (interactive "sName: \nr")
  (let ((text (buffer-substring-no-properties beg end)))
    (write-blog name)
    (goto-char (point-max))
    (insert text)))

(defvar htmlize-output-type)
(declare-function htmlize-buffer "htmlize")
(declare-function htmlize-region "htmlize")

(defun blog-buffer-html (name)
  "Htmlize the buffer (NAME is the title), then blog it."
  (interactive "sName: ")
  (setq htmlize-output-type "font")
  (let ((buffer (htmlize-buffer)))
    (with-current-buffer buffer
      (blog-buffer name))
    (kill-buffer buffer))
  (setq htmlize-output-type "css"))

(defun blog-region-html (name beg end)
  "Htmlize the region, then blog it as NAME."
  (interactive "sName: \nr")
  (setq htmlize-output-type "font")
  (let ((buffer (htmlize-region beg end)))
    (with-current-buffer buffer
      (blog-buffer name))
    (kill-buffer buffer))
  (setq htmlize-output-type "css"))

(defun blog-grep-edit-blog (regexp)
  "Use find-grep-dired to find REGEXP in the posts."
  (interactive "sregexp:")
  (find-grep-dired (expand-file-name blog-posts-dir (blog--repo)) regexp))

(provide 'blog)

;;; blog.el ends here
