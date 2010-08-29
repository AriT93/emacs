;;; infohack.el --- a hack to format info file.
;; Copyright (C)  2001  Free Software Foundation, Inc.

;; Author: Shenghuo Zhu <zsh@cs.rochester.edu>
;; Keywords: info

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

(require 'texinfmt)

(defun infohack-remove-unsupported ()
  (goto-char (point-min))
  (while (re-search-forward "@\\(end \\)?ifnottex" nil t) 
    (replace-match ""))
  (goto-char (point-min))
  (while (search-forward "\n@iflatex\n" nil t)
    (delete-region (1+ (match-beginning 0))
		   (search-forward "\n@end iflatex\n"))))

(defun infohack (file)
  (let ((dest-directory default-directory)
	(max-lisp-eval-depth (max max-lisp-eval-depth 600))
	coding-system)
    (find-file file)
    (setq buffer-read-only nil)
    (setq coding-system buffer-file-coding-system)
    (infohack-remove-unsupported)
    (texinfo-every-node-update) 
    (texinfo-format-buffer t) ;; Don't save any file.
    (setq default-directory dest-directory)
    (setq buffer-file-name 
	  (expand-file-name (file-name-nondirectory buffer-file-name)
			    default-directory))
    (setq buffer-file-coding-system coding-system)
    (if (> (buffer-size) 100000)
	(Info-split))
    (save-buffer)))

(eval-and-compile
  (when (string-match "windows-nt\\|os/2\\|emx\\|cygwin"
                      (symbol-name system-type))
    (defun subst-char-in-region (START END FROMCHAR TOCHAR &optional NOUNDO)
      "From START to END, replace FROMCHAR with TOCHAR each time it occurs.
If optional arg NOUNDO is non-nil, don't record this change for undo
and don't mark the buffer as really changed.
Both characters must have the same length of multi-byte form."
      (let ((original-buffer-undo-list buffer-undo-list)
            (modified (buffer-modified-p)))
        (if NOUNDO
            (setq buffer-undo-list t))
        (goto-char START)
        (let ((from (char-to-string FROMCHAR))
              (to (char-to-string TOCHAR)))
          (while (search-forward from END t)
            (replace-match to t t)))
        (if NOUNDO
            (progn (setq buffer-undo-list original-buffer-undo-list)
                   (set-buffer-modidifed-p modified)))))))

(defun batch-makeinfo ()
  "Emacs makeinfo in batch mode."
  (infohack (car command-line-args-left))
  (setq command-line-args-left nil))

;;; infohack.el ends here
