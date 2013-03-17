;;; js-utils.el --- 
;; 
;; Filename: bookmarkletify-js.el
;; Description: 
;; Author: 
;; Maintainer: 
;; Created: 2011-12-27T04:12:56+0100
;; Version: 
;; Last-Updated: 
;;           By: 
;;     Update #: 0
;; URL: 
;; Keywords: 
;; Compatibility: 
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;; Commentary: 
;; 
;; 
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;; Change Log:
;; 
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3, or
;; (at your option) any later version.
;; 
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;; 
;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;; Code:



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; plovr

(defcustom jsut-plovr-jar-file "PATH-TO/plovr-96feca4d303b.jar"
  "Path to plovr jar file..
For information about plovr see URL `http://plovr.com/'."
  :type 'file
  :group 'js-utils)

(defsubst jsut-plovr-file (js-file) (concat js-file ".plovr.js"))
(defsubst jsut-plovr-src-file (plovr-file) (replace-regexp-in-string "\.plovr\.js$" "" plovr-file))

;;;###autoload
(defun jsut-plovr-dev-info (plovr-file)
  "Get info for how to start plovr in dev mode for PLOVR-FILE."
  (interactive (list (buffer-file-name)))
  (when (or (string-match-p "\.plovr.js$" plovr-file)
            (y-or-n-p "File does not end in .plovr.js - are you sure it is a plovr conf file? "))
    (let* ((buf (get-buffer-create "*JSUT plovr dev info*"))
           (plovr-buf (find-buffer-visiting plovr-file))
           (was-visiting plovr-buf)
           id)
      (setq plovr-buf (or plovr-buf (find-file-noselect plovr-file)))
      (with-current-buffer plovr-buf
        (let ((here (point)))
          (save-restriction
            (widen)
            (goto-char (point-min))
            (search-forward "{")
            (let ((json (json-read-object)))
              (goto-char here)
              (setq id (cdr (assoc 'id json)))))))

      (with-current-buffer buf
        (erase-buffer)
        (insert
         "To start plovr as a compiling server enter this in a command shell:\n"
         "  java -jar "
         (shell-quote-argument (convert-standard-filename jsut-plovr-jar-file))
         " serve "
         (shell-quote-argument (file-name-nondirectory plovr-file))
         "\n\nThen access the compiled js file with\n"
         "  http://localhost:9810/compile?id=" id
         ))
      (display-buffer buf))))

;;;###autoload
(defun jsut-plovr-edit-conf (js-file)
  "Edit plovr config file for JS-FILE."
  (interactive (list (buffer-file-name)))
  (let* ((js-file-is-plovr (string-match-p "\.plovr.js$" js-file))
         (plovr-file (if js-file-is-plovr
                         js-file
                       (jsut-plovr-file js-file)))
         (buf (find-file plovr-file)))
    (when (and (not js-file-is-plovr)
               (= 0 (buffer-size buf)))
      (let* ((plovr-template "
// See http://www.plovr.com/options.html
{
    \"id\": %S,
    \"inputs\": [
        %S,
    ],
    \"paths\": \".\",
    \"externs\": [
        %S,
    ],
    \"custom-externs-only\": false,
    \"mode\":\"advanced\",
    \"output-file\": %S,
    \"output-wrapper\": \"/* Copyright 2011 YOUR NAME */ (function(){%%output%%})();\",
    \"output-charset\": \"UTF-8\"
}")
             (id (file-name-sans-extension (file-name-nondirectory js-file)))
             (inp (file-name-nondirectory js-file))
             (ext (concat (file-name-nondirectory (file-name-sans-extension js-file)) "-externs.js"))
             (out (concat (file-name-nondirectory (file-name-sans-extension js-file)) "-cld.js"))
             (plovr-conf (format plovr-template id inp ext out))
             (buf (find-file plovr-file)))
        (with-current-buffer buf
          (insert plovr-conf))))))

(defvar jsut-plovr-sentinel nil)
(defvar jsut-plovr-buf nil)

;;;###autoload
(defun jsut-plovr-compile (js-file)
  "Compile JS-FILE with plovr/closure compiler."
  (interactive (list (buffer-file-name)))
  (if (not (file-exists-p jsut-plovr-jar-file))
      (when (y-or-n-p "Can't find plovr. Do you want to customize jsut-plovr-jar-file? ")
        (customize-option-other-window 'jsut-plovr-jar-file))
    (let* ((plovr-file (if (string-match-p "\.plovr.js$" js-file)
                           js-file
                         (jsut-plovr-file js-file)))
           (cmd-template "java -jar %s build %s"))
      (if (not (file-exists-p plovr-file))
          (if (not (y-or-n-p "A specific plovr config file is needed, but not found. Create it? "))
              (message "Can't compile without this file")
            (message "Creating stub plovr config file")
            (jsut-plovr-edit-conf js-file))
        (let ((plovr-buf (find-file-noselect plovr-file))
              (bad-msg nil)
              (compile-command (format cmd-template
                                       (shell-quote-argument
                                        (convert-standard-filename jsut-plovr-jar-file))
                                       (shell-quote-argument
                                        (convert-standard-filename
                                         (file-relative-name plovr-file))))))
          (with-current-buffer plovr-buf
            (let ((here (point)))
              (save-restriction
                (widen)
                (goto-char (point-min))
                (if (not (re-search-forward "^\s*['\"]output-file['\"]\s*:\s*['\"]\\(.*?\\)['\"]" nil t))
                    (setq bad-msg (format "Can't find output-file in %s" plovr-file))
                  (let* ((output (match-string 1))
                         (outdir (file-name-directory output))
                         (outexp (expand-file-name outdir))
                         )
                    (message "outexp=%S" outexp)
                    (unless (file-directory-p outexp)
                      (if (yes-or-no-p (format "Output dir %S does not exist. Create it? " outexp))
                          (mkdir outdir t)
                        (setq bad-msg "Can't compile")))))
              (goto-char here))))
          (if bad-msg
              (message "%s" bad-msg)
            (message "cmd=%s" compile-command)
            (let* ((buf (call-interactively 'compile))
                   (proc (when buf (get-buffer-process buf))))
              (message "buf=%S" buf)
              (when buf
                (with-current-buffer buf
                  (set (make-local-variable 'jsut-plovr-buf) plovr-buf)
                  (put 'jsut-plovr-buf 'permanent-local t)
                  (when proc
                    (let ((sent (process-sentinel proc)))
                      (set (make-local-variable 'jsut-plovr-sentinel) sent)
                      (put 'jsut-plovr-sentinel 'permanent-local t)
                      (set-process-sentinel proc 'jsut-plovr-compile-sentinel))))
              ))))))))

(defun jsut-plovr-compile-sentinel (process event)
  (with-current-buffer (process-buffer process)
    (message "%S: %S, %S" event jsut-plovr-buf jsut-plovr-sentinel)
    (funcall jsut-plovr-sentinel process event)
    (jsut-plovr-copy-more-to-output jsut-plovr-buf (current-buffer))
    ))

(defun jsut-plovr-copy-more-to-output (plovr-buf proc-buf)
  (interactive (list (current-buffer) nil))
  (with-current-buffer plovr-buf
    (save-restriction
      (widen)
      (goto-char (point-min))
      (if (not (re-search-forward "^\s*['\"]output-file['\"]\s*:\s*['\"]\\(.*?\\)['\"]" nil t))
          (error "Can't find output-file in %s" plovr-buf)
        (let* ((output-file (match-string 1))
               (output-buf (find-buffer-visiting output-file))
               )
          (when output-buf (kill-buffer output-buf))
          (setq output-buf (find-file-noselect output-file))
          (with-current-buffer output-buf (revert-buffer t t t))
          (goto-char (point-min))
          (while (re-search-forward "^\s*//\s+Add\.\\(.*?\\)\s*:\s*\"\\(.*?\\)\"" nil t)
            (let* ((w (match-string-no-properties 1))
                   (where (cond ((string= w "Last") 'last)
                                ((string= w "First") 'first)))
                   (input-file (match-string-no-properties 2))
                   (input-full (expand-file-name input-file)))
              (if (not where)
                  (message "Add %S not recognized" w)
                (message "Adding to output %s: %S" w input-file)
                (when proc-buf
                  (with-current-buffer proc-buf
                    (let ((inhibit-read-only t))
                      (insert
                       (format "\nAdding to output %s: %S" w input-file)))))
                (with-current-buffer output-buf
                  (widen)
                  (if (eq where 'last)
                      (progn
                        (goto-char (point-max))
                        (insert "\n\n"))
                    (goto-char (point-min))
                    (insert "\n\n")
                    (goto-char (point-min)))
                  (insert-file-contents input-full)))))
          (with-current-buffer output-buf (basic-save-buffer))
          (with-current-buffer proc-buf
            (let ((inhibit-read-only t))
              (insert "\nDone adding to output.")))
          )))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Bookmarklets

;;;###autoload
(defun jsut-bookmarkletify (js-bm-buffer)
  "Given js bookmarklet code make HTML suitable for adding bookmarklet.
JS-BM-BUFFER should contain the bookmarklet javascript code
source.  This may include comments and new line characters."
  (interactive (list (current-buffer)))
  (with-current-buffer js-bm-buffer
    (let* ((js-in (buffer-substring-no-properties (point-min) (point-max)))
           (js js-in)
           (outbuf (get-buffer-create "*BOOKMARK*")))
      (setq js (replace-regexp-in-string "/\\*\\(?:.\\|\n\\)*\\*/" "" js))
      (setq js (replace-regexp-in-string "\\(^\\|;\\)\s*//.*\n" "\\1" js))
      (if (string-match-p "[^\\]\"" js)
          (message "The javascript code contains doubble-quotes (\"). This can't be used in bookmarklets.")
        (setq js (replace-regexp-in-string "\n" " " js))
        (setq js (replace-regexp-in-string "\s+" " " js))
        (with-current-buffer outbuf
          (erase-buffer)
          (html-mumamo-mode)
          (insert
           "\n<a href=\"javascript:"
           js
           "; void 0;\">BOOKMARKLET</a>\n"
           "<!-- Bookmark length = " (number-to-string (length js)) " -->\n\n"
           ))
        (display-buffer outbuf)))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; jQuery bookmarklets

;;;###autoload
(defun jsut-jquery-include-it (buffer)
  ;; jqpath : "https://ajax.googleapis.com/ajax/libs/jquery/1.7.1/jquery.min.js",
  (interactive (list (current-buffer)))
  (let* ((buf-file (buffer-file-name buffer))
         (plovr-file (if (string-match-p "\.plovr.js$" buf-file)
                         buf-file
                         (jsut-plovr-file buf-file)))
         (js-file (jsut-plovr-src-file plovr-file))
         (js-buf (find-file-noselect js-file))
         (plovr-buf (find-file-noselect plovr-file))
         jqpath
         output-file)
    (with-current-buffer plovr-buf
      (let ((here (point)))
        (save-restriction
          (widen)
          (goto-char (point-min))
          (if (not (re-search-forward "^\s*['\"]output-file['\"]\s*:\s*['\"]\\(.*?\\)['\"]" nil t))
              (error (format "Can't find output-file in %s" plovr-file))
            (setq output-file (match-string 1)))
          (goto-char here))))
    (with-current-buffer js-buf
      (let ((here (point)))
        (save-restriction
          (widen)
          (unless (> (buffer-size) 0)
            (error "Output-file must be created first"))
          (goto-char (point-min))
          (if (not (re-search-forward "^\s+jqpath\s*:\s*\"\\(.*?\\)\"" nil t))
              (error (format "Can't find jqpath in %s" js-file))
            (setq jqpath (match-string 1)))
          (goto-char here))))
    (setq jqpath (replace-regexp-in-string "^https:" "http:" jqpath))
    (let* ((buf-sts (web-vcs-url-retrieve-synch jqpath))
           (jq-buf (car buf-sts)))
      (if (not jq-buf)
          (error "Could not fetch %S, status=%S" jqpath (cdr buf-sts))
        (let ((old (find-buffer-visiting output-file))
              (output-buf (find-file-noselect output-file))
              )
          (with-current-buffer output-buf
            (let ((here (point-marker)))
              (save-restriction
                (widen)
                (goto-char (point-min))
                (insert (with-current-buffer jq-buf
                          (buffer-substring-no-properties (point-min) (point-max)))
                        " "))
              (goto-char here))
            (unless old (kill-buffer))
            ))))))

(defvar jsut-bookmarklet-template      (expand-file-name "etc/js/bm-base.js"      nxhtml-install-dir))
(defvar jsut-bookmarklet-file-template (expand-file-name "etc/js/bm-base-file.js" nxhtml-install-dir))

;;;###autoload
(defun jsut-jquery-create-bookmarklet-file ()
  "Create jQuery bookmarklet JavaScript file template.
To make a bookmarklet for this you can use `jsut-jquery-mk-bookmarklet'."
  (interactive)
  (let ((buf-name "*New jQuery bookmarklet file*"))
    (if (get-buffer buf-name)
        (message "Please delete (or save) the buffer %S first!" buf-name)
      (switch-to-buffer (get-buffer-create buf-name))
      (insert-file-contents jsut-bookmarklet-file-template)
      (message "Please save the buffer to the file you want with C-x C-w"))))
    

;;;###autoload
(defun jsut-jquery-mk-bookmarklet (js-buffer)
  "Make bookmarklet javascript template.
JS-BUFFER should contain the javascript code that is loaded from
the bookmarklet.  This code should have markers like those given
by `jsut-jquery-create-bookmarklet-file'.

\(Compare `jsut-bookmarkletify' where the input is just the js
source code in the bookmarklet itself.)
"
  (interactive (list (current-buffer)))
  (let* ((re-my-namespace (rx bol (* space) "var" (+ space) "myNamespace" (* space) "=" (* space)
                              (any "'\"")
                              (submatch (* nonl))
                              (any "'\"")))

         ;; fix-me:
         (re-my-url-src (rx bol (* space) "//" (+ space) "myURL" (* space) "=" (* space)
                            (any "'\"")
                            (submatch (* nonl))
                            (any "'\"")))
         (re-my-url-bm (rx bol (* space) "var" (+ space) "myURL" (* space) "=" (* space)
                           (any "'\"")
                           (submatch (* nonl))
                           (any "'\"")))
         my-namespace
         my-url)
    (with-current-buffer js-buffer
      (let ((here (point)))
        (save-restriction
          (widen)
          (goto-char (point-min))
          (if (not (re-search-forward re-my-namespace nil t))
              (error "Can't find line with 'var myNamespace=...' in source buffer %s" js-buffer)
            (setq my-namespace (match-string 1)))
          (goto-char (point-min))
          (if (not (re-search-forward re-my-url-src nil t))
              (error "Can't find line with '// myURL=...' in source buffer %s" js-buffer)
            (setq my-url (match-string 1)))
          (goto-char here))))
    (when my-namespace
      (let* (;;(tbuf-opened t)
             (template-buf (get-buffer-create (format "*Bookmarklet source for %s*" (buffer-name js-buffer))))
              ;; (or (find-buffer-visiting jsut-bookmarklet-template)
              ;;     (setq tbuf-opened nil)
              ;;     (find-file-noselect (find-buffer-visiting jsut-bookmarklet-template))))
             )
        (with-current-buffer template-buf
          (when (= 0 (buffer-size))
            (insert-file-contents jsut-bookmarklet-template)
            (goto-char (point-min))
            (search-forward "///////")
            (forward-line)
            (delete-region (point-min) (point))
            (insert "// Bookmarklet js source for loading code in " (buffer-name js-buffer)
                    "\n"
                    "// Note: If you change myArgs you may want to save this buffer!")
            (forward-line)
            (js-mode))
          (let ((here (point)))
            (save-restriction
              (widen)
              (goto-char (point-min))
              (if (not (re-search-forward re-my-namespace nil t))
                  (error "Can't find line with 'var myNamespace=...' in template buffer %s" template-buf)
                (replace-match my-namespace t t nil 1))
              (goto-char (point-min))
              (if (not (re-search-forward re-my-url-bm nil t))
                  (error "Can't find line with 'var myURL=...' in template buffer %s" template-buf)
                (replace-match my-url t t nil 1)))
            (goto-char here)))
        (switch-to-buffer template-buf)
        ;; (jsut-bookmarkletify template-buf)
        (message "Please save the buffer to the file you want with C-x C-w")))))

;;;###autoload
(defun jsut-jquery-css-to-js (css-buffer)
  "Convert CSS in CSS-BUFFER to jQuery code.
For faster startup of jQuery bookmarklets.  \(Use plovr to
include this file so the bookmarklet is all contained in one
file.)"
  (interactive (list (current-buffer)))
  (let* ((css (with-current-buffer css-buffer
                (buffer-substring-no-properties (point-min) (point-max))))
         (unique-default (concat (buffer-name css-buffer) " " (current-time-string)))
         ;; (unique (read-string "Give it a unique id: " unique-default))
         (unique unique-default)
         (css-file (buffer-file-name css-buffer))
         (js-file (when css-file (concat css-file ".js")))
         js-buf)
    (when (and js-file
               (file-exists-p js-file))
      (setq js-buf (find-file-noselect js-file))
      (with-current-buffer js-buf
        (revert-buffer t t t)
        (when (> (buffer-size) 0)
          (widen)
          (display-buffer js-buf)
          (unless (yes-or-no-p (format "Replace current %S? " js-file))
            (setq js-buf nil)))))
    (unless js-buf (setq js-buf (get-buffer-create "*jsut-css-to-js Result*")))
    (with-current-buffer js-buf
      (widen)
      (erase-buffer)
      (insert (replace-regexp-in-string "\"" "" css))
      (goto-char (point-min))
      (insert "function addMyCss() {\n"
              "if (jQuery('head').find('style[title=\"" unique "\"]').length == 0)\n"
              "jQuery('head')\n.append('<style title=\"" unique "\" type=\"text/css\">'\n")
      (while (not (eobp))
        (insert "+\"")
        (goto-char (point-at-eol))
        (insert "\\n\"")
        (forward-line))
      (insert "+\"</style>\"\n"
              ");\n"
              "}\n")
      (js-mode)
      (when (buffer-file-name) (basic-save-buffer)))
    (display-buffer js-buf)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; js-utils.el ends here
