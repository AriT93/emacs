;;; gnus-agent.el --- unplugged support for Gnus
;; Copyright (C) 1997, 1998, 1999, 2000, 2001, 2002, 2003
;;        Free Software Foundation, Inc.

;; Author: Lars Magne Ingebrigtsen <larsi@gnus.org>
;; This file is part of GNU Emacs.

;; GNU Emacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.	 See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;;; Code:

(require 'gnus)
(require 'gnus-cache)
(require 'nnmail)
(require 'nnvirtual)
(require 'gnus-sum)
(require 'gnus-score)
(require 'gnus-srvr)
(require 'gnus-util)
(eval-when-compile
  (if (featurep 'xemacs)
      (require 'itimer)
    (require 'timer))
  (require 'cl))

(eval-and-compile
  (autoload 'gnus-server-update-server "gnus-srvr")
  (autoload 'gnus-agent-customize-category "gnus-cus")
)

(defcustom gnus-agent-directory (nnheader-concat gnus-directory "agent/")
  "Where the Gnus agent will store its files."
  :group 'gnus-agent
  :type 'directory)

(defcustom gnus-agent-plugged-hook nil
  "Hook run when plugging into the network."
  :group 'gnus-agent
  :type 'hook)

(defcustom gnus-agent-unplugged-hook nil
  "Hook run when unplugging from the network."
  :group 'gnus-agent
  :type 'hook)

(defcustom gnus-agent-fetched-hook nil
  "Hook run when finished fetching articles."
  :group 'gnus-agent
  :type 'hook)

(defcustom gnus-agent-handle-level gnus-level-subscribed
  "Groups on levels higher than this variable will be ignored by the Agent."
  :group 'gnus-agent
  :type 'integer)

(defcustom gnus-agent-expire-days 7
  "Read articles older than this will be expired.
If you wish to disable Agent expiring, see `gnus-agent-enable-expiration'."
  :group 'gnus-agent
  :type '(number :tag "days"))

(defcustom gnus-agent-expire-all nil
  "If non-nil, also expire unread, ticked and dormant articles.
If nil, only read articles will be expired."
  :group 'gnus-agent
  :type 'boolean)

(defcustom gnus-agent-group-mode-hook nil
  "Hook run in Agent group minor modes."
  :group 'gnus-agent
  :type 'hook)

;; Extracted from gnus-xmas-redefine in order to preserve user settings
(when (featurep 'xemacs)
  (add-hook 'gnus-agent-group-mode-hook 'gnus-xmas-agent-group-menu-add))

(defcustom gnus-agent-summary-mode-hook nil
  "Hook run in Agent summary minor modes."
  :group 'gnus-agent
  :type 'hook)

;; Extracted from gnus-xmas-redefine in order to preserve user settings
(when (featurep 'xemacs)
  (add-hook 'gnus-agent-summary-mode-hook 'gnus-xmas-agent-summary-menu-add))

(defcustom gnus-agent-server-mode-hook nil
  "Hook run in Agent summary minor modes."
  :group 'gnus-agent
  :type 'hook)

;; Extracted from gnus-xmas-redefine in order to preserve user settings
(when (featurep 'xemacs)
  (add-hook 'gnus-agent-server-mode-hook 'gnus-xmas-agent-server-menu-add))

(defcustom gnus-agent-confirmation-function 'y-or-n-p
  "Function to confirm when error happens."
  :version "21.1"
  :group 'gnus-agent
  :type 'function)

(defcustom gnus-agent-synchronize-flags 'ask
  "Indicate if flags are synchronized when you plug in.
If this is `ask' the hook will query the user."
  :version "21.1"
  :type '(choice (const :tag "Always" t)
		 (const :tag "Never" nil)
		 (const :tag "Ask" ask))
  :group 'gnus-agent)

(defcustom gnus-agent-go-online 'ask
  "Indicate if offline servers go online when you plug in.
If this is `ask' the hook will query the user."
  :version "21.1"
  :type '(choice (const :tag "Always" t)
		 (const :tag "Never" nil)
		 (const :tag "Ask" ask))
  :group 'gnus-agent)

(defcustom gnus-agent-mark-unread-after-downloaded t
  "Indicate whether to mark articles unread after downloaded."
  :version "21.1"
  :type 'boolean
  :group 'gnus-agent)

(defcustom gnus-agent-download-marks '(download)
  "Marks for downloading."
  :version "21.1"
  :type '(repeat (symbol :tag "Mark"))
  :group 'gnus-agent)

(defcustom gnus-agent-consider-all-articles nil
  "When non-`nil', the agent will let the agent predicate decide
whether articles need to be downloaded or not, for all articles.  When
`nil', the default, the agent will only let the predicate decide
whether unread articles are downloaded or not.  If you enable this,
groups with large active ranges may open slower and you may also want
to look into the agent expiry settings to block the expiration of
read articles as they would just be downloaded again."
  :version "21.4"
  :type 'boolean
  :group 'gnus-agent)

(defcustom gnus-agent-max-fetch-size 10000000 ;; 10 Mb
  "Chunk size for `gnus-agent-fetch-session'.
The function will split its article fetches into chunks smaller than
this limit."
  :group 'gnus-agent
  :type 'integer)

(defcustom gnus-agent-enable-expiration 'ENABLE
  "The default expiration state for each group.
When set to ENABLE, the default, `gnus-agent-expire' will expire old
contents from a group's local storage.  This value may be overridden
to disable expiration in specific categories, topics, and groups.  Of
course, you could change gnus-agent-enable-expiration to DISABLE then
enable expiration per categories, topics, and groups."
  :group 'gnus-agent
  :type '(radio (const :format "Enable " ENABLE)
                (const :format "Disable " DISABLE)))

(defcustom gnus-agent-expire-unagentized-dirs t
  "*Whether expiration should expire in unagentized directories.
Have gnus-agent-expire scan the directories under
\(gnus-agent-directory) for groups that are no longer agentized.
When found, offer to remove them."
  :type 'boolean
  :group 'gnus-agent)

(defcustom gnus-agent-auto-agentize-methods '(nntp nnimap)
  "Initially, all servers from these methods are agentized.
The user may remove or add servers using the Server buffer.
See Info node `(gnus)Server Buffer'."
  :type '(repeat symbol)
  :group 'gnus-agent)

;;; Internal variables

(defvar gnus-agent-history-buffers nil)
(defvar gnus-agent-buffer-alist nil)
(defvar gnus-agent-article-alist nil
  "An assoc list identifying the articles whose headers have been fetched.  
If successfully fetched, these headers will be stored in the group's overview
file.  The key of each assoc pair is the article ID, the value of each assoc
pair is a flag indicating whether the identified article has been downloaded
\(gnus-agent-fetch-articles sets the value to the day of the download).
NOTES:
1) The last element of this list can not be expired as some 
   routines (for example, get-agent-fetch-headers) use the last
   value to track which articles have had their headers retrieved.
2) The function `gnus-agent-regenerate' may destructively modify the value.")
(defvar gnus-agent-group-alist nil)
(defvar gnus-category-alist nil)
(defvar gnus-agent-current-history nil)
(defvar gnus-agent-overview-buffer nil)
(defvar gnus-category-predicate-cache nil)
(defvar gnus-category-group-cache nil)
(defvar gnus-agent-spam-hashtb nil)
(defvar gnus-agent-file-name nil)
(defvar gnus-agent-send-mail-function nil)
(defvar gnus-agent-file-coding-system 'raw-text)
(defvar gnus-agent-file-loading-cache nil)
(defvar gnus-agent-file-header-cache nil)

;; Dynamic variables
(defvar gnus-headers)
(defvar gnus-score)

;;;
;;; Setup
;;;

(defun gnus-open-agent ()
  (setq gnus-agent t)
  (gnus-agent-read-servers)
  (gnus-category-read)
  (gnus-agent-create-buffer)
  (add-hook 'gnus-group-mode-hook 'gnus-agent-mode)
  (add-hook 'gnus-summary-mode-hook 'gnus-agent-mode)
  (add-hook 'gnus-server-mode-hook 'gnus-agent-mode))

(defun gnus-agent-create-buffer ()
  (if (gnus-buffer-live-p gnus-agent-overview-buffer)
      t
    (setq gnus-agent-overview-buffer
	  (gnus-get-buffer-create " *Gnus agent overview*"))
    (with-current-buffer gnus-agent-overview-buffer
      (mm-enable-multibyte))
    nil))

(gnus-add-shutdown 'gnus-close-agent 'gnus)

(defun gnus-close-agent ()
  (setq gnus-category-predicate-cache nil
	gnus-category-group-cache nil
	gnus-agent-spam-hashtb nil)
  (gnus-kill-buffer gnus-agent-overview-buffer))

;;;
;;; Utility functions
;;;

(defun gnus-agent-read-file (file)
  "Load FILE and do a `read' there."
  (with-temp-buffer
    (ignore-errors
      (nnheader-insert-file-contents file)
      (goto-char (point-min))
      (read (current-buffer)))))

(defsubst gnus-agent-method ()
  (concat (symbol-name (car gnus-command-method)) "/"
	  (if (equal (cadr gnus-command-method) "")
	      "unnamed"
	    (cadr gnus-command-method))))

(defsubst gnus-agent-directory ()
  "The name of the Gnus agent directory."
  (nnheader-concat gnus-agent-directory
		   (nnheader-translate-file-chars (gnus-agent-method)) "/"))

(defun gnus-agent-lib-file (file)
  "The full name of the Gnus agent library FILE."
  (expand-file-name file
		    (file-name-as-directory
		     (expand-file-name "agent.lib" (gnus-agent-directory)))))

(defun gnus-agent-cat-set-property (category property value)
  (if value
      (setcdr (or (assq property category)
              (let ((cell (cons property nil)))
                    (setcdr category (cons cell (cdr category)))
                    cell)) value)
    (let ((category category))
      (while (cond ((eq property (caadr category))
                    (setcdr category (cddr category))
                    nil)
                   (t
                    (setq category (cdr category)))))))
  category)

(eval-when-compile
  (defmacro gnus-agent-cat-defaccessor (name prop-name)
    "Define accessor and setter methods for manipulating a list of the form
\(NAME (PROPERTY1 VALUE1) ... (PROPERTY_N VALUE_N)).
Given the call (gnus-agent-cat-defaccessor func PROPERTY1), the list may be
manipulated as follows:
  (func LIST): Returns VALUE1
  (setf (func LIST) NEW_VALUE1): Replaces VALUE1 with NEW_VALUE1."
    `(progn (defmacro ,name (category)
              (list (quote cdr) (list (quote assq)
                                      (quote (quote ,prop-name)) category)))

            (define-setf-method ,name (category)
              (let* ((--category--temp-- (make-symbol "--category--"))
                     (--value--temp-- (make-symbol "--value--")))
                (list (list --category--temp--) ; temporary-variables
                      (list category)   ; value-forms
                      (list --value--temp--) ; store-variables
                      (let* ((category --category--temp--) ; store-form
                             (value --value--temp--))
                        (list (quote gnus-agent-cat-set-property)
                              category
                              (quote (quote ,prop-name))
                              value))
                      (list (quote ,name) --category--temp--) ; access-form
                      )))))
  )

(defmacro gnus-agent-cat-name (category)
  `(car ,category))

(gnus-agent-cat-defaccessor
 gnus-agent-cat-days-until-old             agent-days-until-old)
(gnus-agent-cat-defaccessor
 gnus-agent-cat-enable-expiration          agent-enable-expiration)
(gnus-agent-cat-defaccessor
 gnus-agent-cat-groups                     agent-groups)
(gnus-agent-cat-defaccessor
 gnus-agent-cat-high-score                 agent-high-score)
(gnus-agent-cat-defaccessor
 gnus-agent-cat-length-when-long           agent-length-when-long)
(gnus-agent-cat-defaccessor
 gnus-agent-cat-length-when-short          agent-length-when-short)
(gnus-agent-cat-defaccessor
 gnus-agent-cat-low-score                  agent-low-score)
(gnus-agent-cat-defaccessor
 gnus-agent-cat-predicate                  agent-predicate)
(gnus-agent-cat-defaccessor
 gnus-agent-cat-score-file                 agent-score-file)
(gnus-agent-cat-defaccessor
 gnus-agent-cat-disable-undownloaded-faces agent-disable-undownloaded-faces)
(gnus-agent-cat-defaccessor
 gnus-agent-cat-enable-undownloaded-faces agent-enable-undownloaded-faces)

(eval-when-compile
  (defsetf gnus-agent-cat-groups (category) (groups)
    (list 'gnus-agent-set-cat-groups category groups)))

(defun gnus-agent-set-cat-groups (category groups)
  (unless (eq groups 'ignore)
    (let ((new-g groups)
          (old-g (gnus-agent-cat-groups category)))
      (cond ((eq new-g old-g)
             ;; gnus-agent-add-group is fiddling with the group
             ;; list. Still, Im done.
             nil
             )
            ((eq new-g (cdr old-g))
             ;; gnus-agent-add-group is fiddling with the group list
             (setcdr (or (assq 'agent-groups category)
                         (let ((cell (cons 'agent-groups nil)))
                           (setcdr category (cons cell (cdr category)))
                           cell)) new-g))
            (t
             (let ((groups groups))
               (while groups
                 (let* ((group        (pop groups))
                        (old-category (gnus-group-category group)))
                   (if (eq category old-category)
                       nil
                     (setf (gnus-agent-cat-groups old-category)
                           (delete group (gnus-agent-cat-groups
                                          old-category))))))
               ;; Purge cache as preceeding loop invalidated it.
               (setq gnus-category-group-cache nil))

             (setcdr (or (assq 'agent-groups category)
                         (let ((cell (cons 'agent-groups nil)))
                           (setcdr category (cons cell (cdr category)))
                           cell)) groups))))))

(defsubst gnus-agent-cat-make (name &optional default-agent-predicate)
  (list name `(agent-predicate . ,(or default-agent-predicate 'false))))

;;; Fetching setup functions.

(defun gnus-agent-start-fetch ()
  "Initialize data structures for efficient fetching."
  (gnus-agent-create-buffer))

(defun gnus-agent-stop-fetch ()
  "Save all data structures and clean up."
  (setq gnus-agent-spam-hashtb nil)
  (save-excursion
    (set-buffer nntp-server-buffer)
    (widen)))

(defmacro gnus-agent-with-fetch (&rest forms)
  "Do FORMS safely."
  `(unwind-protect
       (let ((gnus-agent-fetching t))
	 (gnus-agent-start-fetch)
	 ,@forms)
     (gnus-agent-stop-fetch)))

(put 'gnus-agent-with-fetch 'lisp-indent-function 0)
(put 'gnus-agent-with-fetch 'edebug-form-spec '(body))

(defmacro gnus-agent-append-to-list (tail value)
  `(setq ,tail (setcdr ,tail (cons ,value nil))))

(defmacro gnus-agent-message (level &rest args)
  `(if (<= ,level gnus-verbose)
       (message ,@args)))

;;;
;;; Mode infestation
;;;

(defvar gnus-agent-mode-hook nil
  "Hook run when installing agent mode.")

(defvar gnus-agent-mode nil)
(defvar gnus-agent-mode-status '(gnus-agent-mode " Plugged"))

(defun gnus-agent-mode ()
  "Minor mode for providing a agent support in Gnus buffers."
  (let* ((buffer (progn (string-match "^gnus-\\(.*\\)-mode$"
				      (symbol-name major-mode))
			(match-string 1 (symbol-name major-mode))))
	 (mode (intern (format "gnus-agent-%s-mode" buffer))))
    (set (make-local-variable 'gnus-agent-mode) t)
    (set mode nil)
    (set (make-local-variable mode) t)
    ;; Set up the menu.
    (when (gnus-visual-p 'agent-menu 'menu)
      (funcall (intern (format "gnus-agent-%s-make-menu-bar" buffer))))
    (unless (assq 'gnus-agent-mode minor-mode-alist)
      (push gnus-agent-mode-status minor-mode-alist))
    (unless (assq mode minor-mode-map-alist)
      (push (cons mode (symbol-value (intern (format "gnus-agent-%s-mode-map"
						     buffer))))
	    minor-mode-map-alist))
    (when (eq major-mode 'gnus-group-mode)
      (let ((init-plugged gnus-plugged)
            (gnus-agent-go-online nil))
        ;; g-a-t-p does nothing when gnus-plugged isn't changed.
        ;; Therefore, make certain that the current value does not
        ;; match the desired initial value.
        (setq gnus-plugged :unknown)
        (gnus-agent-toggle-plugged init-plugged)))
    (gnus-run-hooks 'gnus-agent-mode-hook
		    (intern (format "gnus-agent-%s-mode-hook" buffer)))))

(defvar gnus-agent-group-mode-map (make-sparse-keymap))
(gnus-define-keys gnus-agent-group-mode-map
  "Ju" gnus-agent-fetch-groups
  "Jc" gnus-enter-category-buffer
  "Jj" gnus-agent-toggle-plugged
  "Js" gnus-agent-fetch-session
  "JY" gnus-agent-synchronize-flags
  "JS" gnus-group-send-queue
  "Ja" gnus-agent-add-group
  "Jr" gnus-agent-remove-group
  "Jo" gnus-agent-toggle-group-plugged)

(defun gnus-agent-group-make-menu-bar ()
  (unless (boundp 'gnus-agent-group-menu)
    (easy-menu-define
     gnus-agent-group-menu gnus-agent-group-mode-map ""
     '("Agent"
       ["Toggle plugged" gnus-agent-toggle-plugged t]
       ["Toggle group plugged" gnus-agent-toggle-group-plugged t]
       ["List categories" gnus-enter-category-buffer t]
       ["Add (current) group to category" gnus-agent-add-group t]
       ["Remove (current) group from category" gnus-agent-remove-group t]
       ["Send queue" gnus-group-send-queue gnus-plugged]
       ("Fetch"
	["All" gnus-agent-fetch-session gnus-plugged]
	["Group" gnus-agent-fetch-group gnus-plugged])
       ["Synchronize flags" gnus-agent-synchronize-flags t]
       ))))

(defvar gnus-agent-summary-mode-map (make-sparse-keymap))
(gnus-define-keys gnus-agent-summary-mode-map
  "Jj" gnus-agent-toggle-plugged
  "Ju" gnus-agent-summary-fetch-group
  "JS" gnus-agent-fetch-group
  "Js" gnus-agent-summary-fetch-series
  "J#" gnus-agent-mark-article
  "J\M-#" gnus-agent-unmark-article
  "@" gnus-agent-toggle-mark
  "Jc" gnus-agent-catchup)

(defun gnus-agent-summary-make-menu-bar ()
  (unless (boundp 'gnus-agent-summary-menu)
    (easy-menu-define
     gnus-agent-summary-menu gnus-agent-summary-mode-map ""
     '("Agent"
       ["Toggle plugged" gnus-agent-toggle-plugged t]
       ["Mark as downloadable" gnus-agent-mark-article t]
       ["Unmark as downloadable" gnus-agent-unmark-article t]
       ["Toggle mark" gnus-agent-toggle-mark t]
       ["Fetch downloadable" gnus-agent-summary-fetch-group t]
       ["Catchup undownloaded" gnus-agent-catchup t]))))

(defvar gnus-agent-server-mode-map (make-sparse-keymap))
(gnus-define-keys gnus-agent-server-mode-map
  "Jj" gnus-agent-toggle-plugged
  "Ja" gnus-agent-add-server
  "Jr" gnus-agent-remove-server)

(defun gnus-agent-server-make-menu-bar ()
  (unless (boundp 'gnus-agent-server-menu)
    (easy-menu-define
     gnus-agent-server-menu gnus-agent-server-mode-map ""
     '("Agent"
       ["Toggle plugged" gnus-agent-toggle-plugged t]
       ["Add" gnus-agent-add-server t]
       ["Remove" gnus-agent-remove-server t]))))

(defun gnus-agent-make-mode-line-string (string mouse-button mouse-func)
  (if (and (fboundp 'propertize)
	   (fboundp 'make-mode-line-mouse-map))
      (propertize string 'local-map
		  (make-mode-line-mouse-map mouse-button mouse-func))
    string))

(defun gnus-agent-toggle-plugged (set-to)
  "Toggle whether Gnus is unplugged or not."
  (interactive (list (not gnus-plugged)))
  (cond ((eq set-to gnus-plugged)
         nil)
        (set-to
         (setq gnus-plugged set-to)
         (gnus-run-hooks 'gnus-agent-plugged-hook)
         (setcar (cdr gnus-agent-mode-status)
                 (gnus-agent-make-mode-line-string " Plugged"
                                                   'mouse-2
                                                   'gnus-agent-toggle-plugged))
         (gnus-agent-go-online gnus-agent-go-online)
         (gnus-agent-possibly-synchronize-flags))
        (t
         (gnus-agent-close-connections)
         (setq gnus-plugged set-to)
         (gnus-run-hooks 'gnus-agent-unplugged-hook)
         (setcar (cdr gnus-agent-mode-status)
                 (gnus-agent-make-mode-line-string " Unplugged"
                                                   'mouse-2
                                                   'gnus-agent-toggle-plugged))))
  (set-buffer-modified-p t))

(defmacro gnus-agent-while-plugged (&rest body)
  `(let ((original-gnus-plugged gnus-plugged))
    (unwind-protect
        (progn (gnus-agent-toggle-plugged t)
               ,@body)
      (gnus-agent-toggle-plugged original-gnus-plugged))))

(put 'gnus-agent-while-plugged 'lisp-indent-function 0)
(put 'gnus-agent-while-plugged 'edebug-form-spec '(body))

(defun gnus-agent-close-connections ()
  "Close all methods covered by the Gnus agent."
  (let ((methods (gnus-agent-covered-methods)))
    (while methods
      (gnus-close-server (pop methods)))))

;;;###autoload
(defun gnus-unplugged ()
  "Start Gnus unplugged."
  (interactive)
  (setq gnus-plugged nil)
  (gnus))

;;;###autoload
(defun gnus-plugged ()
  "Start Gnus plugged."
  (interactive)
  (setq gnus-plugged t)
  (gnus))

;;;###autoload
(defun gnus-slave-unplugged (&optional arg)
  "Read news as a slave unplugged."
  (interactive "P")
  (setq gnus-plugged nil)
  (gnus arg nil 'slave))

;;;###autoload
(defun gnus-agentize ()
  "Allow Gnus to be an offline newsreader.

The gnus-agentize function is now called internally by gnus when
gnus-agent is set.  If you wish to avoid calling gnus-agentize,
customize gnus-agent to nil.

This will modify the `gnus-setup-news-hook', and
`message-send-mail-real-function' variables, and install the Gnus agent
minor mode in all Gnus buffers."
  (interactive)
  (gnus-open-agent)
  (add-hook 'gnus-setup-news-hook 'gnus-agent-queue-setup)
  (unless gnus-agent-send-mail-function
    (setq gnus-agent-send-mail-function
	  (or message-send-mail-real-function
	      message-send-mail-function)
	  message-send-mail-real-function 'gnus-agent-send-mail))

  ;; If the servers file doesn't exist, auto-agentize some servers and
  ;; save the servers file so this auto-agentizing isn't invoked
  ;; again.
  (unless (file-exists-p (nnheader-concat gnus-agent-directory "lib/servers"))
    (gnus-message 3 "First time agent user, agentizing remote groups...")
    (mapc
     (lambda (server-or-method)
       (let ((method (gnus-server-to-method server-or-method)))
	 (when (memq (car method)
		     gnus-agent-auto-agentize-methods)
	   (push (gnus-method-to-server method)
		 gnus-agent-covered-methods)
	   (setq gnus-agent-method-p-cache nil))))
     (cons gnus-select-method gnus-secondary-select-methods))
    (gnus-agent-write-servers)))

(defun gnus-agent-queue-setup (&optional group-name)
  "Make sure the queue group exists.
Optional arg GROUP-NAME allows to specify another group."
  (unless (gnus-gethash (format "nndraft:%s" (or group-name "queue"))
			gnus-newsrc-hashtb)
    (gnus-request-create-group (or group-name "queue") '(nndraft ""))
    (let ((gnus-level-default-subscribed 1))
      (gnus-subscribe-group (format "nndraft:%s" (or group-name "queue"))
			    nil '(nndraft "")))
    (gnus-group-set-parameter
     (format "nndraft:%s" (or group-name "queue"))
     'gnus-dummy '((gnus-draft-mode)))))

(defun gnus-agent-send-mail ()
  (if gnus-plugged
      (funcall gnus-agent-send-mail-function)
    (goto-char (point-min))
    (re-search-forward
     (concat "^" (regexp-quote mail-header-separator) "\n"))
    (replace-match "\n")
    (gnus-agent-insert-meta-information 'mail)
    (gnus-request-accept-article "nndraft:queue" nil t t)))

(defun gnus-agent-insert-meta-information (type &optional method)
  "Insert meta-information into the message that says how it's to be posted.
TYPE can be either `mail' or `news'.  If the latter, then METHOD can
be a select method."
  (save-excursion
    (message-remove-header gnus-agent-meta-information-header)
    (goto-char (point-min))
    (insert gnus-agent-meta-information-header ": "
	    (symbol-name type) " " (format "%S" method)
	    "\n")
    (forward-char -1)
    (while (search-backward "\n" nil t)
      (replace-match "\\n" t t))))

(defun gnus-agent-restore-gcc ()
  "Restore GCC field from saved header."
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward (concat gnus-agent-gcc-header ":") nil t)
      (replace-match "Gcc:" 'fixedcase))))

(defun gnus-agent-any-covered-gcc ()
  (save-restriction
    (message-narrow-to-headers)
    (let* ((gcc (mail-fetch-field "gcc" nil t))
	   (methods (and gcc
			 (mapcar 'gnus-inews-group-method
				 (message-unquote-tokens
				  (message-tokenize-header
				   gcc " ,")))))
	   covered)
      (while (and (not covered) methods)
	(setq covered (gnus-agent-method-p (car methods))
	      methods (cdr methods)))
      covered)))

;;;###autoload
(defun gnus-agent-possibly-save-gcc ()
  "Save GCC if Gnus is unplugged."
  (when (and (not gnus-plugged) (gnus-agent-any-covered-gcc))
    (save-excursion
      (goto-char (point-min))
      (let ((case-fold-search t))
	(while (re-search-forward "^gcc:" nil t)
	  (replace-match (concat gnus-agent-gcc-header ":") 'fixedcase))))))

(defun gnus-agent-possibly-do-gcc ()
  "Do GCC if Gnus is plugged."
  (when (or gnus-plugged (not (gnus-agent-any-covered-gcc)))
    (gnus-inews-do-gcc)))

;;;
;;; Group mode commands
;;;

(defun gnus-agent-fetch-groups (n)
  "Put all new articles in the current groups into the Agent."
  (interactive "P")
  (unless gnus-plugged
    (error "Groups can't be fetched when Gnus is unplugged"))
  (gnus-group-iterate n 'gnus-agent-fetch-group))

(defun gnus-agent-fetch-group (&optional group)
  "Put all new articles in GROUP into the Agent."
  (interactive (list (gnus-group-group-name)))
  (setq group (or group gnus-newsgroup-name))
  (unless group
    (error "No group on the current line"))

  (gnus-agent-while-plugged
    (let ((gnus-command-method (gnus-find-method-for-group group)))
      (gnus-agent-with-fetch
        (gnus-agent-fetch-group-1 group gnus-command-method)
        (gnus-message 5 "Fetching %s...done" group)))))

(defun gnus-agent-add-group (category arg)
  "Add the current group to an agent category."
  (interactive
   (list
    (intern
     (completing-read
      "Add to category: "
      (mapcar (lambda (cat) (list (symbol-name (car cat))))
	      gnus-category-alist)
      nil t))
    current-prefix-arg))
  (let ((cat (assq category gnus-category-alist))
	c groups)
    (gnus-group-iterate arg
      (lambda (group)
	(when (gnus-agent-cat-groups (setq c (gnus-group-category group)))
	  (setf (gnus-agent-cat-groups c)
                (delete group (gnus-agent-cat-groups c))))
	(push group groups)))
    (setf (gnus-agent-cat-groups cat)
          (nconc (gnus-agent-cat-groups cat) groups))
    (gnus-category-write)))

(defun gnus-agent-remove-group (arg)
  "Remove the current group from its agent category, if any."
  (interactive "P")
  (let (c)
    (gnus-group-iterate arg
      (lambda (group)
	(when (gnus-agent-cat-groups (setq c (gnus-group-category group)))
	  (setf (gnus-agent-cat-groups c)
                (delete group (gnus-agent-cat-groups c))))))
    (gnus-category-write)))

(defun gnus-agent-synchronize-flags ()
  "Synchronize unplugged flags with servers."
  (interactive)
  (save-excursion
    (dolist (gnus-command-method (gnus-agent-covered-methods))
      (when (file-exists-p (gnus-agent-lib-file "flags"))
	(gnus-agent-synchronize-flags-server gnus-command-method)))))

(defun gnus-agent-possibly-synchronize-flags ()
  "Synchronize flags according to `gnus-agent-synchronize-flags'."
  (interactive)
  (save-excursion
    (dolist (gnus-command-method (gnus-agent-covered-methods))
      (when (file-exists-p (gnus-agent-lib-file "flags"))
	(gnus-agent-possibly-synchronize-flags-server gnus-command-method)))))

(defun gnus-agent-synchronize-flags-server (method)
  "Synchronize flags set when unplugged for server."
  (let ((gnus-command-method method))
    (when (file-exists-p (gnus-agent-lib-file "flags"))
      (set-buffer (get-buffer-create " *Gnus Agent flag synchronize*"))
      (erase-buffer)
      (nnheader-insert-file-contents (gnus-agent-lib-file "flags"))
      (if (null (gnus-check-server gnus-command-method))
	  (gnus-message 1 "Couldn't open server %s" (nth 1 gnus-command-method))
	(while (not (eobp))
	  (if (null (eval (read (current-buffer))))
	      (gnus-delete-line)
	    (write-file (gnus-agent-lib-file "flags"))
	    (error "Couldn't set flags from file %s"
		   (gnus-agent-lib-file "flags"))))
	(delete-file (gnus-agent-lib-file "flags")))
      (kill-buffer nil))))

(defun gnus-agent-possibly-synchronize-flags-server (method)
  "Synchronize flags for server according to `gnus-agent-synchronize-flags'."
  (when (or (and gnus-agent-synchronize-flags
		 (not (eq gnus-agent-synchronize-flags 'ask)))
	    (and (eq gnus-agent-synchronize-flags 'ask)
		 (gnus-y-or-n-p (format "Synchronize flags on server `%s'? "
					(cadr method)))))
    (gnus-agent-synchronize-flags-server method)))

;;;
;;; Server mode commands
;;;

(defun gnus-agent-add-server ()
  "Enroll SERVER in the agent program."
  (interactive)
  (let* ((server       (gnus-server-server-name))
         (named-server (gnus-server-named-server))
         (method       (and server
                            (gnus-server-get-method nil server))))
    (unless server
      (error "No server on the current line"))

    (when (gnus-agent-method-p method)
      (error "Server already in the agent program"))

    (push named-server gnus-agent-covered-methods)

    (setq gnus-agent-method-p-cache nil)
    (gnus-server-update-server server)
    (gnus-agent-write-servers)
    (gnus-message 1 "Entered %s into the Agent" server)))

(defun gnus-agent-remove-server ()
  "Remove SERVER from the agent program."
  (interactive)
  (let* ((server       (gnus-server-server-name))
         (named-server (gnus-server-named-server)))
    (unless server
      (error "No server on the current line"))

    (unless (member named-server gnus-agent-covered-methods)
      (error "Server not in the agent program"))

    (setq gnus-agent-covered-methods 
          (delete named-server gnus-agent-covered-methods)
          gnus-agent-method-p-cache nil)

    (gnus-server-update-server server)
    (gnus-agent-write-servers)
    (gnus-message 1 "Removed %s from the agent" server)))

(defun gnus-agent-read-servers ()
  "Read the alist of covered servers."
  (setq gnus-agent-covered-methods 
        (gnus-agent-read-file
         (nnheader-concat gnus-agent-directory "lib/servers"))
        gnus-agent-method-p-cache nil)

  ;; I am called so early in start-up that I can not validate server
  ;; names.  When that is the case, I skip the validation.  That is
  ;; alright as the gnus startup code calls the validate methods
  ;; directly.
  (if gnus-server-alist
      (gnus-agent-read-servers-validate)))

(defun gnus-agent-read-servers-validate ()
  (mapcar (lambda (server-or-method)
            (let* ((server (if (stringp server-or-method)
                               server-or-method
                             (gnus-method-to-server server-or-method)))
                   (method (gnus-server-to-method server)))
              (if method
                  (unless (member server gnus-agent-covered-methods)
                    (push server gnus-agent-covered-methods)
                    (setq gnus-agent-method-p-cache nil))
                (gnus-message 1 "Ignoring disappeared server `%s'" server))))
          (prog1 gnus-agent-covered-methods
            (setq gnus-agent-covered-methods nil))))

(defun gnus-agent-read-servers-validate-native (native-method)
  (setq gnus-agent-covered-methods
        (mapcar (lambda (method)
                  (if (or (not method)
                          (equal method native-method))
                      "native"
                    method)) gnus-agent-covered-methods)))

(defun gnus-agent-write-servers ()
  "Write the alist of covered servers."
  (gnus-make-directory (nnheader-concat gnus-agent-directory "lib"))
  (let ((coding-system-for-write nnheader-file-coding-system)
	(file-name-coding-system nnmail-pathname-coding-system))
    (with-temp-file (nnheader-concat gnus-agent-directory "lib/servers")
      (prin1 gnus-agent-covered-methods
	     (current-buffer)))))

;;;
;;; Summary commands
;;;

(defun gnus-agent-mark-article (n &optional unmark)
  "Mark the next N articles as downloadable.
If N is negative, mark backward instead.  If UNMARK is non-nil, remove
the mark instead.  The difference between N and the actual number of
articles marked is returned."
  (interactive "p")
  (let ((backward (< n 0))
	(n (abs n)))
    (while (and
	    (> n 0)
	    (progn
	      (gnus-summary-set-agent-mark
	       (gnus-summary-article-number) unmark)
	      (zerop (gnus-summary-next-subject (if backward -1 1) nil t))))
      (setq n (1- n)))
    (when (/= 0 n)
      (gnus-message 7 "No more articles"))
    (gnus-summary-recenter)
    (gnus-summary-position-point)
    n))

(defun gnus-agent-unmark-article (n)
  "Remove the downloadable mark from the next N articles.
If N is negative, unmark backward instead.  The difference between N and
the actual number of articles unmarked is returned."
  (interactive "p")
  (gnus-agent-mark-article n t))

(defun gnus-agent-toggle-mark (n)
  "Toggle the downloadable mark from the next N articles.
If N is negative, toggle backward instead.  The difference between N and
the actual number of articles toggled is returned."
  (interactive "p")
  (gnus-agent-mark-article n 'toggle))

(defun gnus-summary-set-agent-mark (article &optional unmark)
  "Mark ARTICLE as downloadable.  If UNMARK is nil, article is marked.
When UNMARK is t, the article is unmarked.  For any other value, the
article's mark is toggled."
  (let ((unmark (cond ((eq nil unmark)
		       nil)
		      ((eq t unmark)
		       t)
		      (t
		       (memq article gnus-newsgroup-downloadable)))))
    (when (gnus-summary-goto-subject article nil t)
      (gnus-summary-update-mark
       (if unmark
           (progn
             (setq gnus-newsgroup-downloadable
                   (delq article gnus-newsgroup-downloadable))
             (gnus-article-mark article))
         (progn
           (setq gnus-newsgroup-downloadable
                 (gnus-add-to-sorted-list gnus-newsgroup-downloadable article))
           gnus-downloadable-mark)
         )
       'unread))))

(defun gnus-agent-get-undownloaded-list ()
  "Construct list of articles that have not been downloaded."
  (let ((gnus-command-method (gnus-find-method-for-group gnus-newsgroup-name)))
    (when (set (make-local-variable 'gnus-newsgroup-agentized)
               (gnus-agent-method-p gnus-command-method))
      (let* ((alist (gnus-agent-load-alist gnus-newsgroup-name))
             (headers (sort (mapcar (lambda (h)
                                      (mail-header-number h))
                                    gnus-newsgroup-headers) '<))
             (cached (and gnus-use-cache gnus-newsgroup-cached))
             (undownloaded (list nil))
             (tail-undownloaded undownloaded)
             (unfetched (list nil))
             (tail-unfetched unfetched))
	(while (and alist headers)
	  (let ((a (caar alist))
		(h (car headers)))
	    (cond ((< a h)
		   ;; Ignore IDs in the alist that are not being
		   ;; displayed in the summary.
		   (setq alist (cdr alist)))
		  ((> a h)
                   ;; Headers that are not in the alist should be
                   ;; fictious (see nnagent-retrieve-headers); they
                   ;; imply that this article isn't in the agent.
		   (gnus-agent-append-to-list tail-undownloaded h)
		   (gnus-agent-append-to-list tail-unfetched    h)
                   (setq headers (cdr headers))) 
		  ((cdar alist)
		   (setq alist (cdr alist))
		   (setq headers (cdr headers))
		   nil                  ; ignore already downloaded
		   )
		  (t
		   (setq alist (cdr alist))
		   (setq headers (cdr headers))
                   
                   ;; This article isn't in the agent.  Check to see
                   ;; if it is in the cache.  If it is, it's been
                   ;; downloaded.
                   (while (and cached (< (car cached) a))
                     (setq cached (cdr cached)))
                   (unless (equal a (car cached))
                     (gnus-agent-append-to-list tail-undownloaded a))))))

	(while headers
          (let ((num (pop headers)))
            (gnus-agent-append-to-list tail-undownloaded num)
            (gnus-agent-append-to-list tail-unfetched    num)))

	(setq gnus-newsgroup-undownloaded (cdr undownloaded)
              gnus-newsgroup-unfetched    (cdr unfetched))))))

(defun gnus-agent-catchup ()
  "Mark as read all unhandled articles.
An article is unhandled if it is neither cached, nor downloaded, nor
downloadable."
  (interactive)
  (save-excursion
    (let ((articles gnus-newsgroup-undownloaded))
      (when (or gnus-newsgroup-downloadable
                gnus-newsgroup-cached)
        (setq articles (gnus-sorted-ndifference
			(gnus-sorted-ndifference
			 (gnus-copy-sequence articles)
			 gnus-newsgroup-downloadable)
			gnus-newsgroup-cached)))

      (while articles
        (gnus-summary-mark-article
         (pop articles) gnus-catchup-mark)))
    (gnus-summary-position-point)))

(defun gnus-agent-summary-fetch-series ()
  (interactive)
  (when gnus-newsgroup-processable
    (setq gnus-newsgroup-downloadable
          (let* ((dl gnus-newsgroup-downloadable)
                 (gnus-newsgroup-downloadable
		  (sort (gnus-copy-sequence gnus-newsgroup-processable) '<))
                 (fetched-articles (gnus-agent-summary-fetch-group)))
            ;; The preceeding call to (gnus-agent-summary-fetch-group)
            ;; updated gnus-newsgroup-downloadable to remove each
            ;; article successfully fetched.

            ;; For each article that I processed, remove its
            ;; processable mark IF the article is no longer
            ;; downloadable (i.e. it's already downloaded)
            (dolist (article gnus-newsgroup-processable)
              (unless (memq article gnus-newsgroup-downloadable)
                (gnus-summary-remove-process-mark article)))
            (gnus-sorted-ndifference dl fetched-articles)))))

(defun gnus-agent-summary-fetch-group (&optional all)
  "Fetch the downloadable articles in the group.
Optional arg ALL, if non-nil, means to fetch all articles."
  (interactive "P")
  (let ((articles
	 (if all gnus-newsgroup-articles
	   gnus-newsgroup-downloadable))
	(gnus-command-method (gnus-find-method-for-group gnus-newsgroup-name))
        fetched-articles)
    (gnus-agent-while-plugged
      (unless articles
        (error "No articles to download"))
      (gnus-agent-with-fetch
        (setq gnus-newsgroup-undownloaded
              (gnus-sorted-ndifference
               gnus-newsgroup-undownloaded
               (setq fetched-articles
                     (gnus-agent-fetch-articles
                      gnus-newsgroup-name articles)))))
      (save-excursion
        (dolist (article articles)
          (let ((was-marked-downloadable 
                 (memq article gnus-newsgroup-downloadable)))
            (cond (gnus-agent-mark-unread-after-downloaded
                   (setq gnus-newsgroup-downloadable
                         (delq article gnus-newsgroup-downloadable))

                   ;; The downloadable mark is implemented as a
                   ;; type of read mark.  Therefore, marking the
                   ;; article as unread is sufficient to clear
                   ;; its downloadable flag.  
                   (gnus-summary-mark-article article gnus-unread-mark))
                  (was-marked-downloadable
                   (gnus-summary-set-agent-mark article t)))
            (when (gnus-summary-goto-subject article nil t)
              (gnus-summary-update-download-mark article))))))
    fetched-articles))

(defun gnus-agent-fetch-selected-article ()
  "Fetch the current article as it is selected.
This can be added to `gnus-select-article-hook' or
`gnus-mark-article-hook'."
  (let ((gnus-command-method gnus-current-select-method))
    (when (and gnus-plugged (gnus-agent-method-p gnus-command-method))
      (when (gnus-agent-fetch-articles
             gnus-newsgroup-name
	     (list gnus-current-article))
	(setq gnus-newsgroup-undownloaded
	      (delq gnus-current-article gnus-newsgroup-undownloaded))
        (gnus-summary-update-download-mark gnus-current-article)))))

;;;
;;; Internal functions
;;;

;;; NOTES:
;;; The agent's active range is defined as follows:
;;;  If the agent has no record of the group, use the actual active
;;;    range.
;;;  If the agent has a record, set the agent's active range to
;;;    include the max limit of the actual active range.
;;;  When expiring, update the min limit to match the smallest of the
;;;    min article not expired or the min actual active range.

(defun gnus-agent-save-active (method)
  (gnus-agent-save-active-1 method 'gnus-active-to-gnus-format))

(defun gnus-agent-save-active-1 (method function)
  (when (gnus-agent-method-p method)
    (let* ((gnus-command-method method)
	   (new (gnus-make-hashtable (count-lines (point-min) (point-max))))
	   (file (gnus-agent-lib-file "active")))
      (funcall function nil new)
      (gnus-agent-write-active file new)
      (erase-buffer)
      (nnheader-insert-file-contents file))))

(defun gnus-agent-write-active (file new &optional literal-replacement)
  (let ((old new))
    (when (and (not literal-replacement)
               (file-exists-p file))
      (setq old (gnus-make-hashtable (count-lines (point-min) (point-max))))
      (with-temp-buffer
        (nnheader-insert-file-contents file)
        (gnus-active-to-gnus-format nil old))
      ;; Iterate over the current active groups, the current active
      ;; range may expand, but NOT CONTRACT, the agent's active range.
      (mapatoms
       (lambda (nsym)
         (let ((new-active (and nsym (boundp nsym) (symbol-value nsym))))
           (when new-active
             (let* ((osym       (intern (symbol-name nsym) old))
                    (old-active (and (boundp osym) (symbol-value osym))))
               (if old-active
                   (let ((new-min (car new-active))
                         (old-min (car old-active))
                         (new-max (cdr new-active))
                         (old-max (cdr old-active)))
                     (if (and (integerp new-min)
                              (< new-min old-min))
                         (setcar old-active new-min))
                     (if (and (integerp new-max)
                              (> new-max old-max))
                         (setcdr old-active new-max)))
                 (set osym new-active))))))
       new))
    (gnus-make-directory (file-name-directory file))
    (let ((nnmail-active-file-coding-system gnus-agent-file-coding-system))
      ;; The hashtable contains real names of groups.  However, do NOT
      ;; add the foreign server prefix as gnus-active-to-gnus-format
      ;; will add it while reading the file.
      (gnus-write-active-file file old nil))))

(defun gnus-agent-possibly-alter-active (group active)
  "Possibly expand a group's active range to include articles
downloaded into the agent."

;; I can't use the agent's active file here as there is no practical
;; mechanism to update the active ranges in that file as the oldest
;; articles are removed from the agent.
  (let* ((gnus-command-method (or gnus-command-method
                                  (gnus-find-method-for-group group)))
         (alist (gnus-agent-load-alist group)))

    (let ((new-min (or (caar gnus-agent-article-alist)
                       (car active)))
          (new-max (or (caar (last gnus-agent-article-alist))
                       (cdr active))))

        (when (< new-min (car active))
          (setcar active new-min))
        (when (> new-max (cdr active))
          (setcdr active new-max)))))

(defun gnus-agent-save-groups (method)
  (gnus-agent-save-active-1 method 'gnus-groups-to-gnus-format))

(defun gnus-agent-save-group-info (method group active)
  (when (gnus-agent-method-p method)
    (let* ((gnus-command-method method)
	   (coding-system-for-write nnheader-file-coding-system)
	   (file-name-coding-system nnmail-pathname-coding-system)
	   (file (gnus-agent-lib-file "active"))
	   oactive-min oactive-max)
      (gnus-make-directory (file-name-directory file))
      (with-temp-file file
	;; Emacs got problem to match non-ASCII group in multibyte buffer.
	(mm-disable-multibyte)
	(when (file-exists-p file)
	  (nnheader-insert-file-contents file)

          (goto-char (point-min))
          (when (re-search-forward
                 (concat "^" (regexp-quote group) " ") nil t)
            (save-excursion
              (setq oactive-max (read (current-buffer)) ;; max
                    oactive-min (read (current-buffer)))) ;; min
            (gnus-delete-line)))
	(insert (format "%S %d %d y\n" (intern group)
			(max (or oactive-max (cdr active)) (cdr active))
                        (min (or oactive-min (car active)) (car active))))
	(goto-char (point-max))
	(while (search-backward "\\." nil t)
	  (delete-char 1))))))

(defun gnus-agent-group-path (group)
  "Translate GROUP into a file name."

  ;; NOTE: This is what nnmail-group-pathname does as of Apr 2003.
  ;; The two methods must be kept synchronized, which is why
  ;; gnus-agent-group-pathname was added.

  (setq group
        (nnheader-translate-file-chars
         (nnheader-replace-duplicate-chars-in-string
          (nnheader-replace-chars-in-string 
           (gnus-group-real-name group)
           ?/ ?_)
          ?. ?_)))
  (if (or nnmail-use-long-file-names
          (file-directory-p (expand-file-name group (gnus-agent-directory))))
      group
    (mm-encode-coding-string
     (nnheader-replace-chars-in-string group ?. ?/)
     nnmail-pathname-coding-system)))

(defun gnus-agent-group-pathname (group)
  "Translate GROUP into a file name."
  ;; nnagent uses nnmail-group-pathname to read articles while
  ;; unplugged.  The agent must, therefore, use the same directory
  ;; while plugged.
  (let ((gnus-command-method (or gnus-command-method
                                 (gnus-find-method-for-group group))))
    (nnmail-group-pathname (gnus-group-real-name group) (gnus-agent-directory))))

(defun gnus-agent-get-function (method)
  (if (gnus-online method)
      (car method)
    (require 'nnagent)
    'nnagent))

(defun gnus-agent-covered-methods ()
  "Return the subset of methods that are covered by the agent."
  (delq nil (mapcar #'gnus-server-to-method gnus-agent-covered-methods)))

;;; History functions

(defun gnus-agent-history-buffer ()
  (cdr (assoc (gnus-agent-method) gnus-agent-history-buffers)))

(defun gnus-agent-open-history ()
  (save-excursion
    (push (cons (gnus-agent-method)
		(set-buffer (gnus-get-buffer-create
			     (format " *Gnus agent %s history*"
				     (gnus-agent-method)))))
	  gnus-agent-history-buffers)
    (mm-disable-multibyte) ;; everything is binary
    (erase-buffer)
    (insert "\n")
    (let ((file (gnus-agent-lib-file "history")))
      (when (file-exists-p file)
	(nnheader-insert-file-contents file))
      (set (make-local-variable 'gnus-agent-file-name) file))))

(defun gnus-agent-close-history ()
  (when (gnus-buffer-live-p gnus-agent-current-history)
    (kill-buffer gnus-agent-current-history)
    (setq gnus-agent-history-buffers
	  (delq (assoc (gnus-agent-method) gnus-agent-history-buffers)
		gnus-agent-history-buffers))))

;;;
;;; Fetching
;;;

(defun gnus-agent-fetch-articles (group articles)
  "Fetch ARTICLES from GROUP and put them into the Agent."
  (when articles
    (gnus-agent-load-alist group)
    (let* ((alist   gnus-agent-article-alist)
           (headers (if (< (length articles) 2) nil gnus-newsgroup-headers))
           (selected-sets (list nil))
           (current-set-size 0)
           article
           header-number)
      ;; Check each article
      (while (setq article (pop articles))
        ;; Skip alist entries preceeding this article
        (while (> article (or (caar alist) (1+ article)))
          (setq alist (cdr alist)))

        ;; Prune off articles that we have already fetched.
        (unless (and (eq article (caar alist))
                     (cdar alist))
          ;; Skip headers preceeding this article
          (while (> article 
                    (setq header-number
                          (let* ((header (car headers)))
                            (if header
                                (mail-header-number header)
                              (1+ article)))))
            (setq headers (cdr headers)))

          ;; Add this article to the current set
          (setcar selected-sets (cons article (car selected-sets)))

          ;; Update the set size, when the set is too large start a
          ;; new one.  I do this after adding the article as I want at
          ;; least one article in each set.
          (when (< gnus-agent-max-fetch-size
                   (setq current-set-size
			 (+ current-set-size
			    (if (= header-number article)
                                (let ((char-size (mail-header-chars
                                                  (car headers))))
                                  (if (<= char-size 0)
                                      ;; The char size was missing/invalid,
                                      ;; assume a worst-case situation of
                                      ;; 65 char/line.  If the line count
                                      ;; is missing, arbitrarily assume a
                                      ;; size of 1000 characters.
                                    (max (* 65 (mail-header-lines
                                                (car headers)))
                                         1000)
                                    char-size))
			      0))))
            (setcar selected-sets (nreverse (car selected-sets)))
            (setq selected-sets (cons nil selected-sets)
                  current-set-size 0))))

      (when (or (cdr selected-sets) (car selected-sets))
        (let* ((fetched-articles (list nil))
               (tail-fetched-articles fetched-articles)
               (dir (gnus-agent-group-pathname group))
               (date (time-to-days (current-time)))
               (case-fold-search t)
               pos crosses id)

          (setcar selected-sets (nreverse (car selected-sets)))
          (setq selected-sets (nreverse selected-sets))

          (gnus-make-directory dir)
          (gnus-message 7 "Fetching articles for %s..." group)

          (unwind-protect
              (while (setq articles (pop selected-sets))
                ;; Fetch the articles from the backend.
                (if (gnus-check-backend-function 'retrieve-articles group)
                    (setq pos (gnus-retrieve-articles articles group))
                  (with-temp-buffer
                    (let (article)
                      (while (setq article (pop articles))
                        (gnus-message 10 "Fetching article %s for %s..."
                                      article group)
                        (when (or
                               (gnus-backlog-request-article group article
                                                             nntp-server-buffer)
                               (gnus-request-article article group))
                          (goto-char (point-max))
                          (push (cons article (point)) pos)
                          (insert-buffer-substring nntp-server-buffer)))
                      (copy-to-buffer
		       nntp-server-buffer (point-min) (point-max))
                      (setq pos (nreverse pos)))))
                ;; Then save these articles into the Agent.
                (save-excursion
                  (set-buffer nntp-server-buffer)
                  (while pos
                    (narrow-to-region (cdar pos) (or (cdadr pos) (point-max)))
                    (goto-char (point-min))
                    (unless (eobp) ;; Don't save empty articles.
                      (when (search-forward "\n\n" nil t)
                        (when (search-backward "\nXrefs: " nil t)
                          ;; Handle cross posting.
                          (goto-char (match-end 0)) ; move to end of header name
                          (skip-chars-forward "^ ") ; skip server name
                          (skip-chars-forward " ")
                          (setq crosses nil)
                          (while (looking-at "\\([^: \n]+\\):\\([0-9]+\\) *")
                            (push (cons (buffer-substring (match-beginning 1)
                                                          (match-end 1))
                                        (string-to-int
					 (buffer-substring (match-beginning 2)
							   (match-end 2))))
                                  crosses)
                            (goto-char (match-end 0)))
                          (gnus-agent-crosspost crosses (caar pos) date)))
                      (goto-char (point-min))
                      (if (not (re-search-forward
                                "^Message-ID: *<\\([^>\n]+\\)>" nil t))
                          (setq id "No-Message-ID-in-article")
                        (setq id (buffer-substring
				  (match-beginning 1) (match-end 1))))
                      (let ((coding-system-for-write
                             gnus-agent-file-coding-system))
                        (write-region (point-min) (point-max)
                                      (concat dir (number-to-string (caar pos)))
                                      nil 'silent))

                      (gnus-agent-append-to-list
		       tail-fetched-articles (caar pos)))
                    (widen)
                    (setq pos (cdr pos)))))

            (gnus-agent-save-alist group (cdr fetched-articles) date)
            (gnus-message 7 ""))
          (cdr fetched-articles))))))

(defun gnus-agent-crosspost (crosses article &optional date)
  (setq date (or date t))

  (let (gnus-agent-article-alist group alist beg end)
    (save-excursion
      (set-buffer gnus-agent-overview-buffer)
      (when (nnheader-find-nov-line article)
	(forward-word 1)
	(setq beg (point))
	(setq end (progn (forward-line 1) (point)))))
    (while crosses
      (setq group (caar crosses))
      (unless (setq alist (assoc group gnus-agent-group-alist))
	(push (setq alist (list group (gnus-agent-load-alist (caar crosses))))
	      gnus-agent-group-alist))
      (setcdr alist (cons (cons (cdar crosses) date) (cdr alist)))
      (save-excursion
	(set-buffer (gnus-get-buffer-create (format " *Gnus agent overview %s*"
						    group)))
	(when (= (point-max) (point-min))
	  (push (cons group (current-buffer)) gnus-agent-buffer-alist)
	  (ignore-errors
	    (nnheader-insert-file-contents
	     (gnus-agent-article-name ".overview" group))))
	(nnheader-find-nov-line (string-to-number (cdar crosses)))
	(insert (string-to-number (cdar crosses)))
	(insert-buffer-substring gnus-agent-overview-buffer beg end)
        (gnus-agent-check-overview-buffer))
      (setq crosses (cdr crosses)))))

(defun gnus-agent-backup-overview-buffer ()
  (when gnus-newsgroup-name
    (let ((root (gnus-agent-article-name ".overview" gnus-newsgroup-name))
          (cnt 0)
          name)
      (while (file-exists-p
	      (setq name (concat root "~"
				 (int-to-string (setq cnt (1+ cnt))) "~"))))
      (write-region (point-min) (point-max) name nil 'no-msg)
      (gnus-message 1 "Created backup copy of overview in %s." name)))
  t)

(defun gnus-agent-check-overview-buffer (&optional buffer)
  "Check the overview file given for sanity.
In particular, checks that the file is sorted by article number
and that there are no duplicates."
  (let ((prev-num -1)
        (backed-up nil))
    (save-excursion
      (when buffer
	(set-buffer buffer))
      (save-restriction
	(widen)
	(goto-char (point-min))

	(while (< (point) (point-max))
	  (let ((p (point))
		(cur (condition-case nil
			 (read (current-buffer))
		       (error nil))))
	    (cond
	     ((or (not (integerp cur))
		  (not (eq (char-after) ?\t)))
              (or backed-up
                  (setq backed-up (gnus-agent-backup-overview-buffer)))
	      (gnus-message 1
			    "Overview buffer contains garbage '%s'."
			    (buffer-substring
			     p (gnus-point-at-eol))))
	     ((= cur prev-num)
	      (or backed-up
                  (setq backed-up (gnus-agent-backup-overview-buffer)))
              (gnus-message 1
			    "Duplicate overview line for %d" cur)
	      (delete-region (point) (progn (forward-line 1) (point))))
	     ((< cur prev-num)
	      (or backed-up
                  (setq backed-up (gnus-agent-backup-overview-buffer)))
              (gnus-message 1 "Overview buffer not sorted!")
	      (sort-numeric-fields 1 (point-min) (point-max))
	      (goto-char (point-min))
	      (setq prev-num -1))
	     (t
	      (setq prev-num cur)))
	    (forward-line 1)))))))

(defun gnus-agent-flush-cache ()
  (save-excursion
    (while gnus-agent-buffer-alist
      (set-buffer (cdar gnus-agent-buffer-alist))
      (let ((coding-system-for-write
	     gnus-agent-file-coding-system))
	(write-region (point-min) (point-max)
		      (gnus-agent-article-name ".overview"
					       (caar gnus-agent-buffer-alist))
		      nil 'silent))
      (setq gnus-agent-buffer-alist (cdr gnus-agent-buffer-alist)))
    (while gnus-agent-group-alist
      (with-temp-file (gnus-agent-article-name
		       ".agentview" (caar gnus-agent-group-alist))
	(princ (cdar gnus-agent-group-alist))
	(insert "\n")
        (princ 1 (current-buffer))
	(insert "\n"))
      (setq gnus-agent-group-alist (cdr gnus-agent-group-alist)))))

(defun gnus-agent-find-parameter (group symbol)
  "Search for GROUPs SYMBOL in the group's parameters, the group's
topic parameters, the group's category, or the customizable
variables.  Returns the first non-nil value found."
  (or (gnus-group-find-parameter group symbol t)
      (gnus-group-parameter-value (cdr (gnus-group-category group)) symbol t)
      (symbol-value
       (cdr
        (assq symbol
              '((agent-short-article . gnus-agent-short-article)
                (agent-long-article . gnus-agent-long-article)
                (agent-low-score . gnus-agent-low-score)
                (agent-high-score . gnus-agent-high-score)
                (agent-days-until-old . gnus-agent-expire-days)
                (agent-enable-expiration
                 . gnus-agent-enable-expiration)
                (agent-predicate . gnus-agent-predicate)))))))

(defun gnus-agent-fetch-headers (group &optional force)
  "Fetch interesting headers into the agent.  The group's overview
file will be updated to include the headers while a list of available
article numbers will be returned."
  (let* ((fetch-all (and gnus-agent-consider-all-articles
                         ;; Do not fetch all headers if the predicate
                         ;; implies that we only consider unread articles.
                         (not (gnus-predicate-implies-unread
                               (gnus-agent-find-parameter group
                                                          'agent-predicate)))))
         (articles (if fetch-all
                       (gnus-uncompress-range (gnus-active group))
                     (gnus-list-of-unread-articles group)))
         (gnus-decode-encoded-word-function 'identity)
         (file (gnus-agent-article-name ".overview" group)))

    (unless fetch-all
      ;; Add articles with marks to the list of article headers we want to
      ;; fetch.  Don't fetch articles solely on the basis of a recent or seen
      ;; mark, but do fetch recent or seen articles if they have other, more
      ;; interesting marks.  (We have to fetch articles with boring marks
      ;; because otherwise the agent will remove their marks.)
      (dolist (arts (gnus-info-marks (gnus-get-info group)))
        (unless (memq (car arts) '(seen recent killed cache))
          (setq articles (gnus-range-add articles (cdr arts)))))
      (setq articles (sort (gnus-uncompress-sequence articles) '<)))

    ;; At this point, I have the list of articles to consider for
    ;; fetching.  This is the list that I'll return to my caller. Some
    ;; of these articles may have already been fetched.  That's OK as
    ;; the fetch article code will filter those out.  Internally, I'll
    ;; filter this list to just those articles whose headers need to
    ;; be fetched.
    (let ((articles articles))
      ;; Remove known articles.
      (when (and (or gnus-agent-cache
                     (not gnus-plugged))
                 (gnus-agent-load-alist group))
        ;; Remove articles marked as downloaded.
        (if fetch-all
            ;; I want to fetch all headers in the active range.
            ;; Therefore, exclude only those headers that are in the
            ;; article alist.
            ;; NOTE: This is probably NOT what I want to do after
            ;; agent expiration in this group.
            (setq articles (gnus-agent-uncached-articles articles group))

          ;; I want to only fetch those headers that have never been
          ;; fetched.  Therefore, exclude all headers that are, or
          ;; WERE, in the article alist.
          (let ((low (1+ (caar (last gnus-agent-article-alist))))
                (high (cdr (gnus-active group))))
            ;; Low can be greater than High when the same group is
            ;; fetched twice in the same session {The first fetch will
            ;; fill the article alist such that (last
            ;; gnus-agent-article-alist) equals (cdr (gnus-active
            ;; group))}.  The addition of one(the 1+ above) then
            ;; forces Low to be greater than High.  When this happens,
            ;; gnus-list-range-intersection returns nil which
            ;; indicates that no headers need to be fetched. -- Kevin
            (setq articles (gnus-list-range-intersection
                            articles (list (cons low high)))))))

      (gnus-message
       10 "gnus-agent-fetch-headers: undownloaded articles are '%s'"
       (gnus-compress-sequence articles t))

      (save-excursion
        (set-buffer nntp-server-buffer)

        (if articles
            (progn
              (gnus-message 7 "Fetching headers for %s..." group)

              ;; Fetch them.
              (gnus-make-directory (nnheader-translate-file-chars
                                    (file-name-directory file) t))

              (unless (eq 'nov (gnus-retrieve-headers articles group))
                (nnvirtual-convert-headers))
              (gnus-agent-check-overview-buffer)
              ;; Move these headers to the overview buffer so that
              ;; gnus-agent-braid-nov can merge them with the contents
              ;; of FILE.
              (copy-to-buffer
	       gnus-agent-overview-buffer (point-min) (point-max))
              (when (file-exists-p file)
                (gnus-agent-braid-nov group articles file))
              (let ((coding-system-for-write
                     gnus-agent-file-coding-system))
                (gnus-agent-check-overview-buffer)
                (write-region (point-min) (point-max) file nil 'silent))
              (gnus-agent-save-alist group articles nil)
              articles)
          (ignore-errors
            (erase-buffer)
            (nnheader-insert-file-contents file)))))
    articles))

(defsubst gnus-agent-copy-nov-line (article)
  (let (art b e)
    (set-buffer gnus-agent-overview-buffer)
    (while (and (not (eobp))
		(< (setq art (read (current-buffer))) article))
      (forward-line 1))
    (beginning-of-line)
    (if (or (eobp)
	    (not (eq article art)))
	(set-buffer nntp-server-buffer)
      (setq b (point))
      (setq e (progn (forward-line 1) (point)))
      (set-buffer nntp-server-buffer)
      (insert-buffer-substring gnus-agent-overview-buffer b e))))

(defun gnus-agent-braid-nov (group articles file)
  "Merge agent overview data with given file.
Takes headers for ARTICLES from `gnus-agent-overview-buffer' and the given
FILE and places the combined headers into `nntp-server-buffer'."
  (let (start last)
    (set-buffer gnus-agent-overview-buffer)
    (goto-char (point-min))
    (set-buffer nntp-server-buffer)
    (erase-buffer)
    (nnheader-insert-file-contents file)
    (goto-char (point-max))
    (forward-line -1)
    (unless (looking-at "[0-9]+\t")
      ;; Remove corrupted lines
      (gnus-message
       1 "Overview %s is corrupted. Removing corrupted lines..." file)
      (goto-char (point-min))
      (while (not (eobp))
	(if (looking-at "[0-9]+\t")
	    (forward-line 1)
	  (delete-region (point) (progn (forward-line 1) (point)))))
      (forward-line -1))
    (unless (or (= (point-min) (point-max))
		(< (setq last (read (current-buffer))) (car articles)))
      ;; We do it the hard way.
      (when (nnheader-find-nov-line (car articles))
        ;; Replacing existing NOV entry
        (delete-region (point) (progn (forward-line 1) (point))))
      (gnus-agent-copy-nov-line (pop articles))

      (ignore-errors
        (while articles
          (while (let ((art (read (current-buffer))))
                   (cond ((< art (car articles))
                          (forward-line 1)
                          t)
                         ((= art (car articles))
                          (beginning-of-line)
                          (delete-region
			   (point) (progn (forward-line 1) (point)))
                          nil)
                         (t
                          (beginning-of-line)
                          nil))))

	  (gnus-agent-copy-nov-line (pop articles)))))

    ;; Copy the rest lines
    (set-buffer nntp-server-buffer)
    (goto-char (point-max))
    (when articles
      (when last
	(set-buffer gnus-agent-overview-buffer)
	(ignore-errors
          (while (<= (read (current-buffer)) last)
            (forward-line 1)))
	(beginning-of-line)
	(setq start (point))
	(set-buffer nntp-server-buffer))
      (insert-buffer-substring gnus-agent-overview-buffer start))))

;; Keeps the compiler from warning about the free variable in
;; gnus-agent-read-agentview.
(eval-when-compile
  (defvar gnus-agent-read-agentview))

(defun gnus-agent-load-alist (group)
  "Load the article-state alist for GROUP."
  ;; Bind free variable that's used in `gnus-agent-read-agentview'.
  (let ((gnus-agent-read-agentview group))
    (setq gnus-agent-article-alist
          (gnus-cache-file-contents
           (gnus-agent-article-name ".agentview" group)
           'gnus-agent-file-loading-cache
           'gnus-agent-read-agentview))))

;; Save format may be either 1 or 2.  Two is the new, compressed
;; format that is still being tested.  Format 1 is uncompressed but
;; known to be reliable.
(defconst gnus-agent-article-alist-save-format 2)

(defun gnus-agent-read-agentview (file)
  "Load FILE and do a `read' there."
  (with-temp-buffer
    (ignore-errors
      (nnheader-insert-file-contents file)
      (goto-char (point-min))
      (let ((alist (read (current-buffer)))
            (version (condition-case nil (read (current-buffer))
                       (end-of-file 0)))
            changed-version)

        (cond
	 ((= version 0)
	  (let ((inhibit-quit t)
		entry)
	    (gnus-agent-open-history)
	    (set-buffer (gnus-agent-history-buffer))
	    (goto-char (point-min))
	    (while (not (eobp))
	      (if (and (looking-at
			"[^\t\n]+\t\\([0-9]+\\)\t\\([^ \n]+\\) \\([0-9]+\\)")
		       (string= (match-string 2)
				gnus-agent-read-agentview)
		       (setq entry (assoc (string-to-number (match-string 3)) alist)))
		  (setcdr entry (string-to-number (match-string 1))))
	      (forward-line 1))
	    (gnus-agent-close-history)
	    (setq changed-version t)))
	 ((= version 1)
	  (setq changed-version (not (= 1 gnus-agent-article-alist-save-format))))
	 ((= version 2)
	  (let (uncomp)
	    (mapcar
	     (lambda (comp-list)
	       (let ((state (car comp-list))
		     (sequence (gnus-uncompress-sequence
				(cdr comp-list))))
		 (mapcar (lambda (article-id)
			   (setq uncomp (cons (cons article-id state) uncomp)))
			 sequence)))
	     alist)
	    (setq alist (sort uncomp 'car-less-than-car)))))
        (when changed-version
          (let ((gnus-agent-article-alist alist))
            (gnus-agent-save-alist gnus-agent-read-agentview)))
        alist))))

(defun gnus-agent-save-alist (group &optional articles state dir)
  "Save the article-state alist for GROUP."
  (let* ((file-name-coding-system nnmail-pathname-coding-system)
	 (prev (cons nil gnus-agent-article-alist))
	 (all prev)
	 print-level print-length item article)
    (while (setq article (pop articles))
      (while (and (cdr prev)
                  (< (caadr prev) article))
	(setq prev (cdr prev)))
      (cond
       ((not (cdr prev))
	(setcdr prev (list (cons article state))))
       ((> (caadr prev) article)
	(setcdr prev (cons (cons article state) (cdr prev))))
       ((= (caadr prev) article)
	(setcdr (cadr prev) state)))
      (setq prev (cdr prev)))
    (setq gnus-agent-article-alist (cdr all))
    (if dir
	(gnus-make-directory dir)
      (gnus-make-directory (gnus-agent-article-name "" group)))
    (with-temp-file (if dir
			(expand-file-name ".agentview" dir)
		      (gnus-agent-article-name ".agentview" group))
      (cond ((eq gnus-agent-article-alist-save-format 1)
             (princ gnus-agent-article-alist (current-buffer)))
            ((eq gnus-agent-article-alist-save-format 2)
             (let ((compressed nil))
               (mapcar (lambda (pair)
                         (let* ((article-id (car pair))
                                (day-of-download (cdr pair))
                                (comp-list (assq day-of-download compressed)))
                           (if comp-list
                               (setcdr comp-list
				       (cons article-id (cdr comp-list)))
                             (setq compressed
				   (cons (list day-of-download article-id)
					 compressed)))
                           nil)) gnus-agent-article-alist)
               (mapcar (lambda (comp-list)
			 (setcdr comp-list
				 (gnus-compress-sequence
				  (nreverse (cdr comp-list)))))
		       compressed)
               (princ compressed (current-buffer)))))
      (insert "\n")
      (princ gnus-agent-article-alist-save-format (current-buffer))
      (insert "\n"))))

(defun gnus-agent-article-name (article group)
  (expand-file-name article
		    (file-name-as-directory
                     (gnus-agent-group-pathname group))))

(defun gnus-agent-batch-confirmation (msg)
  "Show error message and return t."
  (gnus-message 1 msg)
  t)

;;;###autoload
(defun gnus-agent-batch-fetch ()
  "Start Gnus and fetch session."
  (interactive)
  (gnus)
  (let ((gnus-agent-confirmation-function 'gnus-agent-batch-confirmation))
    (gnus-agent-fetch-session))
  (gnus-group-exit))

(defun gnus-agent-fetch-session ()
  "Fetch all articles and headers that are eligible for fetching."
  (interactive)
  (unless gnus-agent-covered-methods
    (error "No servers are covered by the Gnus agent"))
  (unless gnus-plugged
    (error "Can't fetch articles while Gnus is unplugged"))
  (let ((methods (gnus-agent-covered-methods))
	groups group gnus-command-method)
    (save-excursion
      (while methods
	(setq gnus-command-method (car methods))
	(when (and (or (gnus-server-opened gnus-command-method)
		       (gnus-open-server gnus-command-method))
		   (gnus-online gnus-command-method))
	  (setq groups (gnus-groups-from-server (car methods)))
	  (gnus-agent-with-fetch
	    (while (setq group (pop groups))
	      (when (<= (gnus-group-level group)
			gnus-agent-handle-level)
		(if (or debug-on-error debug-on-quit)
		    (gnus-agent-fetch-group-1
		     group gnus-command-method)
		  (condition-case err
		      (gnus-agent-fetch-group-1
		       group gnus-command-method)
		    (error
		     (unless (funcall gnus-agent-confirmation-function
				      (format "Error %s.  Continue? "
					      (error-message-string err)))
		       (error "Cannot fetch articles into the Gnus agent")))
		    (quit
		     (unless (funcall gnus-agent-confirmation-function
				      (format
				       "Quit fetching session %s.  Continue? "
				       (error-message-string err)))
		       (signal 'quit
			       "Cannot fetch articles into the Gnus agent")))))))))
	(setq methods (cdr methods)))
      (gnus-run-hooks 'gnus-agent-fetched-hook)
      (gnus-message 6 "Finished fetching articles into the Gnus agent"))))

(defun gnus-agent-fetch-group-1 (group method)
  "Fetch GROUP."
  (let ((gnus-command-method method)
	(gnus-newsgroup-name group)
	(gnus-newsgroup-dependencies gnus-newsgroup-dependencies)
        (gnus-newsgroup-headers gnus-newsgroup-headers)
	(gnus-newsgroup-scored gnus-newsgroup-scored)
	(gnus-use-cache gnus-use-cache)
	(gnus-summary-expunge-below gnus-summary-expunge-below)
	(gnus-summary-mark-below gnus-summary-mark-below)
	(gnus-orphan-score gnus-orphan-score)
	;; Maybe some other gnus-summary local variables should also
	;; be put here.

        gnus-headers
        gnus-score
        articles arts
	category predicate info marks score-param
	)
    (unless (gnus-check-group group)
      (error "Can't open server for %s" group))

    ;; Fetch headers.
    (when (or gnus-newsgroup-active
              (gnus-active group)
              (gnus-activate-group group))
      (let ((marked-articles gnus-newsgroup-downloadable))
        ;; Identify the articles marked for download
        (unless gnus-newsgroup-active
	  ;; The variable gnus-newsgroup-active was selected as I need
	  ;; a gnus-summary local variable that is NOT bound to any
	  ;; value (its global value should default to nil).
          (dolist (mark gnus-agent-download-marks)
            (let ((arts (cdr (assq mark (gnus-info-marks
                                         (setq info (gnus-get-info group)))))))
              (when arts
                (setq marked-articles (nconc (gnus-uncompress-range arts)
                                             marked-articles))
                ))))
        (setq marked-articles (sort marked-articles '<))

        ;; Fetch any new articles from the server
        (setq articles (gnus-agent-fetch-headers group))

        ;; Merge new articles with marked
        (setq articles (sort (append marked-articles articles) '<))

        (when articles
          ;; Parse them and see which articles we want to fetch.
          (setq gnus-newsgroup-dependencies
                (or gnus-newsgroup-dependencies
                    (make-vector (length articles) 0)))
          (setq gnus-newsgroup-headers
                (or gnus-newsgroup-headers
                    (gnus-get-newsgroup-headers-xover articles nil nil
                                                      group)))
          ;; `gnus-agent-overview-buffer' may be killed for
          ;; timeout reason.  If so, recreate it.
          (gnus-agent-create-buffer)

          ;; Figure out how to select articles in this group
          (setq category (gnus-group-category group))

          (setq predicate
                (gnus-get-predicate
                 (gnus-agent-find-parameter group 'agent-predicate)))

          ;; If the selection predicate requires scoring, score each header
          (unless (memq predicate '(gnus-agent-true gnus-agent-false))
            (let ((score-param
                   (gnus-agent-find-parameter group 'agent-score-file)))
              ;; Translate score-param into real one
              (cond
               ((not score-param))
               ((eq score-param 'file)
                (setq score-param (gnus-all-score-files group)))
               ((stringp (car score-param)))
               (t
                (setq score-param (list (list score-param)))))
              (when score-param
                (gnus-score-headers score-param))))

          (unless (and (eq predicate 'gnus-agent-false)
                       (not marked-articles))
            (let ((arts (list nil)))
              (let ((arts-tail arts)
                    (alist (gnus-agent-load-alist group))
                    (marked-articles marked-articles)
                    (gnus-newsgroup-headers gnus-newsgroup-headers))
                (while (setq gnus-headers (pop gnus-newsgroup-headers))
                  (let ((num (mail-header-number gnus-headers)))
                    ;; Determine if this article is already in the cache
                    (while (and alist
                                (> num (caar alist)))
                      (setq alist (cdr alist)))

                    (unless (and (eq num (caar alist))
                                 (cdar alist))

                      ;; Determine if this article was marked for download.
                      (while (and marked-articles
                                  (> num (car marked-articles)))
                        (setq marked-articles
                              (cdr marked-articles)))

                      ;; When this article is marked, or selected by the
                      ;; predicate, add it to the download list
                      (when (or (eq num (car marked-articles))
                                (let ((gnus-score
                                       (or (cdr
					    (assq num gnus-newsgroup-scored))
                                           gnus-summary-default-score))
                                      (gnus-agent-long-article
                                       (gnus-agent-find-parameter
                                        group 'agent-long-article))
                                      (gnus-agent-short-article
                                       (gnus-agent-find-parameter
                                        group 'agent-short-article))
                                      (gnus-agent-low-score
                                       (gnus-agent-find-parameter
                                        group 'agent-low-score))
                                      (gnus-agent-high-score
                                       (gnus-agent-find-parameter
                                        group 'agent-high-score))
                                      (gnus-agent-expire-days
                                       (gnus-agent-find-parameter
                                        group 'agent-days-until-old)))
                                  (funcall predicate)))
                        (gnus-agent-append-to-list arts-tail num))))))

              (let (fetched-articles)
                ;; Fetch all selected articles
                (setq gnus-newsgroup-undownloaded
                      (gnus-sorted-ndifference
		       gnus-newsgroup-undownloaded
		       (setq fetched-articles
			     (if (cdr arts)
				 (gnus-agent-fetch-articles group (cdr arts))
			       nil))))

                (let ((unfetched-articles
		       (gnus-sorted-ndifference (cdr arts) fetched-articles)))
                  (if gnus-newsgroup-active
                      ;; Update the summary buffer
                      (progn
                        (dolist (article marked-articles)
                          (gnus-summary-set-agent-mark article t))
                        (dolist (article fetched-articles)
                          (if gnus-agent-mark-unread-after-downloaded
                              (gnus-summary-mark-article
			       article gnus-unread-mark))
                          (when (gnus-summary-goto-subject article nil t)
                            (gnus-summary-update-download-mark article)))
                        (dolist (article unfetched-articles)
                          (gnus-summary-mark-article
			   article gnus-canceled-mark)))

                    ;; Update the group buffer.

                    ;; When some, or all, of the marked articles came
                    ;; from the download mark.  Remove that mark.  I
                    ;; didn't do this earlier as I only want to remove
                    ;; the marks after the fetch is completed.

                    (dolist (mark gnus-agent-download-marks)
                      (when (eq mark 'download)
                        (let ((marked-arts
			       (assq mark (gnus-info-marks
					   (setq info (gnus-get-info group))))))
                          (when (cdr marked-arts)
                            (setq marks
				  (delq marked-arts (gnus-info-marks info)))
                            (gnus-info-set-marks info marks)))))
                    (let ((read (gnus-info-read
				 (or info (setq info (gnus-get-info group))))))
                      (gnus-info-set-read
		       info (gnus-add-to-range read unfetched-articles)))

                    (gnus-group-update-group group t)
                    (sit-for 0)

                    (gnus-dribble-enter
                     (concat "(gnus-group-set-info '"
                             (gnus-prin1-to-string info)
                             ")"))))))))))))

;;;
;;; Agent Category Mode
;;;

(defvar gnus-category-mode-hook nil
  "Hook run in `gnus-category-mode' buffers.")

(defvar gnus-category-line-format "     %(%20c%): %g\n"
  "Format of category lines.

Valid specifiers include:
%c  Topic name (string)
%g  The number of groups in the topic (integer)

General format specifiers can also be used.  See Info node
`(gnus)Formatting Variables'.")

(defvar gnus-category-mode-line-format "Gnus: %%b"
  "The format specification for the category mode line.")

(defvar gnus-agent-predicate 'false
  "The selection predicate used when no other source is available.")

(defvar gnus-agent-short-article 100
  "Articles that have fewer lines than this are short.")

(defvar gnus-agent-long-article 200
  "Articles that have more lines than this are long.")

(defvar gnus-agent-low-score 0
  "Articles that have a score lower than this have a low score.")

(defvar gnus-agent-high-score 0
  "Articles that have a score higher than this have a high score.")


;;; Internal variables.

(defvar gnus-category-buffer "*Agent Category*")

(defvar gnus-category-line-format-alist
  `((?c gnus-tmp-name ?s)
    (?g gnus-tmp-groups ?d)))

(defvar gnus-category-mode-line-format-alist
  `((?u user-defined ?s)))

(defvar gnus-category-line-format-spec nil)
(defvar gnus-category-mode-line-format-spec nil)

(defvar gnus-category-mode-map nil)
(put 'gnus-category-mode 'mode-class 'special)

(unless gnus-category-mode-map
  (setq gnus-category-mode-map (make-sparse-keymap))
  (suppress-keymap gnus-category-mode-map)

  (gnus-define-keys gnus-category-mode-map
    "q" gnus-category-exit
    "k" gnus-category-kill
    "c" gnus-category-copy
    "a" gnus-category-add
    "e" gnus-agent-customize-category
    "p" gnus-category-edit-predicate
    "g" gnus-category-edit-groups
    "s" gnus-category-edit-score
    "l" gnus-category-list

    "\C-c\C-i" gnus-info-find-node
    "\C-c\C-b" gnus-bug))

(defvar gnus-category-menu-hook nil
  "*Hook run after the creation of the menu.")

(defun gnus-category-make-menu-bar ()
  (gnus-turn-off-edit-menu 'category)
  (unless (boundp 'gnus-category-menu)
    (easy-menu-define
     gnus-category-menu gnus-category-mode-map ""
     '("Categories"
       ["Add" gnus-category-add t]
       ["Kill" gnus-category-kill t]
       ["Copy" gnus-category-copy t]
       ["Edit category" gnus-agent-customize-category t]
       ["Edit predicate" gnus-category-edit-predicate t]
       ["Edit score" gnus-category-edit-score t]
       ["Edit groups" gnus-category-edit-groups t]
       ["Exit" gnus-category-exit t]))

    (gnus-run-hooks 'gnus-category-menu-hook)))

(defun gnus-category-mode ()
  "Major mode for listing and editing agent categories.

All normal editing commands are switched off.
\\<gnus-category-mode-map>
For more in-depth information on this mode, read the manual
\(`\\[gnus-info-find-node]').

The following commands are available:

\\{gnus-category-mode-map}"
  (interactive)
  (when (gnus-visual-p 'category-menu 'menu)
    (gnus-category-make-menu-bar))
  (kill-all-local-variables)
  (gnus-simplify-mode-line)
  (setq major-mode 'gnus-category-mode)
  (setq mode-name "Category")
  (gnus-set-default-directory)
  (setq mode-line-process nil)
  (use-local-map gnus-category-mode-map)
  (buffer-disable-undo)
  (setq truncate-lines t)
  (setq buffer-read-only t)
  (gnus-run-hooks 'gnus-category-mode-hook))

(defalias 'gnus-category-position-point 'gnus-goto-colon)

(defun gnus-category-insert-line (category)
  (let* ((gnus-tmp-name (format "%s" (car category)))
	 (gnus-tmp-groups (length (gnus-agent-cat-groups category))))
    (beginning-of-line)
    (gnus-add-text-properties
     (point)
     (prog1 (1+ (point))
       ;; Insert the text.
       (eval gnus-category-line-format-spec))
     (list 'gnus-category gnus-tmp-name))))

(defun gnus-enter-category-buffer ()
  "Go to the Category buffer."
  (interactive)
  (gnus-category-setup-buffer)
  (gnus-configure-windows 'category)
  (gnus-category-prepare))

(defun gnus-category-setup-buffer ()
  (unless (get-buffer gnus-category-buffer)
    (save-excursion
      (set-buffer (gnus-get-buffer-create gnus-category-buffer))
      (gnus-category-mode))))

(defun gnus-category-prepare ()
  (gnus-set-format 'category-mode)
  (gnus-set-format 'category t)
  (let ((alist gnus-category-alist)
	(buffer-read-only nil))
    (erase-buffer)
    (while alist
      (gnus-category-insert-line (pop alist)))
    (goto-char (point-min))
    (gnus-category-position-point)))

(defun gnus-category-name ()
  (or (intern (get-text-property (gnus-point-at-bol) 'gnus-category))
      (error "No category on the current line")))

(defun gnus-category-read ()
  "Read the category alist."
  (setq gnus-category-alist
        (or
         (with-temp-buffer
           (ignore-errors
            (nnheader-insert-file-contents (nnheader-concat gnus-agent-directory "lib/categories"))
            (goto-char (point-min))
            ;; This code isn't temp, it will be needed so long as
            ;; anyone may be migrating from an older version.

            ;; Once we're certain that people will not revert to an
            ;; earlier version, we can take out the old-list code in
            ;; gnus-category-write.
            (let* ((old-list (read (current-buffer)))
                   (new-list (ignore-errors (read (current-buffer)))))
              (if new-list
                  new-list
                ;; Convert from a positional list to an alist.
                (mapcar
                 (lambda (c)
                   (setcdr c
                           (delq nil
                                 (gnus-mapcar
                                  (lambda (valu symb)
                                    (if valu
                                        (cons symb valu)))
                                  (cdr c)
                                  '(agent-predicate agent-score-file agent-groups))))
                   c)
                 old-list)))))
         (list (gnus-agent-cat-make 'default 'short)))))

(defun gnus-category-write ()
  "Write the category alist."
  (setq gnus-category-predicate-cache nil
	gnus-category-group-cache nil)
  (gnus-make-directory (nnheader-concat gnus-agent-directory "lib"))
  (with-temp-file (nnheader-concat gnus-agent-directory "lib/categories")
    ;; This prin1 is temporary.  It exists so that people can revert
    ;; to an earlier version of gnus-agent.
    (prin1 (mapcar (lambda (c)
              (list (car c)
                    (cdr (assoc 'agent-predicate c))
                    (cdr (assoc 'agent-score-file c))
                    (cdr (assoc 'agent-groups c))))
                   gnus-category-alist)
           (current-buffer))
    (newline)
    (prin1 gnus-category-alist (current-buffer))))

(defun gnus-category-edit-predicate (category)
  "Edit the predicate for CATEGORY."
  (interactive (list (gnus-category-name)))
  (let ((info (assq category gnus-category-alist)))
    (gnus-edit-form
     (gnus-agent-cat-predicate info)
     (format "Editing the select predicate for category %s" category)
     `(lambda (predicate)
        ;; Avoid run-time execution of setf form
        ;; (setf (gnus-agent-cat-predicate (assq ',category gnus-category-alist))
        ;;       predicate)
        ;; use its expansion instead:
        (gnus-agent-cat-set-property (assq ',category gnus-category-alist)
                                     'agent-predicate predicate)

	(gnus-category-write)
	(gnus-category-list)))))

(defun gnus-category-edit-score (category)
  "Edit the score expression for CATEGORY."
  (interactive (list (gnus-category-name)))
  (let ((info (assq category gnus-category-alist)))
    (gnus-edit-form
     (gnus-agent-cat-score-file info)
     (format "Editing the score expression for category %s" category)
     `(lambda (score-file)
        ;; Avoid run-time execution of setf form
        ;; (setf (gnus-agent-cat-score-file (assq ',category gnus-category-alist))
        ;;       score-file)
        ;; use its expansion instead:
        (gnus-agent-cat-set-property (assq ',category gnus-category-alist)
                                     'agent-score-file score-file)

	(gnus-category-write)
	(gnus-category-list)))))

(defun gnus-category-edit-groups (category)
  "Edit the group list for CATEGORY."
  (interactive (list (gnus-category-name)))
  (let ((info (assq category gnus-category-alist)))
    (gnus-edit-form
     (gnus-agent-cat-groups info)
     (format "Editing the group list for category %s" category)
     `(lambda (groups)
        ;; Avoid run-time execution of setf form
        ;; (setf (gnus-agent-cat-groups (assq ',category gnus-category-alist))
        ;;       groups)
        ;; use its expansion instead:
        (gnus-agent-set-cat-groups (assq ',category gnus-category-alist)
                                   groups)

	(gnus-category-write)
	(gnus-category-list)))))

(defun gnus-category-kill (category)
  "Kill the current category."
  (interactive (list (gnus-category-name)))
  (let ((info (assq category gnus-category-alist))
	(buffer-read-only nil))
    (gnus-delete-line)
    (setq gnus-category-alist (delq info gnus-category-alist))
    (gnus-category-write)))

(defun gnus-category-copy (category to)
  "Copy the current category."
  (interactive (list (gnus-category-name) (intern (read-string "New name: "))))
  (let ((info (assq category gnus-category-alist)))
    (push (let ((newcat (gnus-copy-sequence info)))
            (setf (gnus-agent-cat-name newcat) to)
            (setf (gnus-agent-cat-groups newcat) nil)
            newcat)
	  gnus-category-alist)
    (gnus-category-write)
    (gnus-category-list)))

(defun gnus-category-add (category)
  "Create a new category."
  (interactive "SCategory name: ")
  (when (assq category gnus-category-alist)
    (error "Category %s already exists" category))
  (push (gnus-agent-cat-make category)
	gnus-category-alist)
  (gnus-category-write)
  (gnus-category-list))

(defun gnus-category-list ()
  "List all categories."
  (interactive)
  (gnus-category-prepare))

(defun gnus-category-exit ()
  "Return to the group buffer."
  (interactive)
  (kill-buffer (current-buffer))
  (gnus-configure-windows 'group t))

;; To avoid having 8-bit characters in the source file.
(defvar gnus-category-not (list '! 'not (intern (format "%c" 172))))

(defvar gnus-category-predicate-alist
  '((spam . gnus-agent-spam-p)
    (short . gnus-agent-short-p)
    (long . gnus-agent-long-p)
    (low . gnus-agent-low-scored-p)
    (high . gnus-agent-high-scored-p)
    (read . gnus-agent-read-p)
    (true . gnus-agent-true)
    (false . gnus-agent-false))
  "Mapping from short score predicate symbols to predicate functions.")

(defun gnus-agent-spam-p ()
  "Say whether an article is spam or not."
  (unless gnus-agent-spam-hashtb
    (setq gnus-agent-spam-hashtb (gnus-make-hashtable 1000)))
  (if (not (equal (mail-header-references gnus-headers) ""))
      nil
    (let ((string (gnus-simplify-subject (mail-header-subject gnus-headers))))
      (prog1
	  (gnus-gethash string gnus-agent-spam-hashtb)
	(gnus-sethash string t gnus-agent-spam-hashtb)))))

(defun gnus-agent-short-p ()
  "Say whether an article is short or not."
  (< (mail-header-lines gnus-headers) gnus-agent-short-article))

(defun gnus-agent-long-p ()
  "Say whether an article is long or not."
  (> (mail-header-lines gnus-headers) gnus-agent-long-article))

(defun gnus-agent-low-scored-p ()
  "Say whether an article has a low score or not."
  (< gnus-score gnus-agent-low-score))

(defun gnus-agent-high-scored-p ()
  "Say whether an article has a high score or not."
  (> gnus-score gnus-agent-high-score))

(defun gnus-agent-read-p ()
  "Say whether an article is read or not."
  (gnus-member-of-range (mail-header-number gnus-headers)
			(gnus-info-read (gnus-get-info gnus-newsgroup-name))))

(defun gnus-category-make-function (predicate)
  "Make a function from PREDICATE."
  (let ((func (gnus-category-make-function-1 predicate)))
    (if (and (= (length func) 1)
	     (symbolp (car func)))
	(car func)
      (gnus-byte-compile `(lambda () ,func)))))

(defun gnus-agent-true ()
  "Return t."
  t)

(defun gnus-agent-false ()
  "Return nil."
  nil)

(defun gnus-category-make-function-1 (predicate)
  "Make a function from PREDICATE."
  (cond
   ;; Functions are just returned as is.
   ((or (symbolp predicate)
	(functionp predicate))
    `(,(or (cdr (assq predicate gnus-category-predicate-alist))
	   predicate)))
   ;; More complex predicate.
   ((consp predicate)
    `(,(cond
	((memq (car predicate) '(& and))
	 'and)
	((memq (car predicate) '(| or))
	 'or)
	((memq (car predicate) gnus-category-not)
	 'not))
      ,@(mapcar 'gnus-category-make-function-1 (cdr predicate))))
   (t
    (error "Unknown predicate type: %s" predicate))))

(defun gnus-get-predicate (predicate)
  "Return the function implementing PREDICATE."
  (or (cdr (assoc predicate gnus-category-predicate-cache))
      (let ((func (gnus-category-make-function predicate)))
	(setq gnus-category-predicate-cache
	      (nconc gnus-category-predicate-cache
		     (list (cons predicate func))))
	func)))

(defun gnus-predicate-implies-unread (predicate)
  "Say whether PREDICATE implies unread articles only.
It is okay to miss some cases, but there must be no false positives.
That is, if this predicate returns true, then indeed the predicate must
return only unread articles."
  (eq t (gnus-function-implies-unread-1 
         (gnus-category-make-function-1 predicate))))

(defun gnus-function-implies-unread-1 (function)
  "Recursively evaluate a predicate function to determine whether it can select
any read articles.  Returns t if the function is known to never
return read articles, nil when it is known to always return read
articles, and t_nil when the function may return both read and unread
articles."
  (let ((func (car function))
        (args (mapcar 'gnus-function-implies-unread-1 (cdr function))))
    (cond ((eq func 'and)
           (cond ((memq t args) ; if any argument returns only unread articles
                  ;; then that argument constrains the result to only unread articles.
                  t)
                 ((memq 't_nil args) ; if any argument is indeterminate
                  ;; then the result is indeterminate
                  't_nil)))
          ((eq func 'or)
           (cond ((memq nil args) ; if any argument returns read articles
                  ;; then that argument ensures that the results includes read articles.
                  nil)
                 ((memq 't_nil args) ; if any argument is indeterminate
                  ;; then that argument ensures that the results are indeterminate
                  't_nil)
                 (t ; if all arguments return only unread articles
                  ;; then the result returns only unread articles
                  t)))
          ((eq func 'not)
           (cond ((eq (car args) 't_nil) ; if the argument is indeterminate
                  ; then the result is indeterminate
                  (car args))
                 (t ; otherwise
                  ; toggle the result to be the opposite of the argument
                  (not (car args)))))
          ((eq func 'gnus-agent-read-p)
           nil) ; The read predicate NEVER returns unread articles
          ((eq func 'gnus-agent-false)
           t) ; The false predicate returns t as the empty set excludes all read articles
          ((eq func 'gnus-agent-true)
           nil) ; The true predicate ALWAYS returns read articles
          ((catch 'found-match
             (let ((alist gnus-category-predicate-alist))
               (while alist
                 (if (eq func (cdar alist))
                     (throw 'found-match t)
                   (setq alist (cdr alist))))))
           't_nil) ; All other predicates return read and unread articles
          (t
           (error "Unknown predicate function: %s" function)))))

(defun gnus-group-category (group)
  "Return the category GROUP belongs to."
  (unless gnus-category-group-cache
    (setq gnus-category-group-cache (gnus-make-hashtable 1000))
    (let ((cs gnus-category-alist)
	  groups cat)
      (while (setq cat (pop cs))
	(setq groups (gnus-agent-cat-groups cat))
	(while groups
	  (gnus-sethash (pop groups) cat gnus-category-group-cache)))))
  (or (gnus-gethash group gnus-category-group-cache)
      (assq 'default gnus-category-alist)))

(defun gnus-agent-expire-group (group &optional articles force)
  "Expire all old articles in GROUP.
If you want to force expiring of certain articles, this function can
take ARTICLES, and FORCE parameters as well.

The articles on which the expiration process runs are selected as follows:
  if ARTICLES is null, all read and unmarked articles.
  if ARTICLES is t, all articles.
  if ARTICLES is a list, just those articles.
FORCE is equivalent to setting the expiration predicates to true."
  (interactive
   (list (let ((def (or (gnus-group-group-name)
                        gnus-newsgroup-name)))
           (let ((select (read-string (if def
                                          (concat "Group Name ("
                                                  def "): ")
                                        "Group Name: "))))
             (if (and (equal "" select)
                      def)
                 def
               select)))))

  (if (not group)
      (gnus-agent-expire articles group force)
    (let ( ;; Bind gnus-agent-expire-stats to enable tracking of
	   ;; expiration statistics of this single group
          (gnus-agent-expire-stats (list 0 0 0.0)))
      (if (or (not (eq articles t))
              (yes-or-no-p
               (concat "Are you sure that you want to "
                       "expire all articles in " group ".")))
          (let ((gnus-command-method (gnus-find-method-for-group group))
                (overview (gnus-get-buffer-create " *expire overview*"))
                orig)
            (unwind-protect
                (let ((active-file (gnus-agent-lib-file "active")))
                  (when (file-exists-p active-file)
                    (with-temp-buffer
                      (nnheader-insert-file-contents active-file)
                      (gnus-active-to-gnus-format
                       gnus-command-method
                       (setq orig (gnus-make-hashtable
                                   (count-lines (point-min) (point-max))))))
                    (save-excursion
                      (gnus-agent-expire-group-1
                       group overview (gnus-gethash-safe group orig)
                       articles force))
                    (gnus-agent-write-active active-file orig t)))
              (kill-buffer overview))))
      (gnus-message 4 (gnus-agent-expire-done-message)))))

(defun gnus-agent-expire-group-1 (group overview active articles force)
  ;; Internal function - requires caller to have set
  ;; gnus-command-method, initialized overview buffer, and to have
  ;; provided a non-nil active

  (let ((dir (gnus-agent-group-pathname group)))
    (when (boundp 'gnus-agent-expire-current-dirs)
      (set 'gnus-agent-expire-current-dirs 
           (cons dir 
                 (symbol-value 'gnus-agent-expire-current-dirs))))

    (if (and (not force)
             (eq 'DISABLE (gnus-agent-find-parameter group 
                                                     'agent-enable-expiration)))
        (gnus-message 5 "Expiry skipping over %s" group)
      (gnus-message 5 "Expiring articles in %s" group)
      (gnus-agent-load-alist group)
      (let* ((stats (if (boundp 'gnus-agent-expire-stats)
                        ;; Use the list provided by my caller
                        (symbol-value 'gnus-agent-expire-stats)
                      ;; otherwise use my own temporary list
                      (list 0 0 0.0)))
             (info (gnus-get-info group))
             (alist gnus-agent-article-alist)
             (day (- (time-to-days (current-time))
                     (gnus-agent-find-parameter group 'agent-days-until-old)))
             (specials (if (and alist
                                (not force))
                           ;; This could be a bit of a problem.  I need to
                           ;; keep the last article to avoid refetching
                           ;; headers when using nntp in the backend.  At
                           ;; the same time, if someone uses a backend
                           ;; that supports article moving then I may have
                           ;; to remove the last article to complete the
                           ;; move.  Right now, I'm going to assume that
                           ;; FORCE overrides specials.
                           (list (caar (last alist)))))
             (unreads ;; Articles that are excluded from the
              ;; expiration process
              (cond (gnus-agent-expire-all
                     ;; All articles are marked read by global decree
                     nil)
                    ((eq articles t)
                     ;; All articles are marked read by function
                     ;; parameter
                     nil)
                    ((not articles)
                     ;; Unread articles are marked protected from
                     ;; expiration Don't call
                     ;; gnus-list-of-unread-articles as it returns
                     ;; articles that have not been fetched into the
                     ;; agent.
                     (ignore-errors
                       (gnus-agent-unread-articles group)))
                    (t
                     ;; All articles EXCEPT those named by the caller
                     ;; are protected from expiration
                     (gnus-sorted-difference
                      (gnus-uncompress-range
                       (cons (caar alist)
                             (caar (last alist))))
                      (sort articles '<)))))
             (marked ;; More articles that are excluded from the
              ;; expiration process
              (cond (gnus-agent-expire-all
                     ;; All articles are unmarked by global decree
                     nil)
                    ((eq articles t)
                     ;; All articles are unmarked by function
                     ;; parameter
                     nil)
                    (articles
                     ;; All articles may as well be unmarked as the
                     ;; unreads list already names the articles we are
                     ;; going to keep
                     nil)
                    (t
                     ;; Ticked and/or dormant articles are excluded
                     ;; from expiration
                     (nconc
                      (gnus-uncompress-range
                       (cdr (assq 'tick (gnus-info-marks info))))
                      (gnus-uncompress-range
                       (cdr (assq 'dormant
                                  (gnus-info-marks info))))))))
             (nov-file (concat dir ".overview"))
             (cnt 0)
             (completed -1)
             dlist
             type)

        ;; The normal article alist contains elements that look like
        ;; (article# .  fetch_date) I need to combine other
        ;; information with this list.  For example, a flag indicating
        ;; that a particular article MUST BE KEPT.  To do this, I'm
        ;; going to transform the elements to look like (article#
        ;; fetch_date keep_flag NOV_entry_marker) Later, I'll reverse
        ;; the process to generate the expired article alist.

        ;; Convert the alist elements to (article# fetch_date nil
        ;; nil).
        (setq dlist (mapcar (lambda (e)
                              (list (car e) (cdr e) nil nil)) alist))

        ;; Convert the keep lists to elements that look like (article#
        ;; nil keep_flag nil) then append it to the expanded dlist
        ;; These statements are sorted by ascending precidence of the
        ;; keep_flag.
        (setq dlist (nconc dlist
                           (mapcar (lambda (e)
                                     (list e nil 'unread  nil))
                                   unreads)))
        (setq dlist (nconc dlist
                           (mapcar (lambda (e)
                                     (list e nil 'marked  nil))
                                   marked)))
        (setq dlist (nconc dlist
                           (mapcar (lambda (e)
                                     (list e nil 'special nil))
                                   specials)))

        (set-buffer overview)
        (erase-buffer)
        (buffer-disable-undo)
        (when (file-exists-p nov-file)
          (gnus-message 7 "gnus-agent-expire: Loading overview...")
          (nnheader-insert-file-contents nov-file)
          (goto-char (point-min))

          (let (p)
            (while (< (setq p (point)) (point-max))
              (condition-case nil
                  ;; If I successfully read an integer (the plus zero
                  ;; ensures a numeric type), prepend a marker entry
                  ;; to the list
                  (push (list (+ 0 (read (current-buffer))) nil nil
                              (set-marker (make-marker) p))
                        dlist)
                (error
                 (gnus-message 1 "gnus-agent-expire: read error \
occurred when reading expression at %s in %s.  Skipping to next \
line." (point) nov-file)))
              ;; Whether I succeeded, or failed, it doesn't matter.
              ;; Move to the next line then try again.
              (forward-line 1)))

          (gnus-message
           7 "gnus-agent-expire: Loading overview... Done"))
        (set-buffer-modified-p nil)

        ;; At this point, all of the information is in dlist.  The
        ;; only problem is that much of it is spread across multiple
        ;; entries.  Sort then MERGE!!
        (gnus-message 7 "gnus-agent-expire: Sorting entries... ")
        ;; If two entries have the same article-number then sort by
        ;; ascending keep_flag.
        (let ((special 0)
              (marked 1)
              (unread 2))
          (setq dlist
                (sort dlist
                      (lambda (a b)
                        (cond ((< (nth 0 a) (nth 0 b))
                               t)
                              ((> (nth 0 a) (nth 0 b))
                               nil)
                              (t
                               (let ((a (or (symbol-value (nth 2 a))
                                            3))
                                     (b (or (symbol-value (nth 2 b))
                                            3)))
                                 (<= a b))))))))
        (gnus-message 7 "gnus-agent-expire: Sorting entries... Done")
        (gnus-message 7 "gnus-agent-expire: Merging entries... ")
        (let ((dlist dlist))
          (while (cdr dlist)            ; I'm not at the end-of-list
            (if (eq (caar dlist) (caadr dlist))
                (let ((first (cdr (car dlist)))
                      (secnd (cdr (cadr dlist))))
                  (setcar first (or (car first)
                                    (car secnd))) ; fetch_date
                  (setq first (cdr first)
                        secnd (cdr secnd))
                  (setcar first (or (car first)
                                    (car secnd))) ; Keep_flag
                  (setq first (cdr first)
                        secnd (cdr secnd))
                  (setcar first (or (car first)
                                    (car secnd))) ; NOV_entry_marker

                  (setcdr dlist (cddr dlist)))
              (setq dlist (cdr dlist)))))
        (gnus-message 7 "gnus-agent-expire: Merging entries... Done")

        (let* ((len (float (length dlist)))
               (alist (list nil))
               (tail-alist alist))
          (while dlist
            (let ((new-completed (truncate (* 100.0
                                              (/ (setq cnt (1+ cnt))
                                                 len))))
		  message-log-max)
              (when (> new-completed completed)
                (setq completed new-completed)
                (gnus-message 7 "%3d%% completed..."  completed)))
            (let* ((entry          (car dlist))
                   (article-number (nth 0 entry))
                   (fetch-date     (nth 1 entry))
                   (keep           (nth 2 entry))
                   (marker         (nth 3 entry)))

              (cond
               ;; Kept articles are unread, marked, or special.
               (keep
                (gnus-agent-message 10
                                    "gnus-agent-expire: %s:%d: Kept %s article%s."
                                    group article-number keep (if fetch-date " and file" ""))
                (when fetch-date
                  (unless (file-exists-p
                           (concat dir (number-to-string
                                        article-number)))
                    (setf (nth 1 entry) nil)
                    (gnus-agent-message 3 "gnus-agent-expire cleared \
download flag on %s:%d as the cached article file is missing."
                                        group (caar dlist)))
                  (unless marker
                    (gnus-message 1 "gnus-agent-expire detected a \
missing NOV entry.  Run gnus-agent-regenerate-group to restore it.")))
                (gnus-agent-append-to-list
                 tail-alist
                 (cons article-number fetch-date)))

               ;; The following articles are READ, UNMARKED, and
               ;; ORDINARY.  See if they can be EXPIRED!!!
               ((setq type
                      (cond
                       ((not (integerp fetch-date))
                        'read) ;; never fetched article (may expire
                       ;; right now)
                       ((not (file-exists-p
                              (concat dir (number-to-string
                                           article-number))))
                        (setf (nth 1 entry) nil)
                        'externally-expired) ;; Can't find the cached
                       ;; article.  Handle case
                       ;; as though this article
                       ;; was never fetched.

                       ;; We now have the arrival day, so we see
                       ;; whether it's old enough to be expired.
                       ((< fetch-date day)
                        'expired)
                       (force
                        'forced)))

                ;; I found some reason to expire this entry.

                (let ((actions nil))
                  (when (memq type '(forced expired))
                    (ignore-errors      ; Just being paranoid.
                      (let ((file-name (concat dir (number-to-string
                                                article-number))))
                        (incf (nth 2 stats) (nth 7 (file-attributes file-name)))
                        (incf (nth 1 stats))
                        (delete-file file-name))
                      (push "expired cached article" actions))
                    (setf (nth 1 entry) nil)
                    )

                  (when marker
                    (push "NOV entry removed" actions)
                    (goto-char marker)

                    (incf (nth 0 stats))

                    (let ((from (gnus-point-at-bol))
                          (to (progn (forward-line 1) (point))))
                      (incf (nth 2 stats) (- to from))
                      (delete-region from to)))

                  ;; If considering all articles is set, I can only
                  ;; expire article IDs that are no longer in the
                  ;; active range (That is, articles that preceed the
                  ;; first article in the new alist).
                  (if (and gnus-agent-consider-all-articles
                           (>= article-number (car active)))
                      ;; I have to keep this ID in the alist
                      (gnus-agent-append-to-list
                       tail-alist (cons article-number fetch-date))
                    (push (format "Removed %s article number from \
article alist" type) actions))

		  (when actions
		    (gnus-agent-message 8 "gnus-agent-expire: %s:%d: %s"
					group article-number
					(mapconcat 'identity actions ", ")))))
               (t
                (gnus-agent-message
                 10 "gnus-agent-expire: %s:%d: Article kept as \
expiration tests failed." group article-number)
                (gnus-agent-append-to-list
                 tail-alist (cons article-number fetch-date)))
               )

              ;; Clean up markers as I want to recycle this buffer
              ;; over several groups.
              (when marker
                (set-marker marker nil))

              (setq dlist (cdr dlist))))

          (setq alist (cdr alist))

          (let ((inhibit-quit t))
            (unless (equal alist gnus-agent-article-alist)
              (setq gnus-agent-article-alist alist)
              (gnus-agent-save-alist group)

              ;; The active list changed, set the agent's active range
              ;; to match the beginning of the list.
              (if alist
                  (setcar active (caar alist))))

            (when (buffer-modified-p)
              (let ((coding-system-for-write
                     gnus-agent-file-coding-system))
                (gnus-make-directory dir)
                (write-region (point-min) (point-max) nov-file nil
                              'silent)
                ;; clear the modified flag as that I'm not confused by
                ;; its status on the next pass through this routine.
                (set-buffer-modified-p nil)))

            (when (eq articles t)
              (gnus-summary-update-info))))))))

(defun gnus-agent-expire (&optional articles group force)
  "Expire all old articles.
If you want to force expiring of certain articles, this function can
take ARTICLES, GROUP and FORCE parameters as well.

The articles on which the expiration process runs are selected as follows:
  if ARTICLES is null, all read and unmarked articles.
  if ARTICLES is t, all articles.
  if ARTICLES is a list, just those articles.
Setting GROUP will limit expiration to that group.
FORCE is equivalent to setting the expiration predicates to true."
  (interactive)
  
  (if group
      (gnus-agent-expire-group group articles force)
    (if (or (not (eq articles t))
            (yes-or-no-p "Are you sure that you want to expire all \
articles in every agentized group."))
        (let ((methods (gnus-agent-covered-methods))
              ;; Bind gnus-agent-expire-current-dirs to enable tracking
              ;; of agent directories.
              (gnus-agent-expire-current-dirs nil)
              ;; Bind gnus-agent-expire-stats to enable tracking of
              ;; expiration statistics across all groups
              (gnus-agent-expire-stats (list 0 0 0.0))
              gnus-command-method overview orig)
          (setq overview (gnus-get-buffer-create " *expire overview*"))
          (unwind-protect
              (while (setq gnus-command-method (pop methods))
                (let ((active-file (gnus-agent-lib-file "active")))
                  (when (file-exists-p active-file)
                    (with-temp-buffer
                      (nnheader-insert-file-contents active-file)
                      (gnus-active-to-gnus-format
                       gnus-command-method
                       (setq orig (gnus-make-hashtable
                                   (count-lines (point-min) (point-max))))))
                    (dolist (expiring-group (gnus-groups-from-server
                                             gnus-command-method))
                      (let* ((active
                              (gnus-gethash-safe expiring-group orig)))
                                        
                        (when active
                          (save-excursion
                            (gnus-agent-expire-group-1
                             expiring-group overview active articles force)))))
                    (gnus-agent-write-active active-file orig t))))
            (kill-buffer overview))
          (gnus-agent-expire-unagentized-dirs)
          (gnus-message 4 (gnus-agent-expire-done-message))))))

(defun gnus-agent-expire-done-message ()
  (if (and (> gnus-verbose 4)
           (boundp 'gnus-agent-expire-stats))
      (let* ((stats (symbol-value 'gnus-agent-expire-stats))
             (size (nth 2 stats))
            (units '(B KB MB GB)))
        (while (and (> size 1024.0)
                    (cdr units))
          (setq size (/ size 1024.0)
                units (cdr units)))

        (format "Expiry recovered %d NOV entries, deleted %d files,\
 and freed %f %s." 
                (nth 0 stats) 
                (nth 1 stats) 
                size (car units)))
    "Expiry...done"))

(defun gnus-agent-expire-unagentized-dirs ()
  (when (and gnus-agent-expire-unagentized-dirs
             (boundp 'gnus-agent-expire-current-dirs))
    (let* ((keep (gnus-make-hashtable))
	   ;; Formally bind gnus-agent-expire-current-dirs so that the
	   ;; compiler will not complain about free references.
	   (gnus-agent-expire-current-dirs
	    (symbol-value 'gnus-agent-expire-current-dirs))
           dir)

      (gnus-sethash gnus-agent-directory t keep)
      (while gnus-agent-expire-current-dirs
	(setq dir (pop gnus-agent-expire-current-dirs))
	(when (and (stringp dir)
		   (file-directory-p dir))
	  (while (not (gnus-gethash dir keep))
	    (gnus-sethash dir t keep)
	    (setq dir (file-name-directory (directory-file-name dir))))))

      (let* (to-remove
             checker
             (checker
              (function
               (lambda (d)
                 "Given a directory, check it and its subdirectories for 
              membership in the keep hash.  If it isn't found, add 
              it to to-remove." 
                 (let ((files (directory-files d))
                       file)
                   (while (setq file (pop files))
                     (cond ((equal file ".") ; Ignore self
                            nil)
                           ((equal file "..") ; Ignore parent
                            nil)
                           ((equal file ".overview") 
                            ;; Directory must contain .overview to be
                            ;; agent's cache of a group.
                            (let ((d (file-name-as-directory d))
                                  r)
                              ;; Search ancestor's for last directory NOT
                              ;; found in keep hash.
                              (while (not (gnus-gethash
                                           (setq d (file-name-directory d)) keep))
                                (setq r d
                                      d (directory-file-name d)))
                              ;; if ANY ancestor was NOT in keep hash and
                              ;; it it's already in to-remove, add it to
                              ;; to-remove.                          
                              (if (and r
                                       (not (member r to-remove)))
                                  (push r to-remove))))
                           ((file-directory-p (setq file (nnheader-concat d file)))
                            (funcall checker file)))))))))
        (funcall checker (expand-file-name gnus-agent-directory))

        (when (and to-remove
                   (or gnus-expert-user
                       (gnus-y-or-n-p
                        "gnus-agent-expire has identified local directories that are\
 not currently required by any agentized group.	 Do you wish to consider\
 deleting them?")))
          (while to-remove
            (let ((dir (pop to-remove)))
              (if (gnus-y-or-n-p (format "Delete %s? " dir))
                  (let* (delete-recursive
                         (delete-recursive
                          (function
                           (lambda (f-or-d)
                             (ignore-errors
                               (if (file-directory-p f-or-d)
                                   (condition-case nil
                                       (delete-directory f-or-d)
                                     (file-error
                                      (mapcar (lambda (f)
                                                (or (member f '("." ".."))
                                                    (funcall delete-recursive
                                                             (nnheader-concat
                                                              f-or-d f))))
                                              (directory-files f-or-d))
                                      (delete-directory f-or-d)))
                                 (delete-file f-or-d)))))))
                    (funcall delete-recursive dir))))))))))

;;;###autoload
(defun gnus-agent-batch ()
  "Start Gnus, send queue and fetch session."
  (interactive)
  (let ((init-file-user "")
	(gnus-always-read-dribble-file t))
    (gnus))
  (let ((gnus-agent-confirmation-function 'gnus-agent-batch-confirmation))
    (gnus-group-send-queue)
    (gnus-agent-fetch-session)))

(defun gnus-agent-unread-articles (group)
  (let* ((read (gnus-info-read (gnus-get-info group)))
	 (known (gnus-agent-load-alist group))
	 (unread (list nil))
	 (tail-unread unread))
    (while (and known read)
      (let ((candidate (car (pop known))))
	(while (let* ((range (car read))
		      (min   (if (numberp range) range (car range)))
		      (max   (if (numberp range) range (cdr range))))
		 (cond ((or (not min)
			    (< candidate min))
			(gnus-agent-append-to-list tail-unread candidate)
			nil)
		       ((> candidate max)
			(setq read (cdr read))
                        ;; return t so that I always loop one more
                        ;; time.  If I just iterated off the end of
                        ;; read, min will become nil and the current
                        ;; candidate will be added to the unread list.
                        t))))))
    (while known
      (gnus-agent-append-to-list tail-unread (car (pop known))))
    (cdr unread)))

(defun gnus-agent-uncached-articles (articles group &optional cached-header)
  "Restrict ARTICLES to numbers already fetched.
Returns a sublist of ARTICLES that excludes thos article ids in GROUP
that have already been fetched.
If CACHED-HEADER is nil, articles are only excluded if the article itself
has been fetched."

  ;; Logically equivalent to: (gnus-sorted-difference articles (mapcar
  ;; 'car gnus-agent-article-alist))

  ;; Functionally, I don't need to construct a temp list using mapcar.

  (if (and (or gnus-agent-cache (not gnus-plugged))
           (gnus-agent-load-alist group))
    (let* ((ref gnus-agent-article-alist)
           (arts articles)
           (uncached (list nil))
           (tail-uncached uncached))
      (while (and ref arts)
        (let ((v1 (car arts))
              (v2 (caar ref)))
          (cond ((< v1 v2) ; v1 does not appear in the reference list
		 (gnus-agent-append-to-list tail-uncached v1)
                 (setq arts (cdr arts)))
                ((= v1 v2)
                 (unless (or cached-header (cdar ref)) ; v1 is already cached
		   (gnus-agent-append-to-list tail-uncached v1))
                 (setq arts (cdr arts))
                 (setq ref (cdr ref)))
                (t ; reference article (v2) preceeds the list being filtered
                 (setq ref (cdr ref))))))
      (while arts
	(gnus-agent-append-to-list tail-uncached (pop arts)))
      (cdr uncached))
    ;; if gnus-agent-load-alist fails, no articles are cached.
    articles))

(defun gnus-agent-retrieve-headers (articles group &optional fetch-old)
  (save-excursion
    (gnus-agent-create-buffer)
    (let ((gnus-decode-encoded-word-function 'identity)
	  (file (gnus-agent-article-name ".overview" group))
	  cached-articles uncached-articles)
      (gnus-make-directory (nnheader-translate-file-chars
			    (file-name-directory file) t))

      ;; Populate temp buffer with known headers
      (when (file-exists-p file)
	(with-current-buffer gnus-agent-overview-buffer
	  (erase-buffer)
	  (let ((nnheader-file-coding-system
		 gnus-agent-file-coding-system))
	    (nnheader-insert-nov-file file (car articles)))))

      (if (setq uncached-articles (gnus-agent-uncached-articles articles group
                                                                t))
	  (progn
            ;; Populate nntp-server-buffer with uncached headers
	    (set-buffer nntp-server-buffer)
	    (erase-buffer)
            (cond ((not (eq 'nov (let (gnus-agent) ; Turn off agent
                                   (gnus-retrieve-headers
                                    uncached-articles group fetch-old))))
                   (nnvirtual-convert-headers))
                  ((eq 'nntp (car gnus-current-select-method))
                   ;; The author of gnus-get-newsgroup-headers-xover
                   ;; reports that the XOVER command is commonly
                   ;; unreliable. The problem is that recently
                   ;; posted articles may not be entered into the
                   ;; NOV database in time to respond to my XOVER
                   ;; query.
                   ;;
                   ;; I'm going to use his assumption that the NOV
                   ;; database is updated in order of ascending
                   ;; article ID.  Therefore, a response containing
                   ;; article ID N implies that all articles from 1
                   ;; to N-1 are up-to-date.  Therefore, missing
                   ;; articles in that range have expired.

                   (set-buffer nntp-server-buffer)
                   (let* ((fetched-articles (list nil))
                          (tail-fetched-articles fetched-articles)
                          (min (cond ((numberp fetch-old)
                                      (max 1 (- (car articles) fetch-old)))
                                     (fetch-old
                                      1)
                                     (t
                                      (car articles))))
                          (max (car (last articles))))

                     ;; Get the list of articles that were fetched
                     (goto-char (point-min))
                     (let ((pm (point-max)))
                       (while (< (point) pm)
                         (when (looking-at "[0-9]+\t")
                           (gnus-agent-append-to-list
                            tail-fetched-articles
                            (read (current-buffer))))
                         (forward-line 1)))

                     ;; Clip this list to the headers that will
                     ;; actually be returned
                     (setq fetched-articles (gnus-list-range-intersection
                                             (cdr fetched-articles)
                                             (cons min max)))

                     ;; Clip the uncached articles list to exclude
                     ;; IDs after the last FETCHED header.  The
                     ;; excluded IDs may be fetchable using HEAD.
                     (if (car tail-fetched-articles)
                         (setq uncached-articles
                               (gnus-list-range-intersection
                                uncached-articles
                                (cons (car uncached-articles)
                                      (car tail-fetched-articles)))))

                     ;; Create the list of articles that were
                     ;; "successfully" fetched.  Success, in this
                     ;; case, means that the ID should not be
                     ;; fetched again.  In the case of an expired
                     ;; article, the header will not be fetched.
                     (setq uncached-articles
                           (gnus-sorted-nunion fetched-articles
                                               uncached-articles))
                     )))

            ;; Erase the temp buffer
	    (set-buffer gnus-agent-overview-buffer)
	    (erase-buffer)

            ;; Copy the nntp-server-buffer to the temp buffer
	    (set-buffer nntp-server-buffer)
	    (copy-to-buffer gnus-agent-overview-buffer (point-min) (point-max))

            ;; Merge the temp buffer with the known headers (found on
            ;; disk in FILE) into the nntp-server-buffer
	    (when (and uncached-articles (file-exists-p file))
	      (gnus-agent-braid-nov group uncached-articles file))

            ;; Save the new set of known headers to FILE
	    (set-buffer nntp-server-buffer)
	    (let ((coding-system-for-write
		   gnus-agent-file-coding-system))
	      (gnus-agent-check-overview-buffer)
	      (write-region (point-min) (point-max) file nil 'silent))

            ;; Update the group's article alist to include the newly
            ;; fetched articles.
	    (gnus-agent-load-alist group)
	    (gnus-agent-save-alist group uncached-articles nil)
            )

        ;; Copy the temp buffer to the nntp-server-buffer
        (set-buffer nntp-server-buffer)
	(erase-buffer)
	(insert-buffer-substring gnus-agent-overview-buffer)))

    (if (and fetch-old
	     (not (numberp fetch-old)))
	t				; Don't remove anything.
      (nnheader-nov-delete-outside-range
       (if fetch-old (max 1 (- (car articles) fetch-old))
	 (car articles))
       (car (last articles)))
      t)

    'nov))

(defun gnus-agent-request-article (article group)
  "Retrieve ARTICLE in GROUP from the agent cache."
  (when (and gnus-agent
             (or gnus-agent-cache
                 (not gnus-plugged))
             (numberp article))
    (let* ((gnus-command-method (gnus-find-method-for-group group))
           (file (gnus-agent-article-name (number-to-string article) group))
           (buffer-read-only nil))
      (when (and (file-exists-p file)
                 (> (nth 7 (file-attributes file)) 0))
        (erase-buffer)
        (gnus-kill-all-overlays)
        (let ((coding-system-for-read gnus-cache-coding-system))
          (insert-file-contents file))
        t))))

(defun gnus-agent-regenerate-group (group &optional reread)
  "Regenerate GROUP.
If REREAD is t, all articles in the .overview are marked as unread.
If REREAD is not nil, downloaded articles are marked as unread."
  (interactive
   (list (let ((def (or (gnus-group-group-name)
                        gnus-newsgroup-name)))
           (let ((select (read-string (if def
                                          (concat "Group Name ("
                                                  def "): ")
                                        "Group Name: "))))
             (if (and (equal "" select)
                      def)
                 def
               select)))
         (catch 'mark
           (while (let ((c (read-char-exclusive 
                            "Mark as unread: (n)one / (a)ll / all (d)ownloaded articles? (n)"
                            )))
                    (cond ((or (eq c ?\r) (eq c ?n) (eq c ?N))
                           (throw 'mark nil))
                          ((or (eq c ?a) (eq c ?A))
                           (throw 'mark t))
                          ((or (eq c ?d) (eq c ?D))
                           (throw 'mark 'some)))
                    (message "Ignoring unexpected input")
                    (sit-for 1)
                    t)))))

  (when group
      (gnus-message 5 "Regenerating in %s" group)
      (let* ((gnus-command-method (or gnus-command-method
                                      (gnus-find-method-for-group group)))
             (file (gnus-agent-article-name ".overview" group))
             (dir (file-name-directory file))
             point
             (downloaded (if (file-exists-p dir)
                             (sort (mapcar (lambda (name) (string-to-int name))
                                           (directory-files dir nil "^[0-9]+$" t))
                                   '>)
                           (progn (gnus-make-directory dir) nil)))
             dl nov-arts
             alist header
             regenerated)

        (mm-with-unibyte-buffer
          (if (file-exists-p file)
              (let ((nnheader-file-coding-system
                     gnus-agent-file-coding-system))
                (nnheader-insert-file-contents file)))
          (set-buffer-modified-p nil)

          ;; Load the article IDs found in the overview file.  As a
          ;; side-effect, validate the file contents.
          (let ((load t))
            (while load
              (setq load nil)
              (goto-char (point-min))
              (while (< (point) (point-max))
                (cond ((and (looking-at "[0-9]+\t")
                            (<= (- (match-end 0) (match-beginning 0)) 9))
                       (push (read (current-buffer)) nov-arts)
                       (forward-line 1)
                       (let ((l1 (car nov-arts))
                             (l2 (cadr nov-arts)))
                         (cond ((not l2)
                                nil)
                               ((< l1 l2)
                                (gnus-message 3 "gnus-agent-regenerate-group: NOV\
 entries are NOT in ascending order.")
                                ;; Don't sort now as I haven't verified
                                ;; that every line begins with a number
                                (setq load t))
                               ((= l1 l2)
                                (forward-line -1)
                                (gnus-message 4 "gnus-agent-regenerate-group: NOV\
 entries contained duplicate of article %s.	 Duplicate deleted." l1)
                                (gnus-delete-line)
                                (setq nov-arts (cdr nov-arts))))))
                      (t
                       (gnus-message 1 "gnus-agent-regenerate-group: NOV\
 entries contained line that did not begin with an article number.  Deleted\
 line.")
                       (gnus-delete-line))))
              (if load
                  (progn
                    (gnus-message 5 "gnus-agent-regenerate-group: Sorting NOV\
 entries into ascending order.")
                    (sort-numeric-fields 1 (point-min) (point-max))
                    (setq nov-arts nil)))))
          (gnus-agent-check-overview-buffer)

          ;; Construct a new article alist whose nodes match every header
          ;; in the .overview file.  As a side-effect, missing headers are
          ;; reconstructed from the downloaded article file.
          (while (or downloaded nov-arts)
            (cond ((and downloaded
                        (or (not nov-arts)
                            (> (car downloaded) (car nov-arts))))
                   ;; This entry is missing from the overview file
                   (gnus-message 3 "Regenerating NOV %s %d..." group
                                 (car downloaded))
                   (let ((file (concat dir (number-to-string (car downloaded)))))
                     (mm-with-unibyte-buffer
                       (nnheader-insert-file-contents file)
                       (nnheader-remove-body)
                       (setq header (nnheader-parse-naked-head)))
                     (mail-header-set-number header (car downloaded))
                     (if nov-arts
                         (let ((key (concat "^" (int-to-string (car nov-arts))
                                            "\t")))
                           (or (re-search-backward key nil t)
                               (re-search-forward key))
                           (forward-line 1))
                       (goto-char (point-min)))
                     (nnheader-insert-nov header))
                   (setq nov-arts (cons (car downloaded) nov-arts)))
                  ((eq (car downloaded) (car nov-arts))
                   ;; This entry in the overview has been downloaded
                   (push (cons (car downloaded)
                               (time-to-days
                                (nth 5 (file-attributes
                                        (concat dir (number-to-string
                                                     (car downloaded))))))) alist)
                   (setq downloaded (cdr downloaded))
                   (setq nov-arts (cdr nov-arts)))
                  (t
                   ;; This entry in the overview has not been downloaded
                   (push (cons (car nov-arts) nil) alist)
                   (setq nov-arts (cdr nov-arts)))))

          ;; When gnus-agent-consider-all-articles is set,
          ;; gnus-agent-regenerate-group should NOT remove article IDs from
          ;; the alist.  Those IDs serve as markers to indicate that an
          ;; attempt has been made to fetch that article's header.

          ;; When gnus-agent-consider-all-articles is NOT set,
          ;; gnus-agent-regenerate-group can remove the article ID of every
          ;; article (with the exception of the last ID in the list - it's
          ;; special) that no longer appears in the overview.  In this
          ;; situtation, the last article ID in the list implies that it,
          ;; and every article ID preceeding it, have been fetched from the
          ;; server.

          (if gnus-agent-consider-all-articles
              ;; Restore all article IDs that were not found in the overview file.
              (let* ((n (cons nil alist))
                     (merged n)
                     (o (gnus-agent-load-alist group)))
                (while o
                  (let ((nID (caadr n))
                        (oID (caar o)))
                    (cond ((not nID)
                           (setq n (setcdr n (list (list oID))))
                           (setq o (cdr o)))
                          ((< oID nID)
                           (setcdr n (cons (list oID) (cdr n)))
                           (setq o (cdr o)))
                          ((= oID nID)
                           (setq o (cdr o))
                           (setq n (cdr n)))
                          (t
                           (setq n (cdr n))))))
                (setq alist (cdr merged)))
            ;; Restore the last article ID if it is not already in the new alist
            (let ((n (last alist))
                  (o (last (gnus-agent-load-alist group))))
              (cond ((not o)
                     nil)
                    ((not n)
                     (push (cons (caar o) nil) alist))
                    ((< (caar n) (caar o))
                     (setcdr n (list (car o)))))))

          (let ((inhibit-quit t))
            (if (setq regenerated (buffer-modified-p))
                (let ((coding-system-for-write gnus-agent-file-coding-system))
                  (write-region (point-min) (point-max) file nil 'silent)))

            (setq regenerated (or regenerated
                                  (and reread gnus-agent-article-alist)
                                  (not (equal alist gnus-agent-article-alist))))

            (setq gnus-agent-article-alist alist)

            (when regenerated
              (gnus-agent-save-alist group)
       
              ;; I have to alter the group's active range NOW as
              ;; gnus-make-ascending-articles-unread will use it to
              ;; recalculate the number of unread articles in the group

              (let ((group (gnus-group-real-name group))
                    (group-active (gnus-active group)))
                (when group-active
                  (let ((new-min (or (caar gnus-agent-article-alist)
                                     (car group-active)))
                        (new-max (or (caar (last gnus-agent-article-alist))
                                     (cdr group-active))))

                    (when (< new-min (car group-active))
                      (setcar group-active new-min))
             
                    (when (> new-max (cdr group-active))
                      (setcdr group-active new-max))))))))

        (when (and reread gnus-agent-article-alist)
          (gnus-make-ascending-articles-unread
           group
           (delq nil (mapcar (function (lambda (c)
                                         (cond ((eq reread t)
                                                (car c))
                                               ((cdr c)
                                                (car c)))))
                             gnus-agent-article-alist)))

          (when (gnus-buffer-live-p gnus-group-buffer)
            (gnus-group-update-group group t)
            (sit-for 0)))

        (gnus-message 5 nil)
        regenerated)))

;;;###autoload
(defun gnus-agent-regenerate (&optional clean reread)
  "Regenerate all agent covered files.
If CLEAN, obsolete (ignore)."
  (interactive "P")
  (let (regenerated)
    (gnus-message 4 "Regenerating Gnus agent files...")
    (dolist (gnus-command-method (gnus-agent-covered-methods))
        (dolist (group (gnus-groups-from-server gnus-command-method))
          (setq regenerated (or (gnus-agent-regenerate-group group reread)
                                regenerated))))
    (gnus-message 4 "Regenerating Gnus agent files...done")

    regenerated))

(defun gnus-agent-go-online (&optional force)
  "Switch servers into online status."
  (interactive (list t))
  (dolist (server gnus-opened-servers)
    (when (eq (nth 1 server) 'offline)
      (if (if (eq force 'ask)
	      (gnus-y-or-n-p
	       (format "Switch %s:%s into online status? "
		       (caar server) (cadar server)))
	    force)
	  (setcar (nthcdr 1 server) 'close)))))

(defun gnus-agent-toggle-group-plugged (group)
  "Toggle the status of the server of the current group."
  (interactive (list (gnus-group-group-name)))
  (let* ((method (gnus-find-method-for-group group))
	 (status (cadr (assoc method gnus-opened-servers))))
    (if (eq status 'offline)
	(gnus-server-set-status method 'closed)
      (gnus-close-server method)
      (gnus-server-set-status method 'offline))
    (message "Turn %s:%s from %s to %s." (car method) (cadr method)
	     (if (eq status 'offline) 'offline 'online)
	     (if (eq status 'offline) 'online 'offline))))

(defun gnus-agent-group-covered-p (group)
  (gnus-agent-method-p (gnus-group-method group)))

(add-hook 'gnus-group-prepare-hook
          (lambda ()
            'gnus-agent-do-once

            (when (listp gnus-agent-expire-days)
              (beep)
              (beep)
              (gnus-message 1 "WARNING: gnus-agent-expire-days no longer\
 supports being set to a list.")(sleep-for 3)
              (gnus-message 1 "Change your configuration to set it to an\
 integer.")(sleep-for 3)
              (gnus-message 1 "I am now setting group parameters on each\
 group to match the configuration that the list offered.")

              (save-excursion
                (let ((groups (gnus-group-listed-groups)))
                  (while groups
                    (let* ((group (pop groups))
                           (days gnus-agent-expire-days)
                           (day (catch 'found
                                  (while days
                                    (when (eq 0 (string-match
                                                 (caar days)
                                                 group))
                                      (throw 'found (cadar days)))
                                    (setq days (cdr days)))
                                  nil)))
                      (when day
                        (gnus-group-set-parameter group 'agent-days-until-old
                                                  day))))))

              (let ((h gnus-group-prepare-hook))
                (while h
                  (let ((func (pop h)))
                    (when (and (listp func)
                               (eq (cadr (caddr func)) 'gnus-agent-do-once))
                      (remove-hook 'gnus-group-prepare-hook func)
                      (setq h nil)))))

              (gnus-message 1 "I have finished setting group parameters on\
 each group. You may now customize your groups and/or topics to control the\
 agent."))))

(provide 'gnus-agent)

;;; gnus-agent.el ends here
