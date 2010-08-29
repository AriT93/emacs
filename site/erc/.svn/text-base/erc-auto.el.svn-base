;;; -*-emacs-lisp-*-

;; Copyright (C) 2002 Free Software Foundation, Inc.

(defvar generated-autoload-file)
(defvar command-line-args-left)
(defun generate-autoloads ()
  (interactive)
  (require 'autoload)
  (setq generated-autoload-file (car command-line-args-left))
  (setq command-line-args-left (cdr command-line-args-left))
  (batch-update-autoloads))

(provide 'erc-auto)
;;; Generated autoloads follow (made by autoload.el).

;;;### (autoloads (erc-select) "erc" "erc.el" (17053 17713))
;;; Generated autoloads from erc.el

(autoload (quote erc-select) "erc" "\
Select connection parameters and run ERC. 
Non-interactively, it takes keyword arguments 
   (server (erc-compute-server))
   (port   (erc-compute-port))
   (nick   (erc-compute-nick))
   password 
   (full-name (erc-compute-full-name)))
That is, if called with (erc-select :server \"irc.freenode.net\" :full-name \"Harry S Truman\") , 
server and full-name will be set to those values, whereas erc-compute-port, erc-compute-nick and 
erc-compute-full-name will be invoked for those parameters' values" t nil)

;;;***

;;;### (autoloads nil "erc-autojoin" "erc-autojoin.el" (17053 17713))
;;; Generated autoloads from erc-autojoin.el
 (autoload 'erc-autojoin-mode "erc-autojoin" nil t)

;;;***

;;;### (autoloads nil "erc-bbdb" "erc-bbdb.el" (17053 17713))
;;; Generated autoloads from erc-bbdb.el
 (autoload 'erc-bbdb-mode "erc-bbdb")

;;;***

;;;### (autoloads nil "erc-button" "erc-button.el" (17053 17713))
;;; Generated autoloads from erc-button.el
 (autoload 'erc-button-mode "erc-button" nil t)

;;;***

;;;### (autoloads (erc-chess-ctcp-query-handler erc-cmd-CHESS) "erc-chess"
;;;;;;  "erc-chess.el" (17053 17713))
;;; Generated autoloads from erc-chess.el

(defvar erc-ctcp-query-CHESS-hook (quote (erc-chess-ctcp-query-handler)))

(autoload (quote erc-cmd-CHESS) "erc-chess" "\
Initiate a chess game via CTCP to NICK.
NICK should be the first and only arg to /chess" nil nil)

(autoload (quote erc-chess-ctcp-query-handler) "erc-chess" nil nil nil)

;;;***

;;;### (autoloads nil "erc-compat" "erc-compat.el" (17053 17713))
;;; Generated autoloads from erc-compat.el
 (autoload 'erc-define-minor-mode "erc-compat")

;;;***

;;;### (autoloads (erc-complete) "erc-complete" "erc-complete.el"
;;;;;;  (17053 17713))
;;; Generated autoloads from erc-complete.el

(autoload (quote erc-complete) "erc-complete" "\
Complete nick at point.
See `erc-try-complete-nick' for more technical info.
This function is obsolete, use `erc-pcomplete' instead." t nil)

;;;***

;;;### (autoloads (erc-ctcp-query-DCC pcomplete/erc-mode/DCC erc-cmd-DCC)
;;;;;;  "erc-dcc" "erc-dcc.el" (17053 17713))
;;; Generated autoloads from erc-dcc.el

(autoload (quote erc-cmd-DCC) "erc-dcc" "\
Parser for /dcc command.
This figures out the dcc subcommand and calls the appropriate routine to
handle it.  The function dispatched should be named \"erc-dcc-do-FOO-command\",
where FOO is one of CLOSE, GET, SEND, LIST, CHAT, etc." nil nil)

(autoload (quote pcomplete/erc-mode/DCC) "erc-dcc" "\
Provides completion for the /DCC command." nil nil)

(defvar erc-ctcp-query-DCC-hook (quote (erc-ctcp-query-DCC)) "\
Hook variable for CTCP DCC queries")

(autoload (quote erc-ctcp-query-DCC) "erc-dcc" "\
The function called when a CTCP DCC request is detected by the client.
It examines the DCC subcommand, and calls the appropriate routine for
that subcommand." nil nil)

;;;***

;;;### (autoloads (erc-ezb-initialize erc-ezb-select-session erc-ezb-select
;;;;;;  erc-ezb-add-session erc-ezb-end-of-session-list erc-ezb-init-session-list
;;;;;;  erc-ezb-identify erc-ezb-notice-autodetect erc-ezb-lookup-action
;;;;;;  erc-ezb-get-login erc-cmd-ezb) "erc-ezbounce" "erc-ezbounce.el"
;;;;;;  (17053 17713))
;;; Generated autoloads from erc-ezbounce.el

(autoload (quote erc-cmd-ezb) "erc-ezbounce" "\
Send EZB commands to the EZBouncer verbatim." nil nil)

(autoload (quote erc-ezb-get-login) "erc-ezbounce" "\
Return an appropriate EZBounce login for SERVER and PORT.
Look up entries in `erc-ezb-login-alist'. If the username or password
in the alist is `nil', prompt for the appropriate values." nil nil)

(autoload (quote erc-ezb-lookup-action) "erc-ezbounce" nil nil nil)

(autoload (quote erc-ezb-notice-autodetect) "erc-ezbounce" "\
React on an EZBounce NOTICE request." nil nil)

(autoload (quote erc-ezb-identify) "erc-ezbounce" "\
Identify to the EZBouncer server." nil nil)

(autoload (quote erc-ezb-init-session-list) "erc-ezbounce" "\
Reset the EZBounce session list to NIL." nil nil)

(autoload (quote erc-ezb-end-of-session-list) "erc-ezbounce" "\
Indicate the end of the EZBounce session listing." nil nil)

(autoload (quote erc-ezb-add-session) "erc-ezbounce" "\
Add an EZBounce session to the session list." nil nil)

(autoload (quote erc-ezb-select) "erc-ezbounce" "\
Select an IRC server to use by EZBounce, in ERC style." nil nil)

(autoload (quote erc-ezb-select-session) "erc-ezbounce" "\
Select a detached EZBounce session." nil nil)

(autoload (quote erc-ezb-initialize) "erc-ezbounce" "\
Add EZBouncer convenience functions to ERC." nil nil)

;;;***

;;;### (autoloads (erc-fill) "erc-fill" "erc-fill.el" (17053 17713))
;;; Generated autoloads from erc-fill.el
 (autoload 'erc-fill-mode "erc-fill" nil t)

(autoload (quote erc-fill) "erc-fill" "\
Fill a region using the function referenced in `erc-fill-function'.
You can put this on `erc-insert-modify-hook' and/or `erc-send-modify-hook'." nil nil)

;;;***

;;;### (autoloads (erc-create-imenu-index) "erc-imenu" "erc-imenu.el"
;;;;;;  (17053 17713))
;;; Generated autoloads from erc-imenu.el

(autoload (quote erc-create-imenu-index) "erc-imenu" nil nil nil)

;;;***

;;;### (autoloads (erc-chanlist erc-cmd-LIST) "erc-list" "erc-list.el"
;;;;;;  (17053 17713))
;;; Generated autoloads from erc-list.el

(autoload (quote erc-cmd-LIST) "erc-list" "\
Display a buffer containing a list of channels on the current server.
Optional argument CHANNEL specifies a single channel to list (instead of every
available channel)." t nil)

(autoload (quote erc-chanlist) "erc-list" "\
Show a channel listing of the current server in a special mode.
Please note that this function only works with IRC servers which conform
to RFC and send the LIST header (#321) at start of list transmission." t nil)

;;;***

;;;### (autoloads (erc-save-buffer-in-logs erc-logging-enabled) "erc-log"
;;;;;;  "erc-log.el" (17053 17713))
;;; Generated autoloads from erc-log.el
 (autoload 'erc-log-mode "erc-log" nil t)

(autoload (quote erc-logging-enabled) "erc-log" "\
Return non-nil if logging is enabled for BUFFER.
If BUFFER is nil, the value of `current-buffer' is used.
Logging is enabled if `erc-log-channels-directory' is non-nil, the directory
is writeable (it will be created as necessary) and
`erc-enable-logging' returns a non-nil value." nil nil)

(autoload (quote erc-save-buffer-in-logs) "erc-log" "\
Append BUFFER contents to the log file, if logging is enabled.
If BUFFER is not provided, current buffer is used.
Logging is enabled if `erc-logging-enabled' returns non-nil.

This is normally done on exit, to save the unsaved portion of the
buffer, since only the text that runs off the buffer limit is logged
automatically.

You can save every individual message by putting this function on
`erc-insert-post-hook'." t nil)

;;;***

;;;### (autoloads (erc-delete-dangerous-host erc-add-dangerous-host
;;;;;;  erc-delete-keyword erc-add-keyword erc-delete-fool erc-add-fool
;;;;;;  erc-delete-pal erc-add-pal) "erc-match" "erc-match.el" (17053
;;;;;;  17713))
;;; Generated autoloads from erc-match.el
 (autoload 'erc-match-mode "erc-match")

(autoload (quote erc-add-pal) "erc-match" "\
Add pal interactively to `erc-pals'." t nil)

(autoload (quote erc-delete-pal) "erc-match" "\
Delete pal interactively to `erc-pals'." t nil)

(autoload (quote erc-add-fool) "erc-match" "\
Add fool interactively to `erc-fools'." t nil)

(autoload (quote erc-delete-fool) "erc-match" "\
Delete fool interactively to `erc-fools'." t nil)

(autoload (quote erc-add-keyword) "erc-match" "\
Add keyword interactively to `erc-keywords'." t nil)

(autoload (quote erc-delete-keyword) "erc-match" "\
Delete keyword interactively to `erc-keywords'." t nil)

(autoload (quote erc-add-dangerous-host) "erc-match" "\
Add dangerous-host interactively to `erc-dangerous-hosts'." t nil)

(autoload (quote erc-delete-dangerous-host) "erc-match" "\
Delete dangerous-host interactively to `erc-dangerous-hosts'." t nil)

;;;***

;;;### (autoloads (erc-server-select erc-determine-network) "erc-nets"
;;;;;;  "erc-nets.el" (17053 17713))
;;; Generated autoloads from erc-nets.el

(autoload (quote erc-determine-network) "erc-nets" "\
Return the name of the network or \"Unknown\" as a symbol.  Use the
server parameter NETWORK if provided, otherwise parse the server name and
search for a match in `erc-networks-alist'." nil nil)

(autoload (quote erc-server-select) "erc-nets" "\
Interactively select a server to connect to using `erc-server-alist'." t nil)

;;;***

;;;### (autoloads (erc-cmd-WHOLEFT) "erc-netsplit" "erc-netsplit.el"
;;;;;;  (17053 17713))
;;; Generated autoloads from erc-netsplit.el
 (autoload 'erc-netsplit-mode "erc-netsplit")

(autoload (quote erc-cmd-WHOLEFT) "erc-netsplit" "\
Show who's gone." nil nil)

;;;***

;;;### (autoloads (erc-nickserv-identify erc-nickserv-identify-mode)
;;;;;;  "erc-nickserv" "erc-nickserv.el" (17053 17713))
;;; Generated autoloads from erc-nickserv.el
 (autoload 'erc-services-mode "erc-nickserv" nil t)

(autoload (quote erc-nickserv-identify-mode) "erc-nickserv" "\
Set up hooks according to which MODE the user has chosen." t nil)

(autoload (quote erc-nickserv-identify) "erc-nickserv" "\
Send an \"identify <PASSWORD>\" message to NickServ.
When called interactively, read the password using `read-passwd'." t nil)

;;;***

;;;### (autoloads (pcomplete/erc-mode/NOTIFY erc-cmd-NOTIFY) "erc-notify"
;;;;;;  "erc-notify.el" (17053 17713))
;;; Generated autoloads from erc-notify.el
 (autoload 'erc-notify-mode "erc-notify" nil t)

(autoload (quote erc-cmd-NOTIFY) "erc-notify" "\
Change `erc-notify-list' or list current notify-list members online.
Without args, list the current list of notificated people online,
with args, toggle notify status of people." nil nil)

(autoload (quote pcomplete/erc-mode/NOTIFY) "erc-notify" nil nil nil)

;;;***

;;;### (autoloads nil "erc-page" "erc-page.el" (17053 17713))
;;; Generated autoloads from erc-page.el
 (autoload 'erc-page-mode "erc-page")

;;;***

;;;### (autoloads nil "erc-pcomplete" "erc-pcomplete.el" (17053 17713))
;;; Generated autoloads from erc-pcomplete.el
 (autoload 'erc-completion-mode "erc-pcomplete" nil t)

;;;***

;;;### (autoloads nil "erc-replace" "erc-replace.el" (17053 17713))
;;; Generated autoloads from erc-replace.el
 (autoload 'erc-replace-mode "erc-replace")

;;;***

;;;### (autoloads nil "erc-ring" "erc-ring.el" (17053 17713))
;;; Generated autoloads from erc-ring.el
 (autoload 'erc-ring-mode "erc-ring" nil t)

;;;***

;;;### (autoloads nil "erc-sound" "erc-sound.el" (17053 17713))
;;; Generated autoloads from erc-sound.el
 (autoload 'erc-sound-mode "erc-sound")

;;;***

;;;### (autoloads (erc-speedbar-browser) "erc-speedbar" "erc-speedbar.el"
;;;;;;  (17053 17713))
;;; Generated autoloads from erc-speedbar.el

(autoload (quote erc-speedbar-browser) "erc-speedbar" "\
Initialize speedbar to display an ERC browser.
This will add a speedbar major display mode." t nil)

;;;***

;;;### (autoloads nil "erc-stamp" "erc-stamp.el" (17053 17713))
;;; Generated autoloads from erc-stamp.el
 (autoload 'erc-timestamp-mode "erc-stamp" nil t)

;;;***

;;;### (autoloads nil "erc-track" "erc-track.el" (17053 17713))
;;; Generated autoloads from erc-track.el
 (autoload 'erc-track-mode "erc-track" nil t)
 (autoload 'erc-track-when-inactive-mode "erc-track" nil t)

;;;***

;;;### (autoloads (erc-truncate-buffer erc-truncate-buffer-to-size)
;;;;;;  "erc-truncate" "erc-truncate.el" (17053 17713))
;;; Generated autoloads from erc-truncate.el
 (autoload 'erc-truncate-mode "erc-truncate" nil t)

(autoload (quote erc-truncate-buffer-to-size) "erc-truncate" "\
Truncates the buffer to the size SIZE.
If BUFFER is not provided, the current buffer is assumed.  The deleted
region is logged if `erc-logging-enabled' returns non-nil." nil nil)

(autoload (quote erc-truncate-buffer) "erc-truncate" "\
Truncates the current buffer to `erc-max-buffer-size'.
Meant to be used in hooks, like `erc-insert-post-hook'." t nil)

;;;***

;;;### (autoloads (erc-xdcc-add-file) "erc-xdcc" "erc-xdcc.el" (17053
;;;;;;  17713))
;;; Generated autoloads from erc-xdcc.el

(autoload (quote erc-xdcc-add-file) "erc-xdcc" "\
Add a file to `erc-xdcc-files'." t nil)

;;;***

;;;### (autoloads nil "erc-autoaway" "erc-autoaway.el" (17053 17713))
;;; Generated autoloads from erc-autoaway.el
 (autoload 'erc-autoaway-mode "erc-autoaway")

;;;***
