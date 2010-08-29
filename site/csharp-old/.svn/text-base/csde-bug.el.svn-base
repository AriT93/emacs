;;; csde-bug.el -- CSDEbug Interface
;; $Revision: 1.2 $ $Date: 2001/02/12 05:38:22 $ 

;; Adapted from the JDE by Matt Bruce <matt.bruce@morganstanley.com>
;; Maintainer:  Matt Bruce

;; JDE Author: Paul Kinnucan <paulk@mathworks.com>
;; JDE Maintainer: Paul Kinnucan

;; Keywords: csharp, tools

;; Copyright (C) 2001 Matt Bruce

;; The JDE is Copyright (C) 1997, 1998, 1999, 2000, 2001 Paul Kinnucan.

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

;; The latest version of the CSDE is available at
;; <URL:http://www.sourceforge.com/>.

;; Please send any comments, bugs, or upgrade requests to
;; Matt Bruce (matt.bruce@morganstanley.com)

;;; Code:

(require 'csde-parse)
(require 'csde-dbs)

(when csde-xemacsp
  (defun line-beginning-position ()
    (point-at-bol))
  (defun line-end-position ()
    (point-at-eol)))

(defgroup csde-bug nil
  "CSDEbug General Options"
  :group 'csde
  :prefix "csde-bug")


(defcustom csde-bug-debug nil
"*IMPORTANT!!!! Leave this switch in its default state (off) unless
you need to modify the *CSDEbug* Csharp source code. Setting this switch
on causes the CSDE to load *CSDEbug* from its csharp/classes directory
instead of from csde.jar. It also causes the CSDE to run the debugger in
debug server mode. This allows you to use *CSDEbug* to debug itself."
  :group 'csde-bug
  :type 'boolean)

(defcustom csde-bug-jpda-directory ""
  "*Pathname of the directory containing Sun's Csharp Platform Debug Architecture
distribution. You need to set this variable only if this project uses a JDK 1.2 vm."
  :group 'csde-bug
  :type 'file)

(defcustom csde-bug-vm-includes-jpda-p nil
  "*On (non-nil) indicates that the vm used by this project includes JDPA. 
Turn this option on if this project uses a JDK 1.3 (or later) vm."
  :group 'csde-bug
  :type 'boolean)

(defcustom csde-bug-jdk-directory "e:/jdk1.3/"
  "*Specifies the location of the JDK used to debug this application. 
The tools.jar file distributed with JDK 1.3beta (in the jdk1.3/lib directory) contains the JPDA classes. However, the vm's boot classpath does not include this jar. Thus the CSDE must include tools.jar in the application classpath when invoking CSDEbug."
  :group 'csde-bug
  :type 'string)


(defcustom csde-bug-jre-home "" 
"*Home directory of the JRE containing the executable used to 
run debuggee processes.   
This variable specifies the home directory of the Csharp runtime
environment containing the executable, e.g., csharp, to be used to
launch processes (see `csde-bug-vm-executable'). If you do not specify
a home directory, the home directory is the same as that of the
executable used to run the debugger itself."
  :group 'csde-bug :type 'string)


(defcustom csde-bug-vm-executable (list (if (eq system-type 'windows-nt) "csharpw" "csharp"))
  "*Name of the executable used to launch target processes.
This defaults to csharp on Unix platforms and csharpw on Windows platforms"
  :group 'csde-bug
  :type '(list
	  (radio-button-choice 
	  (const "csharp")
	  (const "csharpw")
	  (const "csharp_g"))))

(defcustom csde-bug-raise-frame-p t
  "*Raise frame when a breakpoint is hit."
  :group 'csde-bug
  :type 'boolean)

(defcustom csde-bug-server-socket (cons t "2112") 
  "*Socket where debugger listens for apps needing debugger services.
You can arrange for a vm to connect to CSDEbug via a socket by starting it with the
options -Xdebug and -Xrunjdwp:transport=dt_socket,address=MYHOST:NNNN,
where MYHOST is the name of the machine running the debugger and
NNNN is the socket specified by this variable. To connect via shared
memory, start the debuggee process with the options -Xdebug and
-Xrunjdwp:transport=dt_shmem,address=ANYSTRING, where ANYSTRING is
the value of this variable. If you are running JDK 1.2, you must also
specifiy the options  -Xnoagent and -Dcsharp.compiler=NONE." 
  :group 'csde-bug 
  :type '(cons
	  (boolean :tag "Prompt for")
	  (string :tag "Address")))

(defcustom csde-bug-server-shmem-name (cons t "CSDEbug") 
  "*Shared memory name under which the debugger listens for apps 
needing debugger services. To connect via shared
memory, start the debuggee process with the options -Xdebug and
-Xrunjdwp:transport=dt_shmem,address=ANYSTRING, where ANYSTRING is
the value of this variable. If you are running JDK 1.2, you must also
specifiy the options  -Xnoagent and -Dcsharp.compiler=NONE." 
  :group 'csde-bug 
  :type '(cons
	  (boolean :tag "Prompt for")
	  (string :tag "Name")))


(defcustom csde-bug-debugger-host-address (if csde-xemacsp (system-name) system-name)
  "*Address of system on which CSDEbug is running.
The default value is the value of the standard Emacs variable `system-name'.
The CSDE uses the host address to connect to CSDEBug during startup. On some Windows
systems, the CSDE is unable to connect to the debugger socket under the system name.
If this happens, you can try setting this variable to the absolute address of 
a local host: 127.0.0.1 ."
  :group 'csde-bug
  :type 'string)


(defcustom csde-bug-debugger-command-timeout 10
  "*Length of time the CSDE waits for a response from the debugger to a command."
  :group 'csde-bug
  :type 'integer)

(defcustom csde-bug-saved-breakpoints nil
"*Breakpoints to be set for the current project."
  :group 'csde-bug
  :type '(repeat
	  (cons :tag "Break at"
	   (string :tag "File Name")
	   (integer :tag "Line Number"))))


(defcustom csde-bug-breakpoint-cursor-colors (cons "cyan" "brown")
"*Specifies the foreground and background colors of the debugger's
breakpoint cursor."
  :group 'csde-bug
  :type '(cons  
	  (string :tag "Foreground Color") 
	  (string :tag "Background Color"))
  :set '(lambda (sym val)
	  (make-face 'csde-bug-breakpoint-cursor)
	  (set-face-foreground 'csde-bug-breakpoint-cursor (car val))
	  (set-face-background 'csde-bug-breakpoint-cursor (cdr val))
	  (set-default sym val)))


(defcustom csde-bug-breakpoint-marker-colors (cons "red" "yellow")
"*Specifies the foreground and background colors of the debugger's
breakpoint marker."
  :group 'csde-bug
  :type '(cons :tag "Colors"
	  (string :tag "Foreground") 
	  (string :tag "Background"))
  :set '(lambda (sym val)
	  (make-face 'csde-bug-breakpoint-marker)
	  (set-face-foreground 'csde-bug-breakpoint-marker (car val))
	  (set-face-background 'csde-bug-breakpoint-marker (cdr val))
	  (set-default sym val)))

(defgroup csde-bug-window nil
  "CSDEbug Window Preferences"
  :group 'csde-bug
  :prefix "csde-bug-window")

(defcustom csde-bug-window-message nil
  "Message buffer window preferences."
  :group 'csde-bug-window
  :type 'list)

(defvar csde-bug-menu-spec
  (list "CSDEbug"

	["Step Over"                  csde-bug-step-over 
                                      (csde-dbs-target-process-steppable-p)]

	["Step Into"                  csde-bug-step-into 
	                              (csde-dbs-target-process-steppable-p)]

	["Step Into All"              csde-bug-step-into-all 
	                              (csde-dbs-target-process-steppable-p)]

	["Step Out"                   csde-bug-step-out 
	                              (csde-dbs-target-process-steppable-p)]

	["Continue"                   csde-bug-continue 
	                              (csde-dbs-target-process-runnable-p)]

	["Exit Debugger"              csde-bug-exit 
	                              (csde-dbs-debugger-running-p)]
	"-"
	;; Added by lea
        ["Toggle Breakpoint"          csde-bug-toggle-breakpoint t]
	["Set Conditional Breakpoint" csde-bug-set-conditional-breakpoint nil]
	["Save Breakpoints"           csde-bug-save-breakpoints nil]

	(list
	 "Watch for Field"

	 ["Access"                   csde-bug-watch-field-access 
	                             :style    nil
				     :active   (and 
						(csde-dbs-debugger-running-p)
						(csde-dbs-get-target-process))]


	 ["Modification"             csde-bug-watch-field-modification
	                             :style   nil
	                             :active  (and 
						(csde-dbs-debugger-running-p)
						(csde-dbs-get-target-process))]

	 ["Cancel"                   csde-bug-cancel-watch
	                             :style     nil
				     :active    (and
						 (csde-dbs-debugger-running-p)
						  (csde-dbs-get-target-process)
						  (slot-boundp
						   (csde-dbs-get-target-process)
						   'watch-req))]

	)

	(list
	 "Trace"
	
	 ["Class Prep..."             csde-bug-trace-class-prep
	                              :style    nil
				      :active	 (and 
					          (csde-dbs-debugger-running-p)
						  (csde-dbs-get-target-process))]

	 ["Class Unload..."           csde-bug-trace-class-unload
	                              :style    nil
				      :active	 (and 
					          (csde-dbs-debugger-running-p)
						  (csde-dbs-get-target-process))]


	 ["Method Entry..."           csde-bug-trace-method-entry
	                              :style    nil
				      :active	 (and 
					          (csde-dbs-debugger-running-p)
						  (csde-dbs-get-target-process))]

	 ["Method Exit..."            csde-bug-trace-method-exit
	                              :style    nil
				      :active	 (and 
					          (csde-dbs-debugger-running-p)
						  (csde-dbs-get-target-process))]

	 ["Exceptions..."            csde-bug-trace-exceptions
	                              :style    nil
				      :active	 (and 
					          (csde-dbs-debugger-running-p)
						  (csde-dbs-get-target-process))]


	 ["Cancel..."                csde-bug-cancel-trace
	                             :style     nil
				     :active    (and
						 (csde-dbs-debugger-running-p)
						  (csde-dbs-get-target-process)
						  (slot-boundp
						   (csde-dbs-get-target-process)
						   'trace-req))]

	 )

	"-"


	(list
	 "Display"

	 ["Variable"                  csde-bug-display-variable 
	                              (and 
				       (csde-dbs-debugger-running-p)
				       (csde-dbs-get-target-process))]


	 ["Array"                     csde-bug-display-array
	                              (and 
				       (csde-dbs-debugger-running-p)
				       (csde-dbs-get-target-process))]

	 ["Object"                    csde-bug-display-object
	                              (and 
				       (csde-dbs-debugger-running-p)
				       (csde-dbs-get-target-process))
				      ]

	 ["String"                    csde-bug-display-string
	                              (and 
				       (csde-dbs-debugger-running-p)
				       (csde-dbs-get-target-process))
				      ]

	 ["Local Variables"           csde-bug-display-local-variables 
	                              (and 
				       (csde-dbs-debugger-running-p)
				       (csde-dbs-get-target-process))]

	 ["Loaded Classes"            csde-bug-display-loaded-classes 
	                              (and 
				       (csde-dbs-debugger-running-p)
				       (csde-dbs-get-target-process))]

	 ["Threads"                   csde-bug-show-threads
	                              (and 
				       (csde-dbs-debugger-running-p)
				       (csde-dbs-get-target-process))]

	 ["Object Monitors"           csde-bug-show-object-monitors
	                              (and 
				       (csde-dbs-debugger-running-p)
				       (csde-dbs-get-target-process))]


	 ["Path Info"                 csde-bug-display-path-info
	                              (and 
				       (csde-dbs-debugger-running-p)
				       (csde-dbs-get-target-process))]



	)

	["Evaluate Expression"        csde-bug-evaluate-expression 
	                              (and 
				       (csde-dbs-debugger-running-p)
				       (csde-dbs-get-target-process))]

	(list
	 "Stack"
	 ["Up"                        csde-bug-up-stack 
	                              (and
                                       (csde-dbs-target-process-steppable-p)
				       (let* ((process (csde-dbs-get-target-process))
					      (stack-max 
					       (if (slot-boundp process 'stack)
						   (1- (length (oref process stack)))
						 0))
					      (stack-ptr (oref process stack-ptr)))
					 (< stack-ptr stack-max)))]

	 ["Down"                      csde-bug-down-stack 
	                              (and 
				       (csde-dbs-target-process-steppable-p)
				       (let* ((process (csde-dbs-get-target-process))
					      (stack-ptr (oref process stack-ptr)))
					 (> stack-ptr 0)))]
	 
	 )
	(list
	 "Thread"

	 ["Suspend"                   csde-bug-suspend-thread
	                              (and 
				       (csde-dbs-debugger-running-p)
				       (csde-dbs-get-target-process))]


	 ["Resume"                    csde-bug-resume-thread 
	                              (and 
				       (csde-dbs-debugger-running-p)
				       (csde-dbs-get-target-process))]

	 ["Interrupt"                 csde-bug-interrupt-thread 
	                              (and 
				       (csde-dbs-debugger-running-p)
				       (csde-dbs-get-target-process))]

	 ["Stop"                      csde-bug-stop-thread 
	                              (and 
				       (csde-dbs-debugger-running-p)
				       (csde-dbs-get-target-process))]

;	 ["Show Thread Info"          csde-bug-thread-show-thread-info nil]
	 )

	(list
	 "Processes"
	 ["Start Debugger"            csde-bug-start-debugger 
	                              (not (csde-dbs-debugger-running-p))]

	 ["Launch Process"            csde-bug-launch-process
	                              (csde-dbs-debugger-running-p)]

	 ["Suspend Process"           csde-bug-suspend-process
                                      (let ((process (csde-dbs-get-target-process)))
					(and 
					 (csde-dbs-debugger-running-p)
					 process
					 (not (oref process suspendedp))))]

	 ["Resume Process"           csde-bug-resume-process
	                              (let ((process (csde-dbs-get-target-process)))
					(and 
					 (csde-dbs-debugger-running-p)
					 process
					 (oref process suspendedp)))] 

	 ["Finish Process"            csde-bug-finish-process 
	                               (let ((process (csde-dbs-get-target-process)))
					 (and
					  (csde-dbs-debugger-running-p)
					  process
					  (not (oref process attachedp))))]

	 "-"

	 (list
	  "Attach Process"
	  ["Via Shared Memory"        csde-bug-attach-via-shared-memory
	                              (and 
                                       (eq system-type 'windows-nt)
				       (csde-dbs-debugger-running-p))]

	  ["On Local Host"             csde-bug-attach-local-host
	                               (csde-dbs-debugger-running-p)]
 
	  ["On Remote Host"             csde-bug-attach-remote-host
	                                (csde-dbs-debugger-running-p)] 
          )

	 (list
	  "Listen on"
	  ["Shared Memory"             csde-bug-listen-shmem
	                               (and
					(eq system-type 'windows-nt)
					(csde-dbs-debugger-running-p))]
	  
	  ["Socket"                     csde-bug-listen-socket
					(csde-dbs-debugger-running-p)]
	  )

	 ["Detach Process"            csde-bug-detach-process 
	                               (let ((process (csde-dbs-get-target-process)))
					 (and
					  (csde-dbs-debugger-running-p)
					  process
					  (oref process attachedp)))]


	 "-"

	 ["Set Target Process"        csde-bug-set-target-process 
	                              (> (csde-dbs-proc-set-get-size
					  csde-dbs-the-process-registry)
					  0)]

	 ["Show Processes"            csde-bug-set-show-processes nil]

	 ["Remove Dead Processes"     csde-bug-remove-dead-processes 
	                              (oref csde-dbs-the-process-morgue proc-alist)]

	 )
	(list
	 "Show Buffer"

	 ["Locals"                    csde-bug-show-locals-buf 
	                              (and
				       (csde-dbs-debugger-running-p)
				       (csde-dbs-get-target-process))]

	 ["CLI"                       csde-bug-show-cli-buf
                                      (and    
				       (csde-dbs-debugger-running-p)
				       (csde-dbs-get-target-process))]

	 ["Threads"                   csde-bug-show-threads-buf
	                               (and    
				       (csde-dbs-debugger-running-p)
				       (csde-dbs-get-target-process))]
	 )
	"-"
	["Preferences"                csde-bug-show-preferences t]
	"-"
	["Help"                       csde-bug-help t]
	)
"Defines the CSDE's debug menu.")


(defvar csde-bug-keymap (make-sparse-keymap)
  "Debugger keymap.")

(easy-menu-define  csde-bug-menu csde-bug-keymap "CSDEbug menu" csde-bug-menu-spec)

(defvar csde-bug-minor-mode-p nil
  "If non-nil, show csdebug menu.")
(make-variable-buffer-local 'csde-bug-minor-mode-p)


; (let ((a (assoc 'csde-bug-minor-mode-p minor-mode-map-alist)))
;   (if a
;       (setcdr a csde-bug-keymap)
;     (add-to-list 'minor-mode-map-alist
; 		 (cons 'csde-bug-minor-mode-p
; 		       csde-bug-keymap))))

(defun csde-bug-install-csdebug-menu ()
  "Installs the csdebug menu in the current Csharp source buffer
if the user has selected csdebug as the debugger for the current
project."
  (if (and 
       (or 
	(not csde-xemacsp) 
	(featurep 'infodock)))
      (progn
	(setq csde-bug-minor-mode-p t)
	(let ((a (assoc 'csde-bug-minor-mode-p minor-mode-map-alist)))
	  (if a
	      (setcdr a csde-bug-keymap)
	    (add-to-list 'minor-mode-map-alist
			 (cons 'csde-bug-minor-mode-p
			       csde-bug-keymap)))))
    (if (and 
	 (not (featurep 'infodock))
	 (not (memq 'infodock c-emacs-features))
	 (boundp 'current-menubar)
	 current-menubar
	 (not (car (find-menu-item current-menubar '("CSDEbug")))))
	(if (fboundp 'add-submenu)
	    (add-submenu nil csde-bug-menu)
	  (add-menu nil "CSDEbug" (cdr csde-bug-menu))))))

(defun csde-bug-remove-csdebug-menu ()
  "Removes the csdebug menu from the menubar of the current Csharp source buffer."
  (if (and 
       (or 
	(not csde-xemacsp) 
	(featurep 'infodock)))
      (setq csde-bug-minor-mode-p nil)
    (if (and 
	 (not (featurep 'infodock))
	 (not (memq 'infodock c-emacs-features))
	 (boundp 'current-menubar)
	 current-menubar)
      (if (fboundp 'delete-menu-item)
	  (delete-menu-item '("CSDEbug"))))))

;; (fmakunbound 'csde-bug-key-bindings)
(defcustom csde-bug-key-bindings
  (list (cons "[?\C-c ?\C-z ?\C-s]" 'csde-bug-step-over)
	(cons "[?\C-c ?\C-z ?\C-x]" 'csde-bug-step-into)
	(cons "[?\C-c ?\C-z ?\C-a]" 'csde-bug-step-into-all)
	(cons "[?\C-c ?\C-z ?\C-w]" 'csde-bug-step-out)
	(cons "[?\C-c ?\C-z ?\C-c]" 'csde-bug-continue)
	(cons "[?\C-c ?\C-z ?\C-b]" 'csde-bug-toggle-breakpoint))
  "*Specifies key bindings for CSDEbug.
The value of this variable is an association list. The car of
each element specifies a key sequence. The cdr specifies 
an interactive command that the key sequence executes. To enter
a key with a modifier, type C-q followed by the desired modified
keystroke. For example, to enter C-s (Control s) as the key to be
bound, type C-q C-s in the key field in the customization buffer.
You can use the notation [f1], [f2], etc., to specify function keys."
  :group 'csde-bug
  :type '(repeat
	  (cons :tag "Key binding"
	   (string :tag "Key")
	   (function :tag "Command")))
  :set '(lambda (sym val)
	  ;; Unmap existing key bindings
	  (if (and
	       (boundp 'csde-bug-key-bindings)
	       csde-bug-key-bindings)
	      (mapc 
	       (lambda (binding)
		 (let ((key (car binding))
		       (fcn (cdr binding)))
		   (if (string-match "\\[.+]"key)
		       (setq key (car (read-from-string key))))
		   (define-key csde-bug-keymap key nil)))
	       csde-bug-key-bindings))
	  ;; Map new key bindings.
	  (mapc 
	   (lambda (binding)
	     (let ((key (car binding))
		   (fcn (cdr binding)))
	       (if (string-match "\\[.+]"key)
		   (setq key (car (read-from-string key))))
	       (define-key csde-bug-keymap key fcn)))
	   val)
	  (set-default sym val)))

(defvar csde-bug-breakpoint-marker-overlays nil 
  "List of breakpoint marker overlay positions for this buffer.")

(setq-default csde-bug-breakpoint-marker-overlays nil)
(make-variable-buffer-local 'csde-bug-breakpoint-marker-overlays)

(defvar csde-bug-breakpoint-cursor-overlay nil 
  "Breakpoint cursor overlay position for this buffer.")

(setq-default csde-bug-breakpoint-cursor-overlay nil)
(make-variable-buffer-local 'csde-bug-breakpoint-cursor-overlay)


(defvar csde-bug-breakpoints nil
"Current breakpoints.")

(defun csde-bug-breakpoints-add (bp)
  "Adds this breakpoints to the list of breakpoints."
  (setq csde-bug-breakpoints 
	(cons (cons (oref bp id) bp) 
	      csde-bug-breakpoints)))

(defun csde-bug-breakpoints-remove (bp)
  "Removes BP from list of breakpoints."
  (setq csde-bug-breakpoints
	(remove-if
	 (lambda (assoc)
	   (let* ((xbp (cdr assoc))
		  (xid (oref xbp id))
		  (id (oref bp id)))
	     (equal xid id)))
	 csde-bug-breakpoints)))

(defun csde-bug-breakpoints-find (file line)
  "Finds the breakpoint object for the breakpoint at FILE and LINE."
  (cdr (find-if 
	(lambda (assoc)
	  (let ((bp (cdr assoc)))
	       (and (string= (oref bp file) file)
		    (equal (oref bp line) line))))
	csde-bug-breakpoints)))

(defun csde-bug-make-breakpoint-overlay ()
"Makes a breakpoint overlay at the current line in the current buffer."
  (let ((marker-overlay
	 (make-overlay
	  (line-beginning-position)
	  (line-end-position)
	  (current-buffer) nil t)))
    (overlay-put marker-overlay  'face 'csde-bug-breakpoint-marker)
    (overlay-put marker-overlay 'priority 99) 
    marker-overlay))


(defun csde-bug-highlight-breakpoint (&optional line)
  (save-excursion
    (if line (goto-line line))
    (if csde-xemacsp
	(if (or (not (extent-at (line-beginning-position)))
		(not (eq 'csde-bug-breakpoint-marker
			 (extent-property (extent-at (line-beginning-position))
					  'face nil))))
	    (let ((highlight-extent
		   (make-extent
		    (line-beginning-position)
		    (line-end-position))))
	      (set-extent-face highlight-extent 'csde-bug-breakpoint-marker)
	      (set-extent-priority highlight-extent 99)))
      (let ((marker-overlay
	     (make-overlay
	      (line-beginning-position)
	      (line-end-position)
	      (current-buffer) nil t)))
	(overlay-put marker-overlay  'face 'csde-bug-breakpoint-marker)
	(overlay-put marker-overlay 'priority 99) 
	marker-overlay))))

(defvar csde-bug-breakpoint-id-counter 0
"Counter for generating breakpoint ids")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                            ;;
;; Breakpoint Class                                                           ;;
;;                                                                            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defclass csde-bug-breakpoint ()
  ((id   :initarg :id
	 :type integer
	 :documentation
	 "Indentifies this breakpoint.")
   (file :initarg :file
	 :initform ""
	 :type string
	 :documentation
	 "Pathname of file containing this breakpoint.")
   (line :initarg :line
	 :type integer
	 :documentation
         "Line number of this breakpoint."))
  (:allow-nil-initform t)
  "Class of breakpoints.")


(defmethod initialize-instance ((this csde-bug-breakpoint) &rest fields)
  "Constructor for a breakpoint specification."

  ;; Call parent initializer.
  (call-next-method)

  (assert (oref this id))
  (assert (oref this file))
  (assert (oref this line)))

(defun csde-bug-step-over () 
  "Advances the process to the next line in the current method."
  (interactive)
  (let* ((process (csde-dbs-get-target-process))
	 (cmd
	  (csde-dbs-step "step over" :process process)))
    (csde-dbs-cmd-exec cmd)))


(defun csde-bug-step-into () 
  "Advances to the next step in the method at point except if the method
   belongs to the csharp, csharpx, or sun packages."
  (interactive)
  (let* ((process (csde-dbs-get-target-process))
	 (cmd
	  (csde-dbs-step "step into" :process process :step-type "into")))
    (csde-dbs-cmd-exec cmd)))

(defun csde-bug-step-into-all () 
  "Advances the process into the function invoked at point."
  (interactive)
  (let* ((process (csde-dbs-get-target-process))
	 (cmd
	  (csde-dbs-step "step into" :process process :step-type "into-all")))
    (csde-dbs-cmd-exec cmd)))

(defun csde-bug-step-out () 
  "Advances the process to the next line in the invoking method."
  (interactive)
  (let* ((process (csde-dbs-get-target-process))
	 (cmd
	  (csde-dbs-step "step into" :process process :step-type "out")))
    (csde-dbs-cmd-exec cmd)))


(defun csde-bug-continue () 
  "Runs the target process. Execution continues from the current breakpoint."
  (interactive)
  (let* ((process (csde-dbs-get-target-process))
	 (run (csde-dbs-run-process
	       (format "run %d" (oref process id))
		  :process process)))
    (oset process startupp nil)
    (oset process suspendedp nil)
    (oset process steppablep nil)
    (csde-dbs-cmd-exec run)))



(defun csde-bug-exit () 
  (interactive)
  (if (csde-dbs-debugger-running-p)
      (progn
	(mapc
	 (lambda (assoc)
	   (let* ((process (cdr assoc))
		  (finish-cmd (csde-dbs-finish-process
			       (format "finish %d" (oref process id))
			       :process process))
		  (result (csde-dbs-cmd-exec finish-cmd)))
	     (csde-dbs-proc-move-to-morgue process)))
	 (oref csde-dbs-the-process-registry proc-alist))
	(slot-makeunbound csde-dbs-the-process-registry :target-process)
	(csde-dbs-debugger-quit csde-dbs-the-debugger))
    (error "Debugger is not running.")))

(defun csde-bug-get-line-at-point ()
  (let ((ln (count-lines (point-min) (point))))
    (if (eq (char-before) ?\n)
	(1+ ln)
      ln)))

(add-hook 
 'csde-mode-hook 
 (lambda ()
   (if (buffer-file-name)
       (let ((this-file (file-name-nondirectory (buffer-file-name))))
	 (mapc
	  (lambda (spec)
	    (let* ((file (car spec))
		   (line (cdr spec))
		   (bp (csde-bug-breakpoints-find file line)))
	      (when (not bp)
		(setq csde-bug-breakpoint-id-counter (1+ csde-bug-breakpoint-id-counter))
		(setq bp 
		      (csde-bug-breakpoint 
		       (format "breakpoint%d" csde-bug-breakpoint-id-counter)
		       :id csde-bug-breakpoint-id-counter
		       :file file 
		       :line line))
		(csde-bug-breakpoints-add bp))
	      (if (string-match file this-file)
		  (csde-bug-highlight-breakpoint line))))
	  csde-bug-saved-breakpoints)))))


(defun csde-bug-set-breakpoint()
  "Sets a breakpoint at the current line in the current buffer."
  (interactive)
  (let* ((file (file-name-nondirectory (buffer-file-name)))
         (line (csde-bug-get-line-at-point))
         (bp (csde-bug-breakpoints-find file line))
         (proc (csde-dbs-get-target-process)))
    (when (not bp)
      (setq csde-bug-breakpoint-id-counter (1+ csde-bug-breakpoint-id-counter))
      (setq bp 
            (csde-bug-breakpoint 
             (format "breakpoint%d" csde-bug-breakpoint-id-counter)
             :id csde-bug-breakpoint-id-counter
             :file file 
             :line line))
      (csde-bug-breakpoints-add bp))
    (csde-bug-highlight-breakpoint)
    (if (and bp proc)
        (let* ((set-breakpoint (csde-dbs-set-breakpoint 
                                "set breakpoint" 
                                :process proc
                                :breakpoint bp))
	       (result (csde-dbs-cmd-exec set-breakpoint)))
          (message "Breakpoint set at line %d in class %s." line file)))))

;; test by lea

(defun csde-bug-toggle-breakpoint ()
  "Toggles the breakpoint on the current line."
  (interactive)
  (let*  ((file (file-name-nondirectory (buffer-file-name)))
	  (line (csde-bug-get-line-at-point))
	  (bp (csde-bug-breakpoints-find file line)))
    (if bp
	(csde-bug-clear-breakpoint)
      (csde-bug-set-breakpoint))))


(defun csde-bug-set-conditional-breakpoint () 
  (interactive)
  (message "not implemented"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                            ;;
;; Clear Breakpoint Command                                                   ;;
;;                                                                            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun csde-bug-clear-breakpoint()
  "Clear the breakpoint at the current line in the current buffer."
  (interactive)
  (let* ((file (file-name-nondirectory (buffer-file-name)))
	 (line (csde-bug-get-line-at-point))
	 (bp (csde-bug-breakpoints-find file line))
	 (proc (csde-dbs-get-target-process)))
    (if (and bp proc)
	(let* ((clear-breakpoint
		(csde-dbs-clear-breakpoint 
		 "clear breakpoint"
		 :process proc
		 :breakpoint bp))
	       (result (csde-dbs-cmd-exec clear-breakpoint)))))
    (if bp
	(csde-bug-breakpoints-remove bp))
    (csde-bug-remove-breakpoint-highlight)))


(defun csde-bug-remove-breakpoint-highlight ()
  (if csde-xemacsp
      (map-extents
       (lambda (extent maparg)
	 (delete-extent extent))
       (current-buffer)
       (line-beginning-position) (line-end-position) 
       nil nil
       'face 'csde-bug-breakpoint-marker)     
    (let ((overlays (overlays-at (line-beginning-position))))
      (while overlays
	(let ((overlay (car overlays)))
	  (if (eq (overlay-get overlay 'face) 'csde-bug-breakpoint-marker)
		  (delete-overlay overlay)))
	(setq overlays (cdr overlays))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                            ;;
;; Save Breakpoints Command                                                   ;;
;;                                                                            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun csde-bug-save-breakpoints ()
  "Save breakpoints in project file." 
  (interactive)
  (message "not implemented"))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                            ;;
;; Trace Methods Command                                                      ;;
;;                                                                            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass csde-bug-trace-methods-dialog (csde-dialog)
  ((trace-type               :initarg :trace-type
			     :type string
			     :initform "entry"
			     :documentation
			     "Values may be entry or exit.")
   (thread-restriction-field :initarg :thread-restriction-field
			     :documentation
			     "Text field that contains thread restriction.")
   (suspend-policy-field     :initarg :suspend-policy-field
			     :documentation
			     "Text field that specifies the thread suspension policy.")
   (class-inclusion-field    :initarg :class-inclusion-field
			     :documentation
			     "Specifies class inclusion filters.") 
   (class-exclusion-field    :initarg :class-exclusion-field
			     :documentation
			     "Specifies class exclusion filters.")
   )
  "Class of trace methods dialogs."
)

(defmethod initialize-instance ((this csde-bug-trace-methods-dialog) &rest fields)
  "Constructor for trace methods dialog."

  ;; Call parent initializer.
  (call-next-method)

  (assert (or (string= (oref this trace-type) "entry") 
	      (string= (oref this trace-type) "exit")))
)


(defmethod csde-dialog-create ((this csde-bug-trace-methods-dialog))

  (widget-insert (concat "Trace method " 
			 (oref this trace-type)
			 "\n\n"))

  (oset this thread-restriction-field
	(widget-create
	 'editable-field
	 :format "  %t:  %v\n  %h \n\n"
	 :size 40
	 :tag "Thread restriction"
	 :doc "Restrict trace to the specified thread."))

  (oset this suspend-policy-field
	(widget-create
	 '(choice
	   :tag "Thread Suspension Policy"
	   :value "none"
	   :format "  %t: %[Options%] %v  %h\n\n"
	   :doc "Specify which thread to suspend on method entry or exit."
	   (const "all")
	   (const "thread")
	   (const "none"))))   


  (oset this class-inclusion-field
	(widget-create
	 '(editable-list
	   :format "  %t:\n  %i\n%v  %h\n\n"
	   :entry-format "  %i %d %v\n"
	   :tag "Class inclusion filters"
	   :doc "Regular expressions that specify classes whose methods should be traced."
	   (string
	    :format "%t: %v"
	    :size 40
	    :tag "Filter")))) 

    (oset this class-exclusion-field
	(widget-create
	 '(editable-list
	   :format "  %t:\n  %i\n%v  %h\n\n"
	   :entry-format "  %i %d %v\n"
	   :tag "Class exclusion filters"
	   :doc "Regular expressions that specify classes whose methods should not be traced."
	   (string
	    :format "%t: %v"
	    :size 40
	    :tag "Filter")))) 
  )

(defmethod csde-dialog-ok ((this csde-bug-trace-methods-dialog))
  (let* ((thread-restriction (widget-value (oref this thread-restriction-field)))
	 (thread-suspension-policy (widget-value (oref this suspend-policy-field)))
	 (class-inclusion-filters (widget-value (oref this class-inclusion-field)))
	 (class-exclusion-filters (widget-value (oref this class-exclusion-field)))
	 (process (csde-dbs-get-target-process))
	 (request (csde-dbs-trace-methods-request "trace methods request" 
						 :trace-type (oref this trace-type)))
	 (cmd  (csde-dbs-trace-methods 
		"trace methods command" 
		:process process :trace-request request)))
    
    (if (and thread-restriction (not (string= thread-restriction "")))
        (oset request :thread-restriction thread-restriction))

    (if (and thread-suspension-policy (not (string= thread-suspension-policy "")))
	(oset request :suspend-policy thread-suspension-policy))
    
    (if class-inclusion-filters
	(oset request :inclusion-filters class-inclusion-filters))

    (if class-exclusion-filters
	(oset request :exclusion-filters class-exclusion-filters))

    (csde-dbs-cmd-exec cmd)
    (call-next-method)))
		       

(defun csde-bug-trace-method-entry ()
  "Displays the trace method entry dialog."
  (interactive)
  (let ((dialog (csde-bug-trace-methods-dialog "trace method entry dialog")))
    (csde-dialog-show dialog)))

(defun csde-bug-trace-method-exit ()
  "Displays the trace method exit dialog."
  (interactive)
  (let ((dialog (csde-bug-trace-methods-dialog 
		 "trace method exit dialog" :trace-type "exit")))
    (csde-dialog-show dialog)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                            ;;
;; Trace Classes Command                                                      ;;
;;                                                                            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defclass csde-bug-trace-classes-dialog (csde-dialog)
  ((trace-type               :initarg :trace-type
			     :type string
			     :initform "preparation"
			     :documentation
			     "Values may be preparation or unloading.")
   (suspend-policy-field     :initarg :suspend-policy-field
			     :documentation
			     "Text field that specifies the thread suspension policy.")
   (class-inclusion-field    :initarg :class-inclusion-field
			     :documentation
			     "Specifies class inclusion filters.") 
   (class-exclusion-field    :initarg :class-exclusion-field
			     :documentation
			     "Specifies class exclusion filters.")
   )
  "Class of trace classes dialogs."
)

(defmethod initialize-instance ((this csde-bug-trace-classes-dialog) &rest fields)
  "Constructor for trace classes dialog."

  ;; Call parent initializer.
  (call-next-method)

  (assert (or (string= (oref this trace-type) "preparation") 
	      (string= (oref this trace-type) "unloading")))
)


(defmethod csde-dialog-create ((this csde-bug-trace-classes-dialog))

  (widget-insert (concat "Trace class " 
			 (oref this trace-type)
			 "\n\n"))

  (oset this suspend-policy-field
	(widget-create
	 '(choice
	   :tag "Thread Suspension Policy"
	   :value "none"
	   :format "  %t: %[Options%] %v  %h\n\n"
	   :doc "Specify which thread to suspend on class preparation or unloading."
	   (const "all")
	   (const "thread")
	   (const "none"))))   


  (oset this class-inclusion-field
	(widget-create
	 '(editable-list
	   :format "  %t:\n  %i\n%v  %h\n\n"
	   :entry-format "  %i %d %v\n"
	   :tag "Class inclusion filters"
	   :doc "Regular expressions that specify classes whose preparation or unloading should be traced."
	   (string
	    :format "%t: %v"
	    :size 40
	    :tag "Filter")))) 

    (oset this class-exclusion-field
	(widget-create
	 '(editable-list
	   :format "  %t:\n  %i\n%v  %h\n\n"
	   :entry-format "  %i %d %v\n"
	   :tag "Class exclusion filters"
	   :doc "Regular expressions that specify classes whose preparation or unloading should not be traced."
	   (string
	    :format "%t: %v"
	    :size 40
	    :tag "Filter")))) 
  )

(defmethod csde-dialog-ok ((this csde-bug-trace-classes-dialog))
  (let* ((thread-suspension-policy (widget-value (oref this suspend-policy-field)))
	 (class-inclusion-filters (widget-value (oref this class-inclusion-field)))
	 (class-exclusion-filters (widget-value (oref this class-inclusion-field)))
	 (process (csde-dbs-get-target-process))
	 (request (csde-dbs-trace-classes-request "trace classes request" 
						 :trace-type (oref this trace-type)))
	 (cmd  (csde-dbs-trace-classes 
		"trace classes command" 
		:process process :trace-request request)))
    
    (if (and thread-suspension-policy (not (string= thread-suspension-policy "")))
	(oset request :suspend-policy thread-suspension-policy))
    
    (if class-inclusion-filters
	(oset request :inclusion-filters class-inclusion-filters))

    (if class-exclusion-filters
	(oset request :exclusion-filters class-exclusion-filters))

    (csde-dbs-cmd-exec cmd)
    (call-next-method)))
		       

(defun csde-bug-trace-class-prep ()
  "Displays the trace class preparation dialog."
  (interactive)
  (let ((dialog (csde-bug-trace-classes-dialog "trace class prep dialog")))
    (csde-dialog-show dialog)))

(defun csde-bug-trace-class-unload ()
  "Displays the trace class unloading dialog."
  (interactive)
  (let ((dialog (csde-bug-trace-classes-dialog 
		 "trace class unloading dialog" :trace-type "unloading")))
    (csde-dialog-show dialog)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                            ;;
;; Trace Exceptions Command                                                   ;;
;;                                                                            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defclass csde-bug-trace-exceptions-dialog (csde-dialog)
  ((exception-class-field    :initarg :exception-class
			     :documentation
			     "Class of exception to trace.")
   (trace-type-field         :initarg :trace-type
			     :documentation
			     "Values may be caught, uncaught, or both.")
   (thread-restriction-field :initarg :thread-restriction-field
			     :documentation
			     "Text field that contains thread restriction.")
   (suspend-policy-field     :initarg :suspend-policy-field
			     :documentation
			     "Text field that specifies the thread suspension policy.")
   (class-inclusion-field    :initarg :class-inclusion-field
			     :documentation
			     "Specifies class inclusion filters.") 
   (class-exclusion-field    :initarg :class-exclusion-field
			     :documentation
			     "Specifies class exclusion filters.")
   )
  "Defines a trace exception dialog."
)

(defmethod initialize-instance ((this csde-bug-trace-exceptions-dialog) &rest fields)
  "Constructor for trace exceptions dialog."

  ;; Call parent initializer.
  (call-next-method)

)


(defmethod csde-dialog-create ((this csde-bug-trace-exceptions-dialog))

  (widget-insert (concat "Trace exception\n\n"))

  (oset this exception-class-field
	(widget-create
	 'editable-field
	 :format "  %t:  %v\n  %h \n\n"
	 :size 40
	 :tag "Exception class"
	 :doc "Name of class of exception to trace. May be a wild card pattern of the form *.name. This allows you to omit a package qualifier from a class name. For example, to trace occurences of csharp.io.IOException, specify *.IOException."))

  (oset this trace-type-field
	(widget-create
	 '(choice
	   :tag "Exception type"
	   :value "both"
	   :format "  %t: %[Options%] %v  %h\n\n"
	   :doc "Specify the type of exception to trace."
	   (const "caught")
	   (const "uncaught")
	   (const "both"))))   

  (oset this thread-restriction-field
	(widget-create
	 'editable-field
	 :format "  %t:  %v\n  %h \n\n"
	 :size 40
	 :tag "Thread restriction"
	 :doc "Restrict trace to the specified thread."))

  (oset this suspend-policy-field
	(widget-create
	 '(choice
	   :tag "Thread Suspension Policy"
	   :value "none"
	   :format "  %t: %[Options%] %v  %h\n\n"
	   :doc "Specify which thread to suspend on class preparation or unloading."
	   (const "all")
	   (const "thread")
	   (const "none"))))   


  (oset this class-inclusion-field
	(widget-create
	 '(editable-list
	   :format "  %t:\n  %i\n%v  %h\n\n"
	   :entry-format "  %i %d %v\n"
	   :tag "Class inclusion filters"
	   :doc "Regular expressions that specify classes whose preparation or unloading should be traced."
	   (string
	    :format "%t: %v"
	    :size 40
	    :tag "Filter")))) 

    (oset this class-exclusion-field
	(widget-create
	 '(editable-list
	   :format "  %t:\n  %i\n%v  %h\n\n"
	   :entry-format "  %i %d %v\n"
	   :tag "Class exclusion filters"
	   :doc "Regular expressions that specify classes whose preparation or unloading should not be traced."
	   (string
	    :format "%t: %v"
	    :size 40
	    :tag "Filter")))) 
  )

(defmethod csde-dialog-ok ((this csde-bug-trace-exceptions-dialog))
  (let* ((exception-class (widget-value (oref this exception-class-field)))
	 (trace-type (widget-value (oref this trace-type-field)))
	 (thread-restriction (widget-value (oref this thread-restriction-field)))
	 (thread-suspension-policy (widget-value (oref this suspend-policy-field)))
	 (class-inclusion-filters (widget-value (oref this class-inclusion-field)))
	 (class-exclusion-filters (widget-value (oref this class-inclusion-field)))
	 (process (csde-dbs-get-target-process))
	 (request (csde-dbs-trace-exceptions-request 
		   "trace exceptions request" 
		   :exception-class exception-class
		   :trace-type trace-type))
	 (cmd  (csde-dbs-trace-exceptions
		"trace exceptions command" 
		:process process :trace-request request)))

    (if (and thread-restriction (not (string= thread-restriction "")))
	(oset request :thread-restriction thread-restriction))
    
    (if (and thread-suspension-policy (not (string= thread-suspension-policy "")))
	(oset request :suspend-policy thread-suspension-policy))
    
    (if class-inclusion-filters
	(oset request :inclusion-filters class-inclusion-filters))

    (if class-exclusion-filters
	(oset request :exclusion-filters class-exclusion-filters))

    (csde-dbs-cmd-exec cmd)
    (call-next-method)))
		       

(defun csde-bug-trace-exceptions ()
  "Displays the trace exceptions dialog."
  (interactive)
  (let ((dialog (csde-bug-trace-exceptions-dialog "trace exceptions dialog")))
    (csde-dialog-show dialog)))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                            ;;
;; Cancel Trace Request Command                                               ;;
;;                                                                            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun csde-bug-cancel-trace-request (process request)
  "Cancels a specified trace request on a specified process."
  (let ((cmd (csde-dbs-cancel-trace "cancel trace" :process process
				  :trace-request request)))
    (csde-dbs-cmd-exec cmd)))

(defclass csde-bug-cancel-trace-dialog (csde-dialog)
  ((process          :initarg :process
		     :type csde-dbs-proc)
   (requests         :initarg :requests
		     :type list)
   (check-boxes      :initarg :check-boxes))
)

(defmethod csde-dialog-create ((this csde-bug-cancel-trace-dialog))
  (let ((items
	 (mapcar
	  (lambda (x)
	    (let ((request (cdr x)))
	      (list
	       'const
	       :format "%t %v  %d"
	       :tag "Request"
	       :doc 
	       (concat
		(if (typep request 'csde-dbs-trace-methods-request)
		    (progn
		      (concat
		      (format "Trace method %s." (oref request trace-type))
		      (if (slot-boundp request 'thread-restriction)
			  (format " Thread restriction: %s." 
				  (oref request thread-restriction)))))
		  (format "Trace class %s." (oref request trace-type)))
		(if (slot-boundp request 'suspend-policy)
		    (format " Suspend policy: %s." (oref request suspend-policy)))
		(if (slot-boundp request 'inclusion-filters)
		    (format " Inclusion filters: %s." (oref request inclusion-filters)))
		(if (slot-boundp request 'exclusion-filters)
		    (format " Exclusion filters: %s." (oref request exclusion-filters)))
		)
	       (car x))))
	  (oref this requests))))

  (widget-insert "Check the trace requests you want to cancel.\n\n")

  (oset this check-boxes
	(widget-create
	 (list 
	  'checklist
	  :entry-format "  %b %v\n"
	  :args items
	   )))
  ))

(defmethod csde-dialog-ok ((this csde-bug-cancel-trace-dialog))
  (message (format "Check boxes: %s)" (widget-value (oref this check-boxes))))
  (mapc
   (lambda (id-x)
     (let ((request
	    (cdr 
	     (find-if
	      (lambda (x) (= (car x) id-x))
	     (oref this requests)))))
     (csde-bug-cancel-trace-request  (oref this process) request)))
   (widget-value (oref this check-boxes)))
  (call-next-method))


(defun csde-bug-cancel-trace ()
  "Cancels method and class trace requests for the target process.
If only one trace request is outstanding, this command cancels that request.
Otherwise, this command displays a cancel dialog that lets you choose the
requests to cancel."
 (interactive) 
 (let* ((process (csde-dbs-get-target-process)))
   (if process
       (if (slot-boundp process 'trace-req)
	   (let ((trace-requests (oref process :trace-req)))
	     (if (= (length trace-requests) 1)
		 (csde-bug-cancel-trace-request process (cdr (car trace-requests)))
	       (let ((dialog
		      (csde-bug-cancel-trace-dialog "cancel trace dialog"
						   :process process
						   :requests trace-requests)))
		 (csde-dialog-show dialog))))
	 (error "The target process has no outstanding trace requests"))
     (error "There is no active process."))))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                            ;;
;; Watch Field Command                                                        ;;
;;                                                                            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defclass csde-bug-watch-field-dialog (csde-dialog)
  ((watch-type                :initarg :watch-type
			      :type string
			      :initform "access"
			      :documentation
			      "Watch type: field access or modification.")
   (object-class-widget       :initarg :object-class-widget
			      :documentation
			      "Widget specifying class of objects to watch.")
   (field-name-widget         :initarg :field-name-widget
			      :documentation
			      "Widget specify name of field to watch.")
   (expression-widget         :initarg :expression-widget
			      :documentation
			      "Widget specify watch restriction expression.")
   (object-id-widget          :initarg :object-id-widget
			      :documentation
			      "Widget specify id of object to watch.")
   (thread-restriction-widget :initarg :thread-restriction-widget
			      :documentation
			      "Text field that contains thread restriction.")
   (suspend-policy-widget     :initarg :suspend-policy-widget
			      :documentation
			      "Text field that specifies the thread suspension policy.")
   (class-inclusion-widget    :initarg :class-inclusion-widget
			      :documentation
			     "Specifies class inclusion filters.") 
   (class-exclusion-widget    :initarg :class-exclusion-widget
			     :documentation
			     "Specifies class exclusion filters.")
   )
  "Defines a watch field dialog."
)

(defmethod initialize-instance ((this csde-bug-watch-field-dialog) &rest fields)
  "Constructor for watch field dialog."

  ;; Call parent initializer.
  (call-next-method)

)


(defmethod csde-dialog-create ((this csde-bug-watch-field-dialog))

  (widget-insert (format "Watch for field %s\n\n" (oref this watch-type)))

  (oset this object-class-widget
	(widget-create
	 'editable-field
	 :format "  %t:  %v\n  %h \n\n"
	 :size 40
	 :tag "Class"
	 :doc "Class of object or objects to watch.
May be a wild card pattern of the form *.name. This allows you to omit a package qualifier from a class name. For example, to watch a field of csharp.io.IOException, specify *.IOException."))


  (oset this field-name-widget
	(widget-create
	 'editable-field
	 :format "  %t:  %v\n  %h \n\n"
	 :size 40
	 :tag "Field name"
	 :doc "Name of field to watch.")) 
 
  (oset this expression-widget
	(widget-create
	 'editable-field
	 :format "  %t:  %v\n  %h \n\n"
	 :size 40
	 :tag "Watch expression"
	 :doc "A boolean expression. 
Execution of the process is suspended only if the expression is true. The expression can contain any variable that is in scope when a field changes."))

  (oset this object-id-widget
	(widget-create
	 'editable-field
	 :format "  %t:  %v\n  %h \n\n"
	 :size 40
	 :tag "Object ID"
	 :doc "ID of the object to watch."))

  (oset this thread-restriction-widget
	(widget-create
	 'editable-field
	 :format "  %t:  %v\n  %h \n\n"
	 :size 40
	 :tag "Thread restriction"
	 :doc "Restrict watch to the specified thread."))

  (oset this suspend-policy-widget
	(widget-create
	 '(choice
	   :tag "Thread Suspension Policy"
	   :value "none"
	   :format "  %t: %[Options%] %v  %h\n\n"
	   :doc "Specify which threads to suspend on field access or modification."
	   (const "all")
	   (const "thread")
	   (const "none"))))   


  (oset this class-inclusion-widget
	(widget-create
	 '(editable-list
	   :format "  %t:\n  %i\n%v  %h\n\n"
	   :entry-format "  %i %d %v\n"
	   :tag "Class inclusion filters"
	   :doc "Regular expressions that specify classes whose field should be watched."
	   (string
	    :format "%t: %v"
	    :size 40
	    :tag "Filter")))) 

    (oset this class-exclusion-widget
	(widget-create
	 '(editable-list
	   :format "  %t:\n  %i\n%v  %h\n\n"
	   :entry-format "  %i %d %v\n"
	   :tag "Class exclusion filters"
	   :doc "Regular expressions that specify classes whose fields should not be watched."
	   (string
	    :format "%t: %v"
	    :size 40
	    :tag "Filter")))) 
  )

(defmethod csde-dialog-ok ((this csde-bug-watch-field-dialog))
  (let* ((obj-class (widget-value (oref this object-class-widget)))
	 (field-name (widget-value (oref this field-name-widget)))
	 (expression (widget-value (oref this expression-widget)))
	 (object-id (widget-value (oref this object-id-widget)))
	 (thread-restriction (widget-value (oref this thread-restriction-widget)))
	 (thread-suspension-policy (widget-value (oref this suspend-policy-widget)))
	 (class-inclusion-filters (widget-value (oref this class-inclusion-widget)))
	 (class-exclusion-filters (widget-value (oref this class-inclusion-widget)))
	 (process (csde-dbs-get-target-process))
	 (request (csde-dbs-watch-field-request 
		   "watch field request" 
		   :watch-type (oref this watch-type)
		   :object-class obj-class
		   :field-name field-name))
	 (cmd  (csde-dbs-watch-field
		"watch field command" 
		:process process :watch-request request)))

    (if (and expression (not (string= expression "")))
	(oset request :expression expression))

    (if (and object-id (not (string= object-id "")))
	(oset request :object-id object-id))

    (if (and thread-restriction (not (string= thread-restriction "")))
	(oset request :thread-restriction thread-restriction))
    
    (if (and thread-suspension-policy (not (string= thread-suspension-policy "")))
	(oset request :suspend-policy thread-suspension-policy))
    
    (if class-inclusion-filters
	(oset request :inclusion-filters class-inclusion-filters))

    (if class-exclusion-filters
	(oset request :exclusion-filters class-exclusion-filters))

    (csde-dbs-cmd-exec cmd)
    (call-next-method)))
		       

(defun csde-bug-watch-field-access ()
  "Request that the debugger watch for access of a
field of an object or class of objects."
  (interactive)
  (let ((dialog (csde-bug-watch-field-dialog "watch field dialog")))
    (csde-dialog-show dialog)))

(defun csde-bug-watch-field-modification ()
  "Request that the debugger watch for modifiction of a
field of an object or class of objects."
  (interactive)
  (let ((dialog (csde-bug-watch-field-dialog "watch field dialog" 
					    :watch-type "modification")))
    (csde-dialog-show dialog)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                            ;;
;; Cancel Watch Request Command                                               ;;
;;                                                                            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun csde-bug-cancel-watch-request (process request)
  "Cancels a specified watch field request on a specified process."
  (let ((cmd (csde-dbs-cancel-watch "cancel watch" :process process
				  :watch-request request)))
    (csde-dbs-cmd-exec cmd)))

(defclass csde-bug-cancel-watch-dialog (csde-dialog)
  ((process          :initarg :process
		     :type csde-dbs-proc)
   (requests         :initarg :requests
		     :type list)
   (check-boxes      :initarg :check-boxes))
)

(defmethod csde-dialog-create ((this csde-bug-cancel-watch-dialog))
  (let ((items
	 (mapcar
	  (lambda (x)
	    (let ((request (cdr x)))
	      (list
	       'const
	       :format "%t %v  %d"
	       :tag "Request"
	       :doc 
	       (concat
		(format "Watch type: %s. Class: %s. Field: %s."
		       (oref request watch-type)
		       (oref request object-class)
		       (oref request field-name))
		(if (slot-boundp request 'object-id)
		    (concat " Object id: " (oref request object-id) "."))
		(if (slot-boundp request 'expression)
		    (concat " Expression: " (oref request expression) ".")))
	       (car x))))
	  (oref this requests))))

  (widget-insert "Check the watch requests you want to cancel.\n\n")

  (oset this check-boxes
	(widget-create
	 (list 
	  'checklist
	  :entry-format "  %b %v\n"
	  :args items
	   )))
  ))

(defmethod csde-dialog-ok ((this csde-bug-cancel-watch-dialog))
  (message (format "Check boxes: %s)" (widget-value (oref this check-boxes))))
  (mapc
   (lambda (id-x)
     (let ((request
	    (cdr 
	     (find-if
	      (lambda (x) (= (car x) id-x))
	     (oref this requests)))))
     (csde-bug-cancel-watch-request  (oref this process) request)))
   (widget-value (oref this check-boxes)))
  (call-next-method))


(defun csde-bug-cancel-watch ()
  "Cancels watch requests for the target process.
If only one watch request is outstanding, this command cancels that request.
Otherwise, this command displays a cancel dialog that lets you choose the
requests to cancel."
 (interactive) 
 (let* ((process (csde-dbs-get-target-process)))
   (if process
       (if (slot-boundp process 'watch-req)
	   (let ((watch-requests (oref process :watch-req)))
	     (if (= (length watch-requests) 1)
		 (csde-bug-cancel-watch-request process (cdr (car watch-requests)))
	       (let ((dialog
		      (csde-bug-cancel-watch-dialog "cancel watch dialog"
						   :process process
						   :requests watch-requests)))
		 (csde-dialog-show dialog))))
	 (error "The target process has no outstanding watch requests"))
     (error "There is no active process."))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                            ;;
;; Display Variable Command                                                   ;;
;;                                                                            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun csde-bug-display-variable () 
  (interactive)
  (if (not (csde-dbs-target-process-runnable-p))
      (error "No target process or process is not suspended."))
  (csde-bug-evaluate-expression 
   (if (fboundp 'find-c-expr) (find-c-expr) (gud-find-c-expr))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                            ;;
;; Evaluate Expression Command                                                ;;
;;                                                                            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun csde-bug-evaluate-expression (expression) 
"Evaluates a Csharp expression. The Csharp expression may include
any variables in scope in the program being debugged."
  (interactive
   "sExpression: ")

  (if (not (csde-dbs-target-process-runnable-p))
      (error "No target process or process is not suspended."))
	   
  (if  (string= expression "")
      (error "Empty expression."))

  (let* ((process (csde-dbs-get-target-process))
	 (state-info (oref process state-info))
	 (thread-id (oref state-info thread-id))
	 (evaluate-command
	  (csde-dbs-evaluate 
	   (format "Evaluate %s" expression)
	   :process process
	   :expression expression
	   :thread-id thread-id))
	 (result
	  (csde-dbs-cmd-exec evaluate-command)))
    (if result
	(let* ((object-p (equal (length result) 3))
	       (type  (nth 0 result))
	       (value (nth 1 result))
	       (gc    (if object-p 
			  (if (nth 2 result)
			      "(garbage collected)"
			    "")))
	       (formatted-result 
		(if object-p
		    (format "<%s:%s> %s" type value gc)
		  (format "%s (%s)" value type))))
	  (csde-dbs-proc-display-debug-message
	   process
	   (format "\"%s\" = %s" expression formatted-result))
	  (message formatted-result))
      (message "Error: could not evaluate \"%s\"." expression))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                            ;;
;; Display Array Command                                                      ;;
;;                                                                            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun csde-bug-get-array-length (array)
  (let* ((process (csde-dbs-get-target-process))
	 (get-array-command
	  (csde-dbs-get-array 
	   (format "get_array_length %d" (oref array id))
	   :process process
	   :array array)))
	 (csde-dbs-cmd-exec get-array-command)
	 (if (slot-boundp array 'length)
	     (oref array length))))

(defun csde-bug-get-array-elements (array length)
  (let* ((process (csde-dbs-get-target-process))
	 (get-array-command
	  (csde-dbs-get-array 
	   (format "get_array_elements %d" (oref array id))
	   :process process
	   :array array
	   :index 0
	   :length length)))
	 (csde-dbs-cmd-exec get-array-command)))


(defun csde-bug-display-array (array-id)
  (interactive
   "nArray ID: ")

  (if (not (csde-dbs-target-process-runnable-p))
      (error "No target process or process is not suspended."))

  (let* ((array
	  (csde-dbs-csharp-array 
	   (format "array %d" array-id) :id array-id))
	 (array-length
	  (csde-bug-get-array-length array)))
    (if array-length
	(if (> array-length 0)
	    (progn
	      (csde-bug-get-array-elements array array-length)
	      (csde-dbs-proc-display-debug-message
	       (csde-dbs-get-target-process) 
	       (csde-dbs-csharp-obj-to-string array)))
	  (csde-dbs-proc-display-debug-message
	   (csde-dbs-get-target-process)
	   (format "Array %d has no elements." array-id)))      
      (error "Could not get array elements."))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                            ;;
;; Display Object Command                                                     ;;
;;                                                                            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun csde-bug-display-object (object-id)
  (interactive
   "nObject ID: ")
  (let* ((process (csde-dbs-get-target-process))
	 (get-object-command
	  (csde-dbs-get-object
	   (format "get_object %d" object-id)
	   :process process
	   :object-id object-id))
	 (object
	  (csde-dbs-cmd-exec get-object-command)))
    (if object
	(csde-dbs-proc-display-debug-message
	 (csde-dbs-get-target-process)
	 (csde-dbs-csharp-obj-to-string object))
      (error "Could not get object"))))
 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                            ;;
;; Display String Command                                                     ;;
;;                                                                            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun csde-bug-display-string (object-id) 
  (interactive
   "nObject ID: ")

  (if (not (csde-dbs-target-process-runnable-p))
      (error "No target process or process is not suspended."))

  (let* ((process (csde-dbs-get-target-process))
	 (get-string-command
	  (csde-dbs-get-string
	   (format "get_string %d" object-id)
	   :process process
	   :object-id object-id))
	 (string
	  (csde-dbs-cmd-exec get-string-command)))
    (if string
	(csde-dbs-proc-display-debug-message
	 (csde-dbs-get-target-process)
	 (format "string %d = %s" object-id string))
      (error "Could not get object"))))
 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                            ;;
;; Display Local Variables Command                                            ;;
;;                                                                            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun csde-bug-display-local-variables () 
  (interactive)

  (if (not (csde-dbs-target-process-runnable-p))
      (error "No target process or process is not suspended."))

  (let* ((process (csde-dbs-get-target-process))
	 (state-info (oref process state-info))
	 (thread-id (oref state-info thread-id))
	 (get-locals-command
	  (csde-dbs-get-locals
	   (format "get_locals t%d" thread-id)
	   :process process
	   :thread-id thread-id))
	 (result
	  (csde-dbs-cmd-exec get-locals-command)))
    (if result
	(mapc
	 (lambda (var) 
	   (csde-dbs-proc-display-debug-message
	    (csde-dbs-get-target-process)
	    (csde-dbs-csharp-variable-to-string var)))
	 result)	    
      (error "Could not get locals"))))
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                            ;;
;; Display Loaded Classes Command                                             ;;
;;                                                                            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun csde-bug-display-loaded-classes () 
  "Displays the classes currently loaded by the target process."
  (interactive)

  (if (not (csde-dbs-target-process-runnable-p))
      (error "No target process or process is not suspended."))

  (let* ((process (csde-dbs-get-target-process))
	 (cmd
	  (csde-dbs-get-loaded-classes
	   "get_loaded_classes"
	   :process process))
	 (result
	  (csde-dbs-cmd-exec cmd)))
    (if (not result)	    
      (error "Could not get loaded classes."))))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                            ;;
;; Show Threads Command                                                       ;;
;;                                                                            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun csde-bug-show-threads () 
"Shows all threads and thread-groups running in the target process.
This command displays the threads as a tree structure. To expand
a node of the tree, click the + sign next to the node, using mouse
button 2."
  (interactive)

  (if (not (csde-dbs-get-target-process))
      (error "No target process."))

  (let* ((process (csde-dbs-get-target-process))
	 (get-threads-command
	  (csde-dbs-get-threads
	   "get_threads"
	   :process process))
	 (result
	  (csde-dbs-cmd-exec get-threads-command)))
    (if (not result)	    
      (error "Could not get threads"))))

(defun csde-bug-thread-show-thread-info () 
  (interactive)
  (message "not implemented"))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                            ;;
;; Show Object Monitors                                                       ;;
;;                                                                            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun csde-bug-show-object-monitors (object-id) 
"Shows the threads that are monitoring a specified object, including the thread
that currently owns the object and threads that are waiting to access the object."
  (interactive
   "nObject ID: ")

  (if (not (csde-dbs-target-process-runnable-p))
      (error "No target process or process is not suspended."))

  (let* ((process (csde-dbs-get-target-process))
	 (get-monitors-command
	  (csde-dbs-get-object-monitors
	   "get_object_monitors"
	   :process process :object-id object-id))
	 (result
	  (csde-dbs-cmd-exec get-monitors-command)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                            ;;
;; Display Path Info Command                                                  ;;
;;                                                                            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun csde-bug-display-path-info () 
  "Displays the base directory, boot classpath, and classpath of the target process."
  (interactive)

  (if (not (csde-dbs-target-process-runnable-p))
      (error "No target process or process is not suspended."))

  (let* ((process (csde-dbs-get-target-process))
	 (cmd
	  (csde-dbs-get-path-info
	   "get_path_info"
	   :process process))
	 (result
	  (csde-dbs-cmd-exec cmd)))
    (if (not result)	    
      (error "Could not get path info."))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                            ;;
;; Clear Watchpoint Command                                                   ;;
;;                                                                            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun csde-bug-clear-watchpoint () 
  (interactive)
  (message "not implemented"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                            ;;
;; Up Stack Command                                                           ;;
;;                                                                            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun csde-bug-up-stack () 
  "Moves the source cursor up one frame in the call stack and displays the local
variables at that point in the stack. This command works only for the stack of
the thread at a breakpoint or step point."
  (interactive)

  (if (not (csde-dbs-target-process-steppable-p))
      (error "The target process is not suspended at a breakpoint or steppoint."))

  (if (not 
       (let* ((process (csde-dbs-get-target-process))
	      (stack-max (1- (length (oref process stack))))
	      (stack-ptr (oref process stack-ptr)))
	 (< stack-ptr stack-max)))
      (error "The debugger is displaying the top of the stack."))

  (let* ((process (csde-dbs-get-target-process))
	 (state-info (oref process :state-info))
	 (thread-id (oref state-info :thread-id))
	 (stack (oref process stack))
	 (stack-ptr (1+ (oref process stack-ptr)))
	 (frame (nth stack-ptr stack))
	 (class (nth 1 frame))
	 (file (nth 2 frame))
	 (line (nth 3 frame)))
	 
    (oset process :stack-ptr stack-ptr)
    (csde-dbo-show-line class file line)
    (csde-dbo-update-locals-buf process thread-id stack-ptr)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                            ;;
;; Down Stack Command                                                         ;;
;;                                                                            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun csde-bug-down-stack () 
  "Moves the source cursor down one frame in the call stack and displays the local
variables at that point in the stack. This command works only for the stack of
the thread at a breakpoint or step point."
  (interactive)

  (if (not (csde-dbs-target-process-steppable-p))
      (error "The target process is not suspended at a breakpoint or steppoint."))

  (if (not (let* ((process (csde-dbs-get-target-process))
		  (stack-ptr (oref process stack-ptr)))
	     (> stack-ptr 0)))
      (error "The debugger is displaying the bottom of the stack."))

  (let* ((process (csde-dbs-get-target-process))
	 (state-info (oref process :state-info))
	 (thread-id (oref state-info :thread-id))
	 (stack (oref process stack))
	 (stack-ptr (1- (oref process stack-ptr)))
	 (frame (nth stack-ptr stack))
	 (class (nth 1 frame))
	 (file (nth 2 frame))
	 (line (nth 3 frame)))
	 
    (oset process :stack-ptr stack-ptr)
    (csde-dbo-show-line class file line)
    (csde-dbo-update-locals-buf process thread-id stack-ptr)))



(defun csde-bug-suspend-thread (thread-id) 
"Suspends the thread or group of threads specified by THREAD-ID.
If the thread or group is already suspended, this command increments
the thread's suspend count. Use CSDEBug->Threads->Show Threads (`csde-bug-thread-show-threads')
to display the IDs of all threads and thread groups running in the
target process. Use CSDEBug->Processes->Suspend Process 
(`csde-bug-suspend-process') to suspend the entire process. Use
Threads->Resume Thread (`csde-bug-resume-thread') to resume the thread."
  (interactive
   "nThread ID: ")
  (let* ((process (csde-dbs-get-target-process))
	 (suspend-command
	  (csde-dbs-suspend-thread
	       (format "suspend thread %d" thread-id)
	       :process process
	       :thread-id thread-id)))
    (csde-dbs-cmd-exec suspend-command)))
 

(defun csde-bug-resume-thread (thread-id) 
"Resumes the previously suspended thread or group of threads specified
by THREAD-ID.  This command has no effect if the specified thread or
thread-group is running or was not suspended by you, using the
CSDEBug->Threads->Suspend Thread command (`csde-bug-suspend-thread').
If you suspended the thread more than once, this command reduces the
suspend count by 1. The thread resumes only when the suspend count
reaches 0. Use CSDEBug->Threads->Show Threads
(`csde-bug-thread-show-threads') to display the IDs of all threads and
thread groups running in the target process. Use
CSDEBug->Processes->Resume Process (`csde-bug-resume-process') to resume
the entire process."
  (interactive
   "nThread ID: ")
  (let* ((process (csde-dbs-get-target-process))
	 (resume-command
	  (csde-dbs-resume-thread
	       (format "resume thread %d" thread-id)
	       :process process
	       :thread-id thread-id)))
    (csde-dbs-cmd-exec resume-command)))
 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                            ;;
;; Interrupt Thread Command                                                   ;;
;;                                                                            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun csde-bug-interrupt-thread (thread-id) 
"Interrupts the thread specified by THREAD-ID. The thread cannot be
resumed. Use CSDEBug->Threads->Show Threads
(`csde-bug-thread-show-threads') to display the IDs of all threads
running in the target process. Use Threads->Suspend Thread
(`csde-bug-suspend-thread') to suspend a thread temporarily."
  (interactive
   "nThread ID: ")
  (let* ((process (csde-dbs-get-target-process))
	 (interrupt-command
	  (csde-dbs-interrupt-thread
	       (format "interrupt thread %d" thread-id)
	       :process process
	       :thread-id thread-id)))
    (csde-dbs-cmd-exec interrupt-command)))
 

(defun csde-bug-stop-thread (thread-id exception-id) 
"Stops a thread and throws an exception. THREAD-ID is the id of the thread you want 
to stop. EXCEPTION-ID is the id of the exception object you want to throw. Use 
CSDEBug->Threads->Show Threads (`csde-bug-thread-show-threads') to display the IDs of 
all threads and thread groups running in the target process. Use CSDEBug->Evaluate Expression
to creae the exception object."
 (interactive
   "nThread ID: \nnException Id: ")
  (let* ((process (csde-dbs-get-target-process))
	 (stop-command
	  (csde-dbs-stop-thread
	       (format "stop thread %d" thread-id)
	       :process process
	       :thread-id thread-id
	       :exception-id exception-id)))
    (csde-dbs-cmd-exec stop-command)))

(defun csde-bug-jpda-installed-p ()
  "Returns t if the jpda is installed."
  (interactive)
  (cond
   (csde-bug-vm-includes-jpda-p
    t)
   ((string= csde-bug-jpda-directory "")
    (error "csde-bug-jpda-directory variable is not set.")
    nil)
   ((not (file-exists-p 
	  (expand-file-name "lib/jpda.jar" csde-bug-jpda-directory)))
    (error "Cannot find JPDA jar file at %s"
	     (expand-file-name "lib/jpda.jar" csde-bug-jpda-directory))
    nil)
   (t
    t)))


(defun csde-bug-start-debugger ()
  "Starts the debugger."
  (interactive)
  (if (and (csde-bug-jpda-installed-p)
	   (csde-dbs-debugger-start csde-dbs-the-debugger))
      (message "Debugger started successfully." )
    (message "Could not start debugger.")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                            ;;
;; Launch Process Command                                                     ;;
;;                                                                            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun csde-bug-launch-process () 
  "Starts a virtual machine to run the application
in the current source buffer. Halts at the beginning
of the process to let you set breakpoints. The started
process becomes the target process for debugger 
commands. Select Processes->Set Target Process from the CSDEBug
menu or run the `csde-bug-set-target-process' command
to set another process as the target process."
  (interactive)
  (let* ((main-class (csde-run-get-main-class)))
    (unless (and
	     (csde-dbs-proc-set-find csde-dbs-the-process-registry 
				    :main-class main-class)
	     (not (yes-or-no-p 
		   (format "An instance of %s is already running. Continue?" main-class))))	 
      (let* ((process 
	      (csde-dbs-proc (format "process%d" 
				    (setq csde-dbs-proc-counter 
					  (1+ csde-dbs-proc-counter))) 
			    :id csde-dbs-proc-counter :main-class main-class))
	     (old-target (csde-dbs-get-target-process))
	     (launch (csde-dbs-launch-process 
		      (format "Launch %s" main-class) 
		      :process process
		      :vmexec (car csde-bug-vm-executable)
		      ;; :vmexec "xyz"
		      ))
	     (succeededp t))
	(csde-dbs-proc-set-add csde-dbs-the-process-registry process)
	(if (not (string= csde-bug-jre-home ""))
	    (oset launch :jre-home csde-bug-jre-home))
	(oset csde-dbs-the-process-registry :target-process process)
	(when (not (csde-dbs-cmd-exec launch))	    
	  (csde-dbs-proc-move-to-morgue process)
	  (if old-target
	      (oset csde-dbs-the-process-registry :target-process old-target))
	  (csde-dbs-proc-set-state process "unknown")
	  (csde-dbs-proc-set-state-reason process "Error launching process.")
	  (csde-dbs-proc-set-add csde-dbs-the-process-morgue process)
	  (setq succeededp nil))
	succeededp))))
	

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                            ;;
;; Attach Local Process Command                                               ;;
;;                                                                            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun csde-bug-attach-via-shared-memory (process-name) 
  "Attaches the debugger to a process running on the same machine via shared
memory. This command works only on Windows."
  (interactive
   "sProcess name: ")
  (let* ((process 
	  (csde-dbs-proc (format "process%d" 
				(setq csde-dbs-proc-counter 
				      (1+ csde-dbs-proc-counter))) 
			:id csde-dbs-proc-counter :main-class process-name))
	     (old-target (csde-dbs-get-target-process))
	     (attach (csde-dbs-attach-shmem
		      (format "Attach %s" process-name) 
		      :process process 
		      :process-name process-name)))
    (csde-dbs-proc-set-add csde-dbs-the-process-registry process)
    (oset csde-dbs-the-process-registry :target-process process)
    (when (not (csde-dbs-cmd-exec attach))	    
      (csde-dbs-proc-move-to-morgue process)
      (if old-target
	  (oset csde-dbs-the-process-registry :target-process old-target))
      (csde-dbs-proc-set-state process "unknown")
      (csde-dbs-proc-set-state-reason process "Error launching process.")
      (csde-dbs-proc-set-add csde-dbs-the-process-morgue process)
      nil)
    t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                            ;;
;; Attach Process on Local Host Command                                       ;;
;;                                                                            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun csde-bug-attach-local-host (process-port) 
  "Attaches the debugger to a process running on local host. This command connects 
to the process via a socket."
  (interactive
   "sProcess Port: ")
  (let* ((process 
	  (csde-dbs-proc (format "process%d" 
				(setq csde-dbs-proc-counter 
				      (1+ csde-dbs-proc-counter))) 
			:id csde-dbs-proc-counter :main-class process-port))
	     (old-target (csde-dbs-get-target-process))
	     (attach (csde-dbs-attach-socket
		      (format "Attach %s" process-port) 
		      :process process 
		      :port process-port)))
    (csde-dbs-proc-set-add csde-dbs-the-process-registry process)
    (oset csde-dbs-the-process-registry :target-process process)
    (when (not (csde-dbs-cmd-exec attach))	    
      (csde-dbs-proc-move-to-morgue process)
      (if old-target
	  (oset csde-dbs-the-process-registry :target-process old-target))
      (csde-dbs-proc-set-state process "unknown")
      (csde-dbs-proc-set-state-reason process "Error launching process.")
      (csde-dbs-proc-set-add csde-dbs-the-process-morgue process)
      nil)
    t))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                            ;;
;; Attach Process on Remote Host Command                                      ;;
;;                                                                            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun csde-bug-attach-remote-host (process-host process-port) 
  "Attaches the debugger to a process running on a remote host. This command connects 
to the process via a socket."
 (interactive
   "sHost: \nsProcess Port: ")
  (let* ((process 
	  (csde-dbs-proc (format "process%d" 
				(setq csde-dbs-proc-counter 
				      (1+ csde-dbs-proc-counter))) 
			:id csde-dbs-proc-counter :main-class process-port))
	     (old-target (csde-dbs-get-target-process))
	     (attach (csde-dbs-attach-socket
		      (format "Attach %s" process-port) 
		      :process process 
		      :host process-host
		      :port process-port)))
    (csde-dbs-proc-set-add csde-dbs-the-process-registry process)
    (oset csde-dbs-the-process-registry :target-process process)
    (when (not (csde-dbs-cmd-exec attach))	    
      (csde-dbs-proc-move-to-morgue process)
      (if old-target
	  (oset csde-dbs-the-process-registry :target-process old-target))
      (csde-dbs-proc-set-state process "unknown")
      (csde-dbs-proc-set-state-reason process "Error launching process.")
      (csde-dbs-proc-set-add csde-dbs-the-process-morgue process)
      nil)
    t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                            ;;
;; Listen for Process on Shared Memory Command                                ;;
;;                                                                            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar csde-bug-server-shmem-name-history nil
  "History of shared memory names for debugger.")

(defun csde-bug-listen-shmem (shmem-name)
  "Listens on shared memory for a vm requiring debugging services."
  (interactive
   (list
    (if (car csde-bug-server-shmem-name)
	(read-from-minibuffer "Name: " 
			      (car csde-bug-server-shmem-name-history)
			      nil nil 
			      'csde-bug-server-shmem-name-history)
      (cdr csde-bug-server-shmem-name))))
  (let* ((process 
	  (csde-dbs-proc (format "process%d" 
				(setq csde-dbs-proc-counter 
				      (1+ csde-dbs-proc-counter))) 
			:id csde-dbs-proc-counter :main-class shmem-name))
	     (old-target (csde-dbs-get-target-process))
	     (listen (csde-dbs-listen-for-process
		      (format "Listen %s" shmem-name) 
		      :process process 
		      :address shmem-name)))
    (csde-dbs-proc-set-add csde-dbs-the-process-registry process)
    (oset csde-dbs-the-process-registry :target-process process)
    (when (not (csde-dbs-cmd-exec listen))	    
      (csde-dbs-proc-move-to-morgue process)
      (if old-target
	  (oset csde-dbs-the-process-registry :target-process old-target))
      (csde-dbs-proc-set-state process "unknown")
      (csde-dbs-proc-set-state-reason process "Error listening for process.")
      (csde-dbs-proc-set-add csde-dbs-the-process-morgue process)
      nil)
    t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                            ;;
;; Listen for Process on Socket Command                                       ;;
;;                                                                            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar csde-bug-server-socket-history nil
  "History of sockets used by debugger to listen for debuggee vms.")

(defun csde-bug-listen-socket (socket)
  "Listens on socket for a vm requiring debugging services.
If `csde-bug-server-socket' is set to \"prompt for,\" this command
prompts you to enter the socket on which to listen. Otherwise, it 
listens on the socket specified by `csde-bug-server-socket'."
  (interactive
   (list
    (if (car csde-bug-server-socket)
	(read-from-minibuffer "Socket: " 
			      (car csde-bug-server-socket-history)
			      nil nil 
			      'csde-bug-server-socket-history)
      (cdr csde-bug-server-socket))))
  (let* ((process 
	  (csde-dbs-proc (format "process%d" 
				(setq csde-dbs-proc-counter 
				      (1+ csde-dbs-proc-counter))) 
			:id csde-dbs-proc-counter :main-class socket))
	     (old-target (csde-dbs-get-target-process))
	     (listen (csde-dbs-listen-for-process
		      (format "Listen %s" socket) 
		      :process process 
		      :address socket
		      :transport "socket")))
    (csde-dbs-proc-set-add csde-dbs-the-process-registry process)
    (oset csde-dbs-the-process-registry :target-process process)
    (when (not (csde-dbs-cmd-exec listen))	    
      (csde-dbs-proc-move-to-morgue process)
      (if old-target
	  (oset csde-dbs-the-process-registry :target-process old-target))
      (csde-dbs-proc-set-state process "unknown")
      (csde-dbs-proc-set-state-reason process "Error listening for process.")
      (csde-dbs-proc-set-add csde-dbs-the-process-morgue process)
      nil)
    t))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                            ;;
;; Detach Process Command                                                     ;;
;;                                                                            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun csde-bug-detach-process () 
  "Detaches the debugger from the target process. The target process continues
to run."
  (interactive)
  (csde-bug-finish-process))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                            ;;
;; Suspend Process Command                                                    ;;
;;                                                                            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun csde-bug-suspend-process () 
"Suspends the target process. To suspend a particular thread or thread group,
use CSDEbug->Threads->Suspend Thread (`csde-bug-suspend-thread')."
  (interactive)
  (let* ((process (csde-dbs-get-target-process))
	 (suspend-command
	  (csde-dbs-suspend-thread "suspend process" :process process)))
    (csde-dbs-cmd-exec suspend-command)))


(defun csde-bug-resume-process () 
"Resumes the target process. To resume a particular thread or thread group,
use CSDEbug->Threads->Resume Thread (`csde-bug-resume-thread')."
  (interactive)
  (let* ((process (csde-dbs-get-target-process))
	 (resume-command
	  (csde-dbs-resume-thread "resume process" :process process)))
    (csde-dbs-cmd-exec resume-command)))


(defun csde-bug-finish-process () 
  "Terminates the target process."
  (interactive)
  (let* ((process (csde-dbs-get-target-process))
	 (finish (csde-dbs-finish-process 
		  (format "finish %d" (oref process id))
		  :process process))
	 (result (csde-dbs-cmd-exec finish)))
    (csde-dbs-proc-move-to-morgue process)
    (slot-makeunbound csde-dbs-the-process-registry :target-process)
    (csde-dbs-proc-set-state-reason process "finish")))

(defun csde-bug-set-target-process (process-id) 
  "Sets the process whose process-id is PROCESS-ID to be
the focus of debugger commands."
  (interactive
   "nEnter process id: ")
  (csde-dbs-proc-registry-set-target-proc 
   csde-dbs-the-process-registry process-id))
  
	  
(defun csde-bug-show-processes () 
  (interactive)
  (message "not implemented"))


(defun csde-bug-remove-dead-processes () 
  "Remove dead processes and their associated buffers from the Emacs environment."
  (interactive)
  (if (oref csde-dbs-the-process-morgue proc-alist)
      (csde-dbs-proc-morgue-bury-the-dead csde-dbs-the-process-morgue)))


(defun csde-bug-show-locals-buf ()
  "Show the local variables buffer of the target process.
This command shows the locals buffer in the middle pane of
CSDEBug's three-pane frame configuration."
  (interactive)
  (let* ((process (csde-dbs-get-target-process))
	 (locals-buf (oref process locals-buf))
	 (source-window (selected-window)))
    (set-window-configuration (oref process win-cfg))
    (set-window-buffer (next-window source-window) locals-buf)
    (select-window source-window)))

(defun csde-bug-show-cli-buf ()
  "Show the command-line interface (CLI) buffer of the target process.
This command shows the CLI buffer in the middle pane of CSDEBug's
three-pane window configuration."
  (interactive)
  (let* ((process (csde-dbs-get-target-process))
	(cli-buf (oref process cli-buf))
	(source-window (selected-window)))
    (set-window-configuration (oref process win-cfg))
    (set-window-buffer (next-window source-window) cli-buf)
    (select-window source-window)))


(defun csde-bug-show-threads-buf ()
  "Show the threads buffer of the target process.
This command shows the threads buffer in the middle
pane of the CSDEBug's three-pane window configuration."
  (interactive)
  (let* ((process (csde-dbs-get-target-process))
	(threads-buf (oref process threads-buf))
	(source-window (selected-window)))
    (set-window-configuration (oref process win-cfg))
    (set-window-buffer (next-window source-window) threads-buf)
    (select-window source-window)))


(defun csde-bug-show-preferences () 
  (interactive)
  (customize-apropos "csde-bug" 'groups))


(defun csde-bug-set-breakpoints (process breakpoints)
  "Sets BREAKPOINTS in PROCESS."
  (mapc
   (lambda (assoc)
     (let* ((breakpoint (cdr assoc))
	    (set-breakpoint (csde-dbs-set-breakpoint
			     (format "set breakpoint%d" 
				     (oref breakpoint id))
			     :process process
			     :breakpoint breakpoint))
	    (result (csde-dbs-cmd-exec set-breakpoint)))))
   breakpoints))

;;;###autoload
(defun csde-bug-debug-app ()
  "Runs the debugger on the application in the current source buffer."
  (interactive)
  (if (and 
       (csde-bug-jpda-installed-p)
       (not (csde-dbs-debugger-running-p)))
      (csde-dbs-debugger-start csde-dbs-the-debugger))
  (if (csde-dbs-debugger-running-p)
      (let ((result (csde-bug-launch-process)))
	(if result
	    (let ((process (oref csde-dbs-the-process-registry :target-process)))
	      (csde-bug-set-breakpoints process csde-bug-breakpoints)
	      (setq result (csde-bug-continue)))))))

(defun csde-bug-help ()
  "Displays the CSDEbug User's Guide."
  (interactive)
  (let* ((csde-dir (csde-find-csde-doc-directory))
         (csdebug-help
          (if csde-dir
              (if (and csde-xemacsp
                       (locate-data-directory "csde"))
                  (expand-file-name "csdebug-ug.html" csde-dir)
                (expand-file-name "doc/html/csdebug-ug/csdebug-ug.html" csde-dir)))))       
    (if (and
         csdebug-help
         (file-exists-p csdebug-help))
        (browse-url (concat "file://" (csde-convert-cygwin-path csdebug-help))
                    browse-url-new-window-p)
      (signal 'error '("Cannot find CSDEbug User's Guide.")))))

(defun csde-bug-keys ()
  "Displays CSDEbug keybindings. Use `csde-keys' to display CSDE keybindings."
  (interactive)
  (csde-describe-map 'csde-bug-keymap))


(provide 'csde-bug)

;; $Log: csde-bug.el,v $
;; Revision 1.2  2001/02/12 05:38:22  paulk
;; CSDE 2.2.7
;;
;; Revision 1.57  2000/12/18 05:22:45  paulk
;; *** empty log message ***
;;
;; Revision 1.56  2000/11/18 05:31:43  paulk
;; Fixed bug in csde-bug-remove-breakpoint-highlight.
;;
;; Revision 1.55  2000/11/16 02:58:29  paulk
;; Fixed bug that caused highlight not to be removed from breakpint.
;;
;; Revision 1.54  2000/09/23 04:33:26  paulk
;; Fixed bug in csde-bug-clear-breakpoint command where the command was off by a line. Thanks to Stan Lanning <lanning@pobox.com> for providing this fix.
;;
;; Revision 1.53  2000/08/14 02:33:36  paulk
;; Adds support for Step Into All command.
;;
;; Revision 1.52  2000/07/28 06:27:44  paulk
;; Committing all modified files.
;;
;; Revision 1.51  2000/07/13 05:22:46  paulk
;; *** empty log message ***
;;
;; Revision 1.50  2000/06/12 08:29:32  paulk
;; Restored csde-bug-install-csdebug-menu for XEmacs compatibility.
;; Added csde-bug-debugger-host-address variable.
;;
;; Revision 1.49  2000/05/26 09:18:20  paulk
;; Added variable csde-bug-raise-frame-p to allow a user to specify
;; whether the Emacs window should pop up when a breakpoint is hit.
;;
;; Revision 1.48  2000/05/10 05:36:49  paulk
;; The CSDEbug menu now appears or disappears when you select or deselect CSDEbug as the current debugger.
;;
;; Revision 1.47  2000/03/16 05:08:25  paulk
;; Added CSDEbug option to csde-db-debugger.
;;
;; Revision 1.46  2000/03/03 07:03:21  paulk
;; Fixed bug where csde-bug-launch-process was returning t even when it failed.
;;
;; Revision 1.45  2000/02/17 06:41:09  paulk
;; Added key bindings for debugger.
;;
;; Revision 1.44  2000/02/16 04:59:48  paulk
;; Implemented color customization for breakpoint marker.
;; Implemented persistent breakpoints.
;;
;; Revision 1.43  2000/02/14 06:19:37  paulk
;; Implemented up and down stack commands.
;;
;; Revision 1.42  2000/02/10 02:53:37  paulk
;; Fixed bug where Display->Threads command was not enabled when debugger
;; was attached to a process.
;;
;; Revision 1.41  2000/02/02 05:49:51  paulk
;; Fixed bug in socket listen command.
;;
;; Revision 1.40  2000/02/01 05:58:44  paulk
;; Added commands for listening for applications needing debug services.
;;
;; Revision 1.39  2000/02/01 04:11:54  paulk
;; ReleaseNotes.txt
;;
;; Revision 1.38  2000/01/15 08:04:07  paulk
;; Added show buffer commands.
;;
;; Revision 1.37  2000/01/02 08:07:55  paulk
;; Added attach process commands.
;;
;; Revision 1.36  1999/12/27 08:01:17  paulk
;; Added show object monitors command.
;;
;; Revision 1.35  1999/12/20 07:52:06  paulk
;; Added cancel watchpoint command.
;;
;; Revision 1.34  1999/12/19 06:54:21  paulk
;; Added watch field command.
;;
;; Revision 1.33  1999/12/14 04:46:02  paulk
;; Added CSDEbug->Processes->Remove Dead Processes command.
;;
;; Revision 1.32  1999/12/13 05:54:08  paulk
;; Added csde-bug-vm-executable and csde-bug-jre-home variables.
;; Fixed csde-dbs-launch-process command so that it fails gracefully.
;;
;; Revision 1.31  1999/12/03 08:22:00  paulk
;; Updated CSDEbug to run under JDK 1.3beta.
;;
;; Revision 1.30  1999/11/30 05:46:21  paulk
;; Added CSDEbug->Display->Path Info command.
;;
;; Revision 1.29  1999/11/29 06:58:41  paulk
;; Added CSDEbug->Display->Loaded Classes Command.
;;
;; Revision 1.28  1999/11/27 05:13:49  paulk
;; Added commands for tracing classes.
;;
;; Revision 1.27  1999/11/23 06:37:03  paulk
;; Added Trace->Cancel command.
;;
;; Revision 1.26  1999/11/16 05:58:17  paulk
;; Added trace method commands and skeletons for trace class and cancel
;; trace commands.
;;
;; Revision 1.25  1999/11/04 05:54:07  paulk
;; Added class and method tracing command skeletons.
;;
;; Revision 1.24  1999/10/28 04:18:09  paulk
;; Added interrupt and stop thread commands.
;;
;; Revision 1.23  1999/10/15 05:16:58  paulk
;; Fixed bugs in CSDEbug->Exit Debugger.
;;
;; Revision 1.22  1999/10/14 04:59:23  paulk
;; Added Resume Process and Resume Thread commands.
;;
;; Revision 1.21  1999/10/13 08:16:43  paulk
;; Added suspend process and suspend thread commands.
;;
;; Revision 1.20  1999/10/13 06:19:56  paulk
;; Add CSDEBug->Threads->Show Threads command
;;
;; Revision 1.19  1999/09/28 04:01:57  paulk
;; Patched to use either gud-find-c-expr or find-c-expr.
;;
;; Revision 1.18  1999/09/16 05:36:59  paulk
;; Added get locals command.
;;
;; Revision 1.17  1999/09/13 05:37:33  paulk
;; Enhanced get array command.
;;
;; Revision 1.16  1999/09/10 06:41:50  paulk
;; Finished first cut at get_object command.
;;
;; Revision 1.15  1999/09/08 05:40:45  paulk
;; Updated debugger code to take advantage of new unbound slot capability
;; of eieio.
;;
;; Revision 1.14  1999/09/07 05:12:35  paulk
;; Added get array command.
;;
;; Revision 1.13  1999/09/05 04:35:34  paulk
;; Added initial implementation of evaluate and display variable commands.
;;
;; Revision 1.12  1999/08/30 07:10:41  paulk
;; Converted clear breakpoint command to OOPS.
;;
;; Revision 1.11  1999/08/28 05:34:19  paulk
;; Improved multiple process handling, window configuration.
;;
;; Revision 1.10  1999/08/27 05:27:52  paulk
;; Provided initial support for multiple processes.
;; Fixed csde-find-data-directory to work on XEmacs with a standard
;; CSDE distribution.
;; Ported breakpoint highlighting code to XEmacs. Still has bugs though.
;; Now includes csde-db-option options on vm command-line for process.
;;
;; Revision 1.9  1999/08/24 06:29:43  paulk
;; Reimplemented the constructor for csde-dbs-proc the right way. Renamed
;; csde-bug-counter to csde-bug-breakpoint-counter.
;;
;; Revision 1.8  1999/08/23 06:17:45  paulk
;; Minor bug.
;;
;; Revision 1.7  1999/08/23 05:33:37  paulk
;; Added customization variable csde-bug-jpda-directory. Also check to
;; ensure that this variable specifies a valid path.
;;
;; Revision 1.6  1999/08/23 01:44:25  paulk
;; Updated to use Eric Ludlam's eieio object system.
;;
;; Revision 1.5  1999/08/19 10:18:27  paulk
;; *** empty log message ***
;;
;; Revision 1.4  1999/08/18 01:18:41  paulk
;; Continuing implementation
;;
;; Revision 1.3  1999/08/17 01:06:03  paulk
;; *** empty log message ***
;;
;; Revision 1.2  1999/08/15 23:46:46  paulk
;; Implemented more functionality.
;;
;; Revision 1.1  1999/08/10 09:59:59  paulk
;; Initial revision
;;
