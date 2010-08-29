;;; csde-dbs.el -- CSDEbug Session Interface Functions
;; $Revision: 1.4 $ $Date: 2001/02/12 05:38:24 $ 

;; Adapted from the JDE by Matt Bruce <matt.bruce@morganstanley.com>
;; Maintainer:  Matt Bruce

;; Copyright (C) 2001 by Matt Bruce

;; JDE Author: Paul Kinnucan <paulk@mathworks.com>
;; JDE Maintainer: Paul Kinnucan

;; Keywords: csharp, tools

;; JDE version Copyright (C) 1997, 1998, 1999, 2000, 2001 Paul Kinnucan.

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

;; This is one of a set of packages that make up the 
;; Csharp Development Environment (CSDE) for Emacs. See the
;; CSDE User's Guide for more information.


;; The latest version of the CSDE is available at
;; <URL:http://www.sourceforge.com/>.

;; Please send any comments, bugs, or upgrade requests to
;; Matt Bruce (matt.bruce@morganstanley.com)

;;; Code:

(require 'csde-dbo) 
(require 'eieio)
(require 'csde-widgets)

;; Need csde-run only to get the definition for 
;; save-w32-show-window macro.
(eval-when-compile
  (require 'csde-run))

(defcustom csde-bug-sio-connect-delay 1
  "Length of time in seconds that the CSDE waits
before attempting to connect to the   
debuggee application's standard I/O. This delay
is intended to give CSDEbug time to create the
SIO socket. Try increasing this variable if CSDEbug
hangs while launching an application. If your
system never hangs, you can reduce this setting
to 0 to eliminate the connection delay."
  :group 'csde-bug
  :type 'integer)

(defvar csde-dbs-comint-filter nil
  "Standard comint filter for debugger buffer.")

(defvar csde-dbs-debugger-process-name "csdebug"
"Name of debugger process.")

(defun csde-dbs-get-debugger-process ()
  (get-process csde-dbs-debugger-process-name))


(defvar csde-dbs-debugger-output-buffer-name "*CSDEbug Messages*"
"Name of buffer used to display messages from the debugger.")

(defvar csde-dbs-debugger-socket-process-name "csdebug-socket"
"Name of debugger socket process.")

(defun csde-dbs-get-debugger-socket-process ()
  (get-process csde-dbs-debugger-socket-process-name))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                            ;;
;; Process Set                                                                ;;
;;                                                                            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defclass csde-dbs-proc-set ()
  ((proc-alist     :initarg :proc-alist
		   :type list
		   :initform nil
		   :documentation
		   "List of active processes"))
  "Class of process sets.")

(defmethod csde-dbs-proc-set-add ((this csde-dbs-proc-set) process)
  (oset this :proc-alist
	(cons 
	 (cons (oref process :id) process)
	 (oref this :proc-alist))))

(defmethod csde-dbs-proc-set-remove ((this csde-dbs-proc-set) process)
  (oset this :proc-alist
	(remove-if
	 (lambda (assoc)
	   (let* ((xproc (cdr assoc))
		  (xid (oref xproc id))
		  (id (oref process id)))
	     (equal xid id)))
	 (oref this proc-alist))))

(defmethod csde-dbs-proc-set-get-proc ((this csde-dbs-proc-set) id)
  (cdr (assq id (oref this :proc-alist))))

(defmethod csde-dbs-proc-set-find ((this csde-dbs-proc-set) field value)
  "Finds the process in the set whose FIELD is equal to VALUE."
  (if (slot-boundp this :proc-alist)
      (cdr (find-if
	(lambda (assoc-x)
	  (let ((process-x (cdr assoc-x)))
	    (equal (oref process-x field) value)))
	(oref this :proc-alist)))))

(defmethod csde-dbs-proc-set-contains-p ((this csde-dbs-proc-set) process)
  (assq (oref process :id) (oref this :proc-alist)))

(defmethod csde-dbs-proc-set-get-size ((this csde-dbs-proc-set))
  "Gets the number of processes in this set."
  (if (slot-boundp this 'proc-alist)
      (length (oref this proc-alist))
    0))
	      

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                            ;;
;; Process Registry                                                           ;;
;;                                                                            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defclass csde-dbs-proc-registry (csde-dbs-proc-set)
  ((target-process :initarg :target-process
		   :type csde-dbs-proc
		   :documentation
		   "Process that currently has the debugger command focus."))
  "Class of process registries.")


(defmethod csde-dbs-proc-registry-set-target-proc ((this csde-dbs-proc-registry) &optional id)
  "Sets process specified by ID to be the target process. If ID is not specified, the first
registered process becomes the target process"
  (let ((target-process
	  (if id
	      (let ((process (csde-dbs-proc-set-get-proc this id)))
		(if process
		    (if (csde-dbs-proc-set-contains-p this process)
			process
		      (message "Error: process %s is dead." process-id)
		      nil)
		  (message "Error: process %s does not exist." process-id)
		  nil))
	    (let ((existing-processes 
		   (oref csde-dbs-the-process-registry :proc-alist)))
	      (if existing-processes (cdr (nth 0 existing-processes)))))))
    (when target-process
      (oset this target-process target-process)
      (set-window-configuration (oref target-process win-cfg)))
    target-process))
  

(defvar csde-dbs-the-process-registry 
  (csde-dbs-proc-registry "Process Registry")
  "The debuggee process registry.")

(defun csde-dbs-get-target-process ()
  (and csde-dbs-the-process-registry
       (slot-boundp csde-dbs-the-process-registry :target-process)
       (oref csde-dbs-the-process-registry :target-process)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                            ;;
;; Process Morgue                                                             ;;
;;                                                                            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defclass csde-dbs-proc-morgue (csde-dbs-proc-set) ()
  "Class of process morgues. A process morgue contains dead or dying processes. 
Their carcasses must be kept around until the debugger stops sending messages
concerning them." )

(defmethod csde-dbs-proc-morgue-bury-the-dead ((this csde-dbs-proc-morgue))
  (mapc 
   (lambda (dead-proc-assoc)
     (let* ((dead-proc (cdr dead-proc-assoc))
	    (cli-buffer (if (slot-boundp dead-proc 'cli-buf) (oref dead-proc cli-buf)))
	    (msg-buffer (if (slot-boundp dead-proc 'msg-buf) (oref dead-proc msg-buf)))
	    (locals-buffer (if (slot-boundp dead-proc 'locals-buf) (oref dead-proc locals-buf)))
	    (threads-buffer (if (slot-boundp dead-proc 'threads-buf) (oref dead-proc threads-buf))))
       (if cli-buffer (kill-buffer cli-buffer))
       (if msg-buffer (kill-buffer msg-buffer))
       (if locals-buffer (kill-buffer locals-buffer))
       (if threads-buffer (kill-buffer threads-buffer))))
   (oref this proc-alist))
  (oset this proc-alist nil))


(defvar csde-dbs-the-process-morgue (csde-dbs-proc-morgue "The CSDE Process Morgue")
  "The CSDE process morgue. This morgue contains processes that are dead or
dying, for example, because they have been terminated by the user or the
debugger. Their corpses must be kept around until it is clear they are dead and
the debugger ceases sending messages concerning them.")


(defun csde-dbs-get-process (id)
"Get the process whose id is ID. This function looks first in the process registry
and then in the process morgue for the process."
  (let ((process
	 (csde-dbs-proc-set-get-proc csde-dbs-the-process-registry id)))
    (if (not process)
	(setq process (csde-dbs-proc-set-get-proc csde-dbs-the-process-morgue id)))
    process))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                            ;;
;; Process State Info                                                         ;;
;;                                                                            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defclass csde-dbs-proc-state-info ()
  ((state       :initarg :state)
   (reason      :initarg :reason)
   (thread-id   :initarg :thread-id)
   (thread-name :initarg :thread-name))
  "Class of process state information objects.")


(defmethod csde-dbs-proc-state-info-set ((this csde-dbs-proc-state-info)
					state reason thread-id thread-name)
  (oset this reason reason)
  (oset this state state)
  (oset this thread-id thread-id)
  (oset this thread-name thread-name))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                            ;; 
;; Breakpoint Specification                                                   ;;
;;                                                                            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defclass csde-dbs-proc-bpspec ()
  ((id         :initarg :id
	       :type integer
	       :documentation 
	       "Id assigned to this breakpoint by the debugger.")
   (breakpoint :initarg :breakpoint
	       :type csde-bug-breakpoint
	       :documentation
	       "Instance of `csde-bug-breakpoint'.")
   (resolved   :initarg :resolved))
  (:allow-nil-initform t)
  "Class of breakpoint specifications. A breakpoint specification contains 
process-specific information about a breakpoint")


;; Defines a class of containers for breakpoint specs.
;; Each container lists the process specs for breakpoints set in a
;; particular process.

(defun csde-dbs-proc-bpspecs-add (bpspecs bpspec)
  "Adds BPSPEC to BPSPECS, a process's breakpoint spec list."
  (cons 
   (cons (oref bpspec id) bpspec)
   bpspecs))

(defun csde-dbs-proc-bpspecs-remove (bpspecs bpspec)
  "Removes BPSPEC from BPSPECS"
  (remove-if (lambda (x) 
	       (equal (car x) (oref bpspec id) ))
	     bpspecs))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                            ;;
;; Trace Request Class                                                        ;;
;;                                                                            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defclass csde-dbs-trace-request ()
  ((id                  :initarg :id
	                :type integer
		        :documentation
		        "Trace request id")
   (suspend-policy      :initarg :suspend-policy
		        :type string
		        :initform "none"
		        :documentation
		        "Valid values are all (all threads), thread (current thread), or none")
   (inclusion-filters   :initarg :inclusion-filters
			:type list
			:documentation
			"List of regular expressions specifying classes to include in trace.")
   (exclusion-filters   :initarg :exclusion-filters
			:type list
			:documentation
			"List of regular expressions specifying classes to exclude from trace.")
   (cancel-command      :initarg :cancel-command
			:type string
			:documentation
			"Name of command used to cancel this request.")
   )
"Super class of trace requests."
)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                            ;;
;; Trace Method Request Class                                                 ;;
;;                                                                            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defclass csde-dbs-trace-methods-request (csde-dbs-trace-request)
   ((trace-type         :initarg :trace-type
			:type string
			:initform "entry"
			:documentation 
			"Entry or exit.")
   (thread-restriction  :initarg :thread-restriction
	                :type string
			:documentation
			"Thread to trace."))
   "Trace methods request."
)

(defmethod initialize-instance ((this csde-dbs-trace-methods-request) &rest fields)
  "Constructor for objects of `csde-dbs-trace-methods-request' class."
  (call-next-method)
  (oset this cancel-command "cancel_trace_methods"))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                            ;;
;; Trace Classes Request Class                                                ;;
;;                                                                            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defclass csde-dbs-trace-classes-request (csde-dbs-trace-request)
   ((trace-type         :initarg :trace-type
			:type string
			:initform "preparation"
			:documentation 
			"Valid values are preparation or unloading."))
   "Trace classes request."
)

(defmethod initialize-instance ((this csde-dbs-trace-classes-request) &rest fields)
  "Constructor for objects of `csde-dbs-trace-classes-request' class."
  (call-next-method)
  (oset this cancel-command "cancel_trace_classes"))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                            ;;
;; Trace Exceptions Request Class                                             ;;
;;                                                                            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defclass csde-dbs-trace-exceptions-request (csde-dbs-trace-request)
  ((exception-class    :initarg :exception-class
		       :type string
		       :documentation
		       "Class of exceptions to trace. Can be a wild card pattern.")
   (trace-type         :initarg :trace-type
		       :type string
		       :initform "both"
		       :documentation 
			"Valid values are caught, uncaught, or both."))
   "Trace exceptions request."
)

(defmethod initialize-instance ((this csde-dbs-trace-exceptions-request) &rest fields)
  "Constructor for objects of `csde-dbs-trace-exceptions-request' class."
  (call-next-method)
  (oset this cancel-command "clear"))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                            ;;
;; Watch Field Request Class                                                  ;;
;;                                                                            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defclass csde-dbs-watch-field-request (csde-dbs-trace-request)
  ((watch-type         :initarg :watch-type
		       :type string
		       :documentation
		       "Valid values are \"access\" and \"modification\".")
   (object-class       :initarg :object-class
		       :type string
		       :documentation
		       "Class of object to watch. Can be a wild card pattern.")
   (field-name         :initarg :field-name
		       :type string
		       :documentation 
			"Name of field to watch.")
   (expression         :initarg :expression
		       :type string
		       :documentation 
		       "Boolean expression that must be satisfied to suspend execution.")
   (object-id          :initarg :object-id
		       :type string
		       :documentation 
		       "Id of object to watch."))
   "Watch field request."
)




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                            ;;
;; Application Process Class                                                  ;;
;;                                                                            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defclass csde-dbs-proc ()
  ((id            :initarg :id
                  :type integer
                  :documentation
                  "Id assigned by the CSDE.")
   (main-class    :initarg :main-class
                  :type string
                  :documentation
                  "Main class for this process.")
   (cli-socket    :initarg :cli-socket
                  :type integer
	          :documentation
                  "Number of socket used by the process's command line interface.")
   (cli-buf       :initarg :cli-buf
                  :type buffer
	          :documentation
	          "Buffer for the process's command-line interface.")
   (msg-buf       :initarg :msf-buf
	          :type buffer
	          :documentation
	          "Buffer used to display debugger output for this process")
   (threads-buf   :initarg :threads-buf
		  :type buffer
		  :documentation
		  "Buffer used to display threads.")
   (locals-buf    :initarg :locals-buf
		  :type buffer
		  :documentation
		  "Buffer used to display local variables.")
   (startupp       :initarg :startupp
                  :type boolean
                  :initform nil
                  :documentation
		  "non-nil if this process is in the startup state.")
   (suspendedp    :initarg :suspendedp
                  :type boolean
                  :initform nil
                  :documentation
		  "non-nil if this process has been suspended by the debugger.")
   (steppablep    :initarg :steppablep
                  :type boolean
                  :initform nil
                  :documentation
		  "non-nil if this process can be single-stepped.")
   (state-info    :initarg :state-info
	          :type csde-dbs-proc-state-info
	          :documentation
	          "Process state information.")
   (stack         :initarg :stack
		  :type list
		  :documentation
		  "Lists stack frames for thread of current step or breakpoint.")
   (stack-ptr     :initarg :stack-ptr
		  :type integer
		  :initform 0
		  :documentation
		  "Points to the current frame on the stack.")
   (trace-req     :initarg :trace-req
		  :type list
		  :documentation
                  "List of outstanding trace requests.")
   (watch-req     :initarg :watch-req
		  :type list
		  :documentation
                  "List of outstanding watch field requests.")
   (object-refs   :initarg :object-refs
		  :type list
		  :initform nil
		  :documentation
		  "IDs of debuggee objects currently referenced by the debugger.")
   (bpspecs       :initarg :bpspecs
	          :type list
	          :documentation
                  "Breakpoints set in this process.")
   (last-cmd      :initarg :last-cmd
	          :type csde-dbs-cmd
	          :documentation
                  "Most recent command targeting this process.")
   (win-cfg       :initarg :win-cfg
	          :type window-configuration
	          :documentation
	          "Desired window configuration for this process.")
   (attachedp     :initarg :attachedp
		  :type boolean
		  :initform nil
		  :documentation
		  "Non-nil if the debugger was attached to this process."))
  (:allow-nil-initform t)
  "Class of debuggee processes.")

(defmethod initialize-instance ((this csde-dbs-proc) &rest fields)
  "Constructor for objects of `csde-dbs-proc' class."
  (call-next-method)

  (if (not (slot-boundp this 'state-info))
      (oset this state-info 
	    (csde-dbs-proc-state-info 
	     (format "State Info %d" (oref this id)))))

  (assert (slot-boundp this 'main-class))
  (assert (slot-boundp this 'id))
  
  (oset this msg-buf (get-buffer-create 
		      (format "Process %s(%d)" 
			      (oref this main-class)
			      (oref this id))))
  (save-excursion
    (set-buffer (oref this msg-buf))
    (erase-buffer)	
    (goto-char (point-min))
    (insert 
       (format "*** Debugger Output for Process %s(%d) ***\n\n" 
	       (oref this main-class)
	       (oref this id))))

  (oset this locals-buf (get-buffer-create
			(format "%s(%d) Local Variables"
				(oref this main-class)
				(oref this id))))

  (oset this threads-buf (get-buffer-create
			  (format "%s(%d) Threads"
				  (oref this main-class)
				  (oref this id)))))


(defmethod csde-dbs-proc-set-state ((this csde-dbs-proc) state)
  (let ((state-info (oref this state-info)))
    (oset state-info state state)))

(defmethod csde-dbs-proc-set-state-reason ((this csde-dbs-proc) reason)
  (let ((state-info (oref this state-info)))
    (oset state-info reason reason)))

(defmethod csde-dbs-proc-get-state ((this csde-dbs-proc))
  (oref (oref this state-info) state))

(defmethod csde-dbs-proc-get-state-reason ((this csde-dbs-proc))
  (oref (oref this state-info) reason))

; (defmethod csde-dbs-proc-display-debug-message ((this csde-dbs-proc) message)
;   (let ((buffer
; 	 (oref this msg-buf)))
;     (if buffer
; 	(save-excursion
; 	  (set-buffer buffer)
; 	  (goto-char (point-max))
; 	  (insert (concat message "\n"))))))

(defmethod csde-dbs-proc-display-debug-message ((this csde-dbs-proc) message)
  (let ((buffer
  (oref this msg-buf)))
    (if buffer
 (save-excursion
   (let ((currbuffp (equal buffer (current-buffer))))
     (if (not currbuffp) (other-window -1))
     (set-buffer buffer)
     (goto-char (point-max))
     (insert (concat message "\n"))
     (goto-char (point-max))
     (if (not currbuffp) (other-window 1)))))))


(defmethod csde-dbs-proc-move-to-morgue ((this csde-dbs-proc))
  "Moves this process from the process registry to the process morgue."
  (csde-dbs-proc-set-remove csde-dbs-the-process-registry this)
  (csde-dbs-proc-set-add csde-dbs-the-process-morgue this))

(defmethod csde-dbs-proc-move-to-registry ((this csde-dbs-proc))
  "Moves this process from the registry to the morgue."
  (csde-dbs-proc-set-remove csde-dbs-the-process-morgue this)
  (csde-dbs-proc-set-add csde-dbs-the-process-registry this))


(defmethod csde-dbs-proc-get-bpspec ((this csde-dbs-proc) bp)
  "Gets the process specification for a breakpoint. BP may be either
an instance of `csde-bug-breakpoint' or the debugger-assigned id
for the breakpoint."
  (if (slot-boundp this 'bpspecs)
      (let ((bpspecs (oref this bpspecs)))
	(if (and (object-p bp) (csde-bug-breakpoint-p bp))
	    (let* ((csde-id (oref bp id)))
	      (cdr
	       (find-if
		(lambda (assoc)
		  (let ((spec (cdr assoc)))
		    (equal (oref (oref spec breakpoint) id) csde-id)))
		bpspecs)))
	  (cdr (assoc bp bpspecs))))))

(defmethod csde-dbs-proc-runnable-p ((this csde-dbs-proc))
  (or
   (oref this startupp)
   (oref this suspendedp)
   (oref this steppablep)))

(defun csde-dbs-target-process-runnable-p ()
  (interactive)
  (let ((target (csde-dbs-get-target-process)))
    (and target (csde-dbs-proc-runnable-p target))))

(defun csde-dbs-target-process-steppable-p ()
  (interactive)
  (let ((target (csde-dbs-get-target-process)))
    (and target (oref target steppablep))))

(defun csde-dbs-display-debug-message (proc-id message)
  (let ((process (csde-dbs-get-process proc-id)))
    (if process 
	(csde-dbs-proc-display-debug-message process message)
      (message message))))

(defvar csde-dbs-proc-counter 0
  "Process counter. Used to generate process IDs.")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                            ;; 
;; Csharp Object                                                                ;;
;;                                                                            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defclass csde-dbs-csharp-obj ()
  ((jtype  :initarg :jtype
	   :type string
	   :documentation
	  "Type of this object."))
  "Superclass of Csharp objects.")

(defmethod csde-dbs-csharp-obj-to-string ((this csde-dbs-csharp-obj))
  "")


(defclass csde-dbs-csharp-primitive (csde-dbs-csharp-obj)
  ((value :initarg :value
	  :type (or string number)
	  :documentation
	  "Value of this primitive object."))
  "Class of Csharp primitives.")

(defmethod csde-dbs-csharp-obj-to-string ((this csde-dbs-csharp-primitive))
  (format "%s" (oref this value)))

(defclass csde-dbs-csharp-null (csde-dbs-csharp-obj) ()
  "Csharp null object.")

(defmethod initialize-instance ((this csde-dbs-csharp-null) &rest fields)
  "Constructor for run process command."

  ;; Call parent initializer.
  (call-next-method)

  (oset this jtype "null"))


(defmethod csde-dbs-csharp-obj-to-string ((this csde-dbs-csharp-null))
  "null")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                            ;; 
;; Csharp Variable                                                              ;;
;;                                                                            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defclass csde-dbs-csharp-variable ()
  ((name         :initarg :name
		 :type string
		 :documentation
		 "Name of this variable")
   (jtype        :initarg :jtype
		 :type string
		 :documentation
		 "Type of this variable.")
   (value        :initarg :value
                 :type csde-dbs-csharp-obj
		 :documentation
		 "Value of this variable."))
  "Class that defines the CSDE's representation of a Csharp variable.")

(defmethod csde-dbs-csharp-variable-to-string ((this csde-dbs-csharp-variable))
  (format "%s %s = %s"
	  (oref this jtype)
	  (oref this name)
	  (csde-dbs-csharp-obj-to-string (oref this value))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                            ;; 
;; Csharp Class Instance                                                        ;;
;;                                                                            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defclass csde-dbs-csharp-class-instance (csde-dbs-csharp-obj)
  ((id           :initarg :id
		 :type integer
		 :documentation
		 "Id assigned to this object by the debugger.")
   (gc-flag      :initarg :gc-flag
		 :type boolean
		 :documentation
		 "t if this object has been garbage collected."))
  "Instance of a Csharp class accessed via the debugger.")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                            ;; 
;; Csharp Array                                                                 ;;
;;                                                                            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defclass csde-dbs-csharp-array (csde-dbs-csharp-class-instance)
  ((length     :initarg :length
	       :type integer
	       :documentation
	       "Length of this array.")
   (elements   :initarg :elements
	       :type list
	       :initform nil
	       :documentation
	       "Elements of this array."))
  "Class of Lisp objects representing instances of Csharp arrays.")



(defmethod csde-dbs-csharp-obj-to-string ((this csde-dbs-csharp-array))
  (let ((str (format "<%s:%d%s> %d" 
		     (oref this jtype)
		     (oref this id)
		     (if (oref this gc-flag) ":gc" "")
		     (oref this length)))
	(elements (oref this elements)))
    (if elements
	(let ((sep "\n |- "))
	  (concat 
	   str
	   sep
	   (mapconcat
	    (lambda (element) 
	      (csde-dbs-csharp-obj-to-string element))
	    elements sep)))
      str)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                            ;; 
;; Csharp User-Defined Class Instance                                           ;;
;;                                                                            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defclass csde-dbs-csharp-udci (csde-dbs-csharp-class-instance)
  ((fields       :initarg :fields
		 :type list
		 :initform nil
		 :documentation
		 "Fields of this object."))
  "Class of Lisp objects representing instances of user-defined Csharp classes.")


(defmethod csde-dbs-csharp-udci-add-field ((this csde-dbs-csharp-udci) field)
  (oset this fields
	(nconc (oref this fields) (list (cons (oref field name) field)))))


(defmethod csde-dbs-csharp-obj-to-string ((this csde-dbs-csharp-udci))
  (let ((str (format "<%s:%d%s>" 
		     (oref this jtype)
		     (oref this id)
		     (if (oref this gc-flag) ":gc" "")))
	(fields (oref this fields)))
    (if fields
	(let ((sep "\n |- "))
	  (concat 
	   str
	   sep
	   (mapconcat
	    (lambda (assoc) 
	      (csde-dbs-csharp-variable-to-string (cdr assoc)))
	    fields sep)))
      str)))
	 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                            ;; 
;; Debugger Class                                                             ;;
;;                                                                            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defclass csde-dbs-debugger ()
  ((name          :initarg :name
	          :initform "CSDEbug"
		  :type string
		  :documentation
		  "Name of debugger.")
   (buffer-name   :initarg :buffer-name
                  :initform "*CSDEbug*"
		  :type string
		  :documentation
		  "Name of buffer used to interact with debugger.")
   (buffer        :initarg :buffer
		  :type buffer
		  :documentation
		  "Buffer used to interact with debugger.")
   (process       :initarg :process)
   (comint-filter :initarg :comint-filter)
   (started-p     :initarg :started-p
		  :initform nil
		  :type boolean
		  :documentation
		  "True if debugger started successfully."))
  "Class of debuggers.")

(defmethod csde-dbs-debugger-register-process-filter ((debugger csde-dbs-debugger) filter)
  "Set the process filter for the debugger to FILTER."
  (set-process-filter  (oref debugger process) filter))


(defmethod csde-dbs-debugger-display-message ((debugger csde-dbs-debugger) message)
  "Displays message in the debugger process buffer."
 (let ((buffer
	 (oref debugger buffer)))
    (if buffer
	(save-excursion
	  (set-buffer buffer)
	  (goto-char (process-mark (get-buffer-process buffer)))
	  (insert-before-markers (concat message "\n"))))))

(defmethod csde-dbs-debugger-start((this csde-dbs-debugger))
  "Starts the debugger."
  (if (not (csde-dbs-debugger-running-p))
      (let* ((debugger-buffer-name 
	      (oref this buffer-name))
	     (debugger-buffer 
	      (let ((old-buf (get-buffer debugger-buffer-name)))
		    (if old-buf (kill-buffer old-buf))
		    (get-buffer-create debugger-buffer-name)))
	     (win32-p (eq system-type 'windows-nt))
	     (source-directory default-directory)
	     (working-directory
	      (if (and 
		   csde-run-working-directory
		   (not (string= csde-run-working-directory "")))
		  csde-run-working-directory
		source-directory))	     
	     (vm (csde-dbs-choose-vm))
	     (csde-csharp-directory
	      (concat
	       (csde-find-csde-data-directory)
	       "csharp/"))
	     (vm-args 
		(let (args)
		  (setq args 
			(append 
			 args
			 (list
			  "-classpath"
			  (csde-convert-cygwin-path
                           (if csde-bug-vm-includes-jpda-p
                               (format
                                (if csde-bug-debug
                                    "%sclasses%s%s"   
                                  "%slib/csde.jar%s%s")
                                csde-csharp-directory
                                csde-classpath-separator
                                (expand-file-name 
                                 "lib/tools.jar" csde-bug-jdk-directory))
                             (format 
                              (if csde-bug-debug
                                  "%sclasses%s%s"   
                                "%ssrc%s%slib/csde.jar%s%s" )
                              csde-csharp-directory 
                              csde-classpath-separator
                              csde-csharp-directory 
                              csde-classpath-separator
                              (expand-file-name 
                               "lib/jpda.jar" csde-bug-jpda-directory)))
                           ;; csde-classpath-separator
			   ))))
		  (if csde-bug-debug
		      (setq args 
			    (append args
			     (list "-Xdebug"
				   "-Xnoagent"   
				   "-Xrunjdwp:transport=dt_socket,address=2112,server=y,suspend=n"))))
		  (setq args (append args (list "csde.debugger.Main")))
		  args))		  
	     (command-string 
	      (concat 
	       vm " " 
	       (csde-run-make-arg-string
		vm-args)
	       "\n\n"))
	     debugger-process)
	(oset this started-p nil)
	(setq csde-dbs-debugger-output nil)


	(save-excursion
	  (set-buffer debugger-buffer)
	  (erase-buffer)
	  ;; Set working directory
	  (if (and
	       (file-exists-p working-directory)
	       (file-directory-p working-directory))
	      (cd working-directory)
	    (error "Invalid working directory: %s" working-directory))
	  (insert (concat "cd " working-directory "\n"))
	  (insert command-string)
	  (csde-run-mode))

	(save-w32-show-window
	 (comint-exec debugger-buffer debugger-buffer-name vm nil vm-args)
	 (setq debugger-process (get-process debugger-buffer-name))
	 (oset this process debugger-process)
	 (oset this buffer debugger-buffer)
	 (oset this comint-filter (process-filter debugger-process))
	 (csde-dbs-debugger-register-process-filter this 'csde-dbs-asynch-output-listener)
	 )

	(cd source-directory)

	(bury-buffer debugger-buffer)
	
	;; Wait for response from debugger
	(if (not (accept-process-output debugger-process csde-bug-debugger-command-timeout 0))
	    (progn
	      (message "Error: debugger failed to start.")
	      nil)
	  (oref this started-p)))
    (message "An instance of the debugger is running.")
    (pop-to-buffer (csde-dbs-get-app-buffer-name))
    nil))
    
(defmethod csde-dbs-debugger-quit ((debugger csde-dbs-debugger)) 
  (csde-dbs-do-command -1 "quit")
  (slot-makeunbound debugger :process)
  (slot-makeunbound debugger :buffer)
  (slot-makeunbound debugger :comint-filter))

(defun csde-dbs-debugger-running-p ()
  "*Returns t if the debugger is running."
  (and (slot-boundp csde-dbs-the-debugger 'buffer)
       (oref csde-dbs-the-debugger started-p)
       (comint-check-proc (oref csde-dbs-the-debugger buffer))))

(defvar csde-dbs-the-debugger (csde-dbs-debugger "CSDEbug")
  "The debugger.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                            ;;
;; Debugger Command Class                                                     ;;
;;                                                                            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defclass csde-dbs-cmd ()
  ((process    :initarg :process
	       :type csde-dbs-proc
	       :documentation
	       "Process that this command targets.")
   (id         :initarg :id
	       :type integer
	       "Command id.")
   (name       :initarg :name
               :type string
	       :documentation
	       "Name of command.")
   (result     :initarg :result
	       "Result of executing command.")
   (data       :initarg :data
	       "Data returned by command.")
   (msg        :initarg :msg
	       :type string
	       "Message to display to user in debug buffer.")
   )
  "Super class of debugger commands.")
 

(defmethod initialize-instance ((this csde-dbs-cmd) &rest fields)
  "Constructor for debugger commands. Generates a unique id for this command."
  (call-next-method)
  (setq csde-dbs-cmd-counter (+ csde-dbs-cmd-counter 1))
  (oset this id csde-dbs-cmd-counter))

(defmethod csde-dbs-cmd-make-command-line ((this csde-dbs-cmd))
  "Creates the command line for this command by concatentating
the process id, command id, and command name. If there is no
process, specifies the process id as -1. Derived classes can
extend this method to specify command arguments."
  (let* ((process (oref this process))
	 (process-id (if process (oref process id) -1))
	 (command-id (oref this id))
	 (command-name (oref this name)))
    (format "%s %s %s" process-id command-id command-name)))
    
(defvar csde-dbs-debugger-output nil
  "Contains output from the debugger.")

(defvar csde-dbs-command-reply nil
  "Contains reply to a debugger command.")

(defvar csde-dbs-pending-command 0
"Number of the current command.")

(defun csde-dbs-eval-debugger-output (lisp-form)
  (condition-case error-desc
      (eval (read lisp-form))
    (error 
     (let* ((process (csde-dbs-get-target-process)))
       (if process
	   (csde-dbs-proc-display-debug-message 
	    process 
	    (concat
	     "Error: evaluating debugger output caused a Lisp error.\n"
	     "  See *messages* buffer for details.")))
       (message "Error: evaluating output from the debugger caused a Lisp error.")
       (message "Debugger output: %s." lisp-form)
       (message "Lisp error: %s" error-desc)))))

(defun csde-dbs-extract-exception (debugger-output)
  (let ((lisp-form "")
	(remainder "")
	(output-length (length debugger-output))
	(re "\\(.*Exception:.*[\n]\\)+\\(.*at[^\n]*[\n]\\)+"))
    (if (string-match re debugger-output)
	(let ((start (match-beginning 0))
	      (end (match-end 0)))
	  (setq lisp-form (format "(csde-dbo-unknown-exception \"%s\")" 
				  (substring debugger-output 0 end)))
	  (if (< end output-length)
	      (setq remainder (substring debugger-output end output-length))))
      (setq remainder debugger-output))
    (cons lisp-form remainder)))

(defun csde-dbs-extract-lisp-form (debugger-output)
"Extract first complete Lisp form from debugger output.
Returns (FORM . REMAINDER) where FORM is the Lisp form
or the null string and REMAINDER is the remainder of the
debugger output following the Lisp form."
  (let ((lisp-form "")
	(remainder "")
	(level 0)
	in-string-p
	in-escape-p
	(curr-pos 1)
	(output-length (length debugger-output))
	command-end
	lisp-form-end)
    (setq 
     lisp-form-end
     (catch 'found-lisp-form
       ;; skip over any inital white space.
       (string-match "^[\n\t ]*(" debugger-output)
       (setq curr-pos (match-end 0))

       (while (< curr-pos output-length)

	 (cond 

	  ;; Current character = left slash (escape)
	  ((equal (aref debugger-output curr-pos) ?\\)
	   (if in-string-p
	       (setq in-escape-p (not in-escape-p))))
	  
	  ;; Current character = quotation mark
	  ((equal (aref debugger-output curr-pos) ?\")
	   (if in-string-p
	       (if in-escape-p
		   (setq in-escape-p nil)
		 (setq in-string-p nil))
	     (setq in-string-p t)))

	  ;; Current character = right paren
	  ((and
	    (not in-string-p)
	    (equal (aref debugger-output curr-pos) ?\)))
	     (if (= level 0)
		 (throw 'found-lisp-form curr-pos)
	       (setq level (1- level))
	       (if (< level 0)
		   (error "Error parsing debugger output."))))

	  ;; Current character = left paren
	  ((and
	    (not in-string-p)
	    (equal (aref debugger-output curr-pos) ?\()
	       (setq level (1+ level))))
	  (t
	   (if in-escape-p
	       (setq in-escape-p nil))))

	 (setq curr-pos (1+ curr-pos)))

       -1))
    (if (> lisp-form-end 1)
	(progn
	  (setq lisp-form (substring debugger-output 0 (1+ lisp-form-end)))
	  (when (< lisp-form-end (1- output-length))
	    (setq remainder (substring debugger-output (1+ lisp-form-end) output-length))
	    (if (string-match "(" remainder)
		(setq remainder (substring remainder (string-match "(" remainder)))
	      (setq remainder ""))))
      (setq remainder debugger-output))
    (cons lisp-form remainder)))

(defun csde-dbs-reply-p (form)
  "Returns t if FORM is a command response form."
  (or
   (string-match "csde-dbo-command-result" form)
   (string-match "csde-dbo-command-error" form)))

(defvar csde-dbs-pending-event-queue nil
"Queue of events that occurred before receiving a reply to the last command.")

(defun csde-dbs-command-reply-listener (process output)
  "Listens for a reply to the command specified by
`csde-dbs-pending-command'."
  ;; (message "entering command reply listener")
  (let* ((combined-output (concat csde-dbs-debugger-output output))
	 (parsed-output 
	  (if (string-match "^[\n\t ]*(" combined-output)
	      (csde-dbs-extract-lisp-form combined-output)
	    (csde-dbs-extract-exception combined-output)))			 
	 (form (car parsed-output))
	 (remainder (cdr parsed-output))
	 reply-received)

    ;; (message "form: %s" form)
    ;; (message "remainder: %s" remainder)

    ;; Insert debugger output into the *CSDEbug* buffer.
    (funcall (oref csde-dbs-the-debugger  comint-filter)
	 process output)

    ;; Process the Lisp forms extracted from the debugger output.
    (while (not (string= form ""))

      (if (csde-dbs-reply-p form)
	  
	  ;; The current form is a reply to a debugger command.
	  (progn 
	    (setq csde-dbs-command-reply form)
	    (setq reply-received t)
	    (csde-dbs-eval-debugger-output form))

	;; The form is an event. Postpone processing the event
        ;; until we receive a reply to the last command.
	;; (message "   appending %s to pending event queue" form)
	(setq csde-dbs-pending-event-queue
	      (append csde-dbs-pending-event-queue (list form))))

      ;; Extract the next Lisp form from the debugger output.
      ;; The car of parsed-output is the next form. The cdr
      ;; is the remaining unprocessed debugger output.
      (setq parsed-output
	    (csde-dbs-extract-lisp-form remainder))

      (setq form (car parsed-output))
      (setq remainder (cdr parsed-output))) ;; End of form processing loop.

    (setq csde-dbs-debugger-output remainder)

    (if (not reply-received)
	(when (not (accept-process-output process csde-bug-debugger-command-timeout 0))
	    (message "No response to command %d. (process = %s; timeout = %s sec.)"
		     csde-dbs-pending-command
		     (if (csde-dbs-get-target-process)
			 (oref (csde-dbs-get-target-process) id)
		       "?")
		     csde-bug-debugger-command-timeout)
		    (setq csde-dbs-command-reply nil)))))
	
(defun csde-dbs-asynch-output-listener (process output)
  "Listens at the csdebug socket for asynchronous debugger output."
  (let* ((combined-output (concat csde-dbs-debugger-output output))
	 (parsed-output 
	  (if (string-match "^[\n\t ]*(" combined-output)
	      (csde-dbs-extract-lisp-form combined-output)
	    (csde-dbs-extract-exception combined-output)))		
	 (lisp-form (car parsed-output))
	 (remainder (cdr parsed-output))
	 events)

    ;; (message "asynch form: %s" lisp-form)
    ;; (message "asynch remainder: %s" remainder)

    (funcall (oref  csde-dbs-the-debugger comint-filter)
	     process output)
    ;; Extract events from debugger output.
    (while (not (string= lisp-form ""))
      ;; (message "   evaluating %s" lisp-form)
      ;; (csde-dbs-eval-debugger-output lisp-form)
      (setq events (append events (list lisp-form)))
      (setq parsed-output
	    (csde-dbs-extract-lisp-form remainder))
      (setq lisp-form (car parsed-output))
      (setq remainder (cdr parsed-output)))
    (setq csde-dbs-debugger-output remainder)
    (if events
	(mapc (lambda (event) (csde-dbs-eval-debugger-output event))
	      events))))

(defun csde-dbs-do-command (vm command)
  "Posts the specified command to the debugger and returns its response."
  (let* ((debugger-process 
	  (oref csde-dbs-the-debugger process))
	 (previous-listener (process-filter debugger-process))
	 cmd)	
    (setq csde-dbs-debugger-output "")
    (setq csde-dbs-command-reply "")
    (setq csde-dbs-pending-events nil)
    (setq csde-dbs-cmd-counter (+ csde-dbs-cmd-counter 1))
    (setq csde-dbs-pending-command (number-to-string csde-dbs-cmd-counter))
    (setq cmd (concat (number-to-string vm) " " csde-dbs-pending-command " " command "\n\n"))
    (csde-dbs-debugger-display-message csde-dbs-the-debugger (concat "CSDE> " cmd))
    (set-process-filter debugger-process 'csde-dbs-command-reply-listener)
    (process-send-string debugger-process cmd)
    (when (not (accept-process-output debugger-process csde-bug-debugger-command-timeout 0))
		(message "Error: debugger didn't respond to command:\n%s" cmd)
		(setq csde-dbs-command-reply nil))
    (set-process-filter debugger-process previous-listener)
    (if csde-dbs-command-reply
	(let ((result (csde-dbs-eval-debugger-output csde-dbs-command-reply)))
	  ;; evaluate any events that occurred between issuance and 
	  ;; acknowledgement of this command
	  (mapc (lambda (event) (csde-dbs-eval-debugger-output event))
		csde-dbs-pending-event-queue)
	  (setq csde-dbs-pending-event-queue nil)
	  result))))



(defvar csde-dbs-debugger-socket-number nil
"Number of socket used to communicate with debugger.")


(defun csde-dbs-listen-for-debugger-socket (debugger-process output)
  (set-process-filter debugger-process csde-dbs-app-buffer-filter)
  (eval (read output)))

(defmethod csde-dbs-cmd-success-action ((this csde-dbs-cmd)))


(defmethod csde-dbs-cmd-failure-action ((this csde-dbs-cmd)))


(defmethod csde-dbs-cmd-display-response ((this csde-dbs-cmd))
  (if (slot-boundp this 'msg)
      (csde-dbs-proc-display-debug-message 
       (oref this process)
       (oref this msg))))

(defmethod csde-dbs-cmd-execute-pending-events ((this csde-dbs-cmd))
  "Evaluate any events that occurred between issuance and 
   acknowledgement of this command"
  (let ((events csde-dbs-pending-event-queue))
    ;; Empty queue to avoid recursion if commands are executed
    ;; as a result of processing these events.
    (setq csde-dbs-pending-event-queue nil)
    (mapc (lambda (event) (csde-dbs-eval-debugger-output event))
		events)))


(defmethod csde-dbs-cmd-exec ((this csde-dbs-cmd))
  "Posts the specified command to the debugger and returns its response."
  (let* ((debugger-process 
	  (oref csde-dbs-the-debugger process))
	 (previous-listener (process-filter debugger-process))
	 (target-process (oref this process))
	 (command-line (format "%s\n" (csde-dbs-cmd-make-command-line this))))
	
    (setq csde-dbs-debugger-output "")
    (setq csde-dbs-command-reply "")
    (setq csde-dbs-pending-events nil)
    (setq csde-dbs-pending-command (oref this id))

    (if target-process (oset target-process last-cmd this))
    (csde-dbs-debugger-display-message csde-dbs-the-debugger (concat "CSDE> " command-line))
    (set-process-filter debugger-process 'csde-dbs-command-reply-listener)
    (process-send-string debugger-process command-line)
    (process-send-string debugger-process "\n")

    (when (not (accept-process-output debugger-process csde-bug-debugger-command-timeout 0))
		(message "Error: debugger didn't respond to command:\n%s" command-line)
		(setq csde-dbs-command-reply nil))

    (process-send-string debugger-process "\n")
 
    (set-process-filter debugger-process previous-listener)

    (if csde-dbs-command-reply
	(let ((result (csde-dbs-eval-debugger-output csde-dbs-command-reply)))

	  (oset this :result result)

	  (oset this :data (car (csde-dbo-command-result-data (oref this result))))

	  (if (csde-dbo-command-succeeded-p result)
	      (csde-dbs-cmd-success-action this)
	    (csde-dbs-cmd-failure-action this))

	  (csde-dbs-cmd-display-response this)

	  (csde-dbs-cmd-execute-pending-events this)
	  (oref this :result)))))

(defvar csde-dbs-cmd-counter 0
 "Count of the number of commands issued in this session.")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                            ;;
;; Launch Process Command                                                     ;;
;;                                                                            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defclass csde-dbs-launch-process (csde-dbs-cmd)
  ((main-class  :initarg :main-class
		:type string
		:documentation
		"Class containing this process's main method.")
   (jre-home    :initarg :jre-home
		:type string
		:documentation
		"Home directory of JRE used to launch this process.")
   (vmexec     :initarg :vmexec
	        :type string
		:initform "csharp"
	        :documentation
	        "Name of vm executable used to run process.")
   (vm-args     :initarg :args
	        :type string
		:initform ""
	        :documentation
		"Command line arguments to be passed to vm's main method.")
   (app-args    :initarg :app-args
		:type string
		:initform ""
		:documentation
		"Command line arguments to be passed to app's main method."))
  "Class of launch-process commands.")

(defun csde-dbs-choose-vm ()
  (if (and
       (eq system-type 'windows-nt)
       (string= csde-run-csharp-vm "csharp"))
      csde-run-csharp-vm-w
    csde-run-csharp-vm))


(defun csde-dbs-get-app-buffer-name ()
  (concat "*" (csde-run-get-main-class) "*"))
    
(defmethod initialize-instance ((this csde-dbs-launch-process) &rest fields)
  "Constructor for debugger commands. Generates a unique id for this command."

  ;; Call parent initializer.
  (call-next-method)

  ;; Set command name.
  (oset this name "launch")

  ;; You must specify a process to launch when constructing a launch command."
  (assert (slot-boundp this :process))

  ;; Set main class.
  (if (not (slot-boundp this :main-class))
    (oset this :main-class
	  (oref (oref this :process) :main-class)))

  ;; Set vm.
  ;; (oset this vm (csde-dbs-choose-vm))

  ;; Set vm args
  (oset this vm-args 
 	(concat (mapconcat (lambda (s) s) (csde-db-get-vm-args) " ")
		" "
 		(mapconcat (lambda (s) s) (csde-db-get-vm-args-from-user) " ")))


  ;; Set application arguments.
  (oset this app-args
 	(concat 
	 (if csde-db-option-application-args
	     (mapconcat (lambda (s) s) csde-db-option-application-args " ") 
	   "")
	 " "
	 (mapconcat (lambda (s) s) (csde-db-get-app-args-from-user) " "))))
  


(defmethod csde-dbs-cmd-make-command-line ((this csde-dbs-launch-process))
  "Creates the command line for the launch command."
  (let ((cmd (format "-1 %s %s %s -vmexec %s"
		     (oref this id)                    ;; cid
		     (oref this name)                  ;; launch
		     (oref (oref this process) id)     ;; pid
		     (oref this vmexec))))  

    (if (slot-boundp this 'jre-home)
	(setq cmd (concat cmd " -home " (oref this jre-home))))
		     
    (setq cmd 
	  (format "%s %s %s %s" 
		  cmd
		  (oref this vm-args)            ;; vm args
		  (oref this main-class)         ;; main class
		  (oref this app-args)))         ;; command line args

    (oset this msg
	  (format "Launch command line:\n  %s %s %s %s\n" 
		  (oref this vmexec)
		  (oref this vm-args)            ;; vm args
		  (oref this main-class)         ;; main class
		  (oref this app-args)))         ;; command line args	  
    cmd))    

(defmethod csde-dbs-cmd-success-action ((this csde-dbs-launch-process))
  (call-next-method)
  (delete-other-windows)
  (let* ((source-buffer (current-buffer))
	 (cli-socket
	  (car (csde-dbo-command-result-data result)))
	 (cli-buffer-name 
	  (format "%s(%d) CLI" main-class (oref process id))))

    (oset (oref this process) cli-socket cli-socket)

    ;; Connect to socket used by debugger to transport the
    ;; standard I/O of the debuggee process.
    (sleep-for csde-bug-sio-connect-delay)
    (oset 
     (oref this process)
     cli-buf
     (make-comint 
      cli-buffer-name 
      (cons csde-bug-debugger-host-address cli-socket)))
	  
    (oset this msg
	  (format "%s\nEmacs connected to standard IO port %d for process %s." 
		  (oref this msg)
		  cli-socket
		  (oref this main-class)))

    (pop-to-buffer (oref process msg-buf))
    (pop-to-buffer source-buffer)
    (split-window-vertically)
    (pop-to-buffer (oref process locals-buf))
    (pop-to-buffer source-buffer)
    (oset process win-cfg (current-window-configuration))))

(defmethod csde-dbs-cmd-failure-action ((this csde-dbs-launch-process))
  (delete-other-windows)
  (let ((source-buffer (current-buffer)))
    (oset this  msg
	  (format "%s\nError: debugger unable to launch %s.\n  Reason: %s" 
		  (oref this msg)
		  (oref this main-class)
		  (oref this data)))
      (split-window-vertically)
      (pop-to-buffer (oref process msg-buf))
      (pop-to-buffer source-buffer)
      (oset process win-cfg (current-window-configuration))))
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                            ;;
;; Attach Shared Memory                                                       ;;
;;                                                                            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defclass csde-dbs-attach-shmem (csde-dbs-cmd)
  ((process-name  :initarg :process-name
		  :type string
		  :documentation
		  "Name of process to attach."))
  "Attach debugger to a running process via shared memory.")

(defmethod initialize-instance ((this csde-dbs-attach-shmem) &rest fields)
  "Constructor for attach_shmem command."

  ;; Call parent initializer.
  (call-next-method)

  (assert (oref this process))

  (assert (slot-boundp this 'process-name))

  ;; Set command name.
  (oset this name "attach_shmem"))

(defmethod csde-dbs-cmd-make-command-line ((this csde-dbs-attach-shmem))
  "Creates the command line for the attach_shmem command."
  (format "-1 %s %s %s %s" 
	  (oref this id)
	  (oref this name)                 ;; command name
	  (oref (oref this process) id)    ;; process id
	  (oref this process-name)))       ;; process name   

(defmethod csde-dbs-cmd-success-action ((this csde-dbs-attach-shmem))
  (call-next-method)
  (delete-other-windows)
  (let ((source-buffer (current-buffer)))
    (oset (oref this process) :attachedp t)
    (oset (oref this process) :startupp t)
    (oset this msg  (format "Attached to process %s." 
			    (oref this process-name)))
    (split-window-vertically)
    (pop-to-buffer (oref process msg-buf))
    (pop-to-buffer source-buffer)
    (oset process win-cfg (current-window-configuration))))

(defmethod csde-dbs-cmd-failure-action ((this csde-dbs-attach-shmem))
  (delete-other-windows)
  (let ((source-buffer (current-buffer)))
    (oset this  msg
     (format "Error: cannot attach process %s.\n Reason: %s." 
		    (oref this process-name)
		    (oref this data)))
      (split-window-vertically)
      (pop-to-buffer (oref process msg-buf))
      (pop-to-buffer source-buffer)
      (oset process win-cfg (current-window-configuration))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                            ;;
;; Attach Process via Socket                                                  ;;
;;                                                                            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defclass csde-dbs-attach-socket (csde-dbs-cmd)
  ((port  :initarg :port
	  :type string
	  :documentation
	  "Name of port on which existing process is listening.")
   (host  :initarg :host
	  :type string
	  :documentation
	  "Name of host on which existing process is listening."))
  "Attach debugger to a running process via a socket connection.")

(defmethod initialize-instance ((this csde-dbs-attach-socket) &rest fields)
  "Constructor for attach_socket command."

  ;; Call parent initializer.
  (call-next-method)

  (assert (oref this process))

  (assert (slot-boundp this 'port))

  ;; Set command name.
  (oset this name "attach_socket"))

(defmethod csde-dbs-cmd-make-command-line ((this csde-dbs-attach-socket))
  "Creates the command line for the attach_socket command."
  (let ((cmd
	 (format "-1 %s %s %s -port %s" 
	  (oref this id)
	  (oref this name)                 ;; command name
	  (oref (oref this process) id)    ;; process id
	  (oref this port))))              ;; process name
    (if (slot-boundp this 'host)
	(setq cmd (format "%s -host %s" cmd (oref this host))))
    cmd))

(defmethod csde-dbs-cmd-success-action ((this csde-dbs-attach-socket))
  (call-next-method)
  (delete-other-windows)
  (let ((source-buffer (current-buffer)))
    (oset (oref this process) attachedp t)
    (oset (oref this process) startupp t)
    (oset this msg  (format "Attached to process on port %s of %s." 
			    (oref this port)
			    (if (slot-boundp this 'host)
				(oref this host)
			      "local host")))
    (split-window-vertically)
    (pop-to-buffer (oref process msg-buf))
    (pop-to-buffer source-buffer)
    (oset process win-cfg (current-window-configuration))))

(defmethod csde-dbs-cmd-failure-action ((this csde-dbs-attach-socket))
  (delete-other-windows)
  (let ((source-buffer (current-buffer)))
    (oset this  msg
     (format "Error: cannot attach to process on port %s of %s.\n Reason: %s." 
	     (oref this port)
	     (if (slot-boundp this 'host)
		 (oref this host)
	       "local host")
	     (oref this data)))
      (split-window-vertically)
      (pop-to-buffer (oref process msg-buf))
      (pop-to-buffer source-buffer)
      (oset process win-cfg (current-window-configuration))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                            ;;
;; Listen for Process                                                         ;;
;;                                                                            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defclass csde-dbs-listen-for-process (csde-dbs-cmd)
  ((address   :initarg :address
	      :type string
	      :documentation
	      "Address at which to listen for a debuggee process.")
   (transport :initarg :transport
	      :type string
	      :initform "shmem"
	      :documentation
	      "Transport mechanism used to interact with debuggee process."))
  "Listen for a process requesting debugger services.")

(defmethod initialize-instance ((this csde-dbs-listen-for-process) &rest fields)
  "Constructor for listen command."

  ;; Call parent initializer.
  (call-next-method)

  (assert (oref this process))

  (assert (slot-boundp this 'address))

  (assert (not 
	   (and
	    (not (eq system-type 'windows-nt))
	    (string= (oref this transport) "shmem"))))

  ;; Set command name.
  (oset this name 
	(concat "listen_"
		(oref this transport))))

(defmethod csde-dbs-cmd-make-command-line ((this csde-dbs-listen-for-process))
  "Creates the command line for the listen command."
  (format "-1 %s %s %s %s" 
	  (oref this id)
	  (oref this name)                 ;; command name
	  (oref (oref this process) id)    ;; process id
	  (oref this address)))            ;; process address

(defmethod csde-dbs-cmd-success-action ((this csde-dbs-listen-for-process))
  (call-next-method)
  (delete-other-windows)
  (let ((source-buffer (current-buffer)))
    (oset this msg  (format "Listening for process at %s address: %s." 
			    (if (string= (oref this transport) "shmem")
				"shared memory" "socket")
			    (oref this address)))
    (oset process startupp t)
    (split-window-vertically)
    (pop-to-buffer (oref process locals-buf))
    (split-window-vertically)
    (pop-to-buffer (oref process msg-buf))
    (pop-to-buffer source-buffer)
    (oset process win-cfg (current-window-configuration))))

(defmethod csde-dbs-cmd-failure-action ((this csde-dbs-listen-for-process))
  (delete-other-windows)
  (let ((source-buffer (current-buffer)))
    (oset this  msg
     (format "Error: cannot listen for process at %s address: %s.\n Reason: %s." 
	     (if (string= (oref this transport) "shmem")
		 "shared memory" "socket")
	     (oref this address)
	     (oref this data)))
      (split-window-vertically)
      (pop-to-buffer (oref process msg-buf))
      (pop-to-buffer source-buffer)
      (oset process win-cfg (current-window-configuration))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                            ;;
;; Run Process Command Class                                                  ;;
;;                                                                            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defclass csde-dbs-run-process (csde-dbs-cmd) ()
  "Run process command.")

(defmethod initialize-instance ((this csde-dbs-run-process) &rest fields)
  "Constructor for run process command."

  ;; Call parent initializer.
  (call-next-method)

  (assert (oref this process))

  ;; Set command name.
  (oset this name "run"))


(defmethod csde-dbs-cmd-success-action ((this csde-dbs-run-process))
  (call-next-method)
  (oset this msg (format "Running %s." 
			 (oref (oref this process)  main-class))))

(defmethod csde-dbs-cmd-failure-action ((this csde-dbs-run-process))
  (oset this msg 
	(format "Error: unable to run %s..\n  Reason: %s."
		(oref (oref this process) main-class)
		(oref this result))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                            ;;
;; Finish Process Command Class                                               ;;
;;                                                                            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defclass csde-dbs-finish-process (csde-dbs-cmd) ()
  "Finish process command.")

(defmethod initialize-instance ((this csde-dbs-finish-process) &rest fields)
  "Constructor for finish process command."

  ;; Call parent initializer.
  (call-next-method)

  (assert (slot-boundp this :process))

  ;; Set command name.
  (oset this name "finish"))

(defmethod csde-dbs-cmd-exec ((this csde-dbs-finish-process))
  "Executes the finish process command."
  (let* ((process (oref this :process))
	 (main-class (oref process :main-class))
	 (result (call-next-method)))
    (if (csde-dbo-command-succeeded-p result)
	(progn
	  (csde-dbs-proc-display-debug-message process
	   (concat "Terminating " main-class)))
      (csde-dbs-proc-display-debug-message process
       (concat "Error: debugger unable to terminate: "
	       main-class
	       ".\n  Reason: " 
	       (car (csde-dbo-command-result-data result))))
      nil)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                            ;;
;; Set Breakpoint Command Class                                               ;;
;;                                                                            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defclass csde-dbs-set-breakpoint (csde-dbs-cmd) 
  ((breakpoint    :initarg :breakpoint
	          ;; :type csde-bug-breakpoint
	          :documentation
	          "Breakpoint specification."))
  "Set breakpoint command.")

(defmethod initialize-instance ((this csde-dbs-set-breakpoint) &rest fields)
  "Constructor for set breakpoint command."

  ;; Call parent initializer.
  (call-next-method)

  (assert (oref this process))
  (assert (oref this breakpoint))

  ;; Set command name.
  (oset this name "break absolute"))

(defmethod csde-dbs-cmd-make-command-line ((this csde-dbs-set-breakpoint))
  "Creates the command line for the set breakpoint command."
  (let* (
	 (bp-spec (oref this breakpoint))
	 (file (oref bp-spec file))
	 (line (oref bp-spec line)))
    (format "%s %s %s" 
	    (call-next-method)
	    file     ;; File
	    line)))  ;; Line number    

(defmethod csde-dbs-cmd-success-action ((this csde-dbs-set-breakpoint))
  (call-next-method)
  (let*  ((process (oref this process))
	  (bp-procid (oref this data))
	  (bp-spec (oref this breakpoint))
	  (file (oref bp-spec file))
	  (line (oref bp-spec line))
	  (bpspec (csde-dbs-proc-bpspec "spec" :id bp-procid :breakpoint bp-spec))
	  (bpspecs (if (slot-boundp process :bpspecs) (oref process :bpspecs))))
    (if bpspecs
	(oset process bpspecs (csde-dbs-proc-bpspecs-add bpspecs bpspec))
      (oset process bpspecs (csde-dbs-proc-bpspecs-add nil bpspec)))
    (oset this msg (format "Setting breakpoint at line %s in %s." line file))))


(defmethod csde-dbs-cmd-failure-action ((this csde-dbs-set-breakpoint))
  (let* ((bp-spec (oref this breakpoint))
	 (file (oref bp-spec file))
	 (line (oref bp-spec line)))
    (oset this msg  (format "Error: cannot set breakpoint at line %s in file %s.\n  Reason:" 
			    file line (oref this data)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                            ;;
;; Clear Breakpoint Command Class                                             ;;
;;                                                                            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defclass csde-dbs-clear-breakpoint (csde-dbs-cmd) 
  ((breakpoint    :initarg :breakpoint
	          ;; :type csde-bug-breakpoint
	          :documentation
	          "Breakpoint specification."))
  "Set breakpoint command.")

(defmethod initialize-instance ((this csde-dbs-clear-breakpoint) &rest fields)
  "Constructor for clear breakpoint command."

  ;; Call parent initializer.
  (call-next-method)

  (assert (oref this process))
  (assert (oref this breakpoint))

  ;; Set command name.
  (oset this name "clear"))

(defmethod csde-dbs-cmd-make-command-line ((this csde-dbs-clear-breakpoint))
  "Creates the command line for the clear breakpoint command."
  (let* ((process (oref this process))
	 (breakpoint (oref this breakpoint))
	 (bpspec (csde-dbs-proc-get-bpspec process breakpoint))
	 (bp-procid (oref bpspec id)))
    (format "%s %s"              ;; PID CID clear BPID
	    (call-next-method)
	    bp-procid)))         ;; Id assigned by debugger to this breakpoint
 

(defmethod csde-dbs-cmd-exec ((this csde-dbs-clear-breakpoint))
  "Execute clear breakpoint command."
  (let* ((process (oref this process))
	 (breakpoint (oref this breakpoint))	
	 (file (oref breakpoint file))
	 (line (oref breakpoint line))
	 (proc-id (oref process id))
	 (bpspec (csde-dbs-proc-get-bpspec process breakpoint)))
    (if bpspec
	(let ((bp-procid (oref bpspec id))
	      (result (call-next-method)))
	  (if (csde-dbo-command-succeeded-p result)
	      (let ((bpspecs (oref process bpspecs)))
		(oset process bpspecs 
		      (csde-dbs-proc-bpspecs-remove bpspecs bpspec))
		(csde-dbs-proc-display-debug-message  
		 process
		 (format "Cleared breakpoint at line %s in file %s" line file)))
	    (csde-dbs-proc-display-debug-message 
	     process
	     (format "Error: cannot clear breakpoint at line %s in file %s.\n Reason: %s." 
		     line file (car (csde-dbo-command-result-data result))))
	    nil)))))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                            ;;
;; Step Over/Into/Out Command Class                                           ;;
;;                                                                            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass csde-dbs-step (csde-dbs-cmd)
  ((step-type :initarg :step-type
	      :type string
	      :initform "over"
	      :documentation
	      "Type of step operation: over, into, into-all, out"))
  "Step command.")

(defmethod initialize-instance ((this csde-dbs-step) &rest fields)
  "Constructor for step command."

  ;; Call parent initializer.
  (call-next-method)

  (assert (oref this process))


  ;; Set command name.
  (oset this name (concat "step " (oref this step-type))))

(defmethod csde-dbs-cmd-make-command-line ((this csde-dbs-step))
  "Creates the command line for the step command."
  (format "%s %d" (call-next-method) 
	  (oref (oref (oref this process) state-info) thread-id)))  


(defmethod csde-dbs-cmd-failure-action ((this csde-dbs-step))
  (oset this msg
	(format "Error: unable to step %s.\n Reason: %s"
		(oref this step-type) (oref this data))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                            ;;
;; Step Into Command Class                                                    ;;
;;                                                                            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmethod csde-dbs-proc-step-into ((this csde-dbs-proc))
  (let* ((proc-id (oref this id))
	 (thread-id 
	  (oref (oref this state-info) thread-id))	
	 (result (csde-dbs-do-command proc-id  (format "step into %s" thread-id))))
    (when (not (csde-dbo-command-succeeded-p result))
      (csde-dbs-proc-display-debug-message this
       (format "Error: unable to step into... .\n  Reason: %s"
	       (car (csde-dbo-command-result-data result))))
      nil)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                            ;;
;; Step Out Command Class                                                     ;;
;;                                                                            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmethod csde-dbs-proc-step-out ((this csde-dbs-proc))
  (let* ((proc-id (oref this id))
	 (thread-id 
	  (oref (oref this state-info) thread-id))	
	 (result (csde-dbs-do-command proc-id  (format "step out %s" thread-id))))
    (when (not (csde-dbo-command-succeeded-p result))
      (csde-dbs-proc-display-debug-message this
       (format "Error: unable to step into... .\n  Reason: %s"
	       (car (csde-dbo-command-result-data result))))
      nil)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                            ;;
;; Evaluate Command Class                                                     ;;
;;                                                                            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defclass csde-dbs-evaluate (csde-dbs-cmd) 
  ((expression    :initarg :expression
	          ;; :type string
	          :documentation
	          "Expression to be evaluate. Required.")
   (thread-id     :initarg :thread-id
		  ;; :type integer
		  :documentation
		  "Id of thread that scopes this expression. Required."))
  "Evaluate expression command.")

(defmethod initialize-instance ((this csde-dbs-evaluate) &rest fields)
  "Constructor for evaluate command."

  ;; Call parent initializer.
  (call-next-method)

  (assert (oref this process))
  (assert (oref this expression))
  (assert (oref this thread-id))
 
  ;; Set command name.
  (oset this name "evaluate"))

(defmethod csde-dbs-cmd-make-command-line ((this csde-dbs-evaluate))
  "Creates the command line for the clear breakpoint command."
    (format "%s %s 0 \"%s\""         ;; PID CID evaluate THREAD-ID 0 "EXPRESSION"
	    (call-next-method)       ;; PID CID evaluate
	    (oref this thread-id)    ;; thread id
	    (oref this expression))) ;; expression to be evaluated.
 

(defmethod csde-dbs-cmd-exec ((this csde-dbs-evaluate))
  "Execute evaluate expression command. Returns
(TYPE VALUE GCFLAG) where TYPE is the type of the result,
VALUE is the value, and GCFLAG is t if the result has been
garbage collected."
  (let* ((process (oref this process))
	 (result (call-next-method)))
    (if (csde-dbo-command-succeeded-p result)
	(car (csde-dbo-command-result-data result))
      (csde-dbs-proc-display-debug-message 
       process
       (format "Error: cannot evaluate \"%s\".\n Reason: %s." 
	       (oref this expression)
	       (car (csde-dbo-command-result-data result))))
      nil)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                            ;;
;; Get Array                                                                  ;;
;;                                                                            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defclass csde-dbs-get-array (csde-dbs-cmd) 
  ((array    :initarg :array
	     :type csde-dbs-csharp-array
	     :documentation
	     "Object to represent the array. Required.")
   (index    :initarg :index
	     :type integer
	     :documentation
	     "Index of array slice to be returned.")
   (length   :initarg :length
	     :type integer
	     :documentation "Length of slice to be returned."))
  "Get a slice of the array object specified by ARRAY. INDEX and LENGTH are
the index and length of the slice to be returned. If omitted, this command returns
the length of the first slice of the array. Note that each element of this array
can be another array or some other object.")


(defmethod initialize-instance ((this csde-dbs-get-array) &rest fields)
  "Constructor for get array command."

  ;; Call parent initializer.
  (call-next-method)

  (assert (slot-boundp this :process))
  (assert (slot-boundp this :array))

  (if (slot-boundp this :index)
      (assert (slot-boundp this :length)))

  ;; Set command name.
  (oset this name "get_array"))

(defmethod csde-dbs-cmd-make-command-line ((this csde-dbs-get-array))
  "Creates the command line for the get-object command."
  (let ((cl
	 (format "%s %d" (call-next-method) (oref (oref this array) id)))
	(index (if (slot-boundp this :index) (oref this :index))))
    (if index
	(setq cl 
	      (format "%s %d %d"                ;; PID CID get_array OBJ-ID INDEX LENGTH
		      cl
		      index                     ;; index of slice to be returned.
		      (oref this length))))    ;; length of slice to be returned.
    cl))
 

(defmethod csde-dbs-cmd-exec ((this csde-dbs-get-array))
  "Executes the get-array command. If a slice is specified, 
returns the slice as a list of elements. Otherwise, return
the length of the array."
  (let* ((process (oref this process))
	 (result (call-next-method)))
    (if (csde-dbo-command-succeeded-p result)
	(let* ((array (oref this array))
	       (data   (nth 0 (csde-dbo-command-result-data result)))
	       (type (nth 0 data))
	       (id (nth 1 data))
	       (gc-flag (nth 2 data))
	       (length (nth 3 data))
	       (elements (if (> (length data) 4)
			     (cdr (cdr (cdr (cdr data)))))))
	  (or elements length)
	  (oset array jtype type)
	  (oset array id id)
	  (oset array gc-flag gc-flag)
	  (oset array length length)
	  (oset array elements 
		(mapcar 
		 (lambda (element)
		   (csde-dbs-objectify-value element))
		 elements))
	  array)      
      (csde-dbs-proc-display-debug-message 
       process
       (format "Error: cannot get array %d.\n Reason: %s." 
	       (oref this object-id)
	       (car (csde-dbo-command-result-data result))))
      nil)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                            ;;
;; Abstract Get Object                                                        ;;
;;                                                                            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defclass csde-dbs-abstract-get-object (csde-dbs-cmd) 
  ((object-id     :initarg :object-id
	          :type integer
	          :documentation
	          "Id of object. Required."))
  "Parent class of get object commands.")


(defmethod initialize-instance ((this csde-dbs-abstract-get-object) &rest fields)
  "Constructor for get-object command."

  ;; Call parent initializer.
  (call-next-method)

  (assert (slot-boundp this :process))
  (assert (slot-boundp this :object-id)))

(defmethod csde-dbs-cmd-make-command-line ((this csde-dbs-abstract-get-object))
  "Creates the command line for the get-object command."

  (format "%s %d" (call-next-method) (oref this object-id)))
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                            ;;
;; Get Object                                                                 ;;
;;                                                                            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defclass csde-dbs-get-object (csde-dbs-abstract-get-object) ()
  "Class of generic get-object commands. These commands return the fields of
the object.")


(defmethod initialize-instance ((this csde-dbs-get-object) &rest fields)
  "Constructor for get-object command."

  ;; Call parent initializer.
  (call-next-method)

  ;; Set command name.
  (oset this name "get_object"))

(defun csde-dbs-objectify-value (value-form)
  (let ((lvf        (length value-form))
	(value-type (car value-form)))
    (cond
     ((and (= lvf 1) (string= value-type "null"))
      (csde-dbs-csharp-null "null"))
     ((= lvf 2)
      (csde-dbs-csharp-primitive
       "primitive" 
       :jtype  value-type
       :value  (nth 1 value-form)))
     ((= lvf 3)
      (if (string-match "\\[\\]" value-type)
	  (csde-dbs-csharp-array
	   (format "array %d" (nth 1 value-form))
	   :jtype value-type
	   :id (nth 1 value-form)
	   :gc-flag (nth 2 value-form))
	(csde-dbs-csharp-udci
	 (format "obj %d" (nth 1 value-form))
	 :jtype    value-type
	 :id       (nth 1 value-form)
	 :gc-flag  (nth 2 value-form)))))))

(defun csde-dbs-objectify-variable (variable-form)
  (let* ((var-name   (car (car variable-form)))
	 (var-type   (cdr (car variable-form)))
	 (value-form (cdr variable-form))
	 (value      (csde-dbs-objectify-value 
		      value-form)))				
    (csde-dbs-csharp-variable
     (format "variable %s" var-name)
     :name var-name
     :jtype (mapconcat (lambda (x) x) (nreverse var-type) " ")
     :value value)))
 
(defmethod csde-dbs-cmd-exec ((this csde-dbs-get-object))
  "Executes the get-object command. Returns a Lisp object of type
`csde-dbs-csharp-class-instance' that represents the Csharp object."
  (let* ((process (oref this process))
	 (result (call-next-method)))
    (if (csde-dbo-command-succeeded-p result)
	(let* ((obj     (car (csde-dbo-command-result-data result)))
	       (type    (nth 0 obj))
	       (id      (nth 1 obj))
	       (gc-flag (nth 2 obj))
	       (fields  (if (> (length obj) 3)
			    (nth 3 obj)))
	       (object  (csde-dbs-csharp-udci
			 (format "obj %d" id)
			 :jtype type
			 :id id
			 :gc-flag gc-flag)))
	  (if fields
	      (mapc
	       (lambda (variable-form)
		 (let ((field
			(csde-dbs-objectify-variable variable-form)))
		   (csde-dbs-csharp-udci-add-field object field)))
	       fields))
	  object)	    
      (csde-dbs-proc-display-debug-message 
       process
       (format "Error: cannot get object %d.\n Reason: %s." 
	       (oref this object-id)
	       (car (csde-dbo-command-result-data result))))
      nil)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                            ;;
;; Get String                                                                 ;;
;;                                                                            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defclass csde-dbs-get-string (csde-dbs-abstract-get-object) ()
  "Get the value of a string object.")


(defmethod initialize-instance ((this csde-dbs-get-string) &rest fields)
  "Constructor for get-string command."

  ;; Call parent initializer.
  (call-next-method)

  ;; Set command name.
  (oset this name "get_string"))

(defmethod csde-dbs-cmd-exec ((this csde-dbs-get-string))
  "Executes the get_string command. Returns the string."
  (let* ((process (oref this process))
	 (result (call-next-method)))
    (if (csde-dbo-command-succeeded-p result)
	(nth 3 (car (csde-dbo-command-result-data result)))	    
      (csde-dbs-proc-display-debug-message 
       process
       (format "Error: cannot get string %d.\n Reason: %s." 
	       (oref this object-id)
	       (car (csde-dbo-command-result-data result))))
      nil)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                            ;;
;; Get Locals                                                                 ;;
;;                                                                            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defclass csde-dbs-get-locals (csde-dbs-cmd) 
  ((thread-id         :initarg :thread-id
	              :type integer
	              :documentation
	              "ID of thread whose local variables are being queried.")
   (stack-frame-index :initarg :stack-frame-index
		      :type integer
		      :initform 0
		      :documentation
		      "Index of stack frame containing requested local variables."))
  "Get variables local to a specified thread and stack frame.")


(defmethod initialize-instance ((this csde-dbs-get-locals) &rest fields)
  "Constructor for get-string command."

  ;; Call parent initializer.
  (call-next-method)

  (assert (slot-boundp this 'thread-id))

  ;; Set command name.
  (oset this name "get_locals"))


(defmethod csde-dbs-cmd-make-command-line ((this csde-dbs-get-locals))
  "Creates the command line for the get-locals command."
  (format "%s %d %d" 
	  (call-next-method) 
	  (oref this thread-id)
	  (oref this stack-frame-index)))
 

(defmethod csde-dbs-cmd-exec ((this csde-dbs-get-locals))
  "Executes the get-locals command. Returns a list of Lisp objects of type
`csde-dbs-csharp-variable' that represents the local variables."
  (let* ((process (oref this process))
	 (result (call-next-method)))
    (if (csde-dbo-command-succeeded-p result)
	(let* ((variable-forms (car (csde-dbo-command-result-data result)))
	       (variables      (if variable-forms
				   (mapcar
				    (lambda (variable-form)
			                (csde-dbs-objectify-variable variable-form))
				    variable-forms))))
	  variables)	    
      (csde-dbs-proc-display-debug-message 
       process
       (format "Error: cannot get local variables.\n Reason: %s." 
	       (car (csde-dbo-command-result-data result))))
      nil)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                            ;;
;; Get This                                                                   ;;
;;                                                                            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defclass csde-dbs-get-this (csde-dbs-cmd)
  ((thread-id         :initarg :thread-id
	              :type integer
	              :documentation
	              "ID of thread of stack frame whose this object is required.")
   (stack-frame-index :initarg :stack-frame-index
		      :type integer
		      :initform 0
		      :documentation
		      "Index of stack frame whose this object is required."))
  "Get this object of a specified stack frame.")


(defmethod initialize-instance ((this csde-dbs-get-this) &rest fields)
  "Constructor for get_this command."

  ;; Call parent initializer.
  (call-next-method)

  (assert (slot-boundp this 'process))
  (assert (slot-boundp this 'thread-id))
  (assert (slot-boundp this 'stack-frame-index))

  ;; Set command name.
  (oset this name "get_this"))

(defmethod csde-dbs-cmd-make-command-line ((this csde-dbs-get-this))
  "Creates the command line for the get_this command."
  (format "%s %d %d" 
	  (call-next-method) 
	  (oref this thread-id)
	  (oref this stack-frame-index)))
 
(defmethod csde-dbs-cmd-success-action ((this csde-dbs-get-this))
  (call-next-method)
  (let ((this-obj (oref this :data)))
    (oset 
     this 
     :result 
     (if (string= (nth 0 this-obj) "null")
	 (csde-dbs-csharp-null "null")
       (csde-dbs-csharp-udci
	  "this object"
	  :jtype (nth 0 this-obj)
	  :id (nth 1 this-obj))))))

(defmethod csde-dbs-cmd-failure-action ((this csde-dbs-get-this))
 (oset 
  this 
  msg 
  (format 
   "Error: unable to get this object for stack frame %s on thread %d.\n Reason: %s." 
   (oref this stack-frame-index)
   (oref this thread-id)
   (oref this result))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                            ;;
;; Get Loaded Classes Command Class                                           ;;
;;                                                                            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defclass csde-dbs-get-loaded-classes (csde-dbs-cmd) ()
  "Gets the classes loaded by a specified process.")

(defmethod initialize-instance ((this csde-dbs-get-loaded-classes) &rest fields)
  "Constructor for get_loaded_classes command."

  ;; Call parent initializer.
  (call-next-method)

  (assert (oref this process))

  ;; Set command name.
  (oset this name "get_loaded_classes"))

(defmethod csde-dbs-cmd-exec ((this csde-dbs-get-loaded-classes))
  "Executes the get_loaded_classes command."
  (let* ((process (oref this process))
	 (result (call-next-method)))
    (if (csde-dbo-command-succeeded-p result)
	(let ((classes (car (csde-dbo-command-result-data result))))
	  (csde-dbs-proc-display-debug-message 
	   process
	   (format "Loaded classes:\n  %s." 
		   (mapconcat (lambda (x) x) classes "\n  ")))
	  t)
      (csde-dbs-proc-display-debug-message process
             (format "Error: unable to list loaded classes.\n  Reason: %s."
		     (car (csde-dbo-command-result-data result))))
      nil)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                            ;;
;; Get Path Info Command Class                                                ;;
;;                                                                            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defclass csde-dbs-get-path-info (csde-dbs-cmd) ()
  "Gets the base directory, boot classpath, and classpath of the specified process.")

(defmethod initialize-instance ((this csde-dbs-get-path-info) &rest fields)
  "Constructor for get_path_information command."

  ;; Call parent initializer.
  (call-next-method)

  (assert (oref this process))

  ;; Set command name.
  (oset this name "get_path_information"))

(defmethod csde-dbs-cmd-exec ((this csde-dbs-get-path-info))
  "Executes the get_path_info command."
  (let* ((process (oref this process))
	 (result (call-next-method)))
    (if (csde-dbo-command-succeeded-p result)
	(let* ((data (csde-dbo-command-result-data result))
	       (base-dir (nth 0 data))
	       (boot-classpath (nth 1 data))
	       (classpath (nth 2 data)))
	  (csde-dbs-proc-display-debug-message 
	   process
	   (format (concat
		    "\nPath information\n\n  Base directory:\n    %s\n\n  "
		    "Boot classpath:\n    %s\n\n  Application Classpath:\n    %s\n")
		   base-dir
		   (mapconcat (lambda (x) x) boot-classpath "\n    ")
		   (mapconcat (lambda (x) x) classpath "\n    ")))
	  t)
      (csde-dbs-proc-display-debug-message process
             (format "Error: unable to display path information.\n  Reason: %s."
		     (car (csde-dbo-command-result-data result))))
      nil)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                            ;;
;; Get Threads                                                                ;;
;;                                                                            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defclass csde-dbs-get-threads (csde-dbs-cmd) ()
  "Get all the threads for this process.")


(defmethod initialize-instance ((this csde-dbs-get-threads) &rest fields)
  "Constructor for suspend-thread command."

  ;; Call parent initializer.
  (call-next-method)

  ;; Set command name.
  (oset this name "get_threads"))

(defun csde-dbs-map-thread-to-tree (thread)
  (list (quote csde-widget-tree) :tag (concat (nth 2 thread) " thread")
	:value nil
	(list (quote csde-widget-tree) :tag (concat "id: " (number-to-string (nth 1 thread))))
	(list (quote csde-widget-tree) :tag (concat "status: " (nth 3 thread)))
	(list (quote csde-widget-tree) :tag (concat "state: " (nth 4 thread)))
	(csde-dbs-map-stack-to-tree (nth 5 thread))))


(defun csde-dbs-map-threadgroup-to-tree (threadgroup)
  (nconc
   (list (quote csde-widget-tree) :tag (concat (nth 2 threadgroup) " thread group")
	:value nil)
   (mapcar
    (lambda (x)
      (csde-dbs-map-thread-to-tree x))
    (nth 3 threadgroup))
   (mapcar
    (lambda (x)
      (csde-dbs-map-threadgroup-to-tree x))
    (nth 4 threadgroup))))

(defun csde-dbs-map-stack-to-tree (stack)
  (nconc
   (list (quote csde-widget-tree) :tag "Stack")
   (if (listp stack)
       (mapcar
	(lambda (x)
	  (list (quote csde-widget-tree) :tag
		(format "%s.%s(%s:%s)" (nth 1 x) (nth 4 x) (nth 2 x)
			(nth 3 x))))
	stack))))

(defun csde-dbs-map-threads-to-tree (threads)
  (nconc
   (list (quote csde-widget-tree) :tag "Threads")
	(mapcar
	 (lambda (x)
	   (if (string= (nth 0 x) "Thread")
	       (csde-dbs-map-thread-to-tree x)
	     (if (string= (nth 0 x) "ThreadGroup")
		 (csde-dbs-map-threadgroup-to-tree x))))
	 threads)))


(defmethod csde-dbs-cmd-exec ((this csde-dbs-get-threads))
  "Executes the get-threads command. Returns a list of thread information."
  (let* ((process (oref this process))
	 (result (call-next-method)))
    (if (csde-dbo-command-succeeded-p result)
	(let* ((thread-list (car (csde-dbo-command-result-data result)))
	       (buf (oref process threads-buf)))
	  (set-window-configuration (oref process win-cfg))
	  (set-window-buffer 
	   (next-window (frame-first-window)) buf)
	  (set-buffer buf)
	  (let ((inhibit-read-only t))
	    (erase-buffer)) 
	  (if (not csde-xemacsp)
	      (let ((all (overlay-lists)))  
		(mapcar 'delete-overlay (car all))    
		(mapcar 'delete-overlay (cdr all))))
	  (apply 'widget-create (csde-dbs-map-threads-to-tree thread-list))
	  (use-local-map widget-keymap)
	  (widget-setup))	    
      (csde-dbs-proc-display-debug-message 
       process
       (format "Error: cannot get local variables.\n Reason: %s." 
	       (car (csde-dbo-command-result-data result))))
      nil)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                            ;;
;; Get Thread                                                             ;;
;;                                                                            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defclass csde-dbs-get-thread (csde-dbs-cmd)
  ((thread-id     :initarg :thread-id
	          :type integer
	          :documentation
	          "Id of thread to be queried."))
  "Gets information about a thread, including the method call stack.")


(defmethod initialize-instance ((this csde-dbs-get-thread) &rest fields)
  "Constructor for suspend-thread command."

  ;; Call parent initializer.
  (call-next-method)

  (assert (slot-boundp this 'process))
  (assert (slot-boundp this 'thread-id))

  ;; Set command name.
  (oset this name "get_thread"))

(defmethod csde-dbs-cmd-make-command-line ((this csde-dbs-get-thread))
  "Creates the command line for the get_thread command."
  (format "%s %d" (call-next-method) (oref this thread-id)))

(defmethod csde-dbs-cmd-success-action ((this csde-dbs-get-thread))
  (call-next-method)
  (oset this :result (oref this :data)))

(defmethod csde-dbs-cmd-failure-action ((this csde-dbs-get-thread))
 (oset this msg (format "Error: unable to get info for thread %d.\n Reason: %s." 
			(oref this thread-id)
			(oref this result))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                            ;;
;; Get Object Monitors                                                        ;;
;;                                                                            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defclass csde-dbs-get-object-monitors (csde-dbs-cmd) 
  ((object-id     :initarg :object-id
	          :type integer
	          :documentation
	          "Id of object. Required."))
  "Get threads that are monitoring the specified object.")


(defmethod initialize-instance ((this csde-dbs-get-object-monitors) &rest fields)
  "Constructor for get_object_monitors command."

  ;; Call parent initializer.
  (call-next-method)

  (assert (slot-boundp this :object-id))

  ;; Set command name.
  (oset this name "get_object_monitors"))

(defmethod csde-dbs-cmd-make-command-line ((this csde-dbs-get-object-monitors))
  "Creates the command line for the get_object_monitors command."

  (format "%s %d" (call-next-method) (oref this object-id)))
 
(defmethod csde-dbs-cmd-exec ((this csde-dbs-get-object-monitors))
  "Executes the get_object_monitors command."
  (let* ((process (oref this process))
	 (result (call-next-method))
	 msg)
    (if (csde-dbo-command-succeeded-p result)
	(let* ((data (car (csde-dbo-command-result-data result)))
	       (obj-id (nth 0 data))
	       (obj-type (nth 1 data))
	       (obj-gc (nth 2 data))
	       (owner (nth 3 data))
	       (waiting (nth 4 data)))

	  (setq msg (format "\nThe following threads are monitoring <%s:%s>:\n"
			    obj-type obj-id))

	  (setq 
	   msg 
	   (concat 
	    msg   
	    "  Current owner:"
	    (if (listp owner)
		(concat
		 "\n"
		 "    Name:   " (nth 1 owner) "\n" 
		 "    Id:     " (nth 2 owner) "\n"
		 "    Status: " (nth 3 owner) "\n"
		 "    State:  " (nth 4 owner) "\n")
	      (if (stringp owner)
		  (concat " " owner)))))

	  (if waiting
	      (setq 
	       msg 
	       (concat 
		msg 
		"\n  Waiting threads:"
		(if (listp waiting)
		    (progn
		      "\n"
		      (mapconcat
		      (lambda (thread)
		        (concat 			
			 "    Name:   " (nth 1 thread) "\n" 
			 "    Id:     " (nth 2 thread) "\n"
			 "    Status: " (nth 3 thread) "\n"
			 "    State:  " (nth 4 thread) "\n"))
		      waiting "\n"))
		  (if (stringp waiting) (concat " " waiting "\n")))))))		
      (setq msg
	    (format "Error: cannot get object monitors for  %d.\n Reason: %s." 
		    (oref this object-id)
		    (car (csde-dbo-command-result-data result)))))
    (csde-dbs-proc-display-debug-message process msg)
    nil))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                            ;;
;; Suspend Thread                                                             ;;
;;                                                                            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defclass csde-dbs-suspend-thread (csde-dbs-cmd)
  ((thread-id     :initarg :thread-id
	          :type integer
	          :documentation
	          "Id of thread or thread-group to be suspended. If omitted, all threads are suspended."))
  "Suspend a thread of this process.")


(defmethod initialize-instance ((this csde-dbs-suspend-thread) &rest fields)
  "Constructor for suspend-thread command."

  ;; Call parent initializer.
  (call-next-method)

  ;; Set command name.
  (oset this name "suspend"))

(defmethod csde-dbs-cmd-make-command-line ((this csde-dbs-suspend-thread))
  "Creates the command line for the suspend_thread command."
    (if (slot-boundp this 'thread-id)
	(format "%s %d" (call-next-method) (oref this thread-id))
      (call-next-method)))

(defmethod csde-dbs-cmd-success-action ((this csde-dbs-suspend-thread))
  (call-next-method)
  (if (slot-boundp this 'thread-id)
	(oset this msg (format "Thread %d suspended." (oref this thread-id)))
    (oset this msg "All threads suspended.")
    (oset (oref this process) suspendedp t)))

(defmethod csde-dbs-cmd-failure-action ((this csde-dbs-suspend-thread))
 (oset this msg (format "Error: unable to suspend thread.\n Reason: %s." 
	       (oref this result))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                            ;;
;; Resume Thread                                                             ;;
;;                                                                            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defclass csde-dbs-resume-thread (csde-dbs-cmd)
  ((thread-id     :initarg :thread-id
	          :type integer
	          :documentation
	          "Id of thread or thread-group to be resumed. If omitted, all threads are resumed."))
  "Resume a thread of this process.")


(defmethod initialize-instance ((this csde-dbs-resume-thread) &rest fields)
  "Constructor for resume-thread command."

  ;; Call parent initializer.
  (call-next-method)

  ;; Set command name.
  (oset this name "resume"))

(defmethod csde-dbs-cmd-make-command-line ((this csde-dbs-resume-thread))
  "Creates the command line for the resume_thread command."
    (if (slot-boundp this 'thread-id)
	(format "%s %d" (call-next-method) (oref this thread-id))
      (call-next-method)))

(defmethod csde-dbs-cmd-success-action ((this csde-dbs-resume-thread))
  (call-next-method)
  (if (slot-boundp this 'thread-id)
	(oset this msg (format "Thread %d resumed." (oref this thread-id)))
    (oset this msg "All threads resumed.")
    (oset (oref this process) suspendedp nil)))

(defmethod csde-dbs-cmd-failure-action ((this csde-dbs-resume-thread))
  (oset this msg 
	(format "Error: unable to resume thread.\n Reason: %s." 
		(oref this result))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                            ;;
;; Stop Thread                                                                ;;
;;                                                                            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defclass csde-dbs-stop-thread (csde-dbs-cmd)
  ((thread-id     :initarg :thread-id
	          :type integer
	          :documentation
	          "Id of thread to be stopped.")
   (exception-id  :initarg :exception-id
	          :type integer
	          :documentation
	          "Id of thread to be stopped."))
  "Stops the specified thread in the target process and throw the specified
exception. You can use the evaluate expression command to create the exception
object.")


(defmethod initialize-instance ((this csde-dbs-stop-thread) &rest fields)
  "Constructor for stop-thread command."

  ;; Call parent initializer.
  (call-next-method)

 (assert (slot-boundp this 'thread-id))
 (assert (slot-boundp this 'exception-id))

  ;; Set command name.
  (oset this name "stop"))

(defmethod csde-dbs-cmd-make-command-line ((this csde-dbs-stop-thread))
  "Creates the command line for the resume_thread command."
  
  (format "%s %d %d" (call-next-method) (oref this thread-id) 
	  (oref this exception-id)))

(defmethod csde-dbs-cmd-exec ((this csde-dbs-stop-thread))
  "Executes the stop_thread command."
  (let* ((process (oref this process))
	 (result (call-next-method))
	 (command-succeeded-p (csde-dbo-command-succeeded-p result)))
    (csde-dbs-proc-display-debug-message
	 process
	 (if command-succeeded-p
	     (format "Thread %d stopped." (oref this thread-id))
	   (format "Error: unable to stop thread %d.\n Reason: %s." 
		   (oref this thread-id)
		   (car (csde-dbo-command-result-data result)))))
    command-succeeded-p))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                            ;;
;; Interrupt Thread                                                             ;;
;;                                                                            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defclass csde-dbs-interrupt-thread (csde-dbs-cmd)
  ((thread-id     :initarg :thread-id
	          :type integer
	          :documentation
	          "Id of thread to be interrupted."))
  "Interrupt a thread of this process. An interrupted thread cannot be resumed.")


(defmethod initialize-instance ((this csde-dbs-interrupt-thread) &rest fields)
  "Constructor for suspend-thread command."

  ;; Call parent initializer.
  (call-next-method)

 (assert (slot-boundp this 'thread-id))

  ;; Set command name.
  (oset this name "interrupt"))

(defmethod csde-dbs-cmd-make-command-line ((this csde-dbs-interrupt-thread))
  "Creates the command line for the interrupt_thread command."
  (format "%s %d" (call-next-method) (oref this thread-id)))

(defmethod csde-dbs-cmd-exec ((this csde-dbs-interrupt-thread))
  "Executes the interrupt_thread command."
  (let* ((process (oref this process))
	 (result (call-next-method))
	 (command-succeeded-p (csde-dbo-command-succeeded-p result)))
    (csde-dbs-proc-display-debug-message
	 process
	 (if command-succeeded-p
	     (format "Thread %d interrupted." (oref this thread-id))
	   (format "Error: unable to interrupt thread %d.\n Reason: %s." 
		   (oref this thread-id)
		   (car (csde-dbo-command-result-data result)))))
    command-succeeded-p))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                            ;;
;; Trace Methods                                                              ;;
;;                                                                            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defclass csde-dbs-trace-methods (csde-dbs-cmd)
  ((trace-request  :initarg :trace-request
		   :type csde-dbs-trace-methods-request
		   :documentation 
		   "Trace method request."))
  "Trace method entries or exits.")


(defmethod initialize-instance ((this csde-dbs-trace-methods) &rest fields)
  "Constructor for trace_methods command."

  ;; Call parent initializer.
  (call-next-method)

 (assert (or
	  (string= (oref (oref this trace-request) trace-type) "entry")
	  (string= (oref (oref this trace-request) trace-type) "exit")))

  ;; Set command name.
  (oset this name "trace_methods"))

(defmethod csde-dbs-cmd-make-command-line ((this csde-dbs-trace-methods))
  "Creates the command line for the trace_methods command."
  (let* ((request (oref this trace-request))
	 (cmd (format "%s %s" (call-next-method) (oref request trace-type))))

    (if (slot-boundp request 'thread-restriction)
	(setq cmd (format "%s -tname %s" cmd (oref request thread-restriction))))

    (if (slot-boundp request 'suspend-policy)
	(setq cmd (format "%s -sp %s" cmd (oref request suspend-policy))))

    (if (slot-boundp request 'inclusion-filters)
	(setq cmd 
	      (format 
	       "%s -cf \"%s\"" 
	       cmd
	       (mapconcat (lambda (x) x) (oref request inclusion-filters) " "))))

    (if (slot-boundp request 'exclusion-filters)
	(setq cmd 
	      (format 
	       "%s -cef \"%s\"" 
	       cmd
	       (mapconcat (lambda (x) x) (oref request exclusion-filters) " "))))

    cmd))

(defmethod csde-dbs-cmd-exec ((this csde-dbs-trace-methods))
  "Executes the trace_methods command."
  (let* ((process (oref this process))
	 (result (call-next-method))
	 (command-succeeded-p (csde-dbo-command-succeeded-p result))
	 (request (oref this trace-request))
	 (request-id (car (csde-dbo-command-result-data result))))

    (when command-succeeded-p
      (oset request id request-id)
      (if (slot-boundp process 'trace-req)
	  (oset 
	   process 
	   trace-req 
	   (nconc (oref process trace-req) 
		  (list (cons request-id request))))
	(oset process trace-req (list (cons request-id request)))))

    (csde-dbs-proc-display-debug-message
	 process
	 (if command-succeeded-p
	     (format "Trace method %s enabled. Use request id %s to cancel." 
                     (oref request trace-type) request-id)
	   (format "Error: unable to enable trace.\n Reason: %s." 
		   (car (csde-dbo-command-result-data result)))))
    command-succeeded-p))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                            ;;
;; Trace Classes                                                              ;;
;;                                                                            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defclass csde-dbs-trace-classes (csde-dbs-cmd)
  ((trace-request  :initarg :trace-request
		   :type csde-dbs-trace-classes-request
		   :documentation 
		   "Trace classes request."))
  "Trace class preparations or unloadings.")


(defmethod initialize-instance ((this csde-dbs-trace-classes) &rest fields)
  "Constructor for trace_classes command."

  ;; Call parent initializer.
  (call-next-method)

 (assert (or
	  (string= (oref (oref this trace-request) trace-type) "preparation")
	  (string= (oref (oref this trace-request) trace-type) "unloading")))

  ;; Set command name.
  (oset this name "trace_classes"))

(defmethod csde-dbs-cmd-make-command-line ((this csde-dbs-trace-classes))
  "Creates the command line for the trace_methods command."
  (let* ((request (oref this trace-request))
	 (cmd (format "%s %s" (call-next-method) (oref request trace-type))))

    (if (slot-boundp request 'suspend-policy)
	(setq cmd (format "%s -sp %s" cmd (oref request suspend-policy))))

    (if (slot-boundp request 'inclusion-filters)
	(setq cmd 
	      (format 
	       "%s -cf \"%s\"" 
	       cmd
	       (mapconcat (lambda (x) x) (oref request inclusion-filters) " "))))

    (if (slot-boundp request 'exclusion-filters)
	(setq cmd 
	      (format 
	       "%s -cef \"%s\"" 
	       cmd
	       (mapconcat (lambda (x) x) (oref request exclusion-filters) " "))))

    cmd))

(defmethod csde-dbs-cmd-exec ((this csde-dbs-trace-classes))
  "Executes the trace_classes command."
  (let* ((process (oref this process))
	 (result (call-next-method))
	 (command-succeeded-p (csde-dbo-command-succeeded-p result))
	 (request (oref this trace-request))
	 (request-id (car (csde-dbo-command-result-data result))))

    (when command-succeeded-p
      (oset request id request-id)
      (if (slot-boundp process 'trace-req)
	  (oset 
	   process 
	   trace-req 
	   (nconc (oref process trace-req) 
		  (list (cons request-id request))))
	(oset process trace-req (list (cons request-id request)))))

    (csde-dbs-proc-display-debug-message
	 process
	 (if command-succeeded-p
	     (format "Trace class %s enabled. Use request id %s to cancel." 
                     (oref request trace-type) request-id)
	   (format "Error: unable to enable trace.\n Reason: %s." 
		   (car (csde-dbo-command-result-data result)))))
    command-succeeded-p))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                            ;;
;; Trace Exceptions                                                           ;;
;;                                                                            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defclass csde-dbs-trace-exceptions (csde-dbs-cmd)
  ((trace-request  :initarg :trace-request
		   :type csde-dbs-trace-exceptions-request
		   :documentation 
		   "Trace exceptions request."))
  "Trace exceptions.")


(defmethod initialize-instance ((this csde-dbs-trace-exceptions) &rest fields)
  "Constructor for trace_exceptions command."

  ;; Call parent initializer.
  (call-next-method)

 (assert (or
	  (string= (oref (oref this trace-request) trace-type) "both")
	  (string= (oref (oref this trace-request) trace-type) "caught")
	  (string= (oref (oref this trace-request) trace-type) "uncaught")))

  ;; Set command name.
  (oset this name "trace_exceptions"))

(defmethod csde-dbs-cmd-make-command-line ((this csde-dbs-trace-exceptions))
  "Creates the command line for the trace_exceptions command."
  (let* ((request (oref this trace-request))
	 (cmd (format "%s %s %s" 
		      (call-next-method) 
		      (oref request exception-class)
		      (oref request trace-type))))

    (if (slot-boundp request 'suspend-policy)
	(setq cmd (format "%s -sp %s" cmd (oref request suspend-policy))))

    (if (slot-boundp request 'inclusion-filters)
	(setq cmd 
	      (format 
	       "%s -cf \"%s\"" 
	       cmd
	       (mapconcat (lambda (x) x) (oref request inclusion-filters) " "))))

    (if (slot-boundp request 'exclusion-filters)
	(setq cmd 
	      (format 
	       "%s -cef \"%s\"" 
	       cmd
	       (mapconcat (lambda (x) x) (oref request exclusion-filters) " "))))

    cmd))

(defmethod csde-dbs-cmd-exec ((this csde-dbs-trace-exceptions))
  "Executes the trace_exceptions command."
  (let* ((process (oref this process))
	 (result (call-next-method))
	 (command-succeeded-p (csde-dbo-command-succeeded-p result))
	 (request (oref this trace-request))
	 (request-id (car (csde-dbo-command-result-data result))))

    (when command-succeeded-p
      (oset request id request-id)
      (if (slot-boundp process 'trace-req)
	  (oset 
	   process 
	   trace-req 
	   (nconc (oref process trace-req) 
		  (list (cons request-id request))))
	(oset process trace-req (list (cons request-id request)))))

    (csde-dbs-proc-display-debug-message
	 process
	 (if command-succeeded-p
	     (format "Trace exception %s enabled. Use request id %s to cancel." 
                     (oref request exception-class) request-id)
	   (format "Error: unable to enable trace.\n Reason: %s." 
		   (car (csde-dbo-command-result-data result)))))
    command-succeeded-p))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                            ;;
;; Cancel Trace Requests                                                      ;;
;;                                                                            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defclass csde-dbs-cancel-trace (csde-dbs-cmd)
  ((trace-request  :initarg :trace-request
		   :type csde-dbs-trace-request
		   :documentation 
		   "Trace request."))
  "Cancel a trace request.")


(defmethod initialize-instance ((this csde-dbs-cancel-trace) &rest fields)
  "Constructor for cancel_trace command."

  ;; Call parent initializer.
  (call-next-method)

 (assert (slot-boundp this 'trace-request))

  ;; Set command name.
 (oset this name (oref (oref this trace-request) cancel-command)))


(defmethod csde-dbs-cmd-make-command-line ((this csde-dbs-cancel-trace))
  "Creates the command line for the cancel_trace command."
  (format "%s %s" (call-next-method) (oref (oref this trace-request) id)))


(defmethod csde-dbs-cmd-exec ((this csde-dbs-cancel-trace))
  "Executes the cancel_trace command."
  (let* ((process (oref this process))
	 (result (call-next-method))
	 (command-succeeded-p (csde-dbo-command-succeeded-p result)))

    (if command-succeeded-p
	(let* ((canceled-request-id (oref (oref this trace-request) id))
	       (requests 
		(remove-if
		 (lambda (r)
		   (= (car r) canceled-request-id))
		 (oref process trace-req))))
	  (if requests
	      (oset process trace-req requests)
	    (slot-makeunbound process 'trace-req))))

    (csde-dbs-proc-display-debug-message
	 process
	 (if command-succeeded-p
	     (format "Canceled trace request %s." 
                     (oref (oref this trace-request) id))
	   (format "Error: unable to cancel trace %s.\n Reason: %s." 
		   (oref (oref this trace-request) id)
		   (car (csde-dbo-command-result-data result)))))

    command-succeeded-p))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                            ;;
;; Watch Field                                                                ;;
;;                                                                            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defclass csde-dbs-watch-field (csde-dbs-cmd)
  ((watch-request  :initarg :watch-request
		   :type csde-dbs-watch-field-request
		   :documentation 
		   "Watch field request."))
  "Watch a field of an object or a specified class of objects.")


(defmethod initialize-instance ((this csde-dbs-watch-field) &rest fields)
  "Constructor for watch field command."

  ;; Call parent initializer.
  (call-next-method)

  (let ((request (oref this watch-request)))

    (assert (or
	     (string= (oref request watch-type) "access")
	     (string= (oref request watch-type) "modification")))

    (assert (slot-boundp request 'object-class))
    (assert (slot-boundp request 'field-name)))
 
  ;; Set command name.
  (oset this name "watch"))

(defmethod csde-dbs-cmd-make-command-line ((this csde-dbs-watch-field))
  "Creates the command line for the watch-field command."
  (let* ((request (oref this watch-request))
	 (cmd (format 
	       "%s %s %s %s" 
	       (call-next-method) 
	       (oref request object-class)
	       (oref request field-name)
	       (concat "for_" (oref request watch-type)))))

    (if (slot-boundp request 'object-id)
	(setq cmd (format "%s -oid %s" cmd (oref request object-id))))

    (if (slot-boundp request 'expression)
	(setq cmd (format "%s -if %s" cmd (oref request expression))))

    (if (slot-boundp request 'suspend-policy)
	(setq cmd (format "%s -sp %s" cmd (oref request suspend-policy))))

    (if (slot-boundp request 'inclusion-filters)
	(setq cmd 
	      (format 
	       "%s -cf \"%s\"" 
	       cmd
	       (mapconcat (lambda (x) x) (oref request inclusion-filters) " "))))

    (if (slot-boundp request 'exclusion-filters)
	(setq cmd 
	      (format 
	       "%s -cef \"%s\"" 
	       cmd
	       (mapconcat (lambda (x) x) (oref request exclusion-filters) " "))))

    cmd))

(defmethod csde-dbs-cmd-exec ((this csde-dbs-watch-field))
  "Executes the watch-field command."
  (let* ((process (oref this process))
	 (result (call-next-method))
	 (command-succeeded-p (csde-dbo-command-succeeded-p result))
	 (request (oref this watch-request))
	 (request-id (car (csde-dbo-command-result-data result))))

    (when command-succeeded-p
      (oset request id request-id)
      (if (slot-boundp process 'watch-req)
	  (oset 
	   process 
	   watch-req 
	   (nconc (oref process watch-req) 
		  (list (cons request-id request))))
	(oset process watch-req (list (cons request-id request)))))

    (csde-dbs-proc-display-debug-message
	 process
	 (if command-succeeded-p
	     (format "Watch request field for field %s of %s instance of class %s is enabled. Use request id %s to cancel." 
                     (oref request field-name) 
		     (if (slot-boundp request 'object-id)
			 (oref request object-id)
		       "any")
		     (oref request object-class)
		     request-id)
	   (format "Error: unable to enable watch request.\n Reason: %s." 
		   (car (csde-dbo-command-result-data result)))))
    command-succeeded-p))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                            ;;
;; Cancel Watch Requests                                                      ;;
;;                                                                            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defclass csde-dbs-cancel-watch (csde-dbs-cmd)
  ((watch-request  :initarg :watch-request
		   :type csde-dbs-watch-field-request
		   :documentation 
		   "Watch request."))
  "Cancel a watch request.")


(defmethod initialize-instance ((this csde-dbs-cancel-watch) &rest fields)
  "Constructor for cancel_watch command."

  ;; Call parent initializer.
  (call-next-method)

 (assert (slot-boundp this 'watch-request))

  ;; Set command name.
 (oset this name "clear"))


(defmethod csde-dbs-cmd-make-command-line ((this csde-dbs-cancel-watch))
  "Creates the command line for the clear command."
  (format "%s %s" (call-next-method) (oref (oref this watch-request) id)))


(defmethod csde-dbs-cmd-exec ((this csde-dbs-cancel-watch))
  "Executes the cancel watch command."
  (let* ((process (oref this process))
	 (result (call-next-method))
	 (command-succeeded-p (csde-dbo-command-succeeded-p result)))

    (if command-succeeded-p
	(let* ((canceled-request-id (oref (oref this watch-request) id))
	       (requests 
		(remove-if
		 (lambda (r)
		   (= (car r) canceled-request-id))
		 (oref process watch-req))))
	  (if requests
	      (oset process watch-req requests)
	    (slot-makeunbound process 'watch-req))))

    (csde-dbs-proc-display-debug-message
	 process
	 (if command-succeeded-p
	     (format "Canceled watch request %s." 
                     (oref (oref this watch-request) id))
	   (format "Error: unable to cancel watch request %s.\n Reason: %s." 
		   (oref (oref this watch-request) id)
		   (car (csde-dbo-command-result-data result)))))

    command-succeeded-p))


(provide 'csde-dbs)

; $Log: csde-dbs.el,v $
; Revision 1.4  2001/02/12 05:38:24  paulk
; CSDE 2.2.7
;
; Revision 1.63  2001/01/23 07:37:43  paulk
; Removed typo from csde-dbs-proc-set-find.
;
; Revision 1.62  2001/01/06 05:11:57  paulk
; Fixed regression bug caused by reimplementation of the cygpath conversion function.
;
; Revision 1.61  2000/12/18 05:22:45  paulk
; *** empty log message ***
;
; Revision 1.60  2000/10/25 03:04:42  paulk
; Added a new variable, csde-bug-sio-connect-delay.
;
;   This variable specifies the length of time in seconds
;   that the CSDE waits before attempting to connect to the
;   debuggee application's standard I/O. This delay
;   is intended to give CSDEbug time to create the
;   SIO socket. Previously, the CSDE would attempt to
;   connect immediately, possibly before CSDEbug had time
;   to create a socket for the standard I/O. This might
;   explain the launch command timeout failures that some users have
;   experienced, especially on Windows/NT.
;
; Revision 1.59  2000/09/21 02:27:55  paulk
; Now include csde-run.el when compiling to get the definition for save-w32-show-window macro.
;
; Revision 1.58  2000/09/05 04:58:14  paulk
; Fixed csde-dbs-debugger-display-message.
;
; Revision 1.57  2000/08/14 02:31:57  paulk
; Adds support for Step Into All command.
;
; Revision 1.56  2000/07/28 06:27:45  paulk
; Committing all modified files.
;
; Revision 1.55  2000/06/12 08:35:38  paulk
; Now uses the value of csde-bug-debugger-host-address as the address of the socket for the CLI channel.
;
; Revision 1.54  2000/04/23 06:19:29  paulk
; Fixed some problems with the process launch command. Among others,
; the command now uses `system-name', instead of an absolute
; address (127.0.0.1) to refer to the local host when connecting to
; the port used to transport the debuggee process's standard I/O.
; This fixes the failure to start processes that occurs on some
; Windows networks.
;
; Revision 1.53  2000/04/18 01:20:52  paulk
; Fixes a bug in the csde-dbs-cmd-failure-action method for csde-dbs-get-this and a bug in the csde-dbs-cmd-success-action for csde-dbs-listen-for process.
;
; Revision 1.52  2000/04/13 09:20:54  paulk
; Removed one stray reference to deleted function csde-proc-steppable-p.
;
; Revision 1.51  2000/04/13 09:00:09  paulk
; Added steppablep field to process object. Set whenever process hits a breakpoint or step event.
; Modified csde-dbs-target-process-steppable-p to test steppablep field.
; This fixes menu enabling bug.
; Debugger lisp output parser now suspends paren balancing in strings. This fixes Lisp eval error when a Csharp variable includes unbalanced parentheses.
;
; Revision 1.50  2000/04/10 05:22:55  paulk
; Added command to get the this object for a specified stack frame.
;
; Revision 1.49  2000/04/05 05:00:02  paulk
; Fixed thread-tree code to ignore No information Available threads.
;
; Revision 1.48  2000/03/27 07:31:54  paulk
; Now sets the working directory to csde-run-working-directory (if not null) before starting the debugger.
;
; Revision 1.47  2000/03/17 04:19:02  paulk
; Display threads now includes the stack for each thread. Thanks to Paul Michael Reilly <pmr@pajato.com> for implementing this.
;
; Revision 1.46  2000/03/16 05:05:06  paulk
; Enabled interactive reading of vm and application arguments for CSDEbug sessions. Thanks to Steve Haflich <smh@franz.com> for this enhancement.
;
; Revision 1.45  2000/02/17 06:36:31  paulk
; Fixed scrolling in process debug message window. Thanks to "Martin
; Dickau" <mdickau@byallaccounts.com> for this fix.
;
; Revision 1.44  2000/02/17 06:23:44  paulk
; csde-dbs-cmd now copies and then empties the pending event queue before
; processing the events. This fixes an infinite recursion bug that can
; occur when stepping through code.
;
; Revision 1.43  2000/02/16 04:41:41  paulk
; Implemented Cygwin/XEmacs compatiblity fixes provided by Fred Hart
; <cfhart@Z-TEL.com>.
;
; Revision 1.42  2000/02/14 06:19:37  paulk
; Implemented up and down stack commands.
;
; Revision 1.41  2000/02/10 02:53:38  paulk
; Fixed bug where Display->Threads command was not enabled when debugger
; was attached to a process.
;
; Revision 1.40  2000/02/01 05:59:54  paulk
; Added commands for listening for applications needing debug services.
;
; Revision 1.39  2000/02/01 04:11:55  paulk
; ReleaseNotes.txt
;
; Revision 1.38  2000/01/17 09:36:39  paulk
; Implemented array and object inspectors.
;
; Revision 1.37  2000/01/15 08:04:08  paulk
; Added show buffer commands.
;
; Revision 1.36  2000/01/02 08:07:55  paulk
; Added attach process commands.
;
; Revision 1.35  1999/12/27 08:01:17  paulk
; Added show object monitors command.
;
; Revision 1.34  1999/12/20 07:52:06  paulk
; Added cancel watchpoint command.
;
; Revision 1.33  1999/12/19 06:54:21  paulk
; Added watch field command.
;
; Revision 1.32  1999/12/14 04:46:02  paulk
; Added CSDEbug->Processes->Remove Dead Processes command.
;
; Revision 1.31  1999/12/13 05:54:08  paulk
; Added csde-bug-vm-executable and csde-bug-jre-home variables.
; Fixed csde-dbs-launch-process command so that it fails gracefully.
;
; Revision 1.30  1999/12/03 08:22:00  paulk
; Updated CSDEbug to run under JDK 1.3beta.
;
; Revision 1.29  1999/11/30 05:46:22  paulk
; Added CSDEbug->Display->Path Info command.
;
; Revision 1.28  1999/11/29 06:58:41  paulk
; Added CSDEbug->Display->Loaded Classes Command.
;
; Revision 1.27  1999/11/27 05:13:49  paulk
; Added commands for tracing classes.
;
; Revision 1.26  1999/11/23 06:37:04  paulk
; Added Trace->Cancel command.
;
; Revision 1.25  1999/11/16 05:58:17  paulk
; Added trace method commands and skeletons for trace class and cancel
; trace commands.
;
; Revision 1.24  1999/11/04 05:52:42  paulk
; Added trace-mode fields to csde-dbs-proc class. Needed to support trace mode.
; Added object-refs field to csde-dbs-proc class. Needed to support object reference management.
;
; Revision 1.23  1999/10/28 04:18:09  paulk
; Added interrupt and stop thread commands.
;
; Revision 1.22  1999/10/14 04:59:23  paulk
; Added Resume Process and Resume Thread commands.
;
; Revision 1.21  1999/10/13 08:16:43  paulk
; Added suspend process and suspend thread commands.
;
; Revision 1.20  1999/10/13 06:19:00  paulk
; Add CSDEBug->Show Threads command
;
; Revision 1.19  1999/09/28 04:12:50  paulk
; start debugger method now checks whether debugger actually started
; and returns nil if the debugger did not start.
;
; Revision 1.18  1999/09/18 03:55:58  paulk
; Fixed bug in the launch-process command where the command was failing
; to convert the application arguments from a list of arguments to a
; string of arguments. Thanks to "Matthew
; Weymar"<mweymar@hamilton-partners.com> for reporting the bug.
;
; Revision 1.17  1999/09/16 05:36:59  paulk
; Added get locals command.
;
; Revision 1.16  1999/09/13 05:37:33  paulk
; Enhanced get array command.
;
; Revision 1.15  1999/09/10 06:41:50  paulk
; Finished first cut at get_object command.
;
; Revision 1.14  1999/09/08 05:40:46  paulk
; Updated debugger code to take advantage of new unbound slot capability
; of eieio.
;
; Revision 1.13  1999/09/07 05:12:36  paulk
; Added get array command.
;
; Revision 1.12  1999/09/05 04:35:34  paulk
; Added initial implementation of evaluate and display variable commands.
;
; Revision 1.11  1999/08/30 07:10:41  paulk
; Converted clear breakpoint command to OOPS.
;
; Revision 1.10  1999/08/28 05:34:20  paulk
; Improved multiple process handling, window configuration.
;
; Revision 1.9  1999/08/27 05:27:53  paulk
; Provided initial support for multiple processes.
; Fixed csde-find-data-directory to work on XEmacs with a standard
; CSDE distribution.
; Ported breakpoint highlighting code to XEmacs. Still has bugs though.
; Now includes csde-db-option options on vm command-line for process.
;
; Revision 1.8  1999/08/24 06:29:43  paulk
; Reimplemented the constructor for csde-dbs-proc the right way. Renamed
; csde-bug-counter to csde-bug-breakpoint-counter.
;
; Revision 1.7  1999/08/24 03:26:39  paulk
; Fixed a couple of NT-related problems. In particular, add an extra
; line feed after debugger commands to force flushing of debugger output
; buffer and modified csde-dbs-process-runnable-p to recognize an
; "unknown" state as runnable if the process is suspended.
;

;; End of csde-dbs.el
