;;; csde-dbo.el -- CSDEbug output functions
;; $Revision: 1.2 $ $Date: 2001/02/12 05:38:24 $ 

;; Adapted from the JDE by Matt Bruce <matt.bruce@morganstanley.com>
;; Maintainer:  Matt Bruce

;; Copyright (C) 2001 by Matt Bruce

;; JDE Author: Paul Kinnucan <paulk@mathworks.com>
;; JDE Maintainer: Paul Kinnucan

;; Keywords: csharp, tools

;; JDE version Copyright (C) 1997, 1998, 1999 Paul Kinnucan.

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

(require 'eieio)

(defclass csde-dbo-thread ()
  ((id      :initarg :id)
   (name    :initarg :name)
   (state   :initarg :state)
   (status  :initarg :status))
  "Process thread.")

(defun csde-dbo-make-thread-obj (thread-spec)
  (csde-dbo-thread "thread"
		  :id     (nth 1 thread-spec)
		  :name   (nth 2 thread-spec)
		  :state  (nth 3 thread-spec)
		  :status (nth 4 thread-spec)))

(defun csde-dbo-query-source-file (class)
  (let ((source-file
	 (read-file-name 
	  (format "Cannot find %s source. Enter path: " class))))
  (if (and
       source-file
       (file-exists-p source-file)
       (not (file-directory-p source-file)))
      (find-file-noselect source-file))))

(defun csde-dbo-find-source-file (class file)
  (interactive "sclass: \nsfile: ")
  (let* ((source-dir
	 (csde-db-search-src-dirs 
	  file
	  (csde-parse-get-package-from-name class)))
	 (source-file
	  (if source-dir
	      (if (eq (aref source-dir (1- (length source-dir))) ?/)
		  (concat source-dir file)
		(concat source-dir "/" file))))
	 (source-buffer
	  (if (and
	       source-file
	       (file-exists-p source-file))
	      (find-file-noselect source-file)
	    (csde-dbo-query-source-file class))))
    source-buffer))

(defun csde-dbo-show-line (class file line)
  "Shows the source at LINE in CLASS."
  (let* ((buffer (csde-dbo-find-source-file class file))
	 (window 
	  (and buffer
	       (or (get-buffer-window buffer)
		   (selected-window))))
	  pos) 
    (if buffer
	(progn
	  (if (not (get-buffer-window buffer))
	      (set-window-buffer window buffer))
	  (save-excursion
	    (set-buffer buffer)
	    (csde-bug-install-csdebug-menu)
	    (save-restriction
	      (widen)
	      (goto-line line)
	      (setq pos (point))
	      (setq overlay-arrow-string "=>")
	      (or overlay-arrow-position
		  (setq overlay-arrow-position (make-marker)))
	      (set-marker overlay-arrow-position (point) (current-buffer)))
	    (cond ((or (< pos (point-min)) (> pos (point-max)))
		   (widen)
		   (goto-char pos))))
	  (set-window-point window overlay-arrow-position)))))	 

(defun csde-dbo-command-result (id &rest args)
"Returns the result of normally executing command specified by ID.
The result consists of a list whose first element is the command ID,
whose second element is the symbol `normal' to indicate a normal
result and whose remaining element is a list of optional result data."
 (list id 'normal args))

(defun csde-dbo-command-error (id &rest args)
"Returns a command error result. The result consists of list whose first
element is the command's id, whose second element is the symbol `error'
 to indicate that an error occured and whose third element is a list
of optional error data." 
 (list id 'error args))

(defun csde-dbo-command-result-id (result)
  (nth 0 result))

(defun csde-dbo-command-succeeded-p (result)
  (equal (nth 1 result) 'normal))

(defun csde-dbo-command-result-data (result)
  (nth 2 result))

(defun csde-dbo-report-ids-in-use (id)
)

(defun csde-dbo-init-debug-session ()
  (oset csde-dbs-the-debugger started-p t))

(defun csde-dbo-debug (debug-info)
  (message "Debug message: %s" debug-info))

(defun csde-dbo-spec-resolved (proc-id spec-id)
  "Notifies resolution of breakpoint, watchpoint, or 
exception spec."
  (let* ((proc (csde-dbs-get-process proc-id))
	 (bpspec (if proc (csde-dbs-proc-get-bpspec proc spec-id)))
	 (bp (if bpspec (oref bpspec breakpoint)))
	 (file (if bp (oref bp file)))
	 (line (if bp (oref bp line))))
    (and proc file line
	 (csde-dbs-proc-display-debug-message 
	  proc
	  (format "Resolved breakpoint set in %s at line %s." file line)))))

(defun csde-dbo-error (proc-id message)
  (csde-dbs-display-debug-message proc-id message))

(defun csde-dbo-message (proc-id message)
  (csde-dbs-display-debug-message proc-id message))


(defun csde-dbo-unknown-exception (exception)
  (csde-dbs-proc-display-debug-message 
   (csde-dbs-get-target-process) exception))

(defun csde-dbo-vm-start-event (process-id status process-state)
  (let* ((process (csde-dbs-get-process process-id))
	 (thread-id (nth 1 process-state))
	 (thread-name (nth 2 process-state))
	 (state (nth 3 process-state))
	 (reason (nth 4 process-state)))
    (if process
	(let ((state-info (oref process state-info)))
	  (csde-dbs-proc-state-info-set state-info state reason thread-id thread-name)
	  (oset process startupp t)
	  (csde-dbs-proc-display-debug-message process "vm started...")
	  (cond
	   ((string= process-status "all")
	    (csde-dbs-proc-display-debug-message process "All threads suspended...")))
	  ;; Sometimes the debugger is tardy responding to a launch command and thus the CSDE thinks the
	  ;; process is dead. In this case, move the process back to the registry and
          ;; make it the target process.
	  (when (csde-dbs-proc-set-contains-p csde-dbs-the-process-morgue process)
	    (csde-dbs-proc-move-to-registry process)
	    (oset csde-dbs-the-process-registry :target-process process)))
      (message "Start Event Error: can't find process object for process id %d" process-id))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                            ;; 
;; Breakpoint Event Handler                                                   ;;
;;                                                                            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun csde-dbo-update-locals-buf (process thread frame)
  (let* ((cmd (csde-dbs-get-locals 
	      "get locals" 
	      :process process
	      :thread-id thread
	      :stack-frame-index frame))
	 (locals (csde-dbs-cmd-exec cmd))
	 var)

    (save-excursion
      (set-buffer (oref process locals-buf))
      (kill-all-local-variables)
      (let ((inhibit-read-only t))
	(erase-buffer)) 

      (if csde-xemacsp
	  (map-extents (lambda (extent ignore)
		 (delete-extent extent)
		 nil))
	(let ((all (overlay-lists)))  
	  (mapcar 'delete-overlay (car all))    
	  (mapcar 'delete-overlay (cdr all)))) 

      (goto-char (point-min))

      ;; Insert the this object for this stack frame.
      (let* ((cmd (csde-dbs-get-this
		   "get this"
		   :process process
		   :thread-id thread-id
		   :stack-frame-index frame))
	     (this-obj (csde-dbs-cmd-exec cmd)))
	(if (not (typep this-obj 'csde-dbs-csharp-null))
	    (widget-create 'csde-widget-csharp-obj :tag "this" 
			   :process process :object-id (oref this-obj :id))))	    

      ;; Insert the local variables for this stack frame.
      (while locals
	(setq var (car locals) locals (cdr locals))
	(let* ((var-name (oref var name))
	       (var-type (oref var jtype))
	       (var-value (oref var value))
	       (var-tag (format "%s %s" var-type var-name)))
	  (cond
	   ((typep var-value 'csde-dbs-csharp-udci)
	    (if (string= (oref var-value :jtype) "csharp.lang.String")
		(let* ((cmd (csde-dbs-get-string 
				   "get string"
				   :process process
				   :object-id (oref var-value id)))
		       (str-val (csde-dbs-cmd-exec cmd)))
		  (widget-create 'csde-widget-tree :tag var-tag :value t
				 (list 'csde-widget-tree :tag str-val)))
	      (widget-create 'csde-widget-csharp-obj :tag var-tag
			     :process process :object-id (oref var-value :id))))
	   ((typep var-value 'csde-dbs-csharp-array)
	    (widget-create 'csde-widget-csharp-array :tag var-tag
			   :process process :object var-value))
	   ((typep var-value 'csde-dbs-csharp-primitive)
	    (widget-create 'csde-widget-tree :tag var-tag :value t
			   (list 'csde-widget-tree 
				 :tag (format "%s" (oref var-value value)))))
	   ((typep var-value 'csde-dbs-csharp-null)
	    (widget-create 'csde-widget-tree :tag var-tag :value t
			   (list 'csde-widget-tree :tag "null")))
	   (t
	    (error "Unidentified type of local variable: %s" var-tag)))))
      (use-local-map widget-keymap)
      (widget-setup))))


(defun csde-dbo-update-stack (process thread-id) 
  (let* ((cmd  (csde-dbs-get-thread "get_thread" 
				   :process process
				   :thread-id thread-id))
	 (thread-info (csde-dbs-cmd-exec cmd))
	 (stack (nth 5 thread-info)))
    (oset process :stack stack)
    (oset process :stack-ptr 0)))

(defun csde-dbo-breakpoint-hit-event (process-id process-status process-state spec-id location a2 a3)
  (let ((process (csde-dbs-get-process process-id)))
    (if process
	(let ((class (nth 0 location))
	      (file (nth 1 location))
	      (line-no (nth 2 location))
	      (thread-id (nth 1 process-state))
	      (thread-name (nth 2 process-state))
	      (state (nth 3 process-state))
	      (reason (nth 4 process-state))
	      (state-info (oref process state-info)))
	  (if state-info
	      (progn
		(csde-dbs-proc-state-info-set state-info state reason thread-id thread-name)
		(csde-dbo-update-locals-buf process thread-id 0)
		(csde-dbo-update-stack process thread-id)
		(oset process steppablep t)
		(csde-dbs-display-debug-message 
		 process-id
		 (format "Breakpoint hit at line %d in %s (%s) on thread %s. All threads suspended." 
			 line-no class file thread-name))
		(csde-dbo-show-line class file line-no)
		;; (message (selected-frame))
		(when csde-bug-raise-frame-p (raise-frame))
		)
	    (message "Breakpoint hit event error: state info object missing for process %d." process-id)))
      (message "Breakpoint hit event error: process object for process %d is missing." process-id))))

(defun csde-dbo-step-event (proc-id status process-state location)
  "Handler for step events."
  (let ((process (csde-dbs-get-process proc-id)))
    (if process
	(let ((class (nth 0 location))
	      (file (nth 1 location))
	      (line-no (nth 2 location))
	      (thread-id (nth 1 process-state))
	      (thread-name (nth 2 process-state))
	      (state (nth 3 process-state))
	      (reason (nth 4 process-state))
	      (state-info (oref process state-info)))
	  (if state-info
	      (progn
		(csde-dbs-proc-state-info-set state-info state reason thread-id thread-name)
		(csde-dbo-update-locals-buf process thread-id 0)
		(csde-dbo-update-stack process thread-id)
		(oset process steppablep t)
		(csde-dbs-display-debug-message 
		 process-id
		 (format "Stepped to line %d in %s (%s) on thread %s. All threads suspended." 
			 line-no class file thread-name))
		(csde-dbo-show-line class file line-no))
	    (message "Step event error: state info missing for process %d" proc-id)))
      (message "Step event error: could not find process %d." proc-id))))


(defun csde-dbo-exception-event (proc-id status process-state spec-id exception-spec a3)
  (let ((process (csde-dbs-get-process proc-id)))    
    (if process
	(let ((exception-class (nth 0 exception-spec))
	      (exception-object (nth 1 exception-spec))
	      (thread-id (nth 1 process-state))
	      (thread-name (nth 2 process-state))
	      (state (nth 3 process-state))
	      (reason (nth 4 process-state))
	      (state-info (oref process state-info)))
	  (csde-dbs-display-debug-message 
	   proc-id
	   (format "Exception of class %s occurred on thread %s" 
			 exception-class thread-name))))))


(defun csde-dbo-vm-disconnected-event (process-id process-status thread)
  (let ((process (csde-dbs-get-process process-id)))
    (when process
	(csde-dbs-proc-display-debug-message process "vm disconnected...")
	(setq overlay-arrow-position nil)
	(csde-dbs-proc-set-state process "vm disconnected")
	(when (csde-dbs-proc-set-contains-p csde-dbs-the-process-registry process)
	  (csde-dbs-proc-move-to-morgue process)
	  (slot-makeunbound csde-dbs-the-process-registry :target-process)))))

(defun csde-dbo-invalid-break (process-id arg2 reason)
  (csde-dbs-proc-display-debug-message 
   (csde-dbs-get-process process-id)
   (concat "Invalid break error.\n  Reason: " reason)))

(defun csde-dbo-vm-death-event (process-id process-status thread)
  (let* ((process (csde-dbs-get-process process-id))
	 (main-class (oref process main-class)))
    (csde-dbs-proc-display-debug-message 
     process
     (format "%s process ended." main-class))
    (when (csde-dbs-proc-set-contains-p csde-dbs-the-process-registry process)
      (csde-dbs-proc-move-to-morgue process)
      (slot-makeunbound csde-dbs-the-process-registry :target-process))
    (setq overlay-arrow-position nil)))

(defclass csde-dbo-method ()
  ((class    :initarg :class
	     :type string)
   (name     :initarg :name
	     :type string)
   (returns  :initarg :returns
	     :type string)
   (args     :initarg :args
	     :type list)
   (kind     :initarg :kind
	     :type string))	     
  "Method")

(defmethod csde-dbo-to-string ((this csde-dbo-method))
  (format "<%s %s.%s(%s)>"
	  (oref this :returns)
	  (oref this :class)
	  (oref this :name)
	  (mapconcat (lambda (x) x) (oref this :args) ",")))	     

(defun csde-dbo-make-method (spec)
  (let ((m 
	 (csde-dbo-method "method"
			 :class   (nth 0 spec)
			 :name    (nth 1 spec)
			 :returns (nth 2 spec)
			 :args    (nth 3 spec))))
    (if (nth 4 spec)
	(oset m :kind (nth 4 spec)))
    m))

(defun csde-dbo-class-prepare-event (process-id process-status thread-spec class-name)
  (let* ((thread (csde-dbo-make-thread-obj thread-spec))
	 (process (csde-dbs-get-process process-id)))
    (csde-dbs-proc-display-debug-message 
     process
     (format "Preparing class %s.\n  Thread: %s. Status: %s.\n" 
	     class-name
	     (oref thread name)
	     (oref thread status)))))

(defun csde-dbo-class-unload-event (process-id process-status thread-spec class-name)
  (let* ((thread (csde-dbo-make-thread-obj thread-spec))
	 (process (csde-dbs-get-process process-id)))
    (csde-dbs-proc-display-debug-message 
     process
     (format "Unloading class %s.\n  Thread: %s. Status: %s.\n" 
	     class-name
	     (oref thread name)
	     (oref thread status)))))

(defun csde-dbo-method-entry-event (process-id process-status thread-spec method-spec)
  (let* ((thread (csde-dbo-make-thread-obj thread-spec))
	 (method (csde-dbo-make-method method-spec))
	 (method-sig (csde-dbo-to-string method))
	 (process (csde-dbs-get-process process-id)))
    (csde-dbs-proc-display-debug-message 
     process
     (format "Entering %s.%s\n  Thread: %s\n  Signature: %s\n" 
	     (oref method class) 
	     (oref method name)
	     (oref thread name)
	     method-sig))))
         
(defun csde-dbo-method-exit-event (process-id process-status thread-spec method-spec)
  (let* ((thread (csde-dbo-make-thread-obj thread-spec))
	 (method (csde-dbo-make-method method-spec))
	 (method-sig (csde-dbo-to-string method))
	 (process (csde-dbs-get-process process-id)))
    (csde-dbs-proc-display-debug-message 
     process
     (format "Exiting %s.%s\n  Thread: %s\n  Signature: %s\n" 
	     (oref method class) 
	     (oref method name)
	     (oref thread name)
	     method-sig))))

(defun csde-dbo-watchpoint-hit-event (process-id process-status thread-spec request-id &rest data)
  (let* ((thread (csde-dbo-make-thread-obj thread-spec))
	 
	 ;; Object whose field was accessed or modified.
	 (obj-spec (nth 0 data))
	 (obj-class (nth 0 obj-spec))
	 (obj-id (nth 1 obj-spec))
	 (obj-gc-flag  (nth 2 obj-spec))
	 ;; Field that was accessed or modified.
	 (field-spec (nth 1 data))
	 (field-decl (nth 0 field-spec))
	 (field-name (nth 0 field-decl))
	 (field-type (nth 1 field-decl))
	 (field-qual (if (> (length field-decl) 3) (nth 2 field-decl)))
	 (field-value-type (nth 1 field-spec))
	 (field-value (nth 2 field-spec))
	 ;; Breakpoint data
	 (breakpoint-spec (nth 2 data))
	 (breakpoint-class (nth 0 breakpoint-spec))
	 (breakpoint-file (nth 1 breakpoint-spec))
	 (breakpoint-line (nth 2 breakpoint-spec))
	 ;; Object match data
	 (obj-match (nth 3 data))
	 ;; Thread match data
	 (thread-match (nth 4 data))
	 ;; Expression true data
	 (expression-true (nth 5 data))
	 (process (csde-dbs-get-process process-id))
	 )

    (if (string= (oref thread status) "suspended by debugger")
	(csde-dbo-show-line breakpoint-class breakpoint-file breakpoint-line))

    (csde-dbs-proc-display-debug-message 
     process
     (format "<%s:%s> accessed or modified at line %s in %s.\n  Watched field: %s %s %s = %s\n" 
	     obj-class obj-id breakpoint-line breakpoint-file 
	     (if field-qual field-qual "") field-type field-name field-value))
  ))         

(defun csde-dbo-event-set (process-id process-status thread &rest events)
  "Invoked when a set of debugger events occurs. EVENTS is a list of
lists. The first element is the name of a function that handles the event.
The remaining elements are arguments to pass to the handler." 
   (mapc
    (lambda (event)
      (let ((handler (car event))
 	   (args (cdr event)))
        (apply handler 
	       (append (list process-id process-status thread) args))))
    events))



(provide 'csde-dbo)
