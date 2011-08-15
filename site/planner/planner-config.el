;;; Sacha's configuration for planner.el
;; Sacha Chua <sacha@free.net.ph>

;;;_+ Loading

;; This directory contains all the latest emacs-wiki and planner files.
(add-to-list 'load-path (expand-file-name "~/emacs/site/planner/contrib"))

;; Separate file to make this config less scary
(require 'planner)
(require 'planner-publish)

(require 'remember-config)

(load-library "planner")
(load-library "planner-accomplishments")
(require 'planner-appt)
(load-library "planner-bookmark")
(require 'planner-calendar)
(load-library "planner-cyclic")
(load-library "planner-deadline")
(require 'planner-diary)
(load-library "planner-erc")
;;(load-library "planner-experimental")
(load-library "planner-lisp")
(load-library "planner-multi")
(load-library "planner-notes-index")
(load-library "planner-report")
(load-library "planner-rss")
(load-library "planner-schedule")
(require 'planner-tasks-overview)
(load-library "planner-timeclock")
(load-library "planner-timeclock-summary")
(load-library "planner-trunk")
(load-library "planner-vm")
(require 'psvn)
(require 'planner-psvn)
(setq planner-psvn-log-edit-notice-commit-function t)
;;_+ Keybindings

;; This reminds me what I'm working on. C-u F9 F9 jumps to the task, too.
(global-set-key (kbd "<f9> p SPC") 'planner-goto-today)
(global-set-key (kbd "<f9> P SPC") 'planner-goto)
(global-set-key (kbd "<f9> r SPC") 'remember)
(global-set-key (kbd "<f9> R SPC") 'remember-region)
(global-set-key (kbd "<f9> t SPC") 'planner-create-task-from-buffer)
(global-set-key (kbd "<f9> T SPC") 'planner-create-task)
(global-set-key (kbd "<f9> i SPC") 'planner-timeclock-in)
(global-set-key (kbd "<f9> n SPC") 'planner-create-note)

;; I use F9 p to go to today's page, anyway.
(define-key planner-mode-map (kbd "C-c C-n") 'planner-create-note-from-task)
(define-key planner-mode-map (kbd "C-c C-e") 'planner-edit-task-description)
;; I use an after-save-hook to publish, so I can remap C-c C-p
(define-key planner-mode-map (kbd "C-c C-p") 'planner-task-pending)

;;;_+ Basic setup

(setq planner-carry-tasks-forward 0)
(setq planner-expand-name-favor-future-p nil)
(setq planner-task-dates-favor-future-p t)
(setq planner-default-task-priority "B")
(setq planner-expand-name-default ".")
(setq planner-task-format "#%s%s %s %s%s")
;; I don't need my tasks renumbered.
(setq planner-renumber-tasks-automatically nil)
(setq planner-align-tasks-automatically nil)
(setq planner-renumber-notes-automatically nil)
(setq planner-day-page-template "\<calendar\>\n\n* Tasks\n\n\n* Schedule\n\n\n* Timeclock\n\n\n* Accomplishments\n\n\n* Notes")
;; Do not automatically add task IDs. I used to set this to non-nil,
;; but realized that I didn't edit my task descriptions that often. If
;; I want to edit a task, I can just add the task ID _before_ editing.
(setq planner-id-add-task-id-flag nil)

(add-to-list 'planner-markup-tags '("notes" nil nil nil planner-notes-tag))
(add-to-list 'planner-markup-tags '("example" t nil t emacs-wiki-example-tag))

;; I don't mind having lots of open planner files
(setq planner-tasks-file-behavior nil)

(setq planner-diary-use-diary t)
(planner-diary-insinuate)
(planner-insinuate-calendar)
(planner-appt-use-tasks-and-schedule)
(planner-appt-insinuate)
(setq planner-appt-task-use-appointments-section-flag t)
(planner-appt-schedule-cyclic-insinuate)
(setq planner-appt-update-appts-on-save-flag t)
(planner-accomplishments-insinuate)
(planner-timeclock-summary-insinuate)
(setq planner-cyclic-diary-file "~/.diary.cyclic-tasks")

(provide 'planner-config)

;;; planner-config.el ends here
