;;; ari-custom.el --- holds all my own private stuff -*- lexical-binding: t; -*-
;;add a comment
;;; Commentary:
;;; this is my custom functions
;;; History: long ago in a different time.  check git log for deets
;;; Code:

(defun dos-to-unix ()
  "Convert DOS line endings to Unix line endings in the current buffer."
  (interactive)
  (save-excursion
    (save-restriction
      (widen)
      (goto-char (point-min))
      (while (search-forward "\r\n" nil t)
        (replace-match "\n" nil t)))))



(defun unix-to-dos ()
  "Convert Unix line endings to DOS line endings in the current buffer."
  (interactive)
  (save-excursion
    (save-restriction
      (widen)
      (goto-char (point-min))
      (while (search-forward "\n" nil t)
        (replace-match "\r\n" nil t)))))

(define-key isearch-mode-map (kbd "C-o")
  (lambda ()
    (interactive)
    (let ((case-fold-search isearch-case-fold-search))
      (occur (if isearch-regexp isearch-string
               (regexp-quote isearch-string))))))

(defun djcb-opacity-modify (&optional dec)
       "Modify the transparency of the Emacs frame in 10% steps.
If DEC is non-nil, decrease transparency by 10%, otherwise increase it.
Transparency is constrained between `frame-alpha-lower-limit' and 100.
Returns the new alpha value, or nil if no change was made."
       (interactive "P")
       (condition-case err
           (let* ((alpha-or-nil (frame-parameter nil 'alpha))
                  (oldalpha (if alpha-or-nil alpha-or-nil 100))
                  (newalpha (if dec (- oldalpha 10) (+ oldalpha 10))))
             (if (and (>= newalpha frame-alpha-lower-limit) (<= newalpha 100))
                 (progn
                   (modify-frame-parameters nil (list (cons 'alpha newalpha)))
                   (message "Frame alpha set to %d%%" newalpha)
                   newalpha)
               (message "Alpha value %d is out of bounds (%d-100)"
                        newalpha frame-alpha-lower-limit)
               nil))
         (error
          (message "Error modifying frame opacity: %s" (error-message-string err))
          nil)))

(defun fg/jira-update-heading ()
    "Update current org heading with data from Jira.
Fetches issue details from Jira using the JIRAISSUEKEY property and updates
the heading text and properties with current issue data. Requires jiralib2 to
be configured and the point to be on an org heading with a JIRAISSUEKEY property.

Returns nil if not on a heading or JIRAISSUEKEY is missing, otherwise updates
the heading and properties."
    (interactive)
    (condition-case err
        (when-let* ((pt (point))
                    (issue-key (and (org-at-heading-p)
                                    (org-entry-get pt "JIRAISSUEKEY"))))
          (let-alist (jiralib2-get-issue issue-key)
      ;; Update headline
      (let ((headline (format "%s %s" .key .fields.summary)))
        (message "Updating %s" headline)
        (org-edit-headline headline))
      ;; Update properties
      (cl-loop
       for (property value)
       on (list
           "JiraAssignee" .fields.assignee.displayName
           "JiraCreated" .fields.created
           "JiraIssueKey" .key
           "JiraIssueType" .fields.issuetype.name
           "JiraPriority" .fields.priority.name
           "JiraProjectKey" .fields.project.key
           "JiraReporter" .fields.reporter.displayName
           "JiraStatus" .fields.status.name
           "JiraSummary" .fields.summary)
       by #'cddr
       do (org-entry-put pt property value))))
      (error
       (message "Error updating Jira heading: %s" (error-message-string err))
       nil)))

(defun my/org-roam-weekly-recap (&optional week-offset)
  "Generate a weekly recap from daily notes.
WEEK-OFFSET is the number of weeks before current (0 = current week)."
  (interactive "P")
  (let* ((week-offset (or week-offset 0))
         (today (current-time))
         (day-of-week (string-to-number (format-time-string "%w" today)))
         ;; Calculate start of week (Sunday)
         (start-of-week (time-subtract today 
                                      (seconds-to-time (* 86400 (+ day-of-week (* 7 week-offset))))))
         (end-of-week (time-add start-of-week (seconds-to-time (* 86400 6))))
         (start-date-str (format-time-string "%Y-%m-%d" start-of-week))
         (end-date-str (format-time-string "%Y-%m-%d" end-of-week))
         (recap-buffer-name (format "*Weekly Recap: %s to %s*" 
                                   start-date-str end-date-str))
         (all-highlights '())
         (all-goals '()))
    
    ;; Create a new buffer for the recap
    (with-current-buffer (get-buffer-create recap-buffer-name)
      (erase-buffer)
      (org-mode)
      
      ;; Add header
      (insert (format "* Weekly Recap: %s to %s\n\n" 
                     start-date-str end-date-str))
      
      ;; Query all daily notes in the date range using org-ql
      (let ((daily-entries (org-ql-query
                           :from (org-roam-list-files)
                           :where `(and (property "CATEGORY" "daily")
                                       ;; Files that fall within our date range
                                       (ts :from ,start-date-str :to ,end-date-str)))))
        
        ;; Process each daily entry
        (if daily-entries
            (dolist (entry daily-entries)
              (with-current-buffer (find-file-noselect entry)
                (let ((title (or (save-excursion
                                   (goto-char (point-min))
                                   (when (re-search-forward "^#\\+title: \\([0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}\\)" nil t)
                                     (match-string 1)))
                                 (file-name-sans-extension (file-name-nondirectory entry))))
                      (headlines '()))
                  
                  ;; Find all level 2 headlines
                  (org-map-entries
                   (lambda ()
                     (when (= (org-current-level) 2)
                       ;; Check for special tags/properties
                       (let ((heading (org-get-heading t t t t))
                             (tags (org-get-tags)))
                         ;; Collect highlights and goals based on tags
                         (when (member "highlight" tags)
                           (push (format "- %s (from %s)" heading title) all-highlights))
                         (when (member "goal" tags)
                           (push (format "- %s (from %s)" heading title) all-goals)))
                       
                       (push (cons (org-get-heading t t t t)
                                   (org-get-entry))
                             headlines)))
                   nil 'file)
                  
                  ;; Add entry to recap
                  (with-current-buffer recap-buffer-name
                    (insert (format "** %s\n" title))
                    
                    ;; Add each headline and content
                    (dolist (headline (nreverse headlines))
                      (insert (format "*** %s\n%s\n" 
                                     (car headline)
                                     (cdr headline))))
                    
                    (insert "\n")))))
          
          ;; Fallback to file-based search if org-ql query returns no results
          (let ((daily-files '()))
            ;; Find all daily notes for the specified week by filename
            (dolist (file (org-roam-list-files))
              (let ((file-name (file-name-nondirectory file)))
                ;; Check if filename matches date format YYYY-MM-DD.org
                (when (string-match "\\([0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}\\)\\.org$" file-name)
                  (let ((file-date (match-string 1 file-name)))
                    ;; Check if file date is within our range
                    (when (and (string-greaterp file-date start-date-str)
                               (string-lessp file-date end-date-str))
                      (push file daily-files))))))
            
            (if daily-files
                ;; Process files found by filename pattern
                (dolist (file daily-files)
                  (with-current-buffer (find-file-noselect file)
                    (let ((title (file-name-sans-extension (file-name-nondirectory file)))
                          (headlines '()))
                      
                      ;; Find all level 2 headlines
                      (org-map-entries
                       (lambda ()
                         (when (= (org-current-level) 2)
                           ;; Check for special tags
                           (let ((heading (org-get-heading t t t t))
                                 (tags (org-get-tags)))
                             (when (member "highlight" tags)
                               (push (format "- %s (from %s)" heading title) all-highlights))
                             (when (member "goal" tags)
                               (push (format "- %s (from %s)" heading title) all-goals)))
                           
                           (push (cons (org-get-heading t t t t)
                                       (org-get-entry))
                                 headlines)))
                       nil 'file)
                      
                      ;; Add entry to recap
                      (with-current-buffer recap-buffer-name
                        (insert (format "** %s\n" title))
                        
                        ;; Add each headline and content
                        (dolist (headline (nreverse headlines))
                          (insert (format "*** %s\n%s" 
                                         (car headline)
                                         (cdr headline))))
                        
                        ))))
              ;; No entries found by any method
              (insert "No daily entries found for this week.\n")))))
      
      ;; Add summary section with automatically collected highlights and goals
      (insert "* Summary\n\n")
      (insert "** Weekly highlights\n")
      (if all-highlights
          (dolist (highlight (nreverse all-highlights))
            (insert highlight "\n"))
        (insert "\n"))
      
      (insert "** Goals for next week\n")
      (if all-goals
          (dolist (goal (nreverse all-goals))
            (insert goal ""))
        (insert "\n"))
      
      (goto-char (point-min))
      (switch-to-buffer recap-buffer-name))))

(defun my/simple-weekly-recap (&optional week-offset)
  "Generate simple weekly recap by combining daily notes."
  (interactive "P")
  (let* ((week-offset (or week-offset 0))
         (today (current-time))
         (start-of-week (time-subtract today 
                                      (seconds-to-time (* 86400 
                                                        (+ (string-to-number (format-time-string "%w" today))
                                                           (* 7 week-offset))))))
         (recap-buffer (generate-new-buffer "*Weekly Recap*")))
    
    (with-current-buffer recap-buffer
      (org-mode)
      (insert (format "* Weekly Recap: %s\n\n" 
                      (format-time-string "%Y-W%V" start-of-week)))
      
      ;; Loop through each day of the week
      (dotimes (day 7)
        (let* ((current-date (time-add start-of-week (seconds-to-time (* 86400 day))))
               (date-string (format-time-string "%Y-%m-%d" current-date))
               (filename (expand-file-name (format "daily/%s.org" date-string)
                                         org-roam-directory)))
          
          (when (file-exists-p filename)
            (insert (format "** %s\n" date-string))
            (insert-file-contents filename)
            (insert "\n\n"))))
      
      (switch-to-buffer recap-buffer))))

(defun unfill-paragraph ()
  "Takes a multi-line paragraph and makes it into a single line of text."
  (interactive)
  (let ((fill-column (point-max)))
    (fill-paragraph nil)))

(defun unfill-region (beg end)
  "Unfill the region, joining text paragraphs into a single
    logical line.  This is useful, e.g., for use with
    `visual-line-mode'."
  (interactive "*r")
  (let ((fill-column (point-max)))
    (fill-region beg end)))

(provide 'ari-custom-new)
;;; ari-custom-new.el ends here
