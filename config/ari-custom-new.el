;;; ari-custom.el --- holds all my own private stuff
;;add a comment
;;; Commentary:
;;; History:
;;; Code:

(defun dos-to-unix()
  (interactive)
  (save-excursion
    (beginning-of-buffer)
    (replace-string "\r\n" "\n")))


(defun unix-to-dos()
  (interactive)
  (save-excursion
    (beginning-of-buffer)
    (replace-string"\n" "\r\n")))

(define-key isearch-mode-map (kbd "C-o")
  (lambda ()
    (interactive)
    (let ((case-fold-search isearch-case-fold-search))
      (occur (if isearch-regexp isearch-string
               (regexp-quote isearch-string))))))

(defun djcb-opacity-modify (&optional dec)
  "modify the transparency of the emacs frame; if DEC is t,
decrease the transparency, otherwise increase it in 10%-steps"
  (let* ((alpha-or-nil (frame-parameter nil 'alpha)) ; nil before setting
         (oldalpha (if alpha-or-nil alpha-or-nil 100))
         (newalpha (if dec (- oldalpha 10) (+ oldalpha 10))))
    (when (and (>= newalpha frame-alpha-lower-limit) (<= newalpha 100))
      (modify-frame-parameters nil (list (cons 'alpha newalpha))))))

(defun fg/jira-update-heading ()
"Update heading for Jira Issue at point."
(interactive)
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
     do (org-entry-put pt property value)))))

(provide 'ari-custom-new)
;;; ari-custom.el ends here
