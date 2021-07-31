;;; mu4e-view-xwidget --- Summary:
;;; Commentary:
;;; Code:

(require 'mu4e-proc)
(require 'mu4e-view-common)
(defun mu4e-action-view-with-xwidget (msg)
  "View the body of MSG inside xwidget-webkit.
This is only available in Emacs 25+; also see the discussion of
privacy aspects in `(mu4e) Displaying rich-text messages'."
  (unless (fboundp 'xwidget-webkit-browse-url)
    (mu4e-error "No xwidget support available"))
  (xwidget-webkit-browse-url
   (concat "file://" (mu4e~write-body-to-html msg)) t))

(defun mu4e~write-body-to-html (msg)
  "Write MSG's body (either html or text) to a temporary file;
return the filename."
  (let* ((html (mu4e-message-field msg :body-html))
         (txt (mu4e-message-field msg :body-txt))
         (tmpfile (mu4e-make-temp-file "html"))
         (attachments (cl-remove-if (lambda (part)
                                      (or (null (plist-get part :attachment))
                                          (null (plist-get part :cid))))
                                    (mu4e-message-field msg :parts))))
    (unless (or html txt)
      (mu4e-error "No body part for this message"))
    (with-temp-buffer
      (insert "<head><meta charset=\"UTF-8\"></head>\n")
      (insert (concat "<p><strong>From</strong>: "
                      (mu4e~action-header-to-html msg :from) "</br>"))
      (insert (concat "<strong>To</strong>: "
                      (mu4e~action-header-to-html msg :to) "</br>"))
      (insert (concat "<strong>Date</strong>: "
                      (format-time-string mu4e-view-date-format (mu4e-message-field msg :date)) "</br>"))
      (insert (concat "<strong>Subject</strong>: " (mu4e-message-field msg :subject) "</p>"))
      (insert (or html (concat "<pre>" txt "</pre>")))
      (write-file tmpfile)
      ;; rewrite attachment urls
      (mapc (lambda (attachment)
              (goto-char (point-min))
              (while (re-search-forward (format "src=\"cid:%s\""
                                                (plist-get attachment :cid)) nil t)
                (if (plist-get attachment :temp)
                    (replace-match (format "src=\"%s\""
                                           (plist-get attachment :temp)))
                  (replace-match (format "src=\"%s%s\"" temporary-file-directory
                                         (plist-get attachment :name)))
                  (let ((tmp-attachment-name
                         (format "%s%s" temporary-file-directory
                                 (plist-get attachment :name))))
                    (mu4e~proc-extract 'save (mu4e-message-field msg :docid)
                                       (plist-get attachment :index)
                                       mu4e-decryption-policy tmp-attachment-name)
                    (mu4e-remove-file-later tmp-attachment-name)))))
            attachments)
      (save-buffer)
      tmpfile)))

(defun mu4e~action-header-to-html (msg field)
  "Convert the FIELD of MSG to an HTML string."
  (mapconcat
   (lambda(c)
     (let* ((name (when (car c)
                    (replace-regexp-in-string "[[:cntrl:]]" "" (car c))))
            (email (when (cdr c)
                     (replace-regexp-in-string "[[:cntrl:]]" "" (cdr c))))
            (addr (if mu4e-view-show-addresses
                      (if name (format "%s <%s>" name email) email)
                    (or name email))) ;; name may be nil
            ;; Escape HTML entities
            (addr (replace-regexp-in-string "&" "&amp;" addr))
            (addr (replace-regexp-in-string "<" "&lt;" addr))
            (addr (replace-regexp-in-string ">" "&gt;" addr)))
       addr))
   (mu4e-message-field msg field) ", "))

(provide 'mu4e-view-xwidget)
;;; mu4e-view-xwidget ends here
