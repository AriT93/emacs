(message "loading gnus-config")

(setq gnus-build-sparse-threads t)
(setq gnus-message-archive-group "MyPosts")
(if( string-equal system-name "SYS286558")
	(progn
	  (setq gnus-nntp-server "news.ilstu.edu")
	  (setq gnus-secondary-servers (quote ("news.ilstu.edu")))
	  (setq gnus-select-method (quote (nntp "news.ilstu.edu")))
	  )
  (progn
	(setq gnus-nntp-server "news.verizon.net")
	(setq gnus-secondary-servers (quote ("news.verizon.net")))
	(setq gnus-select-method (quote (nntp "news.verizon.net")))
	)
  )
(setq gnus-show-all-headers nil)
(setq gnus-summary-thread-gathering-function (quote gnus-gather-threads-by-references))
(setq gnus-thread-sort-functions (quote (gnus-thread-sort-by-number gnus-thread-sort-by-author gnus-thread-sort-by-subject gnus-thread-sort-by-date gnus-thread-sort-by-score gnus-thread-sort-by-total-score)))
(setq gnus-your-organization "Happy Young Miscreants")
(setq mark-even-if-inactive t)
(setq message-dont-reply-to-names nil)
(setq message-mail-user-agent nil)
(setq message-required-news-headers (quote (From Newsgroups Subject Date Message-ID Lines)))
(setq message-send-mail-function (quote smtpmail-send-it))
(setq message-user-organization "AriT93")
(setq mm-download-directory nil )
(setq user-full-name "AriT93")
(setq user-mail-address "arit93@yahoo.com")
(setq
;; gnus-summary-line-format "%U%R%z%(%[%4L: %-20,20n%]%B %s%)\n"
 gnus-summary-line-format ":%U%R%B %s%-40=|%4L-%k|%-20,20a|%&user-date;\n"
 gnus-summary-same-subject ""
 gnus-sum-thread-tree-root " >"
 gnus-sum-thread-tree-single-indent "  "
 gnus-sum-thread-tree-vertical "|"
 gnus-sum-thread-tree-indent " "
 gnus-sum-thread-tree-leaf-with-other "+-> "
 gnus-sum-thread-tree-single-leaf "`-> "
 )
(setq
 gnus-use-adaptive-scoring t
 gnus-kill-files "~/News"
 gnus-decay-scores t)

(provide 'gnus-config)