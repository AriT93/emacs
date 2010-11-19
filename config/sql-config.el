(message "loading sql-config")
(add-hook 'sql-mode-hook 'my-sql-mode-hook)
(defun my-sql-mode-hook()
  (message "SQL mode hook executed")
  (define-key sql-mode-map [f5] 'sql-send-buffer))

(setq sql-db2-program "db2cmd")
(setq sql-db2-options '("-c" "-i" "-w" "db2" "-v" ))


;;(setq sql-db2-program "db2cmd db2clp.bat db2.exe")
(setq sql-ms-program "osql")
(setq sql-mysql-program "c:/cygwin/usr/local/bin/mysql")
(setq sql-pop-to-buffer-after-send-region nil)
(setq sql-product (quote ms))


(provide 'sql-config)
