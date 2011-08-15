;; Copyright (C) 2007 by Tapsell-Ferrier Limited

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc.,   51 Franklin Street, Fifth Floor,
;; Boston, MA  02110-1301  USA

(require 'json)

(defvar twitter-usernamepassword-history nil)

(defun twitter (msg usernamepassword &optional imagename)
  (interactive
   (if current-prefix-arg
       (append (twitter-ask)
               (list (read-file-name "imagename: " "~/twitter/")))
     (twitter-ask)))

  ;; Set the image (maybe)
  (if imagename
      (let ((resize-mini-windows nil))
        (shell-command-to-string (concat "curl -L -s "
                                         (format "-u %s " usernamepassword)
                                         (format "-F \"user[profile_image]=@%s\" " (expand-file-name imagename))
                                         "http://twitter.com/account/picture"))))
  ;; Send the entry
  (let ((resize-mini-windows nil)
        (twitter-return-value
         (shell-command-to-string (concat (format "curl -s -u %s " usernamepassword)
                                          (format "-d \"status=%s\" " msg)
                                          "http://twitter.com/statuses/update.json"))))
    (let ((stats (json-read-from-string twitter-return-value)))
      (message "%s" stats)
      (display-message-or-buffer (format "Created at: %s" (cdr (assoc 'created_at stats)))))))

(defun twitter-ask ()
  (list
   (read-from-minibuffer "status: ")
   (read-from-minibuffer "username:password: "
                         (car twitter-usernamepassword-history)
                         nil nil
                         'twitter-usernamepassword-history)))
;; End