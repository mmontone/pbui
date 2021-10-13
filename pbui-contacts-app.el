;;; pbui-contacts-app.el --- A PBUI demo application  -*- lexical-binding: t -*-

;; Copyright (C) 2021 Mariano Montone

;; Author: Mariano Montone <marianomontone@gmail.com>
;; URL: https://github.com/mmontone/pbui
;; Keywords: user-interface
;; Version: 0.1
;; Package-Requires: ((emacs "25") (dash "2.19.1"))

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; A PBUI demo application.

;; Load and then M-x contacts-app to run.

;;; Code:

(require 'request)
(require 's)
(require 'calendar)
(require 'pbui)

(defvar contacts-app:contacts nil)

(defmacro with-text-properties (properties &rest body)
  `(let ((tp-start (point)))
     ,@body
     (set-text-properties tp-start (point) ,properties)))

(defun contacts-app:user-fullname (user)
  (format "%s %s" (alist-get 'first (alist-get 'name user))
          (alist-get 'last (alist-get 'name user))))

(defun contacts-app:user-id (user)
  (alist-get 'value (alist-get 'id user)))

(defun contacts-app:user-email (user)
  (alist-get 'email user))

(defun contacts-app:phone (user)
  (alist-get 'phone user))

(defun contacts-app:create-user-birthday-event (user)
  (list 'title (format "%s birthday" (contacts-app:user-fullname user))
        'date (let ((date (parse-time-string (alist-get 'date (alist-get 'dob user)))))
                (setf (nth 5 date) (nth 2 (calendar-current-date)))
                date)
        'description (format "It is %s birthday." (contacts-app:user-fullname user))))

(defun contacts-app ()
  "Contacts application entry point command."
  (interactive)
  (request "https://randomuser.me/api/?results=50"
    :success
    (cl-function
     (lambda (&key data &allow-other-keys)
       (setf contacts-app:contacts (alist-get 'results (json-read-from-string data)))
       (contacts-app-create-buffer)))))

(defun contacts-app-create-buffer ()
  (let ((buffer (get-buffer-create "*contacts-app*")))
    (with-current-buffer buffer
      (cl-loop for user across contacts-app:contacts
               do
               ;; Heading
               (insert "* ")
               (with-text-properties
                (list 'presentation
                      (list 'type 'contacts-app:user
                            'value user
                            'printer 'contacts-app:user-fullname))
                (insert (contacts-app:user-fullname user)))

               (insert " - ")

               (with-text-properties
                (list 'presentation
                      (list 'type 'email
                            'value (alist-get 'email user)))
                (insert (alist-get 'email user)))

               (newline)

               ;; Body
               (insert "- Fullname: " (alist-get 'first (alist-get 'name user))
                       " " (alist-get 'last (alist-get 'name user)))
               (newline)
               (insert "- Email: " (propertize (alist-get 'email user)
                                               'presentation
                                               (list 'type 'email
                                                     'value (alist-get 'email user))))
               (newline)
               (insert "- Phone: " (propertize (alist-get 'phone user)
                                               'presentation
                                               (list 'type 'phone
                                                     'value (alist-get 'phone user))))
               (newline)
               (insert "- Cell: " (propertize (alist-get 'cell user)
                                              'presentation
                                              (list 'type 'phone
                                                    'value (alist-get 'cell user))))

               (newline)
               (insert "- Birthdate: "
                       (let ((birthdate (alist-get 'date (alist-get 'dob user))))
                         ;;(propertize birthdate
                         ;;          'presentation
                         ;;          (list 'type 'date
                         ;;                'value birthdate))
                         (propertize birthdate
                                     'presentation
                                     (list 'type 'calendar-event
                                           'value (contacts-app:create-user-birthday-event user)
                                           'printer (lambda (event)
                                                      (format "Calendar event - %s" (getf event 'title)))))))

               (newline))
      (outline-mode)
      (outline-hide-sublevels 1)
      (setq buffer-read-only t))
    (pop-to-buffer buffer)))

(def-presentation-command (contacts-app:delete-contacts
                           :title "Delete contacts"
                           :description "Delete contacts")
  ((users contacts-app:user))
  (when (yes-or-no-p (format "Delete %d selected contacts?"
                             (length users)))
    (let ((users-ids (remove-if 'null (mapcar 'contacts-app:user-id users))))
      (setf contacts-app:contacts
            (cl-delete-if (lambda (user)
                            (cl-member (contacts-app:user-id user)
                                       users-ids))
                          contacts-app:contacts))
      (let ((buffer (get-buffer "*contacts-app*")))
        (with-current-buffer buffer
          (with-write-buffer
           (erase-buffer)
           (contacts-app-create-buffer))))
      (message "Selected contacts deleted"))))

(def-presentation-command (contacts-app:send-email
                           :title "Send email"
                           :description "Send email to contacts")
                           
  ((users contacts-app:user))
  (call-process "/usr/bin/xdg-open" nil nil nil
                (format "mailto:%s" (s-join "," (mapcar 'contacts-app:user-email users)))))

(def-presentation-command (contacts-app:add-event-to-google-calendar
                           :title "Add event to Google calendar"
                           :description "Add event to Google calendar")
  ((event calendar-event))
  (call-process "/usr/bin/xdg-open" nil nil nil
                (format "http://www.google.com/calendar/render?action=TEMPLATE&text=%s&dates=%s&details=%s&location=%s"
                        (getf event 'title)
                        (format "%s%s%s"
                                (nth 5 (getf event 'date))
                                (nth 4 (getf event 'date))
                                (nth 3 (getf event 'date)))
                        (getf event 'description)
                        "")))

(def-presentation-command (contacts-app:send-files-in-email
                           :title "Send files by email"
                           :description "Send files by email"
                           :applyable-when (lambda (args)
                                             (and args
                                                  (some (lambda (arg)
                                                           (member (getf arg 'type) '(contacts-app:user email)))
                                                        args)
						  (some (lambda (arg)
							  (eql (getf arg 'type) 'file)) args))))
  ((users contacts-app:user) (emails email) (files file))
  (let ((all-emails (append (mapcar 'contacts-app:user-email users)
                            emails)))
    (call-process "/usr/bin/thunderbird" nil nil nil
                  "-compose"
                  (format "to='%s',attachment='%s'"
                          (s-join "," (mapcar 'contacts-app:user-email users))
                          (s-join "," files)))))

(provide 'pbui-contacts-app)

;;; pbui-contacts-app ends here
