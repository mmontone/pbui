(require 'request)
(require 's)
(require 'pbui)

(defvar contacts-app:contacts nil)

(defun contacts-app:fetch-users ()
  (request "https://randomuser.me/api/?results=50"
    :success 'contacts-app:read-user))

(cl-defun contacts-app:read-user (&key data &allow-other-keys)
  (setf contacts-app:contacts (alist-get 'results (json-read-from-string data))))

(contacts-app:fetch-users)

(defmacro with-text-properties (properties &rest body)
  `(let ((tp-start (point)))
     ,@body
     (set-text-properties tp-start (point) ,properties)))

(defun contacts-app:user-fullname (user)
  (format "%s %s" (alist-get 'first (alist-get 'name user))
	  (alist-get 'last (alist-get 'name user))))

(defun contacts-app:create-user-birthday-event (user)
  (list 'title (format "%s birthday" (contacts-app:user-fullname user))
	'date (let ((date (parse-time-string (alist-get 'date (alist-get 'dob user)))))
		(setf (nth 5 date) (nth 2 (calendar-current-date)))
		date)
	'description (format "It is %s birthday." (contacts-app:user-fullname user))))

(defun contacts-app ()
  (interactive)
  
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
			;;	     'presentation
			;;	     (list 'type 'date
			 ;;		   'value birthdate))
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

(defun contacts-app:user-id (user)
  (alist-get 'value (alist-get 'id user)))

(defun contacts-app:user-email (user)
  (alist-get 'email user))

(defun contacts-app:phone (user)
  (alist-get 'phone user))

(def-presentation-command (contacts-app:delete-contacts
			   :title "Delete contacts"
			   :description "Delete contacts"
			   :applyable-when (lambda (args)
					     (every (lambda (arg)
						      (eql (getf arg 'type)
							   'contacts-app:user))
						    args)))
  (users)
  (let ((users-ids (remove-if 'null (mapcar 'contacts-app:user-id users))))
    (setf contacts-app:contacts
	  (cl-delete-if (lambda (user)
			  (cl-member (contacts-app:user-id user)
				     users-ids))
			contacts-app:contacts))
    (let ((buffer (get-buffer "*contacts-app*")))
      (with-current-buffer buffer
	(erase-buffer)
	(contacts-app)))
    (message "Users deleted")))

(def-presentation-command (contacts-app:send-email
			   :title "Send email"
			   :description "Send email to contacts"
			   :applyable-when (lambda (args)
					     (every (lambda (arg)
						      (eql (getf arg 'type)
							   'contacts-app:user))
						    args)))
  (&rest users)
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

(provide 'contacts-app)
