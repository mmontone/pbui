(require 'pbui)

(def-presentation-command (email-commands:attach-files
			   :title "Attach file(s) to email"
			   :description "Attach files to email composition"
			   :applyable-when (lambda (ps)
					     (and mml-mode
						  (cl-some (lambda (p)
							     (eql (presentation-type p) 'file))
							   ps))))
  ((files file))
  (dolist (file files)
    (mml-attach-file file)))

(def-presentation-command (email-commands:set-email-to-field
			   :title "Set email message 'to' field"
			   :description "Set email message 'to' field"
			   :applyable-when (lambda (ps)
					     (and mml-mode
						  (cl-some (lambda (p)
							     (eql (presentation-type p) 'email))
							   ps))))
  ((emails email))
  (message-goto-to)
  (dolist (email emails)
    (insert email)
    (insert ", "))
  (delete-region (- (point) 2) (point)))

(provide 'pbui-email)
