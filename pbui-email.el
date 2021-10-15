(require 'pbui)
(require 'mml)

(def-presentation-command (email-commands:attach-files
			   :title "Attach file(s) to email"
			   :description "Attach files to email composition"
			   :condition (lambda (ps) mml-mode))
  ((files file))
  (dolist (file files)
    (mml-attach-file file)))

(def-presentation-command (email-commands:set-email-to-field
			   :title "Set email message 'to' field"
			   :description "Set email message 'to' field"
			   :condition (lambda (ps) mml-mode))
  ((emails email))
  (message-goto-to)
  (dolist (email emails)
    (insert email)
    (insert ", "))
  (delete-region (- (point) 2) (point)))

(provide 'pbui-email)
