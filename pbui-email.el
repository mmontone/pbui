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

(provide 'pbui-email)
