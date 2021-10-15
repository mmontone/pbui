(require 'pbui)

(def-presentation-command (org-commands:link-files
			   :title "Org: Link file(s)"
			   :condition (lambda (ps) (eql major-mode 'org-mode)))
  ((files file))
  (dolist (file files)
    (org-insert-link nil file)
    (insert " ")))

(provide 'pbui-org)
