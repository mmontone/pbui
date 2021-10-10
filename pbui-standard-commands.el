(require 'pbui)

(def-presentation-command (standard-commands:open-file
                           :title "Open file"
                           :description "Open the file in a new buffer")
  ((file file))
  (find-file file))

(def-presentation-command (standard-commands:copy-file-to-directory
                           :title "Copy file to directory"
                           :description "Copy file to directory")
  ((file file) (dir directory))
  (copy-file file dir)
  (message "File copied to directory"))

(def-presentation-command (standard-commands:move-file-to-directory
                           :title "Move file to directory"
                           :description "Move file to directory")
  ((file file) (dir directory))
  (rename-file file dir)
  (message "File moved to directory"))

(def-presentation-command (standard-commands:move-file-to-trash
                           :title "Move file to trash"
                           :description "Move file to trash")
  ((file file))
  (move-file-to-trash file)
  (message "File moved to trash"))

(defun presentation-type (presentation)
  (getf presentation 'type))

(defun presentation-value (presentation)
  (getf presentation 'value))

(def-presentation-command (standard-commands:copy-files-to-directory
                           :title "Copy files to directory"
                           :description "Copy files to directory"
                           :applyable-when (lambda (args)
                                             (and (every (lambda (arg)
                                                           (eql (getf arg 'type)
                                                                'file))
                                                         (butlast args))
                                                  (eql (getf (car (last args)) 'type)
                                                       'file))))
  (&rest args)
  (let ((files (butlast args))
        (dir (car (last args))))
    (dolist (file files)
      (copy-file file dir))
    (message "Files copied to directory")))

(def-presentation-command (send-email
                           :title "Send email"
                           :description "Send email"
                           :applyable-when (lambda (args)
                                             (every (lambda (arg)
                                                      (eql (getf arg 'type) 'email))
                                                    args)))
  (&rest emails)
  (call-process "/usr/bin/xdg-open" nil nil nil
                (format "mailto:%s" (s-join "," emails))))

(when (featurep 'inspector)
  (defun inspect-presentation-at-point ()
    (interactive)
    (let ((presentation (presentation-at-point)))
      (when presentation
        (inspector-inspect presentation)))))


(provide 'pbui-standard-commands)
