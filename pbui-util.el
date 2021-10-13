(defun inspect-text-properties-at-point ()
  (interactive)
  (inspector-inspect (text-properties-at (point))))

(defun inspect-presentation-at-point ()
  (interactive)
  (let ((presentation (presentation-at-point)))
    (inspector-inspect presentation)))
