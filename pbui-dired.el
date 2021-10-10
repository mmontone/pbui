(require 'dired)
(require 'pbui)

;; dired patch for presentations

(defun dired-insert-set-properties (beg end)
  "Add various text properties to the lines in the region."
  (save-excursion
    (let ((directory-name (buffer-substring-no-properties
                           1
                           (progn
                             (goto-char 0)
                             (end-of-line)
                             (backward-char)
                             (point)))))
      (goto-char beg)
      (while (< (point) end)
        (identity
         (if (not (dired-move-to-filename))
             (unless (or (looking-at-p "^$")
                         (looking-at-p dired-subdir-regexp))
               (put-text-property (line-beginning-position)
                                  (1+ (line-end-position))
                                  'invisible 'dired-hide-details-information))
           (put-text-property (+ (line-beginning-position) 1) (1- (point))
                              'invisible 'dired-hide-details-detail)
           (let* ((filename-start (point))
                  (filename-end (progn (dired-move-to-end-of-filename)
                                       (point)))
                  (filename (buffer-substring-no-properties filename-start filename-end))
		  (filepath (string-trim (concatenate 'string directory-name "/" filename))))
             (add-text-properties
              filename-start
              filename-end
              `(mouse-face
                highlight
                dired-filename t
                help-echo "mouse-2: visit this file in other window"
                presentation (type file value ,filepath))))
           (when (< (+ (point) 4) (line-end-position))
             (put-text-property (+ (point) 4) (line-end-position)
                                'invisible 'dired-hide-details-link))))
        (forward-line 1)))))

(provide 'pbui-dired)
