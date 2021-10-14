;;; pbui-dired.el --- Dired patch for PBUI  -*- lexical-binding: t -*-

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

;; Patch that makes Dired presentation aware (PBUI).

;;; Code:

(require 'dired)
(require 'pbui)
(require 'pbui-standard-commands)

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
                presentation (type ,(if (file-directory-p filepath)
					'directory
				      'file)
				   value ,filepath))))
           (when (< (+ (point) 4) (line-end-position))
             (put-text-property (+ (point) 4) (line-end-position)
                                'invisible 'dired-hide-details-link))))
        (forward-line 1)))))

(provide 'pbui-dired)

;;; pbui-dired ends here
