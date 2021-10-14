;;; pbui-standard-commands.el --- Standard commands for PBUI -*- lexical-binding: t -*-

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

;; A set of standard commands for PBUI.

;;; Code:

(require 'pbui)

(def-presentation-command (standard-commands:open-files
                           :title "Open file(s)"
                           :description "Open file(s) in a new buffer")
  ((files file))
  (dolist (file files)
    (find-file file)))

(def-presentation-command (standard-commands:copy-files-to-directory
                           :title "Copy file(s) to directory"
                           :description "Copy file(s) to directory")
  ((files file) (dir directory))
  (dolist (file files)
    (copy-file file
	       (file-name-concat dir (file-name-nondirectory file))))
  (message (format "%d files copied to %s" (length files) dir)))

(def-presentation-command (standard-commands:move-files-to-directory
                           :title "Move file(s) to directory"
                           :description "Move file(s) to directory")
  ((files file) (dir directory))
  (dolist (file files)
    (rename-file file
		 (file-name-concat dir (file-name-nondirectory file))))
  (message (format "%d files moved to %s" (length files) dir)))

(def-presentation-command (standard-commands:move-files-to-trash
                           :title "Move file(s) to trash"
                           :description "Move file(s) to trash")
  ((files file))
  (dolist (file files)
    (move-file-to-trash file))
  (message (format "%d files moved to trash" (length files))))

(def-presentation-command (send-email
                           :title "Send email"
                           :description "Send email")
                           
  ((emails email))
  (call-process "/usr/bin/xdg-open" nil nil nil
                (format "mailto:%s" (s-join "," emails))))

(when (featurep 'inspector)
  (defun inspect-presentation-at-point ()
    (interactive)
    (let ((presentation (presentation-at-point)))
      (when presentation
        (inspector-inspect presentation)))))


(provide 'pbui-standard-commands)

;;; pbui-standard-commands ends here
