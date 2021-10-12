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
                                             (and args
                                                  (every (lambda (arg)
                                                           (eql (getf arg 'type) 'email))
                                                         args))))
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

;;; pbui-standard-commands ends here
