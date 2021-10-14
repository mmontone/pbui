;;; pbui-calendar.el --- Calendar patch for PBUI  -*- lexical-binding: t -*-

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

;; Patch that makes Calendar presentation aware (PBUI).

;;; Code:

(require 'calendar)

(defun calendar-generate-month (month year indent)
  "Produce a calendar for MONTH, YEAR on the Gregorian calendar.
The calendar is inserted at the top of the buffer in which point is
currently located, but indented INDENT spaces.  The indentation is
done from the first character on the line and does not disturb the
first INDENT characters on the line."
  (let ((blank-days                     ; at start of month
         (mod
          (- (calendar-day-of-week (list month 1 year))
             calendar-week-start-day)
          7))
         (last (calendar-last-day-of-month month year))
         (trunc (min calendar-intermonth-spacing
                     (1- calendar-left-margin)))
         (day 1)
         j)
   (goto-char (point-min))
   (calendar-move-to-column indent)
   (insert
    (calendar-dlet ((month month) (year year))
      (calendar-string-spread (list calendar-month-header)
                              ?\s calendar-month-digit-width)))
   (calendar-ensure-newline)
   (calendar-insert-at-column indent calendar-intermonth-header trunc)
   ;; Use the first N characters of each day to head the columns.
   (dotimes (i 7)
     (setq j (mod (+ calendar-week-start-day i) 7))
     (insert
      (truncate-string-to-width
       (propertize (calendar-day-name j 'header t)
                   'font-lock-face (if (memq j calendar-weekend-days)
                                       'calendar-weekend-header
                                     'calendar-weekday-header))
       calendar-day-header-width nil ?\s)
      (make-string (- calendar-column-width calendar-day-header-width) ?\s)))
   (calendar-ensure-newline)
   (calendar-dlet ((day day) (month month) (year year))
     (calendar-insert-at-column indent calendar-intermonth-text trunc))
   ;; Add blank days before the first of the month.
   (insert (make-string (* blank-days calendar-column-width) ?\s))
   ;; Put in the days of the month.
   (dotimes (i last)
     (setq day (1+ i))
     ;; TODO should numbers be left-justified, centered...?
     (insert (propertize
              (format (format "%%%dd" calendar-day-digit-width) day)
              'mouse-face 'highlight
              'help-echo (calendar-dlet ((day day) (month month) (year year))
                           (eval calendar-date-echo-text t))
              ;; 'date property prevents intermonth text confusing re-searches.
              ;; (Tried intangible, it did not really work.)
              'date t
	      'presentation (list 'type 'date
				  'value (list month day year)
				  'printer (lambda (date)
					     (calendar-iso-date-string
					      date))))
             (make-string
              (- calendar-column-width calendar-day-digit-width) ?\s))
     (when (and (zerop (mod (+ day blank-days) 7))
                (/= day last))
       (calendar-ensure-newline)
       (setq day (1+ day))              ; first day of next week
       (calendar-dlet ((day day) (month month) (year year))
         (calendar-insert-at-column indent calendar-intermonth-text trunc))))))

(provide 'pbui-calendar)
