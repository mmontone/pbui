;;; pbui.el --- Presentation Based User Interface for Emacs  -*- lexical-binding: t -*-

;; Copyright (C) 2021 Mariano Montone

;; Author: Mariano Montone <marianomontone@gmail.com>
;; URL: https://github.com/mmontone/emacs-presentations
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

;; A Presentation Based User Interface for Emacs.

;;; Code:

(require 'cl-lib)
(require 'dash)

(defclass pbui:command ()
  ((name :initarg :name
         :accessor pbui:command-name
         :type symbol
         :documentation "The unique name of the command.")
   (title :initarg :title
          :accessor pbui:command-title
          :type string
          :documentation "Title of the command. Appears in completion mini-buffer.")
   (description :initarg :description
                :type string
                :accessor pbui:command-description
                :documentation "Description of the command.")
   (matching-predicate :initarg :applyable-when
                       :accessor matching-predicate
                       :initform nil
                       :type (or null function symbol)
                       :documentation "When present, this function is used for matching the set of selected presentations.")
   (argument-types :initarg :argument-types
                   :accessor pbui:command-argument-types
                   :initform nil
                   :documentation "The type of arguments accepted by this command.")
   (handler :initarg :handler
            :accessor pbui:command-handler
            :type (or function symbol)
            :documentation "A function for running the command. Takes instances of ARGUMENT-TYPES as arguments.")
   (handler-arglist :initarg :handler-arglist
                    :accessor pbui:handler-arglist
                    :documentation "Used internally by PBUI for destructuring and managing command handlers arguments."))
  (:documentation "A command that runs with selected presentations as arguments."))

(defvar pbui:selected-presentations nil
  "The list of currently selected presentations.")

(defun inspect-text-properties-at-point ()
  (interactive)
  (inspector-inspect (text-properties-at (point))))

(defun inspect-presentation-at-point ()
  (interactive)
  (let ((presentation (presentation-at-point)))
    (inspector-inspect presentation)))

(defun presentation-at-point ()
  (interactive)
  (get-text-property (point) 'presentation))

(defun reset-selected-presentations ()
  (interactive)
  (unhighlight-selected-presentations)
  (setf pbui:selected-presentations nil)
  (pbui:refresh-selected-presentations)
  (message "Selected presentations reseted"))

(defun presentation-selected-p (presentation)
  (cl-find presentation pbui:selected-presentations
           :key (lambda (sel) (getf sel 'presentation))))

(defun ensure-selected-presentation-at-point ()
  "Ensure that presentation at point is selected."
  (interactive)
  (let ((presentation (presentation-at-point)))
    (if presentation
        (when (not (presentation-selected-p presentation))
          (select-presentation-at-point))
      (message "No presentation at point"))))

(defun toggle-presentation-selected-at-point ()
  "Toggle presentation selected at cursor."
  (interactive)
  (let ((presentation (presentation-at-point)))
    (if presentation
        (if (presentation-selected-p presentation)
            (unselect-presentation-at-point)
          (select-presentation-at-point))
      (message "No presentation at point"))))

(defun pbui:goto-selected-presentation (selected-presentation)
  (let ((buffer (getf sel 'buffer)))
    (when (buffer-live-p buffer)
      (switch-to-buffer-other-window buffer)
      (with-current-buffer buffer
        (goto-char (getf sel 'position))))))

(defun pbui:print-presentation (presentation)
  (if (getf presentation 'printer)
      (funcall (getf presentation 'printer) (getf presentation 'value))
    (getf presentation 'value)))

(defun pbui:add-selected-presentation (presentation buffer position)
  ;; We record the presentation and buffer and position for selected presentations
  (let ((selection (list
                    'presentation presentation
                    'buffer (or buffer (current-buffer))
                    'position (or position (point)))))
    (push selection
          pbui:selected-presentations)
    (pbui:refresh-selected-presentations)
    selection))

(defun select-presentation-at-point ()
  "Add the presentation at point to the list of selected presentations."
  (interactive)
  (let ((presentation (presentation-at-point)))
    (if presentation
        (let ((selection
               (pbui:add-selected-presentation presentation (current-buffer) (point))))
          (pbui:highlight-selected-presentation selection)
          (message "Selected: %s" (pbui:print-presentation presentation)))
      (message "No presentation at point"))))

(defun pbui:remove-selected-presentation (presentation)
  (setq pbui:selected-presentations (cl-delete presentation pbui:selected-presentations :key (lambda (sel) (getf sel 'presentation))))
  (pbui:refresh-selected-presentations))

(defun unselect-presentation-at-point ()
  "Remove the presentation at point from the list of selected presentations."
  (interactive)
  (let ((presentation (presentation-at-point)))
    (if presentation
        (progn
          (pbui:remove-selected-presentation presentation)
          (unhighlight-presentation-at-point)
          (message "Unselected: %s"
                   (if (getf presentation 'printer)
                       (funcall (getf presentation 'printer) (getf presentation 'value))
                     (getf presentation 'value))))
      (message "No presentation at point"))))

(defun select-presentation-at-point-and-run-command ()
  "Add presentation at point and run command after."
  (interactive)
  (ensure-selected-presentation-at-point)
  (call-interactively 'run-presentations-command))

(defvar pbui:commands (make-hash-table :test 'equal))

(defun pbui:find-command (command-name)
  "Find PBUI command named with COMMAND-NAME."
  (gethash command-name pbui:commands))

(defmacro def-presentation-command (name-and-options args &rest body)
  (let ((command-name (if (listp name-and-options)
                          (first name-and-options)
                        name-and-options))
        (options (when (listp name-and-options)
                   (rest name-and-options))))
    `(setf (gethash ',command-name pbui:commands)
           (make-instance 'pbui:command
                          :name ',command-name
                          :argument-types ',(and (not (getf options :applyable-when))
                                                 (mapcar 'second args))
                          :handler (lambda ,(if (eql (first args) '&rest)
                                                args
                                              (mapcar 'first args))
                                     ,@body)
                          :handler-arglist ',args
                          ,@options))))

(defun pbui:command-matches (command presentations)
  "Return T if COMMAND matches PRESENTATIONS, and NIL otherwise."
  (if (matching-predicate command)
      (ignore-errors (funcall (matching-predicate command) presentations))
    (and (= (length (pbui:command-argument-types command))
            (length presentations))
         (every 'identity
                (mapcar (lambda (x)
                          (eql (getf (first x) 'type) (second x)))
                        (-zip-lists
                         (reverse presentations)
                         (pbui:command-argument-types command)))))))

(defun pbui:matching-presentations-commands ()
  "Return a list of the matching commands for added presentation arguments."
  (cl-loop for command in (hash-table-values pbui:commands)
           when (pbui:command-matches
                 command
                 (mapcar (lambda (sel) (getf sel 'presentation))
                         pbui:selected-presentations))
           collect command))

(defun pbui:run-command (command)
  (let ((ps (mapcar (lambda (sel)
                      (getf sel 'presentation))
                    pbui:selected-presentations)))
    (if (eql (first (pbui:handler-arglist command))
             '&rest)
        (apply (pbui:command-handler command)
               (mapcar 'presentation-value
                       (reverse ps)))
      ;; else
      ;; destructure handler arg list using presentation types
      (let ((actual-args nil))
        (dolist (argspec (pbui:handler-arglist command))
          (destructuring-bind (argname argtype) argspec
            (if (cl-member (substring (symbol-name argname) -1) '("s" "*")
                           :test 'string=)
                ;; a multi-valued argument
                (push (cl-loop for p in ps
                               when (eql (presentation-type p) argtype)
                               collect (presentation-value p))
                      actual-args)
              ;; a single-valued argument
              (push (cl-find argtype ps :key 'presentation-type)
                    actual-args))))
        (apply (pbui:command-handler command) (reverse actual-args))))))

;; See: http://www.howardism.org/Technical/Emacs/alt-completing-read.html
(defun alt-completing-read (prompt collection &optional predicate require-match initial-input hist def inherit-input-method)
  "Calls `completing-read' but returns the value from COLLECTION.

Simple wrapper around the `completing-read' function that assumes
the collection is either an alist, or a hash-table, and returns
the _value_ of the choice, not the selected choice. For instance,
give a variable of choices like:

    (defvar favorite-hosts '((\"Glamdring\" . \"192.168.5.12\")
                             (\"Orcrist\"   . \"192.168.5.10\")
                             (\"Sting\"     . \"192.168.5.220\")
                             (\"Gungnir\"   . \"192.168.5.25\")))

We can use this function to `interactive' without needing to call
`alist-get' afterwards:

    (defun favorite-ssh (hostname)
      \"Start a SSH session to a given HOSTNAME.\"
      (interactive (list (alt-completing-read \"Host: \" favorite-hosts)))
      (message \"Rockin' and rollin' to %s\" hostname))"

  ;; Yes, Emacs really should have an `alistp' predicate to make this code more readable:
  (cl-flet ((assoc-list-p (obj) (and (listp obj) (consp (car obj)))))

    (let* ((choice
            (completing-read prompt collection predicate require-match initial-input hist def inherit-input-method))
           (results (cond
                     ((hash-table-p collection) (gethash choice collection))
                     ((assoc-list-p collection) (alist-get choice collection def nil 'equal))
                     (t                         choice))))
      (if (listp results) (first results) results))))

(defun pbui:read-command-name ()
  (alt-completing-read "Command: "
                       (mapcar (lambda (cmd)
                                 (cons (pbui:command-title cmd)
                                       (pbui:command-name cmd)))
                               (pbui:matching-presentations-commands))))

(defun run-presentations-command (command-name)
  "Run a command that matches presentation arguments."
  (interactive (list
		(when (pbui:matching-presentations-commands)
		  (pbui:read-command-name))))
  (if (not command-name)
      (message (format "No matching command for the selected presentations (%d selected, %d in other buffers). Press v to view all selected presentations."
		       (length pbui:selected-presentations)
		       (length (remove-if-not (lambda (sel) (eql (getf sel 'buffer) (current-buffer))) pbui:selected-presentations))))
    (let ((command (gethash command-name pbui:commands)))
      (when (null command)
	(message "Command not found: %s" command-name))
      (pbui:run-command command)
      (reset-selected-presentations)
      ;; Disable the global presentations mode after running a command
      (disable-global-pbui-mode))))

(defun presentation (value string &optional type)
  "Add presentation properties to STRING.
TYPE is the type of presentation.
VALUE is is the object being presented."
  (propertize string 'presentation
              (list 'type (or type (type-of value))
                    'value value)))

(defun present (value string &optional type)
  "Create and insert presentation."
  (insert (presentation value string type)))

(defgroup pbui ()
  "Group for presentations")

(defface presentation
  '((t :box '(:line-width 1 :color "gray" :style nil)))
  "Face for presentations in PBUI"
  :group 'pbui)

(defface selected-presentation
  '((t :box '(:line-width 1 :color "gray" :style nil)
       :background "lightyellow"))
  "Face for presentations"
  :group 'pbui)

(defface presentations-button
  '((((type x w32 ns) (class color))    ; Like default mode line
     :box (:line-width 2 :style released-button)
     :background "lightgrey" :foreground "black"))
  "Face for custom buffer buttons if `custom-raised-buttons' is non-nil."
  :group 'pbui)

(defmacro with-write-buffer (&rest body)
  (let ((read-only-p (gensym "read-only")))
    `(let ((,read-only-p buffer-read-only))
       (setq buffer-read-only nil)
       ,@body
       (setq buffer-read-only ,read-only-p))))

(defun highlight-presentation-at-point ()
  "Highlight the presentation at point."
  (let ((presentation (presentation-at-point)))
    (when presentation
      (unless (presentation-selected-p presentation)
        (save-excursion
          (goto-char (1+ (point)))
          (let ((prop (text-property-search-backward 'presentation))
                (prop-next (text-property-search-forward 'presentation)))
            (with-write-buffer
             (put-text-property (prop-match-beginning prop)
                                (prop-match-end prop-next)
                                'font-lock-face 'presentation))))))))

(defun unhighlight-presentation-at-point ()
  (let ((presentation (presentation-at-point)))
    (when presentation
      (unless (presentation-selected-p presentation)
        (save-excursion
          (goto-char (1+ (point)))
          (let ((prop (text-property-search-backward 'presentation))
                (prop-next (text-property-search-forward 'presentation)))
            (with-write-buffer
             (put-text-property (prop-match-beginning prop)
                                (prop-match-end prop-next)
                                'font-lock-face nil))))))))

(defun pbui:highlight-selected-presentation (selection)
  (let ((buffer (getf selection 'buffer)))
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        (save-excursion
          (goto-char (1+ (getf selection 'position)))
          (let ((presentation (presentation-at-point)))
            (when presentation
              (let ((prop (text-property-search-backward 'presentation))
                    (prop-next (text-property-search-forward 'presentation)))
                (with-write-buffer
                 (put-text-property (prop-match-beginning prop)
                                    (prop-match-end prop-next)
                                    'font-lock-face 'selected-presentation))))))))))

(defun pbui:unhighlight-selected-presentation (selection)
  (let ((buffer (getf selection 'buffer)))
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        (save-excursion
          (goto-char (1+ (getf selection 'position)))
          (let ((prop (text-property-search-backward 'presentation))
                (prop-next (text-property-search-forward 'presentation)))
            (with-write-buffer
             (put-text-property (prop-match-beginning prop)
                                (prop-match-end prop-next)
                                'font-lock-face nil))))))))

(defun unhighlight-selected-presentations ()
  "Unhighlight the currently selected presentations."
  (dolist (sel pbui:selected-presentations)
    (pbui:unhighlight-selected-presentation sel)))

(defun highlight-selected-presentations ()
  "Highlight the currently selected presentations."
  (dolist (sel pbui:selected-presentations)
    (pbui:highlight-selected-presentation sel)))

(defun highlight-presentations-in-buffer ()
  "Highlight the presentations in current buffer."
  (save-excursion
    (goto-char 0)
    (while (setq prop (text-property-search-forward 'presentation))
      (put-text-property
       (prop-match-beginning prop)
       (prop-match-end prop)
       'font-lock-face 'presentation))))

(defun propertize-presentations-in-buffer ()
  "Set text properties for presentations in current buffer."
  (interactive)
  (let ((read-only-p buffer-read-only)
        (prop nil))
    (setq buffer-read-only nil)
    (save-excursion
      (goto-char 0)
      (while (setq prop (text-property-search-forward 'presentation))
        (put-text-property
         (prop-match-beginning prop)
         (prop-match-end prop)
         'mouse-face 'highlight)
        (put-text-property
         (prop-match-beginning prop)
         (prop-match-end prop)
         'help-echo (format "%s.

mouse-2: toggle selection of this presentation"
                            (pbui:print-presentation (prop-match-value prop))))
        (put-text-property
         (prop-match-beginning prop)
         (prop-match-end prop)
         'cursor-sensor-functions
         (list
          (lambda (window pos action)
            (save-excursion
              (if (eql action 'entered)
                  (progn
                    (highlight-presentation-at-point)
                    (message
                     (pbui:print-presentation (presentation-at-point))))
                (progn
                  (goto-char pos)
                  (unhighlight-presentation-at-point))))
            )))))
    (setq buffer-read-only read-only-p)))

(defun pbui:highlight-all-buffers ()
  (dolist (buffer (buffer-list))
    (with-current-buffer buffer
      (highlight-presentations-in-buffer))))

(defun pbui:clear-all-buffers ()
  (dolist (buffer (buffer-list))
    (with-current-buffer buffer
      (pbui:clear-highlights-in-buffer))))

(defun pbui:clear-highlights-in-buffer ()
  "Clear the highlighted presentations in buffer."
  (let ((read-only-p buffer-read-only)
        (prop))
    (setq buffer-read-only nil)
    (save-excursion
      (goto-char 0)
      (while (setq prop (text-property-search-forward 'presentation))
        (put-text-property (prop-match-beginning prop)
                           (prop-match-end prop)
                           'font-lock-face nil)
        (put-text-property (prop-match-beginning prop)
                           (prop-match-end prop)
                           'mouse-face nil)
        (put-text-property (prop-match-beginning prop)
                           (prop-match-end prop)
                           'help-echo nil))
      (goto-char 0)
      (while (setq prop (text-property-search-forward 'selected-presentation))
        (put-text-property (prop-match-beginning prop)
                           (prop-match-end prop)
                           'font-lock-face nil))
      (setq buffer-read-only read-only-p))))

(defun goto-next-presentation ()
  (interactive)
  (text-property-search-forward 'presentation nil
                                (lambda (value pvalue)
                                  (eql value pvalue)))
  (let ((prop
         (text-property-search-forward 'presentation)))
    (when prop
      (goto-char (prop-match-beginning prop)))))

(defun goto-previous-presentation ()
  (interactive)
  (text-property-search-backward 'presentation nil
                                 (lambda (value pvalue)
                                   (eql value pvalue)))
  (let ((prop
         (text-property-search-backward 'presentation)))
    (when prop
      (goto-char (prop-match-beginning prop)))))

;;--- PBUI mode ----------------------------------------------------------------

(defvar pbui-mode-map
  (let ((map (make-keymap)))
    (define-key map (kbd "SPC") 'toggle-presentation-selected-at-point)
    (define-key map (kbd "X") 'select-presentation-at-point-and-run-command)
    (define-key map (kbd "<deletechar>") 'reset-selected-presentations)
    (define-key map (kbd "x") 'run-presentations-command)
    (define-key map (kbd "f") 'goto-next-presentation)
    (define-key map (kbd "b") 'goto-previous-presentation)
    (define-key map (kbd "n") 'goto-next-presentation)
    (define-key map (kbd "p") 'goto-previous-presentation)
    (define-key map (kbd "q") 'disable-global-pbui-mode)
    (define-key map (kbd "<escape>") 'disable-global-pbui-mode)
    (define-key map (kbd "<mouse-2>") 'toggle-presentation-selected-at-point-handler)
    (define-key map (kbd "v") 'visualize-selected-presentations)
    map))

(define-minor-mode pbui-mode
  "Minor mode with quick keybindings for using presentations."
  ;; The initial value.
  :init-value nil
  ;; The indicator for the mode line.
  :lighter " PBUI"
  ;; The minor mode bindings.
  :keymap pbui-mode-map
  :group 'pbui
  (if pbui-mode
      (pbui:initialize-pbui-mode)
    (pbui:release-pbui-mode)))

(defun pbui:initialize-pbui-mode ()
  "Initialize PBUI mode."
  (propertize-presentations-in-buffer)
  (highlight-selected-presentations)
  (cursor-sensor-mode))

(defun pbui:release-pbui-mode ()
  (unhighlight-selected-presentations)
  (pbui:clear-highlights-in-buffer)
  (cursor-sensor-mode -1))

(easy-menu-define
  pbui-mode-menu pbui-mode-map
  "Menu for PBUI"
  '("PBUI"
    ["Visualize selected presentations" visualize-selected-presentations
     :help "Popup a buffer with the list of selected presentations"]
    ["Next presentation" goto-next-presentation
     :help "Search next presentation in buffer"]
    ["Previous presentation" goto-previous-presentation
     :help "Search previous presentation in buffer"]
    "---"
    ["Toggle presentation selected at point" toggle-presentation-selected-at-point
     :help "Select or unselect presentation at point"]
    ["Run command with presentation at point..." select-presentation-at-point-and-run-command
     :help "Select the presentation at point and run a command"]
    ["Run command..." run-presentations-command
     :help "Run command"]
    ["Reset selected presentations" reset-selected-presentations
     :help "Clear the list of selected presentations"]
    "---"
    ["Customize" pbui:customize
     :help "Customize presentations mode"]
    ["Quit" disable-global-pbui-mode
     :help "Quit Presentations mode"]))

(defun pbui:customize ()
  (interactive)
  (customize-group 'pbui))

(defun pbui:draw-selected-presentations ()
  (setq buffer-read-only nil)
  (erase-buffer)
  (if (not pbui:selected-presentations)
      (insert "There are not presentations selected")
    (progn
      (dolist (sel pbui:selected-presentations)
        (let ((presentation (getf sel 'presentation)))
          (insert-button (format "%s [%s]"
                                 (pbui:print-presentation presentation)
                                 (getf presentation 'type))
                         'help-echo "Go to presentation"
                         'follow-link t
                         'action (lambda (btn)
                                   (ignore btn)
                                   (pbui:goto-selected-presentation sel)))
          (insert " ")
          (insert-button "delete"
                         'help-echo "Remove presentation from the selection list"
                         'face 'presentations-button
                         'action (lambda (btn)
                                   (ignore btn)
                                   (setq pbui:selected-presentations (cl-delete sel pbui:selected-presentations))
                                   (pbui:unhighlight-selected-presentation sel)
                                   (pbui:refresh-selected-presentations)))
          (newline)))
      (newline)
      (insert-button "clear all"
                     'help-echo "Clear all selected presentations"
                     'face 'presentations-button
                     'action (lambda (btn)
                               (ignore btn)
                               (reset-selected-presentations))))))

(defun pbui:refresh-selected-presentations ()
  (let ((buffer (get-buffer "*selected presentations*")))
    (when buffer
      (with-current-buffer buffer
        (setq buffer-read-only nil)
        (erase-buffer)
        (pbui:draw-selected-presentations)
        (setq buffer-read-only t)))))

(defun visualize-selected-presentations ()
  (interactive)
  (if (get-buffer "*selected presentations*")
      (progn
	(pbui:refresh-selected-presentations)
	(switch-to-buffer-other-window "*selected presentations*"))
    (let ((buffer (get-buffer-create "*selected presentations*")))
      (with-current-buffer buffer
        (pbui:draw-selected-presentations)
        (setq buffer-read-only t)
        (local-set-key "q" (lambda () (interactive) (kill-buffer buffer)))
        (local-set-key "g" 'pbui:refresh-selected-presentations)
        (display-buffer buffer)))))

(defun toggle-presentation-selected-at-point-handler (event)
  (interactive "e")
  (let ((window (posn-window (event-end event)))
        (pos (posn-point (event-end event))))
    (if (not (windowp window))
        (error "No presentation chosen"))
    (goto-char pos)
    (toggle-presentation-selected-at-point)))

(define-globalized-minor-mode global-pbui-mode
  pbui-mode
  (lambda ()
    (pbui-mode t)))

(defun disable-global-pbui-mode ()
  (interactive)
  (global-pbui-mode -1))

;; Disable PBUI minor mode for the minibuffer
(add-hook 'minibuffer-setup-hook
	  (lambda ()
	    (when pbui-mode
	      (pbui-mode -1))))

(provide 'pbui)

;;; pbui.el ends here
