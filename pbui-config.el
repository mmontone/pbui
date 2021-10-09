;;; -*- lexical-binding: t -*-

(require 'pbui)

(defvar pbui-mode-map
  (let ((map (make-keymap)))
    (define-key map (kbd "SPC") 'toggle-presentation-selected-at-point)
    (define-key map (kbd "RET") 'toggle-presentation-selected-at-point)
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
  :group 'pbui)

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
    ["Toggle presentation at point" toggle-presentation-at-point
     :help "Select or unselect presentation at point"]
    ["Run command with presentation at point..." select-presentation-at-point-and-run-command
     :help "Select the presentation at point and run a command"]
    ["Run command..." run-presentations-command
     :help "Run command"]
    ["Reset selected presentations" reset-selected-presentations
     :help "Clear the list of selected presentations"]
    "---"
    ["Customize" customize-presentations
     :help "Customize presentations mode"]
    ["Quit" disable-global-pbui-mode
     :help "Quit Presentations mode"]))

(add-hook 'pbui-mode-hook 'propertize-presentations-in-buffer)
(add-hook 'pbui-mode-hook 'highlight-selected-presentations)

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
      (pbui:refresh-selected-presentations)
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
  global-pbui-mode)

(defun global-pbui-mode ()
  (interactive)
  (pbui-mode)
  (cursor-sensor-mode)
  (pbui:highlight-all-buffers)
  (message "Presentations mode entered"))

(defun disable-global-pbui-mode ()
  (interactive)
  (pbui-mode -1)
  (cursor-sensor-mode -1)
  (pbui:clear-all-buffers)
  (message "Presentations mode disabled"))

(global-set-key (kbd "C-c x SPC")
                'toggle-presentation-selected-at-point)
(global-set-key (kbd "C-c x X")
                'select-presentation-at-point-and-run-command)
(global-set-key (kbd "C-c x <deletechar>")
                'reset-presentation-command-arguments)
(global-set-key (kbd "C-c x x")
                'run-presentations-command)
(global-set-key (kbd "C-<return>") 'global-pbui-mode)
