;;; courtesy of https://github.com/wasamasa/dotemacs/blob/master/init.org

(setq comint-prompt-read-only t)

(defun my-comint-preoutput-read-only (text)
  (propertize text 'read-only t))

(add-hook 'comint-preoutput-filter-functions
          'my-comint-preoutput-read-only)

(defun comint-previous-input (arg)
  "Cycle backwards with wrap-around through input history, saving input."
  (interactive "*p")
  (unless (and (eq comint-input-ring-index nil)
               (< arg 0))
    (if (and (eq comint-input-ring-index 0)
             (< arg 0)
             comint-stored-incomplete-input)
        (comint-restore-input)
      (unless (and (eq comint-input-ring-index
                   (- (ring-length comint-input-ring) 1))
                   (> arg 0))
        (comint-previous-matching-input "." arg)))))


(defun my-comint-last-output-beg ()
  (save-excursion
    (comint-goto-process-mark)
    (while (not (or (eq (get-char-property (point) 'field) 'boundary)
                    (= (point) (point-min))))
      (goto-char (previous-char-property-change (point) (point-min))))
    (if (= (point) (point-min))
        (point)
      (1+ (point)))))


(defun my-comint-last-output-end ()
  (save-excursion
    (comint-goto-process-mark)
    (while (not (or (eq (get-char-property (point) 'font-lock-face)
                        'comint-highlight-prompt)
                    (= (point) (point-min))))
      (goto-char (previous-char-property-change (point) (point-min)))
      (let ((overlay (car (overlays-at (point)))))
        (when (and overlay (eq (overlay-get overlay 'font-lock-face)
                               'comint-highlight-prompt))
          (goto-char (overlay-start overlay))))
      (1- (point)))))


(defun my-shell-kill-buffer-sentinel (process event)
  (when (and (memq (process-status process) '(exit signal))
             (buffer-live-p (process-buffer process)))
    (kill-buffer)))


(defun my-kill-process-buffer-on-exit ()
  (set-process-sentinel (get-buffer-process (current-buffer))
                        #'my-shell-kill-buffer-sentinel))

(dolist (hook '(ielm-mode-hook term-exec-hook comint-exec-hook))
  (add-hook hook 'my-kill-process-buffer-on-exit))


(defun my-comint-clear-last-output ()
  (interactive)
  (let ((start (my-comint-last-output-beg))
        (end (my-comint-last-output-end)))
    (let ((inhibit-read-only t))
      (delete-region start end)
      (save-excursion
        (goto-char start)
        (insert (propertize "output cleared"
                            'font-lock-face 'font-lock-comment-face))))))


(defun my-shell-turn-echo-off ()
  (setq comint-process-echoes t))

(add-hook 'shell-mode-hook 'my-shell-turn-echo-off)

(provide 'init-comint)
;;; init-comint.el ends here
