(defvar initial-mode-map
    (let ((map (make-sparse-keymap)))
      (define-key map (kbd "C-c C-c") 'lisp-interaction-mode)
      (define-key map (kbd "C-c d") 'delete-region)
      (define-key lisp-interaction-mode-map (kbd "C-c C-c") 'initial-mode)
      map)
    "Keymap for `initial-mode'.")

(define-derived-mode initial-mode nil "Initial"
  "Major mode for start up buffer.
\\{initial-mode-map}"
  (setq-local
   text-mode-variant t)
  (setq-local
   indent-line-function 'indent-relative))

(defun initial-mode-recenter ()
  (save-excursion
    (goto-char (point-min))
    (forward-line 0)
    (recenter 0)))

(defun initial-mode-check-if-terminal ()
  (when (display-graphic-p)
    (text-scale-decrease 2)))

(defun initial-mode-turn-on-electric-mode ()
  (electric-pair-local-mode))

(dolist (hook '(initial-mode-recenter
                initial-mode-check-if-terminal
                initial-mode-turn-on-electric-mode))
  (add-hook 'initial-mode-hook hook))

(setq initial-major-mode 'initial-mode)

(provide 'init-initial)
;;; init-initial.el ends here
