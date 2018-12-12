(global-set-key (kbd "C-s") 'isearch-forward-regexp)
(global-set-key (kbd "C-r") 'isearch-backward-regexp)
(global-set-key (kbd "C-M-s") 'isearch-forward)
(global-set-key (kbd "C-M-r") 'isearch-backward)

;; (require 'isearch-dabbrev)
;; (define-key isearch-mode-map (kbd "M-/") 'isearch-dabbrev-expand)

(defun isearch-apropos ()
  (interactive)
  (apropos isearch-string))

(define-key isearch-mode-map (kbd "M-s a") 'isearch-apropos)

(defun isearch-forward-other-window (arg)
  "Isearch in the other window."
  (interactive "p")
  (save-selected-window
    (other-window 1)
    (if (= 4 arg)
        (isearch-forward)
      (isearch-forward-regexp))))

(defun isearch-backward-other-window (arg)
  "Isearch in the other window."
  (interactive "p")
  (save-selected-window
    (other-window 1)
    (if (= 4 arg)
        (isearch-backward)
      (isearch-backward-regexp))))

(defun isearch-grab-region-for-search (beg end &optional backward)
  "Grab the current region and use it in as a search term in
isearch. For regions larger than a word, this function can be
useful. By default, isearch-forward is called, but if BACKWARD is
non-nil then isearch-backward is called instead. "
  (interactive "r")
  (when (use-region-p)
    (let ((text (buffer-substring-no-properties beg end)))
      (deactivate-mark)
      (isearch-resume text nil nil (or backward t) text t))))

(define-key isearch-mode-map
  (kbd "M-s o")
  'isearch-occur)

(defun isearch-yank-forward-sexp
    (&optional arg)
  "Pull the next sexp from buffer into search string.
If optional ARG is non-nil, pull in the next ARG sexps."
  (interactive "p")
  (isearch-yank-internal
   (lambda
     ()
     (forward-sexp arg)
     (point))))

(define-key isearch-mode-map
  (kbd "M-s C-M-f")
  'isearch-yank-forward-sexp)

(defun isearch-yank-backward-sexp (&optional arg)
  "Pull the next sexp from buffer into search string.
If optional ARG is non-nil, pull in the next ARG sexps."
  (interactive "p")
  (isearch-yank-internal (lambda () (backward-sexp arg) (point))))

(define-key isearch-mode-map
  (kbd "M-s C-M-b")
  'isearch-yank-backward-sexp)

(defun isearch-exit-other-end
    ()
  "Exit isearch, at the opposite end of the string."
  (interactive)
  (isearch-exit)
  (goto-char isearch-other-end))

(define-key isearch-mode-map
  [(control return)]
  #'isearch-exit-other-end)

(provide 'init-isearch)
;; init-isearch.el ends here
