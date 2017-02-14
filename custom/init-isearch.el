
(global-set-key (kbd "C-s") 'isearch-forward-regexp)
(global-set-key (kbd "C-r") 'isearch-backward-regexp)
(global-set-key (kbd "C-M-s") 'isearch-forward)
(global-set-key (kbd "C-M-r") 'isearch-backward)

;;;###autoload
(defun isearch-apropos ()
  (interactive)
  (apropos isearch-string))

(define-key isearch-mode-map (kbd "M-s a") 'isearch-apropos)

;;;###autoload
(defun isearch-forward-other-window (arg)
  "Isearch in the other window."
  (interactive "p")
  (save-selected-window
    (other-window 1)
    (if (= 4 arg)
        (isearch-forward)
      (isearch-forward-regexp))))

;;;###autoload
(defun isearch-backward-other-window (arg)
  "Isearch in the other window."
  (interactive "p")
  (save-selected-window
    (other-window 1)
    (if (= 4 arg)
        (isearch-backward)
      (isearch-backward-regexp))))

;;;###autoload
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

(define-key isearch-mode-map (kbd "M-s C-M-f") 'isearch-yank-sexp)

;;;###autoload
(defun isearch-yank-sexp (&optional arg)
  "Pull the next sexp from buffer into search string.
If optional ARG is non-nil, pull in the next ARG sexps."
  (interactive "p")
  (isearch-yank-internal (lambda () (forward-sexp arg) (point))))

(provide 'init-isearch)
;; init-isearch.el ends here
