(global-set-key (kbd "C-s") 'isearch-forward-regexp)
(global-set-key (kbd "C-r") 'isearch-backward-regexp)
(global-set-key (kbd "C-M-s") 'isearch-forward)
(global-set-key (kbd "C-M-r") 'isearch-backward)

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

(provide 'init-isearch)
;; init-isearch.el ends here
