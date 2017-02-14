(add-hook 'occur-mode-hook
          (lambda () (next-error-follow-minor-mode)))

;; This is just a nice convenience function, I could just isearch though.
(defun occur-at-point (beg end)
"Perform \\[occur] at point. If region is active, use the text within it."
  (interactive "r")
  (if (use-region-)p
      (occur (buffer-substring-no-properties beg end))
    (occur (thing-at-point 'sexp t))))

;;; Conveniences :)
(define-key occur-mode-map (kbd "n") 'occur-next)
(define-key occur-mode-map (kbd "p") 'occur-prev)

(provide 'init-occur)
;;; init-occur.el ends here
