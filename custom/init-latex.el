
(use-package page-break-lines
  :config
  (dolist (hook '(org-mode-hook LaTeX-mode-hook))
    (add-hook hook 'page-break-lines-mode)))

(provide 'init-latex)
;; init-latex.el ends here
