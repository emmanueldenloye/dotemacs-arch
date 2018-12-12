(use-package yaml-mode
  :ensure t
  :config
  (defun eod-yaml-newline-and-indent ()
    (interactive)
    (define-key yaml-mode-map (kbd "C-m" 'newline-and-indent))))

(provide 'init-yaml)
;; init-yaml.el ends here
