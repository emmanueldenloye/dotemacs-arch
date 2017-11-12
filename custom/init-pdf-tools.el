(use-package pdf-tools
  :ensure t
  :config
  (add-hook 'doc-view-mode-hook
            (lambda ()
              (pdf-tools-install))))

(provide 'init-pdf-tools)
;; init-pdf-tools.el ends here
