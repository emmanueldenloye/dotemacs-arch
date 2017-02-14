(use-package company
  :ensure t
  :pin melpa-stable
  :diminish company-mode
  :config
  (setq company-idle-delay 0.5
        company-selection-wrap-around t)
  (company-mode 1))

(add-hook 'after-init-hook 'global-company-mode)

(provide 'init-company)
;; init-company.el ends here
