(use-package company
  :ensure t
  :defer t
  :pin melpa-stable
  :diminish company-mode
  :init
  (setq company-idle-delay 0.5
        company-selection-wrap-around t)
  :config
  (add-to-list 'company-backends 'company-ispell)
  (company-mode 1)
  (add-hook 'after-init-hook 'global-company-mode))

(provide 'init-company)
;; init-company.el ends here
