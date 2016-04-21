(use-package flycheck
  :ensure t
  :config
  (setq flycheck-check-syntax-automatically '(save-mode-enabled))
  (setq flycheck-checkers (delq 'emacs-lisp-checkdoc flycheck-checkers))
  (global-flycheck-mode t))

(use-package flycheck-tip
  :ensure t
  :config
  (flycheck-tip-use-timer 'verbose))


(provide 'init-flycheck)
;; init-flycheck.el ends here
