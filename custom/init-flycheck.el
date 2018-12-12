(use-package flycheck
  :ensure t
  :defer t
  :config
  (setq flycheck-check-syntax-automatically '(save mode-enable)
        flycheck-checkers (delq 'emacs-lisp-checkdoc flycheck-checkers))
  (global-flycheck-mode nil))

;; (use-package flycheck-tip
;;   :ensure t
;;   :config
;;   (flycheck-tip-use-timer 'verbose))


(provide 'init-flycheck)
;; init-flycheck.el ends here
