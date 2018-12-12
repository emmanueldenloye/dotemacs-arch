(use-package helm-pass
  :ensure t
  :defer t
  :init
  (add-to-list 'load-path "~/.emacs.d/elpa/helm-pass")
  :commands (helm-pass))

(use-package auth-password-store
  :ensure t
  :defer t
  :init
  (add-to-list 'load-path "~/.emacs.d/elpa/auth-password-store")
  :config
  (auth-pass-enable))

(provide 'init-pass)
;; init-pass.el ends here
