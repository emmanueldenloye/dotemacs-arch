(use-package helm-pass
  :init
  (add-to-list 'load-path "/home/emmanuel/.emacs.d/elpa/helm-pass")
  :commands (helm-pass))

(use-package auth-password-store
  :init
  (add-to-list 'load-path "/home/emmanuel/.emacs.d/elpa/auth-password-store")
  :config
  (auth-pass-enable))

(provide 'init-pass)
;; init-pass.el ends here
