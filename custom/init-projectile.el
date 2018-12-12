(use-package projectile
  :ensure t
  :config
  (projectile-mode)
  (setq projectile-enable-caching t
        projectile-completion-system 'default))

(use-package helm-projectile
  :ensure t
  :defer t
  :if helm-mode
  :config
  (helm-projectile-on))

(provide 'init-projectile)
;; init-projectile.el ends here
