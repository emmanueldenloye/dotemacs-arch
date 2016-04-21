(use-package recentf-ext
  :ensure t
  :config
  (recentf-mode)
  (setq
   recentf-max-menu-items 30
   recentf-max-saved-items 5000))

(provide 'init-recentf)
;; init-recentf.el ends here
