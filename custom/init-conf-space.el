;;; set up smartparens for config files
(add-hook 'conf-space-mode-hook 'turn-on-smartparens-mode)
(remove-hook 'conf-space-mode-hook #'disable-paredit-mode)

(provide 'init-conf-space)
;; init-conf-space.el ends here
