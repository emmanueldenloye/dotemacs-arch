(use-package yasnippet
  :ensure t
  :config
  (yas-global-mode 1)
  (add-to-list 'yas-prompt-functions 'shk-yas/helm-prompt))

(provide 'init-yasnippet)
;; init-yasnippet.el ends here
