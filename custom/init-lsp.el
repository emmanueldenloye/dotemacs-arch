(use-package lsp-mode
  :ensure t
  :defer t)

(use-package lsp-ui
  :ensure t
  :defer t)

(use-package lsp-haskell
  :ensure t
  :defer t)

(add-hook 'lsp-mode-hook 'lsp-ui-mode)
;; (add-hook 'haskell-mode-hook #'lsp-haskell-enable)

(provide 'init-lsp)
;; init-lsp.el ends here
