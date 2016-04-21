(use-package prog-mode
  :config


  (add-hook 'prog-mode-hook 'goto-address-mode)

  ;; whenever you create useless whitespace, the whitespace is highlighted
  (add-hook 'prog-mode-hook
            (lambda () (interactive) (setq show-trailing-whitespace 1)))
  (use-package clean-aindent-mode
    :config
    (add-hook 'prog-mode-hook 'clean-aindent-mode)
    (add-hook 'prog-mode-hook 'flyspell-prog-mode)
    (add-hook 'prog-mode-hook 'highlight-numbers-mode)))

(use-package highlight-symbol
  :ensure t
  :config
  (highlight-symbol-nav-mode)
  (setq highlight-symbol-idle-delay 0.5
        highlight-symbol-on-navigation-p t))

(provide 'init-prog-mode)
;; init-prog-mode.el ends here
