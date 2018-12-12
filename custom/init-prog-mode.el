(use-package prog-mode
  :config
  (add-hook 'prog-mode-hook 'goto-address-mode)

  ;; whenever you create useless whitespace, the whitespace is highlighted
  (setq show-trailing-whitespace nil)
  (defun eod-enable/disable-whitespace ()
    (interactive)
    (if show-trailing-whitespace
        (setq show-trailing-whitespace nil)
      (setq show-trailing-whitespace t)))
  (defun prog-comments-auto-fill-mode ()
    (set (make-local-variable 'comment-auto-fill-only-comments) t))

  (add-hook 'prog-mode-hook
            #'eod-enable/disable-whitespace)
  (add-hook 'prog-mode-hook
            #'prog-comments-auto-fill-mode)
  (remove-hook 'prog-mode-hook
            #'flyspell-prog-mode)

  ;; (use-package clean-aindent-mode
  ;;   :diminish t
  ;;   :config
  ;;   (add-hook 'prog-mode-hook 'clean-aindent-mode)
  ;;   (add-hook 'prog-mode-hook 'flyspell-prog-mode)
  ;;   (add-hook 'prog-mode-hook 'highlight-numbers-mode))
  )

(use-package highlight-symbol
  :ensure t
  :diminish t
  :config
  ;; (highlight-symbol-nav-mode)
  (setq highlight-symbol-idle-delay 0.5
        highlight-symbol-on-navigation-p t))

(provide 'init-prog-mode)
;; init-prog-mode.el ends here
