(use-package js2-mode
  :ensure t
  :defer t
  :mode ("\\.js\\'" . js2-mode)
  :init (add-hook 'js2-mode-hook #'js2-imenu-extras-mode))

(use-package xref-js2
  :ensure t)

(use-package js2-refactor
  :ensure t
  :defer t
  :init (add-hook 'js2-mode-hook #'js2-refactor-mode)
  :config
  (js2r-add-keybindings-with-prefix "C-c C-r")
  (define-key js2-mode-map (kbd "C-k") #'js2r-kill)
  (define-key js2-mode-map (kbd "M-.") nil)
  (add-hook 'js2-mode-hook
            (lambda ()
              (add-hook 'xref-backend-functions #'xref-js2-xref-backend nil t))))

(provide 'init-js2)
;; init-js2.el ends here.
