;; (add-hook 'emacs-lisp-mode-hook (lambda () (highlight-symbol-mode -1)))
;; (add-hook 'ielm-mode-hook (lambda () (highlight-symbol-mode -1)))
;; (add-hook 'lisp-mode-hook (lambda () (highlight-symbol-mode -1)))
;; (add-hook 'lisp-interaction-mode-hook (lambda () (highlight-symbol-mode -1)))

(use-package elisp-slime-nav
  :init
  (dolist (hook '(emacs-lisp-mode-hook ielm-mode-hook))
    (add-hook hook 'turn-on-elisp-slime-nav-mode))
  (add-hook 'emacs-lisp-mode-hook 'turn-on-eldoc-mode)
  (add-hook 'lisp-interaction-mode-hook 'turn-on-eldoc-mode)
  (add-hook 'ielem-mode-hook 'turn-on-eldoc-mode))

(provide 'init-emacs-lisp)
;; init-emacs-lisp.el ends here
