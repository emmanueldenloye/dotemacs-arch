(use-package elpy
  :ensure t
  :config
  (elpy-enable)
  (dolist (var '(("python2" . python-mode) ("python3" . python-mode)))
    (add-to-list 'interpreter-mode-alist var))
  (setq python-indent-guess-indent-offset nil))

(provide 'init-python)
;;; init-python.el ends here
