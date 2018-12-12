(use-package elpy
  :ensure t
  :diminish t
  :config
  (elpy-enable)
  (dolist (var '(("python2" . python-mode) ("python3" . python-mode)))
    (add-to-list 'interpreter-mode-alist var))
  (setq python-indent-guess-indent-offset nil)
  (defun python-insert-none-or-pass (&optional insert-pass)
    (interactive "P")
    (insert (if insert-pass "pass" "None")))
  (define-key python-mode-map (kbd "C-c C-u") 'python-insert-none-or-pass)
  (defadvice python-nav-up-list (around nav-up-string (arg))
    "Navigate up a string"
    (if (memq (python-syntax-context-type) '(string))
        (progn
          (while (not (looking-at (rx (any "'\"")))) (backward-char))
          (unless (= (abs arg) 1)
              (python-nav-up-list (1- arg))))
      ad-do-it))
  (ad-activate 'python-nav-up-list))

;; (use-package ein
;;   :ensure t
;;   :init
;;   (use-package ein-loaddefs)
;;   (use-package ein-notebook)
;;   (use-package ein-subpackages))


(provide 'init-python)
;;; init-python.el ends here
