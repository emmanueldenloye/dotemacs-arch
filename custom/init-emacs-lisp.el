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

;;;###autoload
(defun eod-eval-region-or-buffer (beg end)
  "Evaluate the current active region, otherwise evaluate the current buffer."
  (interactive "r")
  (if (use-region-p)
      (progn
        (eval-region beg end)
        (message "Evaluated region."))
    (progn
      (eval-buffer)
      (message "Evaluated buffer."))))

(with-eval-after-load
    (define-key emacs-lisp-mode-map (kbd "C-c C-c") 'eod-eval-region-or-buffer)
  (define-key lisp-interaction-mode-map (kbd "C-c C-c") 'eod-eval-region-or-buffer))

(provide 'init-emacs-lisp)
;; init-emacs-lisp.el ends here
