(use-package yasnippet
  :defer t
  :ensure t
  :init
  ;; (add-hook 'lisp-interaction-mode-hook (lambda () (yas-minor-mode -1)))
  (with-current-buffer "*scratch*"
    (fundamental-mode))
  (setq yas/root-directory
        (remove
         "/home/emmanuel/.emacs.d/elpa/haskell-snippets-20150612.1239/snippets"
         yas/root-directory))
  (add-hook 'lisp-interaction-mode-hook (lambda () (yas-minor-mode 1)))
  (add-hook 'prog-mode-hook (lambda () (yas-minor-mode 1)))
  (add-hook 'org-mode-hook (lambda () (yas-minor-mode 1)))
  (add-to-list 'yas-prompt-functions 'shk-yas/helm-prompt)
  :config
  (defadvice yas-insert-snippet (around yas-insert-snippet-with-ido activate)
    (let ((yas-prompt-functions '(yas-ido-prompt)))
      ad-do-it))
  ;; (yas-global-mode 1)
   ;THIS IS THE IMPORTANT CHANGE. NOW THE INIT TIMES ARE CONSISTENT (AS FAR AS I KNOW).
  )

(provide 'init-yasnippet)
;; init-yasnippet.el ends here
