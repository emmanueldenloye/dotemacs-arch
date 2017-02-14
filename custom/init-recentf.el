(use-package recentf-ext
  :defer t
  :ensure t
  :config
  (recentf-mode)
  (setq
   recentf-max 100
   recentf-max-menu-items 30
   recentf-max-saved-items 5000))

;;; I am not sure if I need this, but whatever.
;; (defun recentf-push-current-buffer ()
;;   "Push the current buffer on the `recentf-list'."
;;   (let ((filename (buffer-file-name)))
;;     (when (file-exists-p filename)
;;       (recentf-push filename))))

(provide 'init-recentf)
;; init-recentf.el ends here
