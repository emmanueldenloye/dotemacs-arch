(use-package session
  :ensure t
  :config
  (session-jump-to-last-change)
  (add-hook 'after-init-hook 'session-initialize)
  (add-to-list 'session-globals-exclude 'org-mark-ring)
  (add-hook 'delete-frame-functions
            (lambda (frame)
              (session-save-session t))))

(provide 'init-session)
;; init-session.el ends here
