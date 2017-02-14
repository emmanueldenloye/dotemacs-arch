(use-package key-chord
  :defer t
  :ensure t
  :config
  (let ((inhibit-message t))
    (key-chord-mode 1))
  (key-chord-define-global "'x" 'expand-abbrev)
  (key-chord-define-global "JJ" 'switch-to-previous-buffer)
  (key-chord-define-global "JK"
                           'switch-to-previous-buffer-other-window)
  (key-chord-define-global ";w" 'ace-window)
  (key-chord-define-global "]e" 'eval-expression)
  (key-chord-define-global ",+" 'er/expand-region)
  (key-chord-define-global ";d" 'eod-duplicate-line)
  (key-chord-define-global ";b" 'helm-mini)
  (key-chord-define-global ";f" 'helm-find-files)
  (key-chord-define-global "yy" 'helm-show-kill-ring)
  (key-chord-define-global ";," (lambda () (interactive) (insert "~")))
  ;; (key-chord-define-global ",." 'eod-insert-dollar)
  (key-chord-define-global ",." (lambda () (interactive) (insert "$")))
  (key-chord-define-global "jc" 'avy-goto-char-in-line)
  (key-chord-define-global "jj" 'avy-goto-char)
  (key-chord-define-global "jk" 'avy-goto-word-or-subword-1))

;; (require 'key-chord)

;; (global-set-key (kbd "C-x C-b") 'ibuffer)
;; (global-set-key (kbd "C-c m") 'mail)

(provide 'init-key-chord-mode)
;; init-key-chord-mode.el ends here
