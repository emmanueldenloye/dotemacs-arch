(use-package key-chord
  :ensure t
  :init
  (key-chord-mode 1)
  :config
  (key-chord-define-global ",x" 'helm-M-x)
  (key-chord-define-global "'x" 'expand-abbrev)
  (key-chord-define-global "JJ" 'switch-to-previous-buffer)
  (key-chord-define-global "JK"
                           'switch-to-previous-buffer-other-window)
  (key-chord-define-global ";w" 'ace-window)
  (key-chord-define-global "uu" 'undo-tree-visualize)
  (key-chord-define-global ";d" 'duplicate-thing)
  (key-chord-define-global ";b" 'helm-mini)
  (key-chord-define-global ";f" 'helm-find-files)
  (key-chord-define-global "yy" 'browse-kill-ring)
  (key-chord-define-global ";," (lambda () (interactive) (insert "~")))
  (key-chord-define-global ",." 'eod-insert-dollar)
  (key-chord-define-global "jc" 'avy-goto-char-in-line)
  (key-chord-define-global "jj" 'avy-goto-char)
  (key-chord-define-global "jk" 'avy-goto-word-or-subword-1))

;; (require 'key-chord)

;; (global-set-key (kbd "C-x C-b") 'ibuffer)
;; (global-set-key (kbd "C-c m") 'mail)

(provide 'init-key-chord-mode)
;; init-key-chord-mode.el ends here
