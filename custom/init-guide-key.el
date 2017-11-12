(use-package guide-key
  :ensure t
  :diminish guide-key-mode
  :config
  (setq guide-key/guide-key-sequence '("C-c" "C-x 4" "C-x 5" "C-x r" "C-x n" "C-c h")
        guide-key/recursive-key-sequence-flag t
        guide-key/popup-window-position 'bottom
        guide-key/idle-delay 2)
  (guide-key-mode 1))

(provide 'init-guide-key)
;;; init-guide-key.el ends here
