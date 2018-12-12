(use-package diff-mode
  :defer t
  :init
  (setq ediff-diff-options "-w"
        ediff-split-window-function 'split-window-horizontally
        ediff-window-setup-function 'ediff-setup-windows-plain)
  :config
  (add-hook 'diff-mode-hook
            (lambda
              ()
              (setq-local whitespace-style
                          '(face
                            tabs
                            tab-mark
                            spaces
                            space-mark
                            trailing
                            indentation::space
                            indentation::tab
                            newline
                            newline-mark))
              (whitespace-mode 1)))
  (add-hook 'dired-mode-hook 'diff-hl-dired-mode)
  (global-diff-hl-mode))

(provide 'init-diff-mode)
;; init-diff-mode.el ends here
