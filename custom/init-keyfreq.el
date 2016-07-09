;;; package -- Commentary
;;; Commentary:
;;; Code:

(use-package keyfreq
  :ensure t
  :config
  (setq keyfreq-excluded-commands
        '(self-insert-commands
          abort-recursive-edit
          previous-line
          keyboard-quit
          next-line))
  (keyfreq-mode 1)
  (keyfreq-autosave-mode 1))

(provide 'init-keyfreq)
;;; init-keyfreq.el ends here
