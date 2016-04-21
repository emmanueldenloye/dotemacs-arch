;;; package --- Summary
;;; Commentary:
;;; Code:

(add-hook 'prog-mode-hook #'rainbow-delimiters-mode)

(add-hook 'emacs-lisp-mode-hook #'rainbow-delimiters-mode)

(add-hook 'org-mode-hook #'rainbow-delimiters-mode)

(provide 'init-rainbow-delimiters)
;;; init-rainbow-delimiters ends here
