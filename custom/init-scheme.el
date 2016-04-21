;; ;;; package --- Summary
;; ;;; Commentary:
;; ;;; Code:

;; (setq scheme-program-name "/usr/bin/scm")
(setq scheme-program-name "/usr/bin/guile")

;;; Also highlight parens
(add-hook 'scheme-mode-hook
          (lambda ()  (setq show-parens-delay 0
                       show-paren-style 'parenthesis)))

(add-hook 'scheme-mode-hook 'enable-paredit-mode)
(add-hook 'inferior-scheme-mode-hook 'enable-paredit-mode)

(provide 'init-scheme)
;; ;;; init-scheme.el ends here
