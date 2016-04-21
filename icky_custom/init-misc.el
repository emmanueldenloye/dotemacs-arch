;;; package --- Summary
;;; Commentary:
;;; Code:

(require-package 'undo-tree)
(setq undo-tree-auto-save-history t)
(setq undo-tree-auto-history-directory-alist
      `(("." .,(concat dotemacs-cache-directory "undo"))))
(setq undo-tree-visualizer-timestamps t)
(setq undo-tree-visualizer-diff t)
(global-undo-tree-mode)

(require-package 'multiple-cursors)
(after 'evil
       (add-hook 'multiple-cursors-mode-enabled-hook #'evil-emacs-state)
       (add-hook 'multiple-cursors-mode-disabled-hook #'evil-normal-state))


(require-package 'wgrep)

(when (executable-find "pt")
  (require-package 'pt)
  (require-package 'wgrep-pt))

(when (executable-find "ag")
  (require-package 'ag)
  (setq ag-highlight-search t)
  (add-hook 'ag-mode-hook (lambda () (toggle-truncate-lines t)))
  (require-package 'wgrep-ag))

(require-package 'project-explorer)
(after 'project-explorer
       (setq pe/cache-directory (concat dotemacs-cache-directory "project-explorer"))
       (setq pe/omit-regex (concat pe/omit-regex "\\|^node_modules$")))


(require-package 'avy)

(require-package 'expand-region)

(require-package 'editorconfig)
(require 'editorconfig)


(require-package 'aggressive-indent)
(require 'aggressive-indent)
(add-to-list 'aggressive-indent-excluded-modes #'stylus-mode)
(add-to-list 'aggressive-indent-excluded-modes #'org-mode)
(add-to-list 'aggressive-indent-excluded-modes #'vimrc-mode)
(add-to-list 'aggressive-indent-excluded-modes #'haskell-mode)
(global-aggressive-indent-mode)

(require-package 'rainbow-delimiters)
(add-hook 'prog-mode-hook #'rainbow-delimiters-mode)

(require-package 'framemove)
(require 'framemove)
(setq framemove-hook-into-windmove t)

(require-package 'discover-my-major)

(require-package 'paradox)
(setq paradox-execute-asynchronously nil)

(require-package 'vlf)
(setq vlf-application 'dont-ask)
(require 'vlf-setup)

(provide 'init-misc)
;;; init-misc ends here

;; (global-auto-revert-mode)
;; (global-hl-line-mode)
;; (add-hook 'after-init-hook 'global-company-mode)
