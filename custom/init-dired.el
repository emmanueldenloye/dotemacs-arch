(setq
 dired-dwim-target t
 dired-recursive-copies 'always
 dired-recursive-deletes 'top
 dired-listing-switches "-lha"
 dired-omit-files "\\.dyn_hi$\\|\\.dyn_o$\\|\\.hi$\\|\\.o$|^\\.?#\\|^\\.$\\|^\\.\\.$|\\.aux$"  ; temporarily add .hi, .hs~
 )

;; automatically refresh dired buffer on changes
(add-hook 'dired-mode-hook 'auto-revert-mode)

;; if it is not Windows, use the following listing switches
(when (not (eq system-type 'windows-nt))
  (setq dired-listing-switches "-lha --group-directories-first"))

;; KEY BINDINGS.
;; (define-key ctl-x-4-map "\C-j" 'dired-jump-other-window)
;; (define-key dired-mode-map "\C-x\M-o" 'dired-omit-mode)
;; (define-key dired-mode-map "*O" 'dired-mark-omitted)
;; (define-key dired-mode-map "\M-(" 'dired-mark-sexp)
;; (define-key dired-mode-map "*(" 'dired-mark-sexp)
;; (define-key dired-mode-map "*)" 'dired-mark-extension)
;; (define-key dired-mode-map "\M-!" 'dired-smart-shell-command)
;; (define-key dired-mode-map "\M-G" 'dired-goto-subdir)
;; (define-key dired-mode-map "F" 'dired-do-find-marked-files)
;; (define-key dired-mode-map "Y" 'dired-do-relsymlink)
;; (define-key dired-mode-map "%Y" 'dired-do-relsymlink-regexp)
;; (define-key dired-mode-map "V" 'dired-do-run-mail)

(use-package dired-x
  :defer t
  :config
  (add-hook 'dired-mode-hook (lambda () (dired-omit-mode)))) ; provide extra commands dired

(use-package dired+
  :defer t)

(use-package wdired
  :defer t)

(setq
 wdired-allow-to-change-premissions t 	; allow to edit permission bits
 widred-allow-to-redirect-links		; allow to edit symlinks
 )


(add-hook 'dired-mode-hook 'diff-hl-dired-mode)

(provide 'init-dired)
;; init-dired.el ends here
