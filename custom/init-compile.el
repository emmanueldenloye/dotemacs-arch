;; setup compilation-mode used by `compile` command

(use-package compile
  :ensure t
  :defer t
  :bind
  ("<f5>" . compile))

(use-package auto-compile
  :ensure t
  ;; :init (auto-compile-on-load-mode)
  )
(setq load-prefer-newer t)

(setq compilation-ask-about-save nil 	; Just save before compiling
      compilation-always-kill t		; Just kill old compile process before starting the new one
      compilation-scroll-output 'first-error) ;Automatically scroll to first
(defun prelude-makefile-mode-defaults ()
  (whitespace-toggle-options '(tabs))
  (setq indent-tabs-mode t))

(setq prelude-makefile-mode-hook 'prelude-makefile-mode-defaults)
(add-hook 'makefile-mode-hook    (lambda ()
				   (run-hooks 'prelude-makefile-mode-hook)))

(provide 'init-compile)
;; init-compile.el ends here
