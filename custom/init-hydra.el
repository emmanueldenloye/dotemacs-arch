(use-package hydra
  :ensure t
  :config
  (global-set-key
   (kbd "C-x t")
   (defhydra toggle (:color blue)
     "toggle"
     ("a" abbrev-mode "abbrev")
     ("s" flyspell-mode "flyspell")
     ("d" toggle-debug-on-error "debug")
     ("c" fci-mode "fCi")
     ("t" toggle-truncate-lines "truncate")
     ("w" whitespace-mode "whitespace")
     ("q" nil "cancel"))))

(provide 'init-hydra)
;;; init-hydra.el ends here
