(use-package cursor-chg
  :ensure t
  :config
  (toggle-cursor-type-when-idle 1)
  (change-cursor-mode 1)
  (curchg-change-cursor-to-idle-type))

(provide 'init-cursor-chg)
;;; init-cursor-chg.el ends here
