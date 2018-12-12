(use-package avy
  :defer t
  :ensure t
  :init
  (setq avy-keys '(?a ?o ?e ?u ?i ?d ?h ?t ?n ?s)
        avy-all-windows t
        avy-background t
        avy-style 'at
        avy-styles-alist
        '((avy-goto-char-2 . post)
          (avy-goto-line . at-full))
        avy-dispatch-alist
        '((?c . avy-action-copy)
          (?m . avy-action-mark)
          (?l . avy-action-teleport)
          (?k . avy-action-kill-stay)
          (?K . avy-action-kill-move)
          (?z . avy-action-ispell)))
  :bind
  (("C-c a c" . avy-copy-line)
   ("C-C a p" . avy-pop-mark)
   ("C-c a m" . avy-move-line)
   ("C-c a M" . avy-move-region)
   ("C-c a t" . avy-goto-char-timer)
   ("C-c a r" . avy-copy-region)
   ("C-c a s" . avy-isearch)
   ("M-g g" . avy-goto-line)
   ("M-g M-g" . avy-goto-line))
  :config
  (avy-setup-default)
  (defadvice avy-goto-line (after avy-goto-line-indentation activate)
    (when (derived-mode-p 'prog-mode 'org-mode)
      (back-to-indentation)))
  (defadvice avy-copy-line (after avy-back-indentation activate)
    (when (derived-mode-p 'prog-mode 'org-mode)
        (back-to-indentation))))

(provide 'init-avy)
;; init-avy.el ends here
