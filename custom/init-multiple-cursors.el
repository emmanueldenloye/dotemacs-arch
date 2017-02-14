(use-package multiple-cursors
  :defer t
  :ensure t
  :bind
  (("C->" . mc/mark-next-word-like-this)
   ("C-x C->" . mc/mark-next-like-this)
   ("C-<" . mc/mark-previous-word-like-this)
   ("C-x C-<" . mc/mark-previous-like-this)
   ("C-c C-<" . mc/mark-all-like-this)
   ("C-S-c C-S-c" . mc/edit-lines)))

(provide 'init-multiple-cursors)
;;; init-multiple-cursors.el ends here
