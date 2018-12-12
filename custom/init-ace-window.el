(use-package ace-window
  :ensure t
  :defer t
  :init
  (setq aw-keys '(?a ?o ?e ?u ?i ?d ?h ?t ?n ?s)
        aw-dispatch-always t
        aw-background nil)
  (defvar aw-dispatch-alist
    '((?x aw-delete-window "Ace - Delete Window")
      (?m aw-swap-window " Ace - Swap Window")
      (?f aw-flip-window)
      (?v aw-split-window-vert " Ace - Split Vert Window")
      (?b aw-split-window-horz " Ace - Split Horz Window")
      (?c delete-other-windows " Ace - Maximize Window")
      (?C delete-other-windows))
    "List of actions for `aw-dispatch-default'."))

(provide 'init-ace-window)
;; init-ace-window.el ends here
