(use-package smartparens
  :ensure t
  :config
  (use-package smartparens-config
    :config
    (setq sp-base-key-bindings 'paredit
          sp-autoskip-closing-pair 'always
          sp-hybrid-kill-entire-symbol nil)
    ;; (add-to-list 'sp-navigate-consider-stringlike-sexp 'haskell-mode)
    (sp-use-paredit-bindings))
  :bind
  (:map smartparens-mode-map
        ("M-R" . sp-rewrap-sexp)
        ("M-J" . sp-join-sexp)
        ("M-N" . sp-next-sexp)
        ("C-c t" . sp-prefix-pair-object)
        ("M-P" . sp-previous-sexp)))
;; (diminish 'smartparens-mode)

(provide 'init-smartparens)
;; init-smartparens.el ends here
