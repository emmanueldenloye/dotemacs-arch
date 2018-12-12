(use-package bibtex
  :defer t
  :config
  (define-key bibtex-mode-map
    (kbd "C-;")
    #'crossref-lookup)
  (defun bibtex-validate-sort
      ()
    (let
        ((bibtex-maintain-sorted-entries t))
      (bibtex-validate)))
  (use-package company-bibtex
    :defer t
    :ensure t
    :config
    (setq company-bibtex-org-citation-regex "-?@") ;This is really cool.
    ))

(provide 'init-bibtex)
;; init-bibtex.el ends here
