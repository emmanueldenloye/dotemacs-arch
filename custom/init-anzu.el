(use-package anzu
  :init
  (setq anzu-lighter ""
        anzu-deactivate-region t
        anzu-search-threshold 1000
        anzu-replace-to-string-separator " => ")
  :bind (("M-%" . anzu-query-replace-regexp)
         ("C-M-%" . anzu-query-replace)))

(provide 'init-anzu)
;; init-anzu.el ends here
