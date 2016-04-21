(use-package anzu
  :init
  (setq anzu-lighter "")
  (setq anzu-deactivate-region t)
  (setq anzu-search-threshold 1000)
  (setq anzu-replace-to-string-separator " => ")
  :bind (("M-%" . anzu-query-replace-regexp)
         ("C-M-%" . anzu-query-replace)))

(provide 'init-anzu)
;; init-anzu.el ends here
