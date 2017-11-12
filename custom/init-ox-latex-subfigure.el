(use-package ox-latex-subfigure
  :init
  (setq org-latex-table-caption-above nil
        org-latex-prefer-user-labels t)
  :load-path "~/.emacs.d/otherlibs/"
  :config (require 'ox-latex-subfigure))

(provide 'init-ox-latex-subfigure)
;; init-ox-latex-subfigure.el ends here
