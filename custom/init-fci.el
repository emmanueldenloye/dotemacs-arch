;; (use-package fill-column-indicator
;;   :config
;;   (setq fci-rule-width 1)
;;   (define-globalized-minor-mode global-fci-mode fci-mode
;;     (lambda () (if (and
;;                     (not (string-match "^*.*\*$" (buffer-name)))
;;                     (not (member major-mode
;;                                  '(dired-mode
;;                                    undo-tree-visualizer-mode
;;                                    erc-mode
;;                                    org-mode))))
;;                    (fci-mode 1))))
;;   (global-fci-mode 1)
;;   (setq fci-rule-color "pink"))

(provide  'init-fci)
;;; init-fci.el ends here
