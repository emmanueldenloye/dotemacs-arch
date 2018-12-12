;; Make it so man pages have focus when I request them
(use-package man
  :defer t
  :init
  (setq Man-notify-method 'aggressive))

;; Make it so help windows have focus by default
(mapc
 (lambda
   (f)
   (ad-add-advice f
                  '(focus nil t
                          (advice .
                                  (lambda
                                    ()
                                    (when ad-do-it
                                      (unless
                                          (string= "*Help*"
                                                   (buffer-name
                                                    (current-buffer)))
                                        (other-window 1))))))
                  'around
                  0)
   (ad-activate f))
 '(apropos command-apropos apropos-command view-lossage
           describe-file describe-bindings describe-mode describe-syntax
           describe-function describe-variable describe-key))

(provide 'init-consistency)
;; init-consistency.el ends here
