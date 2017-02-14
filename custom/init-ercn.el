;; (use-package ercn
;;   :if erc
;;   :ensure t
;;   :init
;;   (setq ercn-notify-rules
;;         '((current-nick . all)
;;           (keyword . all)
;;           (pal . ("#emacs"
;;                   "#/r/ADHD"
;;                   "#haskell-beginners"
;;                   "#numerical-haskell"
;;                   "#haskell"))
;;           (query-buffer . all)))
;;   (defun ercn-do-notify (nickname message)
;;     ;; notification code goes here
;;     )
;;   (add-hook 'ercn-notify-hook 'ercn-do-notify))

;; (add-hook 'erc-server-PRIVMSG-functions 'erc-notifications-PRIVMSG)
;; (add-hook 'erc-text-matched-hook 'erc-notifications-notify-on-match)


(provide 'init-ercn)
;; init-ercn.el ends here
