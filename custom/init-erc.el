(use-package erc
  :defer t
  :init
  (add-to-list
   'load-path
   "~/elisp/erc-extras" t)
  (defvar erc-autojoin-channels-alist)
  (defvar my/erc-mode-hook)
  (setq
   user-login-name
   "emmanuel_erc")
  (setq
   erc-autojoin-channels-alist
   '(("freenode.net" "#haskell"
      "#emacs"
      "#numerical-haskell"
      "#haskell-beginners")
     ;; ("freenode.net" "#okchat")
     ;; ("freenode.net" "#qutebrowser")
     ;; ("irc.oftc.net" "#pentadactyl")
     ))
  (defun eod/erc-mode-defaults ()
    (smartparens-mode))
  :config

  ;; (setq erc-autoaway-idle-method 'irc)
  (eval-after-load 'erc
    '(progn
       (eod/erc-mode-defaults)

       (setq eod/erc-mode-hook 'eod/erc-mode-defaults)

       (add-hook 'erc-mode-hook (lambda ()
                                  (run-hooks 'eod/erc-mode-hook))))))

(provide 'init-erc)
;;; init-erc.el ends here
