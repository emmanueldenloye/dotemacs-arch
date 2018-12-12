(defvar erc-autojoin-channels-alist)
(defvar my/erc-mode-hook)

(use-package erc
  :defer t
  :custom
  (user-login-name
   "emmanuel_erc"
   erc-autojoin-channels-alist
   '(("freenode.net" "#haskell"
      "#emacs"
      "#numerical-haskell"
      "#haskell-beginners")
     ;; ("freenode.net" "#okchat")
     ;; ("freenode.net" "#qutebrowser")
     ;; ("irc.oftc.net" "#pentadactyl")
     )
   erc-server-auto-reconnect t
   erc-server-reconnect-timeout 15)
  :init
  (when (fboundp 'erc-modules)
    (add-to-list 'erc-modules 'notifications))
  (add-to-list
   'load-path
   "~/elisp/erc-extras" t)
  (defun eod/erc-mode-defaults ()
    (smartparens-mode))
  :config
  ;; (setq erc-autoaway-idle-method 'irc)
  (eval-after-load 'erc
    '(progn
       (eod/erc-mode-defaults)

       (setq eod/erc-mode-hook 'eod/erc-mode-defaults)

       (add-hook 'erc-mode-hook (lambda ()
                                  (run-hooks 'eod/erc-mode-hook)))))
  (use-package tls
    :custom
    (tls-program '("openssl s_client -connect %h:%p -no_ssl2 -ign_eof
                                       -CAfile /home/ootput/.private/certs/CAs.pem
                                       -cert /home/ootput/.private/certs/nick.pem"
                     "gnutls-cli --priority secure256
                                 --x509cafile /home/ootput/.private/certs/CAs.pem
                                 --x509certfile /home/ootput/.private/certs/nick.pem -p %p %h"
                     "gnutls-cli --priority secure256 -p %p %h"))
    :after (erc))

  ; M-x start-irc
 (defun start-irc ()
   "Connect to IRC."
   (interactive)
   (erc-tls :server "irc.oftc.net" :port 6697
            :nick "emmanuel-erc" :full-name "emmanuel-erc")
   (erc :server "irc.freenode.net" :port 6667
        :nick "emmanuel" :full-name "emmanuel-erc")))


(provide 'init-erc)
;;; init-erc.el ends here
