(use-package helm
  :ensure t
  :init
  (when (executable-find "curl")
    (setq helm-google-suggest-use-curl-p t))
  (when (executable-find "ack-grep")
    (setq helm-grep-default-command
          "ack grep -Hn --no-group --no-color %e %p %f"
          helm-grep-default-recurse-command
          "ack grep -H --no-group --no-color %e %p %f"))
  :bind (("M-x" . helm-M-x)
         ("C-x C-f" . helm-find-files))
  :bind (:map helm-map
              ("C-'" . ace-jump-helm-line-execute-action)
              ("C-\"" . ace-jump-helm-line))
  :bind (:map minibuffer-local-map
              ("C-c C-l" . helm-minibuffer-history))
  :config (bind-keys :map helm-map
                     ("<tab>" . helm-execute-persistent-action)
                     ("C-i" . helm-execute-persistent-action)
                     ("C-z" . helm-select-action)
                     ("C-c h" . helm-command-prefix-key))
  (add-hook
   'eshell-mode-hook
   #'(lambda ()
       (define-key
         eshell-mode-map
         (kbd "C-c C-l") 'helm-eshell-history)))
  (helm-mode 1)
  (helm-autoresize-mode t)
  (setq
   helm-split-window-in-side-p t ;open helm buffer inside current window,
                                        ;not occupy whole other window
   helm-move-to-line-cycle-in-source t ;move to end or beginning of source when
                                        ;reaching top or bottom of source.
   helm-ff-search-libray-in-sexp t ; search for library in `require' and
                                        ;`declare-function' sexp.
   helm-scroll-amount t            ; scroll 8 lines other window using
                                        ;M-<next>/M-<prior>
   helm-ff-file-name-history-use-recentf t
   helm-M-x-fuzzy-match t
   helm-buffers-fuzzy-matching t
   helm-recentf-fuzzy-match t
   helm-semantic-fuzzy-match t
   helm-imenu-fuzzy-match t
   helm-file-cache-fuzzy-match t
   helm-lisp-fuzzy-completion t
   helm-apropos-fuzzy-match t
   helm-completion-in-region-fuzzy-match t
   helm-mode-fuzzy-match t
   helm-input-idle-delay 0.02
   helm-candidate-number-limit 100
   helm-autoresize-max-height 30
   helm-autoresize-min-height 30
   helm-display-header-line nil
   helm-locate-fuzzy-match t)
  (set-face-attribute 'helm-source-header nil :height 0.1)
  (add-to-list 'helm-sources-using-default-as-input 'helm-source-man-pages)
  (add-to-list
   'helm-boring-file-regexp-list
   '("\\.dyn_hi$" "\\.dyn_o$" "\\.hi$" "\\.o$" "\\.tags$" "^\\.ghci"))
  (use-package helm-config)
  (use-package helm-descbinds
    :config
    (helm-descbinds-mode))
  (use-package helm-unicode))

(define-key shell-mode-map (kbd "C-c C-l") 'helm-comint-input-ring)
(global-unset-key (kbd "C-x c"))
(global-set-key (kbd "C-c h") 'helm-command-prefix)
(global-set-key (kbd "C-x b") 'helm-mini)
(global-set-key (kbd "C-c h o") 'helm-occur)
(global-set-key (kbd "C-h SPC") 'helm-all-mark-rings)
(global-set-key (kbd "C-c h x") 'helm-register)
(global-set-key (kbd "C-c h g") 'helm-google-suggest)
(global-set-key (kbd "C-c h M-:") 'helm-eval-expression-with-eldoc)
(global-set-key (kbd "C-c h C-b") 'helm-bibtex)

(provide 'init-helm)
;;; init-helm.el ends here
