(use-package helm
  :ensure t
  :init
  (dolist (boring-file-extension '(".dyn_hi" ".dyn_o" ".tags"))
    (add-to-list 'completion-ignored-extensions boring-file-extension))
  (when (executable-find "curl")
    ;; (setq helm-google-suggest-use-curl-p t)
    (setq helm-net-prefer-curl t))
  (when (executable-find "ack-grep")
    (setq helm-grep-default-command
          "ack grep -Hn --no-group --no-color %e %p %f"
          helm-grep-default-recurse-command
          "ack grep -H --no-group --no-color %e %p %f"))
  :bind (("C-x c" . nil)
         ("C-c h" . helm-command-prefix)
         ("C-x b" . helm-mini)
         ("M-x" . helm-M-x)
         ("C-c h w" . helm-swoop) ;I am going to have use this more often
         ("C-x C-f" . helm-find-files)
         ("C-c h o" . helm-occur)
         ("C-h SPC" . helm-all-mark-rings)
         ("C-c h x" . helm-register)
         ("C-c h h c" . helm-custom-directory-files)
         ("C-c h C-x (" . helm-execute-kmacro)
         ("C-c h C-c f" . helm-multi-files)
         ("C-c h h t" . helm-themes)
         ("C-c h f" . helm-recentf)
         ("C-c h TAB" . helm-lisp-completion-at-point)
         ("C-c h g" . helm-google-suggest)
         ("C-c h M-:" . helm-eval-expression-with-eldoc)
         ("C-c h C-b" . helm-bibtex)
         ("C-c h C-p" . helm-pass))
  :bind (:map minibuffer-local-map
              ("C-c C-l" . helm-minibuffer-history))
  :bind (:map shell-mode-map
              ("C-c C-l" . helm-comint-input-ring))
  :bind (:map helm-map
              ("C-'" . ace-jump-helm-line-execute-action)
              ("C-\"" . ace-jump-helm-line)
              ("<tab>" . helm-execute-persistent-action)
              ("TAB" . helm-execute-persistent-action)
              ("C-t" . transpose-chars)
              ("C-c C-p" . previous-history-element)
              ("C-c C-p" . next-history-element)
              ("C-c C-t" . helm-toggle-resplit-window)
              ("C-i" . helm-execute-persistent-action)
              ("C-z" . helm-select-action))
  :config
  ;; (defun helm-occur-or-multi-occur (arg)
  ;; (interactive "p")
  ;; (call-interactively
  ;;  (cond ((eq arg 4)
  ;;         #'helm-multi-occur)
  ;;    (t #'helm-occur))))
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
   helm-autoresize-max-height 40
   helm-autoresize-min-height 10
   helm-display-header-line t
   helm-locate-fuzzy-match t)
  (set-face-attribute 'helm-source-header nil :height 0.1)
  (add-to-list 'helm-sources-using-default-as-input 'helm-source-man-pages)
  (use-package helm-config)
  (use-package helm-firefox
    :ensure t)
  (use-package helm-swoop
    :ensure t
    :init
    (setq helm-c-source-swoop-match-functions
          '(helm-mm-exact-match helm-mm-match)
          helm-c-source-swoop-search-functions
          '(helm-mm-exact-search
            helm-mm-search
            helm-candidates-in-buffer-search-default-fn)))
  (use-package helm-descbinds
    :config
    (helm-descbinds-mode))
  (use-package helm-unicode))

(global-set-key
 (kbd "C-c h C-c a")
 'helm-applications)

(defun helm-applications ()
  (interactive)
  (helm
   :sources
   (helm-build-sync-source "Desktop Applications"
     :candidates
     (mapcar
      (lambda (key)
        (string-remove-suffix ".desktop" key))
      (split-string
       (shell-command-to-string
        "ls /usr/share/applications | grep -e '\.desktop$'")))
     :fuzzy-match t
     :action 'helm-application-open)
   :buffer "*helm desktop applications*"
   :full-frame t
   :candidate-number-limit 500))

(defun helm-application-open (name)
  (if (stringp name)
      (eod-start-process name)
    (error (format "%s is invalid" name))))

(defun helm-custom-directory-files ()
  "List the custom-directory files (the files in the folder ~/.emacs.d/custom/)."
  (interactive)
  (require 'find-lisp)
  (helm
   :sources
   (helm-build-sync-source "Custom Files"
     :candidates
     (mapcar
      (lambda (file) (cons (file-name-nondirectory file) file))
             (find-lisp-find-files "~/.emacs.d/custom/" ".*el$"))
     :fuzzy-match t
     :action 'find-file)
   :buffer "*helm custom files*"
   :candidate-number-limit 200))

(provide 'init-helm)
;;; init-helm.el ends here
