(use-package helm
  :ensure t
  :demand t
  :init
  (dolist
      (boring-file-extension
       '(".dyn_hi" ".dyn_o" ".tags"))
    (add-to-list 'completion-ignored-extensions boring-file-extension))
  (when
      (executable-find "curl")
    ;; (setq helm-google-suggest-use-curl-p t)
    (setq helm-net-prefer-curl t))
  (when
      (executable-find "ack-grep")
    (setq helm-grep-default-command
          "ack grep -hn --no-group --no-color %e %p %f"
          helm-grep-default-recurse-command
          "ack grep -h --no-group --no-color %e %p %f"))
  :bind
  (("C-x b" . helm-mini)
   ("C-x C-f" . helm-find-files)
   ("M-x" . helm-M-x))
  :bind
  (:map minibuffer-local-map
        ("C-c C-l" . helm-minibuffer-history))
  :bind
  (:map helm-map
        ("C-'" . ace-jump-helm-line-execute-action)
        ("C-\"" . ace-jump-helm-line)
        ("<tab>" . helm-execute-persistent-action)
        ("<tab>" . helm-execute-persistent-action)
        ("C-t" . transpose-chars)
        ("C-c C-p" . previous-history-element)
        ("C-c C-n" . next-history-element)
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
  (global-set-key (kbd "C-c h") 'helm-command-prefix)
  (global-unset-key (kbd "C-x c"))
  (add-hook
   'eshell-mode-hook
   #'(lambda
       ()
       (define-key
         eshell-mode-map
         (kbd "C-c C-l")
         'helm-eshell-history)))
  (helm-autoresize-mode t)
  (setq
   helm-split-window-in-side-p t ;open helm buffer inside current window,
                                        ;not occupy whole other window
   helm-move-to-line-cycle-in-source t ;move to end or beginning of source when
                                        ;reaching top or bottom of source.
   helm-ff-search-libray-in-sexp t ; search for library in `require' and
                                        ;`declare-function' sexp.
   helm-scroll-amount t            ; scroll 8 lines other window using
                                        ;m-<next>/m-<prior>
   helm-ff-file-name-history-use-recentf t
   helm-m-x-fuzzy-match t
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
  (set-face-attribute 'helm-source-header nil :height 0.7)
  ;; (add-to-list 'helm-completing-read-handlers-alist
  ;;              '(find-file . helm-completing-read-default-1))
  (add-to-list 'helm-sources-using-default-as-input 'helm-source-man-pages)
  (require 'helm-config)
  (require' helm-firefox)
  (progn
    (require 'helm-swoop)
    (setq helm-c-source-swoop-match-functions
          '(helm-mm-exact-match helm-mm-match)
          helm-c-source-swoop-search-functions
          '(helm-mm-exact-search
            helm-mm-search
            helm-candidates-in-buffer-search-default-fn)))
  (progn
    (require 'helm-descbinds)
    (setq helm-descbinds-window-style 'split-window)
    (helm-descbinds-mode 1))
  (require 'helm-unicode))


(defun helm-applications
    ()
  (interactive)
  (helm
   :sources
   (helm-build-sync-source "Desktop Applications"
     :candidates
     (mapcar
      (lambda
        (key)
        (string-remove-suffix ".desktop" key))
      (split-string
       (shell-command-to-string
        "ls /usr/share/applications | grep -e '\.desktop$'")))
     :fuzzy-match t
     :action 'helm-application-open)
   :buffer "*helm desktop applications*"
   :full-frame t
   :candidate-number-limit 500))

(defun helm-application-open
    (name)
  (if
      (stringp name)
      (eod-start-process name)
    (error
     (format "%s is invalid" name))))

(defun helm-custom-directory-files
    ()
  "List the custom-directory files (the files in the folder ~/.emacs.d/custom/)."
  (interactive)
  (require 'find-lisp)
  (helm
   :sources
   (helm-build-sync-source "Custom Files"
     :candidates
     (cons "~/.emacs.d/custom/"
           (mapcar
            (lambda
              (file)
              (cons
               (file-name-nondirectory file)
               file))
            (find-lisp-find-files "~/.emacs.d/custom/" ".*el$")))
     :fuzzy-match t
     :action (helm-make-actions
              "Find file"  'find-file
              "Find file in other window"  'find-file-other-window
              "Find file in other frame"  'find-file-other-frame))
   :buffer "*helm custom files*"
   :candidate-number-limit 200))

(with-eval-after-load 'helm-mode
  (mapcar
   (lambda
     (x)
     (global-set-key
       (kbd
        (concat "C-c h " (car x)))
       (cdr x)))
   '(("w" . helm-swoop)
     ("o" . helm-occur)
     ("C-h SPC" . helm-all-mark-rings)
     ("x" . helm-register)
     ("h c" . helm-custom-directory-files)
     ("C-x (" . helm-execute-kmacro)
     ("C-c f" . helm-multi-files)
     ("h t" . helm-themes)
     ("f" . helm-recentf)
     ("TAB" . helm-lisp-completion-at-point)
     ("g" . helm-google-suggest)
     ("M-:" . helm-eval-expression-with-eldoc)
     ("C-b" . helm-bibtex)
     ("C-c a" . helm-applications)
     ("C-p" . helm-pass))))

(with-eval-after-load 'shell-mode
  (define-key shell-mode-map
    (kbd "C-c C-l" )
    'helm-comint-input-ring))

(provide 'init-helm)

;;; init-helm.el ends here
