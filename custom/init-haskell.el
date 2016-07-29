;;; package --- Summary
;;; Commentary:
;;; Code:
(use-package haskell-interactive-mode
  :init
  (add-hook 'haskell-interactive-mode-hook 'turn-on-smartparens-mode)
  (add-hook 'haskell-interactive-mode-hook 'subword-mode)
  (add-hook 'haskell-interactive-mode-hook 'rainbow-delimiters-mode))
(use-package haskell-mode
  :ensure t
  :init
  (autoload 'ghc-init "ghc" nil t)
  (autoload 'ghc-debug "ghc" nil t)
  :config
  (let ((my-cabal-path (expand-file-name "~/.cabal/bin")))
    (setenv "PATH" (concat my-cabal-path path-separator (getenv "PATH")))
    (add-to-list 'exec-path my-cabal-path))
  (setq ghc-ghc-options '("-idir1" "-idir2"))
  (add-hook 'haskell-mode-hook (lambda ()
                                 (setq )))
  (add-hook 'haskell-mode-hook (lambda () (ghc-init)))
  (add-hook 'haskell-mode-hook 'haskell-auto-insert-module-template)
  (add-hook 'haskell-mode-hook 'subword-mode)
  (add-hook 'haskell-mode-hook 'haskell-doc-mode)
  (add-hook 'haskell-mode-hook 'auto-fill-mode)
  (add-hook 'haskell-mode-hook 'turn-on-haskell-decl-scan)
  (add-hook 'haskell-mode-hook 'turn-on-smartparens-mode)
  (add-hook 'haskell-mode-hook 'rainbow-delimiters-mode-enable)
  (add-hook 'haskell-mode-hook 'company-mode)
  (add-hook 'haskell-mode-hook #'hindent-mode)
  (add-hook 'haskell-mode-hook 'interactive-haskell-mode)
  (add-hook 'haskell-mode-hook (lambda () (setq-local company-idle-delay 1)))
  (bind-keys :map haskell-mode-map
             ("C-c C-c" . haskell-compile)
             ("C-c C-l" . haskell-process-load-or-reload)
             ("C-c C-z" . haskell-interactive-switch)
             ("C-c C-n C-t" . haskell-process-do-type)
             ("C-c C-n C-i" . haskell-process-do-info)
             ("C-c C-n C-c" . haskell-cabal-build)
             ("C-c C-n C-p C-c" . haskell-process-cabal-build)
             ("C-c C-n c" . haskell-process-cabal)
             ("C-c C-o" . haskell-compile)
             ("C-c C-d" . inferior-haskell-send-decl)
             ("C-c C-." . haskell-format-imports)
             ("C-c ." . haskell-indent-align-guards-and-rhs)
             ("C-c ?" . helm-ghc-errors)
             ("C-c C-u" . eod-haskell-mode-insert-undefined-at-point)
             ("M-s" . haskell-sp-splice-sexp)
             ("M-(" . haskell-wrap-with-paren-pair-and-fix-indent)
             ("C-c M-t" . ghc-insert-template-or-signature)
             ("M-t" . transpose-words)
             ("C-c n i" . eod-haskell-navigate-imports)
             ("C-c n g" . haskell-navigate-imports)
             ("C-c v c" . haskell-cabal-visit-file))
  (bind-keys :map haskell-cabal-mode-map
             ("C-c C-z" . haskell-interactive-switch)
             ("C-c C-k" . haskell-interactive-mode-clear)
             ("C-c C-c" . haskell-process-cabal-build)
             ("C-c c" . haskell-process-cabal)
             ("C-c C-o" . haskell-compile))
  (use-package hare
    :init
    ;; (add-to-list 'load-path "~/.cabal/share/HaRe-0.8.2.3/elisp")
    ;; (autoload 'hare-init "hare" nil t)
    (add-to-list 'load-path "~/.cabal/share/i386-linux-ghc-7.10.3/HaRe-0.8.2.3/elisp")
    :config
    ;; (add-hook 'haskell-mode-hook (lambda () (hare-init)))
    ))

(use-package haskell-process)

(use-package haskell-font-lock)

(use-package hindent
  :ensure t
  :config
  (setq hindent-style "chris-done"))

(use-package company-ghc
  :ensure t
  :config
  (add-to-list 'company-backends 'company-ghc))

(use-package company-cabal
  :ensure t
  :config
  (add-to-list 'company-backends 'company-cabal))

(defadvice comment-dwim
    (after haskell-fix-indentation-empty-line activate)
  "Fix the comment so that it starts at the beginning of the line
when the region is not active and the current line is empty."
  (when (and
         (derived-mode-p major-mode 'haskell-parent-mode)
         (not (eq major-mode 'literate-haskell-mode))
         (not (region-active-p))
         ;; (= (point) (line-end-position))
         (looking-back "^ +-- "))
    (save-excursion
      (back-to-indentation)
      (delete-region (line-beginning-position) (point)))))

;; (ad-activate 'comment-dwim)

(defun eod-hindent-defun ()
  "Indent the current haskell function (including the function's
signature) surround point while not moving the point (as far the
user can tell)."
  (interactive)
  (save-excursion
    (mark-defun)
    (next-line)
    (hindent-reformat-region)))

(defun eod-haskell-navigate-imports ()
  (interactive)
  (haskell-navigate-imports)
  (open-line 1)
  (insert "import "))

(defun haskell-wrap-with-paren-pair-and-fix-indent ()
  (interactive)
  (sp-wrap-with-pair "(")
  (haskell-indentation-indent-backwards))

(defun eod-haskell-mode-insert-undefined-at-point ()
  "Insert undefined at point."
  (interactive)
  (when (eq major-mode 'haskell-mode)
    (insert "undefined")))

(defun haskell-sp-splice-sexp (&optional arg)
  "Requisite documentation ARG!"
  (interactive "p")
  (sp-splice-sexp arg)
  (haskell-indentation-indent-backwards))

(defun haskell-insert-doc ()
  "Insert the documentation syntax"
  (interactive)
  (insert " -- | "))

(defun haskell-simple-run ()
  "Run the current haskell file using \"runhaskell\"."
  (interactive)
  (let ((arguments
         (format
          "%s"
          (read-from-minibuffer "Enter arguments (if any): "))))
    (when (eq major-mode 'haskell-mode)
      (async-shell-command
       (concat
        "runhaskell "
        (buffer-file-name)
        " "
        arguments)))))

(define-key haskell-mode-map (kbd "<f12>") 'haskell-simple-run)

(defun eod-haskell-split-bracket ()
  (-when-let (ok (sp-get-enclosing-sexp))
    (if (and (equal "[" (sp-get ok :op))
             (equal "]" (sp-get ok :cl))
             (not (eq (sp-get ok :len-in) 0))
             (eq major-mode 'haskell-mode))
        (sp-get ok :beg)
      nil)))

;;; I work on this later.
(defadvice sp-split-sexp (around haskell-insert-plus-on-split activate)
  (-if-let (bounds (eod-haskell-split-bracket))
      (let ((nsexps (- (length (sp-get-list-items)) 2)))
        ad-do-it
        (if (eq (car (ad-get-arg 0)) 4)
            (message "Work on this later.")
          ;; (progn
          ;;   (save-excursion
          ;;     (goto-char bounds)
          ;;     (setq i 1)
          ;;     (while (<= i nsexps)
          ;;       (sp-forward-sexp)
          ;;       (if (save-excursion
          ;;             (re-search-forward "," nil t))
          ;;           (replace-match " ++" nil nil)
          ;;         (insert " ++"))
          ;;       (setq i (1+ i)))))
          (save-excursion
            (delete-horizontal-space)
            (insert " ++ "))))
    ad-do-it))

;; (ad-deactivate 'sp-split-sexp)

(defadvice sp-join-sexp (before haskell-insert-fix activate)
  (when (and (eq major-mode 'haskell-mode)
             (save-excursion (search-forward "[" nil t)))
    (save-excursion
      (when (search-backward "]" nil t)
        (re-search-forward "++" nil t)
        (replace-match "" nil nil)))))

(defun  haskell-auto-insert-module-template ()
  "Insert  a module template for the newly created buffer."
  (interactive)
  (when (and (= (point-min)
                (point-max))
             (buffer-file-name))
    (insert
     "-- | "
     "\n"
     "\n"
     "module "
     )
    (let ((name (haskell-guess-module-name)))
      (if (string= name "")
          (insert "Main")
        (insert name)))
    (insert
     " where"
     "\n"
     "\n"
     )
    (goto-char (point-min))
    (forward-char 4)))

(add-hook 'inferior-haskell-mode-hook 'subword-mode)
(add-hook 'inferior-haskell-mode-hook 'turn-on-smartparens-mode)

(add-hook 'interactive-haskell-mode-hook 'subword-mode)
(add-hook 'interactive-haskell-mode-hook 'turn-on-smartparens-mode)
(add-hook 'interactive-haskell-mode-hook 'company-mode)

(provide 'init-haskell)
;;; init-haskell.el ends here
