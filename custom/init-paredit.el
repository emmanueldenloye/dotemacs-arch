;; Paredit Settings
(use-package paredit
  :ensure t
  :init
  (autoload 'enable-paredit-mode "paredit"
    "Turn on pseudo-structural editing of Lisp code." t)
  :config
  (defvar paredit-wrap-prefix-key "M-(")
  (bind-keys :map paredit-mode-map
             (paredit-wrap-prefix-key . nil)
             ((concat paredit-wrap-prefix-key " (") . paredit-wrap-round)
             ((concat paredit-wrap-prefix-key " [") . paredit-wrap-bracket)
             ((concat paredit-wrap-prefix-key " {") . paredit-wrap-squiggle)
             ("C-M-)" . paredit-slurp-all-the-way-forward)
             ("C-M-(" . paredit-slurp-all-the-way-backward)
             ("C-M-}" . paredit-barf-all-the-way-forward)
             ("C-M-{" . paredit-barf-all-the-way-backward)
             ("C-c d" . paredit-delete-region)
             ("M-P" . eod-paredit-previous-sexp)
             ("M-N" . eod-paredit-next-sexp)
             ("<C-backspace>" . paredit-backward-kill-word))
  ;; (remove-hook 'emacs-lisp-mode-hook #'enable-paredit-mode)
  (add-hook 'ielm-mode-hook #'enable-paredit-mode)
  ;; (remove-hook 'lisp-mode-hook #'enable-paredit-mode)
  (add-hook 'eval-expression-minibuffer-setup-hook 'smartparens-mode)
  (add-hook 'minibuffer-setup-hook 'smartparens-mode)
  (eldoc-add-command
   'paredit-backward-delete
   'paredit-close-round))

(defun eod-paredit-next-sexp (&optional arg)
  "Run `sp-next-sexp' if smartparens-mode is installed.
The ARG argument is handled by sp-next-sexp."
  (if (fboundp 'smartparens-mode)
      (sp-next-sexp arg)
    (message "Install smartparens-mode")))

(defun eod-paredit-previous-sexp (&optional arg)
  "Run `sp-previous-sexp' if smartparens-mode is installed.
The ARG argument is handled by sp-previous-sexp."
  (if (fboundp 'smartparens-mode)
      (sp-previous-sexp arg)
    (message "Install smartparens-mode")))

(defun paredit-wrap-squiggle (argument)
  (interactive "P")
  (paredit-wrap-sexp argument ?\{ ?\}))
(defun paredit-wrap-bracket (argument)
  (interactive "P")
    (paredit-wrap-sexp argument ?\[ ?\]))

;;;###autoload
(defun  paredit-barf-all-the-way-backward ()
  "Barf all the items from point to beginning of sexp surrounding
the point out."
  (interactive)
  (paredit-split-sexp)
  (paredit-backward-down)
  (paredit-splice-sexp))

;;;###autoload
(defun paredit-barf-all-the-way-forward ()
  "Barf all the items from point to end of the sexp surrounding
the point out."
  (interactive)
  (paredit-split-sexp)
  (paredit-forward-down)
  (paredit-splice-sexp)
  (if (eolp) (delete-horizontal-space)))

;;;###autoload
(defun paredit-slurp-all-the-way-backward ()
  "Slurp baby."
  (interactive)
  (catch 'done
    (while (not (bobp))
      (save-excursion
        (paredit-backward-up)
        (if (eq (char-before) ?\()
            (throw 'done t)))
      (paredit-backward-slurp-sexp))))

;;;###autoload
(defun paredit-slurp-all-the-way-forward ()
  "Slurp baby."
  (interactive)
  (catch 'done
    (while (not (eobp))
      (save-excursion
        (paredit-forward-up)
        (if (eq (char-after) ?\))
            (throw 'done t)))
      (paredit-forward-slurp-sexp))))

;;;###autoload
(defun paredit-delete-indentation (&optional arg)
  "Handle joining lines that end in a comment."
  (interactive "*P")                    ;you are correct
  (let (comt)
    (save-excursion
      (move-beginning-of-line (if arg 1 0))
      (when (skip-syntax-forward "^<" (point-at-eol))
        (setq comt (delete-and-extract-region (point) (point-at-eol))))
      (delete-indentation arg)
      (when comt
        (save-excursion
          (move-end-of-line 1)
          (insert " ")
          (insert comt))))))

(define-key paredit-mode-map (kbd "M-^") 'paredit-delete-indentation)

(provide 'init-paredit)
;; init-paredit.el ends here
