;; Paredit Settings
(use-package paredit
  :ensure t
  :init
  (autoload 'enable-paredit-mode "paredit"
    "Turn on pseudo-structural editing of Lisp code." t)
  :config
  (bind-keys :map paredit-mode-map
             ("C-M-)" . paredit-slurp-all-the-way-forward)
             ("C-M-(" . paredit-slurp-all-the-way-backward)
             ("C-M-}" . paredit-barf-all-the-way-forward)
             ("C-M-{" . paredit-barf-all-the-way-backward)
             ("C-c d" . paredit-delete-region))
  (add-hook 'emacs-lisp-mode-hook #'enable-paredit-mode)
  (add-hook 'ielm-mode-hook #'enable-paredit-mode)
  (add-hook 'lisp-mode-hook #'enable-paredit-mode)
  (add-hook 'lisp-interaction-mode-hook #'enable-paredit-mode)

  (add-hook 'eval-expression-minibuffer-setup-hook #'enable-paredit-mode)
  (add-hook 'minibuffer-setup-hook 'smartparens-mode)
  (eldoc-add-command
   'paredit-backward-delete
   'paredit-close-round))

(defun  paredit-barf-all-the-way-backward ()
  "Barf all the items from point to beginning of sexp surrounding
the point out."
  (interactive)
  (paredit-split-sexp)
  (paredit-backward-down)
  (paredit-splice-sexp))

(defun paredit-barf-all-the-way-forward ()
  "Barf all the items from point to end of the sexp surrounding
the point out."
  (interactive)
  (paredit-split-sexp)
  (paredit-forward-down)
  (paredit-splice-sexp)
  (if (eolp) (delete-horizontal-space)))

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

(provide 'init-paredit)
;; init-paredit.el ends here
