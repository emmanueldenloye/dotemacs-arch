;;; package -- Summary
;;; Commentary:
;;; Code:

(use-package evil
  :ensure t
  :init
  (bind-key ";" 'evil-ex evil-normal-state-map)
  (bind-key ":" 'evil-repeat-find-char evil-motion-state-map)
  (bind-key "C-e" 'evil-end-of-line evil-normal-state-map)
  (bind-key "C-c +" 'evil-numbers/inc-at-pt evil-normal-state-map)
  (bind-key "C-c -" 'evil-numbers/dec-at-pt evil-normal-state-map)
  (bind-key "C-c a" 'evil-avy-goto-char evil-normal-state-map)
  (bind-key "C-c o" 'evil-avy-goto-subword-1 evil-normal-state-map)
  (bind-key "C-c C-a" 'evil-paste-last-insertion evil-insert-state-map)
  (bind-key "C-c C-e" 'evil-copy-from-below evil-insert-state-map)
  (bind-key "C-a" 'smarter-move-beginning-of-line evil-insert-state-map)
  (bind-key "C-e" 'end-of-line evil-insert-state-map)
  :config
  (setq evil-emacs-state-cursor '("white" box))
  (setq evil-normal-state-cursor '("yellow" box))
  (setq evil-visual-state-cursor '("purple" box))
  (setq evil-insert-state-cursor '("green" box))
  (setq evil-motion-state-cursor '("blue" box)))

(use-package evil-surround
  :ensure t)
(use-package evil-anzu
  :ensure t)
(use-package evil-numbers
  :ensure t)
(use-package evil-easymotion
  :ensure t)
(use-package evil-mark-replace
  :ensure t)
(use-package evil-jumper
  :ensure t)
(use-package evil-visualstar
  :ensure t)
(use-package evil-leader
  :ensure t
  :config
  (evil-leader/set-leader "<SPC>")
  (evil-leader/set-key
    "e"  'helm-find-files
    "b"  'helm-mini
    "J"  'switch-to-previous-buffer
    "x"  'helm-M-x
    "k"  'kill-buffer
    "cc" 'flycheck-compile
    "cC" 'flycheck-clear
    "cl" 'flycheck-list-errors
    "cn" 'flycheck-previous-error
    "cp" 'flycheck-next-error))
(use-package evil-org)
(use-package evil-commentary
  :ensure t
  :defer t)
(use-package evil-lisp-state
  :ensure t
  :defer t)
(use-package evil-mark-replace
  :ensure t
  :defer t)
(use-package evil-matchit
  :ensure t
  :defer t)
;; (use-package elscreen)
;; (use-package evil-tabs
;;   :ensure t)
;; (load "elscreen" "ElScreen" t)

;; (defun my-move-key (keymap-from keymap-to key)
;;   "Moves key binding from one keymap to another, deleting from the old location. "
;;   (define-key keymap-to key (lookup-key  keymap-from key))
;;   (define-key keymap-from key nil))

;; (my-move-key evil-motion-state-map evil-normal-state-map (kbd "RET"))
;; (my-move-key evil-motion-state-map evil-normal-state-map " ")

(provide 'init-evil-mode)
;;; init-evil-mode.el ends here
