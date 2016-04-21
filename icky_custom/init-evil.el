;;; package --- Summary
;;; Commentary:
;;; Code:

(defgroup dotemacs-evil nil
  "Configuration options for evil-mode."
  :group 'dotemacs
  :prefix 'dotemacs-evil)

(defcustom dotemacs-evil/evil-state-modes
  '(fundamental-mode
    text-mode
    prog-mode
    sws-mode
    dired-mode
    web-mode
    comint-mode
    log-edit-mode)
  "List of modes that should start in Evil state."
  :type '(repeat (symbol))
  :group 'dotemacs-evil)


(defcustom dotemacs-evil/emacs-state-hooks
  '(org-log-buffer-setup-hook org-caputre-mode-hook)
  "List of hooks to automatically start up in Evil Emacs state."
  :type '(repeat (symbol))
  :group 'dotemacs-evil)

(defcustom dotemacs-evil/emacs-state-minor-modes
  '(git-commit-mode magit-blame-mode)
  "List of minor modes that when active should switch to Emacs state."
  :type '(repeat (symbol))
  :group 'dotemacs-evil)

(defcustom dotemacs-evil/emacs-cursor
  "red"
  "The color of the cursor when in Emacs state."
  :type 'color)


;; (setq evil-search-module 'evil-search)
;; (setq evil-magic 'very-magic)

(setq evil-emacs-state-cursor (,dotemacs-evil/emacs-cursor box))
(setq evil-normal-state-cursor '("green" box))
(setq evil-visual-state-cursor '("orange" box))
(setq evil-insert-state-cursor '("red" bar))
(setq evil-replace-state-cursor '("red" bar))
(setq evil-operator-state-cursor '("red" hollow))


(require-package 'evil)
(require 'evil)

;; (evil-esc-mode)

(require-package 'evil-leader)
(global-evil-leader-mode t)

(require-package 'evil-commentary)
(evil-commentary-mode t)

(require-package 'evil-surround)
(global-evil-surround-mode t)

(require-package 'evil-exchange)
(evil-exchange-install)

(require-package 'evil-anzu)
(require 'evil-anzu)


(setq evil-jumper-auto-center t)
(setq evil-jumper-file (concat dotemacs-cache-directory "evil-jumps"))
(setq evil-jumper-auto-save-interval 36000)
(require-package 'evil-jumper)
(global-evil-jumper-mode t)

(require-package 'evil-matchit)
(defun evilmi-customize-keybinding ()
  (evil-define-key 'normal evil-matchit-mode-map
                   "%" 'evillmi-jump-items))
(global-evil-matchit-mode t)

(require-package 'evil-indent-textobject)
(require 'evil-indent-textobject)

(require-package 'evil-visualstar)
(global-evil-visualstar-mode t)

(require-package 'evil-numbers)

(defun my-major-mode-evil-state-adjust ()
  (if (apply 'derived-mode-p dotemacs-evil/evil-emacs-modes)
      (turn-on-evil-mode)
    (set-cursor-color dotemacs-evil/emacs-cursor)
    (turn-off-evil-mode)))

(add-hook 'after-change-major-mode-hook #'my-major-mode-evil-state-adjust)

(cl-loop for mode in dotemacs-evil/emacs-state-minor-modes
         do (let ((hook (concat symbol-name mode) "-hook")))
         (add-hook (intern hook) (lambda ()
                                   (if ,mode
                                       (evil-emacs-state)
                                     (evil-normal-state)))))


(cl-loop for hook in dotemacs-evil/emacs-state-hooks
         do (add-hook hook #'evil-emacs-state))



;;; Rewrite as a lambda function if you attach this to a keybinding.
;; (defun my-send-string-to-terminal (string)
;;   (send-string-to-terminal string))


(defun my-evil-modeline-change (default-color)
  "Changes the modeline color when the evil mode changes."
  (let ((color (cond ((evil-insert-state-p) '("#002233" . "#ffffff"))
                     ((evil-visual-state-p) '("#330022" . "#ffffff"))
                     ((evil-normal-state-p) default-color)
                     (t '("#440000" . "#ffffff")))))
    (set-face-background 'mode-line (car color))
    (set-face-background 'mode-line (cdr color))))

(lexical-let (default-color (cons (face-background 'mode-line)
                                  (face-foreground 'mode-line)))
  (add-hook 'post-command-hook (lambda () (my-evil-modeline-change default-color))))

(defadvice evil-ex-search-next (after advice-for-evil-ex-search-next activate)
  (recenter))

(defadvice evil-ex-search-previous (after advice-for-evil-ex-search-next activate)
  (recenter))


(after 'edebug
       (add-hook 'edebug-mode-hook (lambda ()
                                     (if edebug-mode
                                         (evil-emacs-state)
                                       (evil-normal-state)))))

(after 'paren
       (ad-enable-advice #'show-paren-function 'around 'evil)
       (ad-activate #'show-paren-function))

(provide 'init-evil)
;;; init-evil ends here
