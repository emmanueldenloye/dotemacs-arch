;; update any change made on file to the current buffer
(global-auto-revert-mode)

(global-set-key (kbd "C-x a r") 'align-regexp)

(show-paren-mode 1)

;; activate whitespace-mode to view all whitepsace characters
(global-set-key (kbd "C-. w") 'whitespace-mode)

(put 'narrow-to-region 'disabled nil)
(put 'narrow-to-page 'disabled nil)
(put 'narrow-to-defun 'disabled nil)

(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)

(global-set-key (kbd "C-c j") 'eod-join-next-line)

(use-package highlight-escape-sequences
  :config
  (hes-mode))

;;; set the time to show partially completed keystrokes to a tenth of a second.
(setq echo-keystrokes 0.25)

(setq frame-title-format '(buffer-file-name "%f" ("%b")))

(mouse-avoidance-mode 'banish)

(setq ispell-dictionary "american")

(setq recenter-positions '(top middle bottom))

(use-package expand-region
  :ensure t
  :bind
  (("C-=" . er/expand-region)))

(setq global-mark-ring-max 5000		; increase global mark ring to contain 500 entries
      mark-ring-max 5000		; increase local mark ring to contain 5000 entries
      mode-require-final-newline t	; add a newline to end of file
      )

(setq
 kill-ring-max 5000			; increase kill-ring capacity.
 kill-whole-line t			; if NIL, kill whole line and move the next line up
 )

(setq-default tab-width 4)

(setq fill-column 80)

(setq sentence-end-double-space nil)

(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-language-environment "UTF-8")
(prefer-coding-system 'utf-8)

(delete-selection-mode)

(setq-default indent-tabs-mode nil)

(setq backup-by-copying-when-mismatch t)

(setq save-interprogram-paste-before-kill t)

(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; remap C-a to `smarter-move-beginning-of-line`
(global-set-key [remap move-beginning-of-line]
                'smarter-move-beginning-of-line)

(use-package misc
  :bind (("C-c z" . zap-up-to-char)
         ("C-c w f" . forward-to-word)
         ("C-c w b" . backward-to-word)
         ("C-c M-@" . copy-from-above-command)))

(use-package duplicate-thing
  :ensure t)

(eval-when-compile
  (defvar savehist-additional-variables)
  (defvar savehist-autosave-interval))

(setq inhibit-startup-screen t)

(setq savehist-additional-variables '(search ring regexp-search-ring) ; also save your regexp search queries
      savehist-autosave-interval 60)	; save every minute

;;; auto refresh dired silently!
(setq global-auto-revert-non-file-buffers t)
(setq auto-revert-verbose nil)

(when (fboundp 'global-prettify-symbols-mode)
  (global-prettify-symbols-mode))

(if (executable-find "aspell")
    (progn
      (setq ispell-program-name "aspell")
      (setq ispell-extra-args '("--sug-mode=ultra")))
  (setq ispell-program-name "ispell"))

(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))

;; the blinking cursor is nothing but an annoyance
(blink-cursor-mode -1)

(setq-default cursor-type 'box)
(setq curchg-default-cursor-color "Pink"
      curchg-input-method-cursor-color "Green")

(setq scroll-margin 0
      scroll-conservatively 100000
      scroll-step 1
      scroll-preserve-screen-position 1)

(size-indication-mode t)

(setq large-file-warning-threshold 100000000) ;; size in bytes

(setq initial-scratch-message
      (with-temp-buffer
        (insert-file-contents "~/Pictures/pepe.txt")
        (buffer-string)))

;;; put fortune in scratch buffer
;; (setq initial-scratch-message
;;       (with-temp-buffer
;;         (insert
;;          (concat
;;           (format
;;            ";; %s\n\n"
;;            (replace-regexp-in-string
;;             "\n" "\n;; "                ; comment each line
;;             (replace-regexp-in-string
;;              "\n$" ""                   ; remove trailing linebreak
;;              (shell-command-to-string "fortune"))))))
;;         (delete-trailing-whitespace (point-min) (point-max))
;;         (buffer-string)))

(use-package whitespace
  :config
  (setq whitespace-style '(face empty tabs lines-tail trailing))
  (global-whitespace-mode -1))

(setq user-mail-address "emmanuel.denloye@gmail.com"
      user-full-name "Emmanuel Oluwadurotimi Denloye-Ito")

(use-package info+
  :ensure t
  :config
  (global-unset-key (kbd "C-h h"))	;original "C-h h" displays "hello world" in different languages
  (define-key 'help-command (kbd "h m") 'discover-my-major))

(use-package help+
  :ensure t)

(setq c-default-style "linux"		;set style to "linux"
      c-basic-offset 4)

(setq gdb-many-windows t		; use  gdb-many-windows by default
      gdb-show-main t)			; Non-nil means display source file containing the main routine at startup

;; (diminish 'volatile-highlights-mode)
;; (global-set-key (kbd "RET") 'newline-and-indent)
;; (require 'volatile-highlights)
;; (volatile-highlights-mode t)
;; (diminish 'volatile-highlights-mode)

;; ;;; get rid of some items in the mode line
(diminish 'eldoc-mode)
(diminish 'company-mode)
(diminish 'elisp-slime-nav-mode)
(diminish 'flycheck-mode)
(diminish 'global-whitespace-mode)

(add-to-list 'auto-mode-alist (cons "\\.hs\\'" 'haskell-mode))
(add-to-list 'auto-mode-alist (cons "\\.hcr\\'" 'haskell-core-mode))

(global-set-key (kbd "C-c r b") 'bookmark-jump-other-window)

;; (when (eq major-mode 'emacs-lisp-mode)
;;   (diminish 'workgroups-mode))

;; (if (fboundp 'with-eval-after-load)
;;     (defmacro after-load (feature &rest body)
;;       "After FEATURE is loaded, evaluate BODY"
;;       (declare (indent defun))
;;       `(with-eval-after-load ,feature ,@body))
;;   (defmacro after-load (feature &rest body)
;;     "After FEATURE is loaded, evaluate BODY."
;;     (declare (indent  defun))
;;     `(eval-after-load ,feature
;;        '(progn ,@body))))

(use-package tetris
  :config
  ;These new bindings will take some time to get used to. It is
  ;tough, put rotating pieces will get easier over time.
  (define-key tetris-mode-map (kbd "h") 'tetris-rotate-prev)
  (define-key tetris-mode-map (kbd "u") 'tetris-rotate-next)
  (define-key tetris-mode-map (kbd "t") 'tetris-move-right)
  (define-key tetris-mode-map (kbd "e") 'tetris-move-left)
  (define-key tetris-mode-map (kbd "s") 'tetris-start-game))

(global-set-key (kbd "C-c b h") 'haskell-buffer-list)

(display-time-mode 1)

(add-hook 'messages-buffer-mode-hook
          (lambda () (turn-off-fci-mode)))  ;I don't want to see that annoying line.

(defadvice push-button (around push-button activate)
  (when (eq major-mode 'help-mode)
    ad-do-it
    (recenter-top-bottom)))

(defadvice elisp-slime-nav-describe-elisp-thing-at-point
    (after switch-to-help-buffer activate)
  (other-window 1))

(global-set-key (kbd "C-c <tab>") 'company-complete)

(defadvice view-echo-area-messages
    (after adjust-view activate)
  (save-selected-window
    (other-window 1)
    (recenter)))

(define-key help-map (kbd "C-k") 'describe-key)

;; (global-set-key (kbd "C-x C-b") 'ibuffer-other-window)

(add-to-list 'custom-theme-load-path "/home/emmanuel/.emacs.d/elpa/creamsody-theme-0.3.6/")

;; (creamsody-modeline) ;I want a nice modeline!

(setq find-function-C-source-directory "~/emacsSource/emacs-25.1/src")

(setq-default indent-tabs-mode nil)

(add-hook 'after-save-hook #'byte-compile-when-byte-compiled-file-exists)

(provide 'init-misc)
;; init-misc.el ends here
