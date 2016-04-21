;;; package --- Summary
;;; Commentary:
;;; Code:

(defmacro bind (&rest commands)
  "Convience macro which creates a lambda interactive command."
  `(lambda ()
     (interactive)
     '@commands))

(require-package 'guide-key)
(require 'guide-key)
(setq guide-key/guide-key-sequence '("C-x" "C-c" ","))
(setq guide-key/recursive-key-sequence-flag t)
(setq guide-key/popup-window-position 'bottom)
(setq guide-key/idle-delay 0.2)
(guide-key-mode 1)

(after 'evil
       (require-package 'key-chord)
       (key-chord-mode 1)

       (after 'evil-leader
	      (evil-leader/set-leader "SPC")
	      (evil-leader/set-key
	       "w" 'save-buffer
	       "e" 'eval-last-sexp
	       "E" 'eval-defun
	       "F" ctl-x-5-prefix
	       "m" 'my-new-eshell-split
	       "c" 'customize-group
	       "b d" 'kill-this-buffer
	       "v" (kbd "C-w v C-w l" )
	       "s" (kbd "C-w s C-w j")
	       "P" 'package-list-packages
	       "V" (bind (term "vim"))
	       "h" help-map
	       "h h" 'help-for-help-internal

	       (after "paradox-autoloads"
		      (evil-leader/set-key "P" 'paradox-list-packages))

	       (after "magit-autoloads"
		      (evil-leader/set-key
		       "g s" 'magit-status
		       "g b" 'magit-blame-popup
		       "g l" 'magit-log-popup
		       "g d" 'magit-diff-popup
		       "g z" 'magit-stash-popup
		       "g c" 'magit-commit-popup)))

	      (after "evil-numbers-autoloads"
		     (define-key evil-normal-state-map (kbd "C-a") 'evil-numbers/inc-at-pt)
		     (define-key evil-normal-state-map (kbd "C-S-a") 'evil-numbers/dec-at-pt))

	      (after "git-gutter+-autoloads"
		     (define-key evil-normal-state-map (kbd "[ h") #'git-gutter+-next-hunk)
		     (define-key evil-normal-state-map (kbd "] h")#'git-gutter+-previous-hunk)

		     (define-key evil-normal-state-map (kbd "SPC g a" ) 'git-gutter+-stage-hunks)
		     (define-key evil-normal-state-map (kbd "SPC g r") 'git-gutter+-revert-hunks)
		     
		     (define-key evil-visual-state-map (kbd "SPC g a" ) 'git-gutter+-stage-hunks)
		     (define-key evil-visual-state-map (kbd "SPC g r") 'git-gutter+-revert-hunks)

		     (evil-ex-define-cmd "Gw" (bind
					       (git-gutter+-stage-whole-buffer))))
	      
	      (define-key evil-visual-state-map (kbd "SPC x") 'execute-extended-command)
	      (define-key evil-normal-state-map (kbd "SPC x") 'execute-extended-command)
	      (define-key evil-normal-state-map (kbd "SPC i" ) 'imenu)
	      (define-key evil-normal-state-map (kbd "SPC f") 'find-file)

	      (after "helm-autoloads"
		     (define-key evil-normal-state-map (kbd "g b") 'helm-mini)
		     (define-key evil-normal-state-map (kbd "SPC a") 'helm-apropos)
		     (define-key evil-normal-state-map (kbd "SPC e") 'helm-recentf)
		     (define-key evil-normal-state-map (kbd "SPC f") 'helm-find-files)
		     (define-key evil-normal-state-map (kbd "SPC o ") 'helm-semantic-or-imenu)
		     (define-key evil-normal-state-map (kbd "SPC t") 'helm-etags-select)
		     (define-key evil-normal-state-map (kbd "SPC y") 'helm-show-kill-ring)
		     (define-key evil-normal-state-map (kbd "SPC m") 'helm-bookmarks)
		     (define-key evil-normal-state-map (kbd "SPC r") 'helm-register)
		     (after "helm-swoop-autoloads"
			    (define-key evil-normal-state-map (kbd "SPC l") 'helm-swoop)
			    (define-key evil-normal-state-map (kbd "SPC L") 'helm-multi-swoop)))
	      (define-key evil-normal-state-map (kbd "C-b") 'evil-scroll-up)
	      (define-key evil-normal-state-map (kbd "C-f") 'evil-scroll-down)

	      (define-key evil-normal-state-map (kbd "[ SPC") (bind (evil-insert-newline-above) (forward-line)))
	      (define-key evil-normal-state-map (kbd "] SPC") (bind (evil-insert-newline-below) (forward-line -1)))
	      (define-key evil-normal-state-map (kbd "[ b") 'previous-buffer)
	      (define-key evil-normal-state-map (kbd "] b") 'next-buffer)
	      (define-key evil-normal-state-map (kbd "[ q") 'previous-error)
	      (define-key evil-normal-state-map (kbd "] q") 'next-error)

	      (after "etags-select-autoloads"
		     (define-key evil-normal-state-map (kbd "g ]") etags-select-find-tag-at-point))

	      (global-set-key (kbd "C-w") 'evil-window-map)
	      (define-key evil-normal-state-map (kbd "C-w h") 'evil-window-left)
	      (define-key evil-normal-state-map (kbd "C-w l") 'evil-window-right)
	      (define-key evil-normal-state-map (kbd "C-w j") 'evil-window-down)
	      (define-key evil-normal-state-map (kbd "C-w k") 'evil-window-up)

	      (define-key evil-motion-state-map "j" 'evil-next-visual-line)
	      (define-key evil-motion-state-map "k" 'evil-previous-visual-line)

	      (define-key evil-normal-state-map (kbd "Y") (kbd "y$"))

	      (define-key evil-visual-state-map (kbd "SPC e") 'eval-region)

	      ;;; emacs lisp
	      (evil-define-key 'normal emacs-lisp-mode-map "K" (bind (help-xref-interned (symbol-at-point))))
	      (after "elisp-slime-nav-autoloads"
		     (evil-defin-key 'normal emacs-lisp-mode-map (kbd "g d") 'elisp-slime-nav-find-elisp-thing-at-point))

	      (after "projectile-autoloads"
		     (define-key evil-normal-state-map (kbd "C-p" 'projectile-find-file))
		     (let ((binding (kbd "SPC /")))
		       (cond ((executable-find "pt")
			      (define-key evil-normal-state-map binding 'projectile-pt))
			     ((executable-find "ag")
			      (define-key evil-normal-state-map binding
				(bind
				 (setq current-prefix-arg t)
				 (call-interactively #'projcetile-ag))))
			     ((executable-find "ack")
			      (define-key evil-normal-state-map binding 'projectile-ack))
			     (t
			      (define-key evil-normal-state-map binding 'projectile-grep))))
		     (after "helm-projectile-autoloads"
			    (define-key evil-normal-state-map (kbd "C-p") 'helm-projectile)))
	      
	      (after "avy-autoloads"
		     (define-key evil-operator-state-map (kbd "z") 'avy-goto-char-2)
		     (define-key evil-normal-state-map (kbd "s") 'avy-goto-char-2)
		     (define-key evil-motion-state-map (kbd "S-SPC") 'avy-goto-line))

	      (after 'eshell
		      (add-hook 'eshell-mode-hook
				(lambda ()
				  (local-set-key (kbd "C-w h") evil-window-left)
				  (local-set-key (kbd "C-w h") evil-window-left)
				  (local-set-key (kbd "C-w h") evil-window-left)
				  (local-set-key (kbd "C-w h") evil-window-left))))

	      ;; butter fingers
	      (evil-ex-define-cmd "Q" 'evil-quit)
	      (evil-ex-define-cmd "Qa" 'evil-quit-all)
	      (evil-ex-define-cmd "QA" 'evil-quit-all)

	      ;; escape minibuffer
	      (define-key minibuffer-local-map [escape] 'my-minibuffer-keyboard-quit)
	      (define-key minibuffer-local-ns-map [escape] 'my-minibuffer-keyboard-quit)
	      (define-key minibuffer-local-completion-map [escape] 'my-minibuffer-keyboard-quit)
	      (define-key minibuffer-local-must-match-map [escape] 'my-minibuffer-keyboard-quit)
	      (define-key minibuffer-local-isearch-map [escape] 'my-minibuffer-keyboard-quit)

	      (define-key minibuffer-local-map (kbd "C-w") 'backward-kill-word)

	      (after 'ivy
		     (define-key ivy-mode-map [escape] (kbd "C-g")))

	      (after "magit-autoloads"
		     (global-set-key (kbd "C-x g") 'magit-dispatch-popup))

	      (after "project-explorer-autoloads"
		     (global-set-key [f2] 'project-explorer-open)
		     (autoload 'pe/show-file "project-explorer")
		     (global-set-key [f3] 'pe/show-file)
		     (after 'project-explorer
			    (define-key project-explorer-mode-map (kbd "C-w l") 'evil-window-right)))

	      (after "multiple-cursors-autoloads"
		     (global-set-key (kbd "C->") 'mc/mark-next-like-this)
		     (global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
		     (global-unset-key (kbd "M-<down-mouse-1>"))
		     (global-set-key (kbd "M-<mouse-1>") 'mc/add-cursor-on-click))

	      (after 'comint
		     (define-key comint-mode-map [up] 'comint-previous-input)
		     (define-key-comint-mode-map [down] 'comint-next-input))

	      (after 'auto-complete
		     (define-key company-active-map (kbd "C-n") 'company-select-next)
		     (define-key company-active-map (kbd "C-p") 'company-select-previous)
		     (define-key company-active-map (kbd "<tab>") 'my-company-tab)
		     (define-key company-active-map (kbd "<backtab>") 'company-select-previous)
		     (after "helm-company-autoloads"
			    (define-key company-mode-map (kbd "<C-return>") 'helm-company)
			    (define-key company-active-map (kbd "<C-return>") 'helm-company)))

	      (after "expand-region-autoloads"
		     (global-set-key (kbd "C-=") 'er/expand-region))

	      (after "helm-autoloads"
		     (add-hook 'eshell-mode-hook
			       (lambda ()
				 (local-set-key (kbd "C-c h") #'helm-eshell-history))))

	      (after 'help-mode
		     (define-key help-mode-map (kbd "n") 'next-line)
		     (define-key help-mode-map (kbd "p") 'previous-line)
		     (define-key help-mode-map (kbd "j") 'next-line)
		     (define-key help-mode-map (kbd "k") 'previous-line))

	      (global-set-key [prior] 'previous-buffer)
	      (global-set-key [next] 'next-buffer)

	      (global-set-key (kbd "C-c c") 'org-capture)
	      (global-set-key (kbd "C-c a") 'org-agenda)
	      (global-set-key (kbd "C-c l") 'org-store-link)
	      (global-set-key (kbd "C-c s") 'my-goto-scratch-buffer)
	      (global-set-key (kbd "C-c e") 'my-eval-and-replace)
	      (global-set-key (kbd "C-c t") 'my-new-eshell-split)

	      (global-set-key (kbd "C-x c") 'calculator)
	      (global-set-key (kbd "C-x C-b") 'ibuffer)
	      (global-set-key (kbd "C-x C-k") 'kill-this-buffer)

	      (global-set-key (kbd "C-x p") 'proced)
	      (after "vkill-autoloads"
		     (autoload 'vkill "vkill" nil t)
		     (global-set-key (kbd "C-x p") 'vkill))

	      (global-set-key (kbd "C-s") 'isearch-forward-regexp)
	      (global-set-key (kbd "C-M-s") 'isearch-forward)
	      (global-set-key (kbd "C-r") 'isearch-backward-regexp)
	      (global-set-key (kbd "C-M-r") 'isearch-backward)

	      ;;; have no use for these default bindings
	      (global-unset-key (kbd "C-x m"))

	      ;; replace with [r]eally [q]uit
	      (global-set-key (kbd "C-x r q") 'save-buffers-kill-terminal)
	      (global-set-key (kbd "C-x C-c") (bind (message "Thou shall not quit")))
	      (after 'evil
		     (defadvice evil-quit (around advice-for-evil-quit activate)
		       (message "Thou shall not quit!")))

	      
(provide 'init-bindings)
;;; init-bindings.el ends here
