(setq explicit-shell-file-name "/bin/zsh")

(defadvice term (around set-shell activate)
  (let ((explicit-shell-file-name "/bin/sh"))
    ad-do-it))

;; (setq gud-chdir-before-run nil)

;; (defun my-term-setup ()
;;   (interactive)
;;   (define-key term-raw-map (kbd "C-y") 'term-send-raw)
;;   (define-key term-raw-map (kbd "C-p") 'term-send-raw)
;;   (define-key term-raw-map (kbd "C-n") 'term-send-raw)
;;   (define-key term-raw-map (kbd "C-s") 'term-send-raw)
;;   (define-key term-raw-map (kbd "C-r") 'term-send-raw)
;;   (define-key term-raw-map (kbd "M-w") 'kill-ring-save)
;;   (define-key term-raw-map (kbd "M-y") 'helm-show-kill-ring)
;;   (define-key term-raw-map (kbd "M-d") (lambda () (interactive) (term-send-raw-string "\ed")))
;;   (define-key term-raw-map (kbd "<C-Backspace>") (lambda () (interactive) (term-send-raw-string "\e\C-?")))
;;   (define-key term-raw-map (kbd "M-p") (lambda () (interactive) (term-send-raw-string "\ep")))
;;   (define-key term-raw-map (kbd "M-n") (lambda () (interactive) (term-send-raw-string "\en")))
;;   (define-key term-raw-map (kbd "M-,") 'term-send-input)
;;   (define-key term-raw-map (kbd "C-c y") 'term-paste)
;;   (define-key term-raw-map (kbd "C-S-y") 'term-paste)
;;   (define-key term-raw-map (kbd "C-h") nil)
;;   (define-key term-raw-map (kbd "M-x") nil)
;;   (define-key term-raw-map (kbd "C-c C-b") 'helm-mini)
;;   (define-key term-raw-map (kbd "C-1") 'zygospore-toggle-delete-other-windows)
;;   (define-key term-raw-map (kbd "C-2") 'split-window-below)
;;   (define-key term-raw-map (kbd "C-3") 'split-window-right)
;;   (define-key term-raw-map (kbd "C-0") 'delete-window))
;; (add-hook 'term-mode-hook 'my-term-setup t)
;; (setq term-buffer-maximum-size 0)

;; (require 'term)

;; (defun visit-ansi-term ()
;;   ""
;;   (interactive)
;;   (let ((is-term (string= "term-mode" major-mode))
;; 	(is-running (term-check-proc (buffer-name)))
;; 	(term-cmd "/bin/zsh")
;; 	(anon-term (get-buffer "*ansi-term*")))
;;     (if is-term
;; 	(if is-running
;; 	    (if (string= "*ansi-term*" (buffer-name))
;; 		;; (call interactivel 'rename-buffer)
;; 		(ansi-term term-cmd)
;; 	      (if anon-term
;; 		  (switch-to-buffer"*ansi-term*")
;; 		(kil-buffer "*ansi-term*")
;; 		(ansi-term term-cmd))
;; 	      (ansi-term term-cmd))))))

;; (global-set-key (kbd "C-x t") 'visit-ansi-term)

;; (require 'shell-pop)
;; (global-set-key (kbd "C-c t") 'shell-pop)

(provide 'init-term)
;; init-term.el ends here
