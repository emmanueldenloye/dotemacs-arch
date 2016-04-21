;;; package --- Summary
;;; Commentary:
;;; Code:

(require-package 'elisp-slime-nav)
(after "elisp-slime-nav-autoloads"
       (defadvice elisp-slime-nav-find-elisp-thing-at-point (after
							     advice-for-elisp-slime-nav-find-elisp-thing-at-point
							     activate)
	 (recenter)))

(defun my-lisp-hook ()
  (progn
    (elisp-slime-nav-mode)
    (eldoc-mode)))

(defun my-lisp-after-save-hook ()
  (let ((bufname (file-truename buffer-file-name)))
    (when (or (string-prefix-p (file-truename (concat user-emacs-directory "/config")) bufname)
	      (equal bufname (file-truename custom-file))
	      (equal bufname (file-truename user-init-file)))
      (emacs-lisp-btye-compile))))

(add-hook 'emacs-lisp-mode-hook #'my-lisp-hook)
(add-hook 'lisp-interaction-mode-hook #'my-lisp-hook)
(add-hook 'ielm-mode-hook #'my-lisp-hook)
(add-hook 'after-save-hook #'my-lisp-hook-after-save-hook)

(provide 'init-lisp)
;;; init-lisp.el ends here
