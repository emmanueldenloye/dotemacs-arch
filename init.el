;;; package --- Summary
;;; Commentary:
;;; Code:

(let ((file-name-handler-alist nil))
  (setq gc-cons-threshold 100000000)
  (setq ad-redefinition-action 'accept)
  (require 'package)
  (let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
                      (not (gnutls-available-p))))
         (url (concat (if no-ssl "http" "https") "://melpa.org/packages/")))
    (add-to-list 'package-archives (cons "melpa" url) t))
  (dolist  (package '(("melpa-stable" . "https://stable.melpa.org/packages/")
                      ("org" . "http://orgmode.org/elpa/")
                      ("marmalade" . "http://marmalade-repo.org/packages/")
                      ("gnu" . "http://elpa.gnu.org/packages/")))
    (add-to-list 'package-archives package))

  (setq package-enable-at-startup nil)
  (package-initialize)
  ;; add your modules path
  (add-to-list 'load-path "~/.emacs.d/custom/")

  (require 'cl)
  (eval-when-compile (require 'use-package))
  (require 'diminish)
  (require 'bind-key)

  (when (package-installed-p 'use-package)
    (setq use-package-verbose t))

;;; Load and execute the code in every .el file in the custom sub
;;; directory. Also load the custom.el file. If there are any errors,
;;; print a message to the back trace buffer.
  (let ((debug-on-error t)
        (custom-directory (concat user-emacs-directory "custom/"))
        (custom-other-libs (concat user-emacs-directory "otherlibs/")))

    (cl-loop for file in (directory-files custom-directory t)
             unless (or (file-directory-p file) (string= (file-name-extension file) "elc"))
             do (require (intern (file-name-base file)) file))

    (cl-loop for file in (directory-files custom-other-libs t)
             unless (or (file-directory-p file) (string= (file-name-extension file) "elc"))
             do (require (intern (file-name-base file)) file))

    (setq custom-file
          (concat user-emacs-directory "custom.el"))

    (when (file-exists-p custom-file)
      (load custom-file)))

  (global-font-lock-mode)
  (put 'set-goal-column 'disabled nil)
  (put 'upcase-region 'disabled nil)
  (put 'downcase-region 'disabled nil)
  (diminish 'paredit-mode)
  (diminish 'helm-mode)
  (diminish 'yas-minor-mode)
  (diminish 'flyspell-mode)
  (setq gc-cons-threshold 800000)
  ;; (semantic-mode -1)
  (put 'erc-remove-text-properties-region 'disabled nil)
  (put 'dired-find-alternate-file 'disabled nil)
  (global-set-key (kbd "C-x C-c") 'dont-kill-emacs))

;; init.el ends here
