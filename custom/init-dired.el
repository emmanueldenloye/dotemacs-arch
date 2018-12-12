(setq
 dired-dwim-target t
 dired-recursive-copies 'always
 dired-recursive-deletes 'top
 dired-listing-switches "-lha"
 dired-omit-verbose nil
 dired-omit-files "\\.dyn_hi$\\|\\.dyn_o$\\|\\.hi$\\|\\.o$|^\\.?#\\|^\\.$\\|^\\.\\.$|\\.aux$" ; temporarily add .hi, .hs~
 )

;; automatically refresh dired buffer on changes
(add-hook 'dired-mode-hook 'auto-revert-mode)

;; if it is not Windows, use the following listing switches
(when
    (not
     (eq system-type 'windows-nt))
  (setq dired-listing-switches "-lha --group-directories-first"))

;; KEY BINDINGS.
;; (define-key ctl-x-4-map "\C-j" 'dired-jump-other-window)
;; (define-key dired-mode-map "\C-x\M-o" 'dired-omit-mode)
;; (define-key dired-mode-map "*O" 'dired-mark-omitted)
;; (define-key dired-mode-map "\M-(" 'dired-mark-sexp)
;; (define-key dired-mode-map "*(" 'dired-mark-sexp)
;; (define-key dired-mode-map "*)" 'dired-mark-extension)
;; (define-key dired-mode-map "\M-!" 'dired-smart-shell-command)
;; (define-key dired-mode-map "\M-G" 'dired-goto-subdir)
;; (define-key dired-mode-map "F" 'dired-do-find-marked-files)
;; (define-key dired-mode-map "Y" 'dired-do-relsymlink)
;; (define-key dired-mode-map "%Y" 'dired-do-relsymlink-regexp)
;; (define-key dired-mode-map "V" 'dired-do-run-mail)

(use-package dired-x
  :defer t
  :config
  (setq dired-bind-jump t
        dired-bind-vm t
        dired-bind-man t
        dired-bind-info t)
  (add-hook 'dired-mode-hook
            (lambda
              ()
              (dired-omit-mode)))
  (add-hook 'dired-mode-hook
            (lambda
              ()
              (dired-hide-details-mode)))
  (add-to-list 'dired-guess-shell-alist-user
               '("\\.pdf"
                 (cond
                  ((= 0
                      (call-process "which" nil nil nil "zathura"))
                   "zathura")
                  ((= 0
                      (call-process "which" nil nil nil "evince"))
                   "evince")
                  (t "xpdf"))))
  (add-to-list 'dired-guess-shell-alist-user
          '("\\.odt"
            "libreoffice"))
  (add-to-list 'dired-guess-shell-alist-user
          '("\\.docx"
            "libreoffice")))

(use-package dired+
  :defer t)

(use-package wdired
  :defer t
  :config
  (setq
   wdired-allow-to-change-premissions t ; allow to edit permission bits
   wdired-allow-to-redirect-links t		; allow to edit symlinks
   ))

(add-hook 'dired-mode-hook 'diff-hl-dired-mode)

(global-set-key
 (kbd "C-x C-j")
 'dired-jump)
(global-set-key
 (kbd "C-x 4 j")
 'dired-jump-other-window)

(defun dired-run-shell-command-with-all-marked-files
    (program)
                                        ;This could be written to
                                        ;choose a default program
                                        ;depending on the file type
  (interactive "sProgram: ")
  (if-let
      ((marked-files
        (dired-get-marked-files)))
      (async-shell-command
       (concat program " "
               (mapconcat
                (lambda
                  (file)
                  (concat "\'"
                          (f-filename file)
                          "\'")
                  )
                marked-files " ")))
    (error "Please mark some files.")))

;;; Create Trash Directory
(setq
 trash-directory "~/Trash"
 delete-by-moving-to-trash t)

(defun dired-create/append-etags-on-mark
    ()
  (interactive)
  (and
   (eq major-mode 'dired-mode)
   (if-let
       ((marked-files
         (dired-get-marked-files)))
       (and
        (or
         (file-exists-p "TAGS")
         (shell-command "touch TAGS"))
        (shell-command
         (concat "etags --append --output=TAGS "
                 (mapconcat
                  (lambda
                    (file)
                    (concat "\'"
                            (f-filename file)
                            "\'"))
                  marked-files " ")))
        (message "The TAGS file has been created.")))))

(use-package dired
  :bind
  (:map dired-mode-map
	("* p" . dired-prev-marked-file)
	("* n" . dired-next-marked-file)))

(provide 'init-dired)
;; init-dired.el ends here
