(defvar backup-directory "~/.backups")
(if (not (file-exists-p backup-directory))
    (make-directory backup-directory t))

(setq
 make-backup-files t
 backup-directory-alist `((".*" . ,backup-directory)) ;; save backup files in ~/.backups
 backup-by-copying t
 version-control t
 delete-old-versions  t
 kept-old-versions 6
 kept-new-versions 9
 auto-save-default t
 auto-save-timeout 20
 auto-save-interval 200
 vc-make-backup-files t
 auto-save-file-name-transforms '((".*" "~/.emacs.d/auto-save-list/" t)))

(provide 'init-backup)
;; init-backup.el ends here
