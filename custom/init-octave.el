(use-package octave
  :defer t
  :bind
  (:map octave-mode-map
        (("C-c C-z" . run-octave)))
  :config
  ;; (setq inferior-octave-program "~/src/octave-4.2.1/run-octave")
  (setq inferior-octave-program "octave")
  (defadvice octave-find-definition
      (around change-major-mode-to-octave-mode activate)
    (when
        (and
         (ignore-errors ad-do-it)
         (buffer-file-name)
         (string= "m"
                  (file-name-extension
                   (buffer-file-name))))
      (octave-mode)))
  (defadvice octave-beginning-of-line
      (after to-indentation activate)
    (back-to-indentation))
  (defun prepare-octave-file ()
    (when (and (string= "m" (file-name-extension (buffer-file-name)))
               (equal major-mode 'objc-mode)
               (= (buffer-size) 0)
               (y-or-n-p "Should this be an octave file?"))
      (octave-mode)
      (add-file-local-variable 'mode 'octave)))
  (add-hook 'find-file-hook 'prepare-octave-file)
  (defun mygoto-octave-scratch-buffer ()
    (interactive)
    (switch-to-buffer
     (get-buffer-create
      "*octave-scratch*"))
    (octave-mode))
  (defun octave-close-figure
      (&optional all)
    (interactive)
    (when-let
        ((proc inferior-octave-process))
      (comint-send-string proc
                          (concat "close"
                                  (or
                                   (and all "all")
                                   "")
                                  "\n"))))
  (define-key octave-mode-map (kbd "C-c C-w") 'octave-close-figure)
  (defun matlabify-code ()
    (when (equal major-mode 'octave-mode)
      (save-excursion
       (save-restriction
         (widen)
         (goto-char (point-min))
         (while (re-search-forward "\\(^\\s-*end\\)[a-z_]+$" nil t)
           (unless (string-match-p "end_unwind_protect" (match-string 0))
             (replace-match "\\1")))
         (goto-char (point-min))
         (while (re-search-forward "#" nil t)
           (when (octave-in-comment-p)
             (replace-match "%"))))))))

(provide 'init-octave)
;; init-octave.el ends here
