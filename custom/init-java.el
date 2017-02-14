;;;###autoload
(defun java-create-tags-file-and-fill (&optional folder)
  (interactive)
  "Include the last forward-slash at the end."
  (let* ((folder (or (when (file-directory-p folder)
                       (unless (directory-name-p folder)
                         (setq folder (concat folder "/")))
                       folder)
                     (file-name-directory (buffer-file-name))
                     (error "Current buffer does not have an associated file.")))
         (tags-file (if (string= "/"
                                 (substring folder (1- (length folder)) (length folder)))
                        (concat folder "TAGS"))))
    (require 'find-lisp)
    (mapc
     (lambda (file)
       (shell-command
        (format
         "etags --append --output=%s %s"
         tags-file file)))
     (find-lisp-find-files folder "\\.java$"))))

;;;###autoload
(defun java-add-class-declaration ()
  (let ((name (file-name-base (buffer-file-name))))
    (when (and (= 0 (buffer-size))
           (string=
            (s-upper-camel-case name)   ;I would have preferred a regexp here.
            name))
      (insert "public class " name " {")
      (save-excursion
        (dotimes (var 2) (newline))
        (insert "}"))
      (forward-char))))

(dolist (hook '(java-add-class-declaration))
  (add-hook 'java-mode-hook hook))

(provide 'init-java)
;; init-java.el ends here
