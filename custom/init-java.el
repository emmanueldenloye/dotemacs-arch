(defun java-create-tags-file-and-fill
    (&optional folder)
  (interactive)
  "Include the last forward-slash at the end."
  (let*
      ((folder
        (or
         (when
             (file-directory-p folder)
           (unless
               (directory-name-p folder)
             (setq folder
                   (concat folder "/")))
           folder)
         (file-name-directory
          (buffer-file-name))
         (error "Current buffer does not have an associated file.")))
       (tags-file
        (if
            (string= "/"
                     (substring folder
                                (1-
                                 (length folder))
                                (length folder)))
            (concat folder "TAGS"))))
    (require 'find-lisp)
    (mapc
     (lambda
       (file)
       (shell-command
        (format
         "etags --append --output=%s %s"
         tags-file file)))
     (find-lisp-find-files folder "\\.java$"))))

(defun java-add-class-declaration
    ()
  (let
      ((name
        (file-name-base
         (buffer-file-name))))
    (when
        (and
         (= 0
            (buffer-size))
         (string=
          (s-upper-camel-case name)
                                        ;I would have preferred a regexp here.
          name))
      (insert "public class " name " {")
      (save-excursion
        (dotimes
            (var 2)
          (newline))
        (insert "}"))
      (forward-char))))

(defun java-capitalize-file-name
    ()
  (-when-let
      (dir-and-file
       (file-name-directory-and-base))
    (rename-this-file-and-buffer
     (concat
      (car dir-and-file)
      (concat
       (capitalize
        (substring
         (cdr dir-and-file)
         nil -4))
       (downcase
        (substring
         (cdr dir-and-file)
         -4))))
     nil)))

(dolist
    (hook
     '(smartparens-mode
       java-add-class-declaration
       ;; java-capitalize-file-name
       ))
  (add-hook 'java-mode-hook hook))

;; (remove-hook 'java-mode-hook 'java-capitalize-file-name)

(provide 'init-java)
;; init-java.el ends here
