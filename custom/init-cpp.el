(add-hook 'c++-mode-hook 'turn-on-smartparens-mode)

(defun opencv-compile
    (run)
  (interactive "p")
  (-if-let*
      ((compiler "g++")
       (packages "`pkg-config --cflags --libs opencv`")
       (file
        (buffer-file-name))
       (object
        (concat
         (file-name-directory
          (buffer-file-name))
         (file-name-base
          (buffer-file-name)))))
      (progn
        (save-buffer)
                                        ;sanity check
        (let
            ((runstr
              (concat "&& " "./"
                      (file-name-base
                       (buffer-file-name)))))
          (async-shell-command
           (string-join
            (list compiler file "-o" object packages
                  (if run runstr ""))
            " "))))
    (message "not currently visiting a file on disk.")))

;;; c++-mode-map isn't available on startup.
(with-eval-after-load 'c++-mode
  (define-key c++-mode-map
    (kbd "C-c C-r")
    'opencv-compile))

(defun c++-clear-to-one-line-between-functions
    ()
  (interactive)
  (save-excursion
    (goto-char
     (point-min))
    (while
        (search-forward-regexp "\\(^#include\\|^using\\)" nil t)
      (forward-line))
    (while
        (<
         (point)
         (point-max))
      (clear-to-one-blank-line)
      (end-of-defun))
    (message "Finished clearing blank lines appropriately!")))

(dolist
    (hook
     '(flycheck-mode))
  (add-hook 'c++-mode-hook hook))

(use-package ggtags
  :ensure t
  :defer t
  :config
  (add-hook 'c-mode-common-hook
            (lambda
              ()
              (when
                  (derived-mode-p 'c-mode 'c++-mode)
                (ggtags-mode 1)))))

(provide 'init-cpp)
;;; init-cpp.el ends here
