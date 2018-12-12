(add-hook 'occur-mode-hook
          (lambda () (next-error-follow-minor-mode)))

;; This is just a nice convenience function, I could just isearch though.
(defun occur-at-point (beg end)
"Perform \\[occur] at point. If region is active, use the text within it."
  (interactive "r")
  (occur
   (if
       (use-region-p)
       (buffer-substring-no-properties beg end)
     (thing-at-point 'sexp t))))

(global-set-key (kbd "C-. C-o") 'occur-at-point)

;;; Conveniences :)
(define-key occur-mode-map (kbd "n") 'occur-next)
(define-key occur-mode-map (kbd "p") 'occur-prev)

(defun occur-dwim ()
  "Call `occur' with a sane default, chosen as the thing under point or selected region"
  (interactive)
  (push (if (region-active-p)
            (buffer-substring-no-properties
             (region-beginning)
             (region-end))
          (let ((sym (thing-at-point 'symbol)))
            (when (stringp sym)
              (regexp-quote sym))))
        regexp-history)
  (call-interactively 'occur))

(defadvice occur-mode-goto-occurrence (after occur-mode-goto-occurrence-advice activate)
  (other-window 1)
  (hydra-occur-dwim/body))

(add-hook 'occur-hook (lambda () (other-window 1)))

(defun reattach-occur ()
  (if (get-buffer "*Occur*")
      (switch-to-buffer-other-window "*Occur*")
    (hydra-occur-dwim/body)))

(defhydra hydra-occur-dwim ()
  "Occur mode"
  ("o" occur-dwim "Start occur-dwim" :color red)
  ("n" occur-next "Next" :color red)
  ("p" occur-prev "Prev" :color red)
  ("d" delete-window "Hide" :color blue)
  ("r" (reattach-occur) "Re-attach" :color red))

(global-set-key (kbd "C-. o") 'hydra-occur-dwim/body)

(provide 'init-occur)
;;; init-occur.el ends here
