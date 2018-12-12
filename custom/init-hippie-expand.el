;; hippie-expand is a better version of dabbrev-expand
;; While dabbrev-expand searches for words you already tyed, in
;; current buffer and other buffers, hippie-expand includes more sources,
;; such as filenames, kill ring...
(global-set-key (kbd "M-/") 'hippie-expand) ; replace dabbrev-expand
(setq
 hippie-expand-try-functions-list
 '(try-expand-dabbrev ;; Try to  expand word "dynamically", searching the entire buffer.
   try-expand-flexible-abbrev ;; Flexible matching.
   try-expand-line ;; Try to complete the current line to an entire line in the buffer.
   try-expand-dabbrev-all-buffers ;; Try to expand word "dynamically", searching all other buffers.
   try-expand-dabbrev-from-kill ;; Try to expand word "dynamically", searching the kill ring.
   try-complete-file-name-partially ;; Try to complete text as a file name, as many characters as unique.
   try-complete-file-name ;; Try to complete text as a file name.
   try-expand-all-abbrevs ;; Try to expand word before point according to all abbrev tables.
   try-expand-list ;; Try to complete list to an entire list in the buffer.
   try-complete-lisp-symbol-partially ;; Try to complete as an Emacs Lisp symbol, as many characters as unique
   try-complete-lisp-symbol)) ;; Try to complete word as an Emacs Lisp symbol.

(defvar my-abbrev-prefix-mark-state nil)

(defadvice abbrev-prefix-mark (after set-state activate)
  (setq my-abbrev-prefix-mark-state t))

(defadvice hippie-expand (around check-abbrev-prefix-state activate)
  (not-if my-abbrev-prefix-mark-state
      ad-do-it
    (let ((hippie-expand-try-functions-list
           (remove 'try-expand-flexible-abbrev hippie-expand-try-functions-list)))
      ad-do-it)
    (setq my-abbrev-prefix-mark-state nil)))

(provide 'init-hippie-expand)
;; init-hippie-expand.el ends here
