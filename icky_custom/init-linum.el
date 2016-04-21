;;; package --- Summary
;;; Commentary:
:;; Code:

(require 'linum-relative)

(add-hook 'prog-mode-hook (lambda ()
                            (linum-mode)
                            (linum-relative-on)))
;; enable linum only in programming modes or when said so at a later time.

(global-set-key (kbd "C-c C-l t") 'linum-relative-toggle)

(provide 'init-linum)
;;; init-linum ends here
