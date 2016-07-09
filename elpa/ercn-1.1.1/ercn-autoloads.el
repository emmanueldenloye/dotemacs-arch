;;; ercn-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (or (file-name-directory #$) (car load-path)))

;;;### (autoloads nil "ercn" "ercn.el" (22344 63293 651610 284000))
;;; Generated autoloads from ercn.el

(autoload 'ercn-match "ercn" "\
Extracts information from the buffer and fires `ercn-notify-hook' if needed.

\(fn)" nil nil)

(autoload 'ercn-fix-hook-order "ercn" "\
Notify before timestamps are added

\(fn &rest ##)" nil nil)

(when (boundp 'erc-modules) (add-to-list 'erc-modules 'ercn))

(eval-after-load 'erc '(progn (unless (featurep 'ercn (require 'ercn))) (add-to-list 'erc-modules 'ercn t)))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; ercn-autoloads.el ends here
