(defun generate-alphabetic-sequence (len &optional start skip upcase prefix suffix separator)
  (setq start (if start  (concat
                          "--start "
                          (if (numberp start)
                              (number-to-string start)
                            start)) "")
        skip (if skip  (concat "--skip" skip) "")
        upcase (if upcase "-u" "")
        prefix (if prefix  (concat "--prefix" prefix) "")
        suffix (if suffix  (concat "--suffix" suffix) "")
        separator (if separator  (concat "--separator" separator) "")
        len (concat "-l" (number-to-string len)))
  (let ((args (concat len " " start " " skip " " upcase " " prefix " " suffix " " separator)))
    (call-process "/home/emmanuel/haskell/lexiGen/Main" nil t nil len)))

(provide 'generateAlphaSequence)
;;; generateAlphaSequence.el ends here
