(defun parseExperiment ()
  ;; assume at start of sexp
  (let (results)
    (with-current-buffer (current-buffer)
    (narrow-to-defun)
    (goto-char (point-min) )
    (setq results (read (current-buffer)))
    (widen))
    (prog1
        (append
     (experiment-get-trial results)
     (experiment-get-permutation-count results)
     (experiment-get-num-simplicies results)
     (experiment-get-max-dimensions results)
     (experiment-get-max-distance results)
     (experiment-get-num-divisions results)
     (experiment-get-all-dims-components results))
      (forward-char))))

(defun gather-results (outfile)
  (while (not (eobp))
    (let ((results (parseExperiment))
          (save-silently t))
    (with-temp-buffer
      (goto-char (point-min))
      (insert (format "%s\n" results))
      (append-to-file (point-min) (point-max) outfile)))))

(defun experiment-get-trial (arg)
  (nth 0 arg))

(defun experiment-get-permutation-count (arg)
  (nth 1 arg))

(defun experiment-get-num-simplicies (arg)
  (nth 2 arg))

(defun experiment-get-max-dimensions (arg)
  (nth 3 arg))

(defun experiment-get-max-distance (arg)
  (nth 4 arg))

(defun experiment-get-num-divisions (arg)
  (nth 5 arg))

(defun experiment-get-all-dims-components (arg)
  (let ((l (-drop 6 arg)))
    ;; (list (length l) l)
    (list (length l)
          (mapcar (lambda (trial)
              (let ((len (length trial)))
                (append (nth (- len 2) trial)
                        (nth (- len 1) trial))))
            l))))

(defun count-sexps-in-region (beg end)
  ;; assumes balanced expressions exist in region and no comments at
  ;; the end
  (interactive "r")
  (save-excursion
    (narrow-to-region
     beg end)
    (goto-char (point-min))             ;this is not perfect
    (loop while (not (eobp)) do (forward-sexp) sum 1)))

(provide 'homologyprocess)
;; homologyprocess.el ends here
