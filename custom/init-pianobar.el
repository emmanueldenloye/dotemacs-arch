;;; package --- Summary
;;; Commentary:
;;; Code:

(autoload 'pianobar "pianobar" nil t)

(setq pianobar-username "emmanuel.denloye@gmail.com")

(setq pianobar-password "nRLfP26eva")   ; I don't care.

(defun pianobar-message-current-song ()
  (interactive)
  (message
   (concat
    "Artist : "
    pianobar-current-artist
    " || Song : "
    pianobar-current-song)))

(defun pianobar-search-current-song ()
  (interactive)
  (let ((search-engine
         (format "%s"
                 (read-from-minibuffer
                  "Enter search engine (default - Duckduckgo): "))))
    (with-temp-buffer
      (insert
       (concat
        pianobar-current-artist
        " - "
        pianobar-current-song))
      (mark-whole-buffer)
      (cond
       ((equal search-engine "google") (eod-google))
       ((equal search-engine "youtube") (eod-youtube))
       (t (eod-duckduckgo))))))

(provide 'init-pianobar)
;;; init-pianobar.el ends here
