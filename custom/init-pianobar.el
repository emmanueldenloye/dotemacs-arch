;;; package --- Summary
;;; Commentary:
;;; Code:

;; (autoload 'pianobar "pianobar" nil t)

(use-package pianobar
  :ensure t
  :config
  (setq pianobar-username "emmanuel.denloye@gmail.com"
        pianobar-password "nRLfP26eva"
        pianobar-station "QuickMix"))

(defun eod-pianobar-play-or-pause
    (arg)
  (interactive "P")
  (if
      (comint-check-proc pianobar-buffer)
      (and
       (pianobar-play-or-pause)
       (when arg
         (if (string= (buffer-name (current-buffer))
                      pianobar-buffer)
             (switch-to-previous-buffer)
             (switch-to-buffer pianobar-buffer))))
    (and
     (y-or-n-p "Pianobar is not currently running. Would you like to start `pianobar'? ")
     (pianobar))))

;; (global-set-key (kbd "M-<f12>") 'eod-pianobar-play-or-pause)

(defun goto/start-pianobar-buffer ()
  (interactive)
  (if (comint-check-proc pianobar-buffer)
      (switch-to-buffer pianobar-buffer)
    (and (y-or-n-p "Pianobar is not currently running. Would you like to start `pianobar'? ") (pianobar))))

(defun pianobar-message-current-song ()
  "Display the current song and artist in the modeline."
  (interactive)
  (message
   (concat
    "Artist : "
    pianobar-current-artist
    " || Song : "
    pianobar-current-song)))

(defun pianobar-upcoming-songs ()
  "Show upcoming songs."
  (interactive)
  (pianobar-send-command ?u t t))

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

;;;;;;;;;;;;;;;;;;;;;;;;;
;; JUNK GOES DOWN HERE ;;
;;;;;;;;;;;;;;;;;;;;;;;;;


;; (defadvice pianobar-play-or-pause
;;     (around eod-pianobar-play-or-pause activate)
;;   (if
;;       (comint-check-proc pianobar-buffer)
;;       ad-do-it
;;     (and
;;         (y-or-n-p "Pianobar is not currently running. Would you like to start `pianobar'? ")
;;       (pianobar))))

;;;;;;;;;;;;;;;;;;;;;;;;;
;; JUNK GOES DOWN HERE ;;
;;;;;;;;;;;;;;;;;;;;;;;;;
