(use-package hydra
  :ensure t
  :defer t
  :config
  (global-set-key
   (kbd "C-x t")
   (defhydra toggle (:color blue)
     "toggle"
     ("a" abbrev-mode "abbrev" :color red)
     ("s" flyspell-mode "flyspell" :color red)
     ("c" check-parens "check-parens" :color red)
     ("T" delete-trailing-whitespace "delete-trailing-whitespace" :color red)
     ("d" toggle-debug-on-error "debug")
     ("D" toggle-debug-on-quit "debug-quit")
     ("g" revert-buffer "revert-buffer")
     ("t" toggle-truncate-lines "truncate" :color red)
     ("l" linum-mode :color red)
     ("w" whitespace-mode "whitespace")
     ("q" nil "cancel")))
  (global-set-key
    (kbd "M-<f12>")
    (defhydra pianobar (:color blue)
;; "
;; _+_ pianobar-love-current-song:    %`pianobar-love-current-song
;; _-_ pianobar-ban-current-song:     %`pianobar-ban-current-song
;; _<up>_ pianobar-volume-up:         %`pianobar-volume-up
;; _<down>_ pianobar-volume-down:     %`pianobar-volume-down
;; _n_ pianobar-next-song:            %`pianobar-next-song
;; _t_ pianobar-play-or-pause:        %`pianobar-play-or-pause
;; _u_ pianobar-shelve-current-song:  %`pianobar-shelve-current-song
;; _g_ pianobar-upcoming-songs:       %`pianobar-upcoming-songs
;; _m_ pianobar-message-current-song: %`pianobar-message-current-song
;; _s_ pianobar-search-current-song:  %`pianobar-search-current-song

;; "
      ("+" pianobar-love-current-song "Love current song" :color red)
      ("-" pianobar-ban-current-song "Ban current song")
      ("<up>" pianobar-volume-up "Turn volume up" :color red)
      ("<down>" pianobar-volume-down "Turn volume down" :color red)
      ("n" pianobar-next-song "Go to next song.":color red)
      ("p" pianobar-play-or-pause "Play/Pause" :color red)
      ("t" pianobar-shelve-current-song "Shelve current song")
      ("u" pianobar-upcoming-songs "Show upcoming songs")
      ("g" goto/start-pianobar-buffer "Go to/Start pianobar")
      ("m" pianobar-message-current-song "Show current song")
      ("s" pianobar-search-current-song "Search for current song on internet")
      ("q" nil "cancel"))))


(provide 'init-hydra)
;;; init-hydra.el ends here
