(dolist (hook '(paredit-mode))
  (add-hook 'clojure-mode-hook hook))

(with-eval-after-load 'clojure-mode
  (defadvice 4clojure-open-question (around 4clojure-open-question-around)
   "Start a cider/nREPL connection if one hasn't already been started when
opening 4clojure questions"
   ad-do-it
   (unless cider-current-clojure-buffer
     (cider-jack-in))))

(provide 'init-clojure)
;;; init-clojure.el ends here
