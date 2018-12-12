(use-package clj-mode
  :ensure t
  :defer t
  :config
  (dolist (hook '(paredit-mode))
    (add-hook 'clojure-mode-hook hook)))

(use-package cider
  :defer t
  :ensure t)

;; (with-eval-after-load 'clojure-mode
;;   (defadvice 4clojure-open-question
;;       (around 4clojure-open-question-around)
;;     "Start a cider/nREPL connection if one hasn't already been started when
;; opening 4clojure questions"
;;     ad-do-it
;;     (unless
;;         (and
;;          (bound-and-true-p 'cider-current-clojure-buffer)
;;          cider-current-clojure-buffer)
;;       (cider-jack-in))))

(provide 'init-clojure)
;;; init-clojure.el ends here
