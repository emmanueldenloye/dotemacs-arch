;; update any change made on file to the current buffer
(global-auto-revert-mode)

(global-set-key (kbd "C-x a r") 'align-regexp)

(show-paren-mode 1)
(put 'narrow-to-region 'disabled nil)
(put 'narrow-to-page 'disabled nil)
(put 'narrow-to-defun 'disabled nil)

(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)

 ;;; I'm going to just use delete-indentation
(global-set-key (kbd "C-c j") 'eod-join-next-line)

(require 'highlight-escape-sequences)

(hes-mode)

;;; set the time to show partially completed keystrokes.
(setq echo-keystrokes 0.25)

(setq frame-title-format '(buffer-file-name "%f" ("%b")))

(mouse-avoidance-mode 'banish)

(setq ispell-dictionary "american")

(setq recenter-positions '(top middle bottom))

(use-package expand-region
  :ensure t
  :bind
  (("C-=" . er/expand-region)))

(setq global-mark-ring-max 5000		; increase global mark ring to contain 500 entries
      mark-ring-max 5000		; increase local mark ring to contain 5000 entries
      mode-require-final-newline t	; add a newline to end of file
      )

(setq
 kill-ring-max 5000			; increase kill-ring capacity.
 kill-whole-line t			; if NIL, kill whole line and move the next line up
 )

(setq-default tab-width 4)

(setq fill-column 70)

(setq sentence-end-double-space nil)

(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-language-environment "UTF-8")
(prefer-coding-system 'utf-8)

(delete-selection-mode)

(setq-default indent-tabs-mode nil)

(setq backup-by-copying-when-mismatch t)

(setq save-interprogram-paste-before-kill t)

(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; remap C-a to `smarter-move-beginning-of-line`
(global-set-key [remap move-beginning-of-line]
                'smarter-move-beginning-of-line)

(use-package misc
  :bind
  (("C-c w f" . forward-to-word)
   ("C-c w b" . backward-to-word)
   ("C-c M-@" . copy-from-above-command)))

;; (use-package duplicate-thing
;;   :ensure t)

(eval-when-compile
  (defvar savehist-additional-variables)
  (defvar savehist-autosave-interval))

(setq inhibit-startup-screen t)

(setq savehist-additional-variables '(search ring regexp-search-ring) ; also save your regexp search queries
      savehist-autosave-interval 60)	; save every minute

;;; helm-completing-read-handlers-alist
;;; auto refresh dired silently!
(setq global-auto-revert-non-file-buffers t)
(setq auto-revert-verbose nil)

(when (fboundp 'global-prettify-symbols-mode)
  (global-prettify-symbols-mode))

(if (executable-find "aspell")
    (progn
      (setq ispell-program-name "aspell")
      (setq ispell-extra-args '("--sug-mode=ultra")))
  (setq ispell-program-name "ispell"))

(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))

;; the blinking cursor is nothing but an annoyance
(blink-cursor-mode -1)

(setq-default cursor-type 'box
              cursor-in-non-selected-windows 'bar)
(setq curchg-default-cursor-color "Pink"
      curchg-input-method-cursor-color "Green")

(setq scroll-margin 0
      scroll-conservatively 100000
      scroll-step 1
      scroll-preserve-screen-position 1)

(size-indication-mode t)

(setq large-file-warning-threshold 100000000) ;; size in bytes

(setq initial-scratch-message
      (with-temp-buffer
        (insert (replace-regexp-in-string "^" ";; "
       "I hole-hardedly agree, but allow me to play doubles
advocate here for a moment. For all intensive purposes I think
you are wrong. In an age where false morals are a diamond dozen,
true virtues are a blessing in the skies. We often put our false
morality on a petal stool like a bunch of pre-Madonnas, but you
all seem to be taking something very valuable for granite. So I
ask of you to mustard up all the strength you can because it is a
doggy dog world out there. Although there is some merit to what
you are saying it seems like you have a huge ship on your
shoulder. In your argument you seem to throw everything in but
the kids Nsync, and even though you are having a feel day with
this I am here to bring you back into reality. I have a sick
sense when it comes to these types of things. It is almost
spooky, because I cannot turn a blonde eye to these glaring flaws
in your rhetoric. I have zero taller ants when it comes to people
spouting out hate in the name of moral righteousness. You just
need to remember what comes around is all around, and when supply
and command fails you will be the first to go. Make my words,
when you get down to brass stacks it doesn't take rocket
appliances to get two birds stoned at once. It's clear who makes
the pants in this relationship, and sometimes you just have to
swallow your prize and accept the facts. You might have to come
to this conclusion through denial and error but I swear on my
mother's mating name that when you put the petal to the medal you
will pass with flying carpets like it’s a peach of cake."))
        (goto-char (point-min))
        (fill-paragraph)
        (buffer-string)))

;; (setq initial-scratch-message
;;       (with-temp-buffer
;;         (insert-file-contents "~/Pictures/pepe.txt")
;;         (buffer-string)))

(use-package whitespace
  :defer t
  :config
  (setq whitespace-style '(face empty tabs lines-tail trailing))
  (global-whitespace-mode -1))

(setq user-mail-address "emmanuel.denloye@gmail.com"
      user-full-name "Emmanuel Oluwadurotimi Denloye")
      ;; user-full-name "Emmanuel Oluwadurotimi Denloye-Ito")

(use-package info+
  :defer t
  :ensure t
  :config
  (global-unset-key (kbd "C-h h"))	;original "C-h h" displays "hello world" in different languages
  (define-key 'help-command (kbd "h m") 'discover-my-major))

(use-package help+
  :defer t
  :ensure t)

(setq c-default-style "linux"           ;set style to "linux"
      c-basic-offset 4)

(setq gdb-many-windows t            ; use  gdb-many-windows by default
      gdb-show-main t) ; Non-nil means display source file containing the main routine at startup

;;; get rid of some items in the mode line
(diminish 'eldoc-mode)
(diminish 'company-mode)
(diminish 'elisp-slime-nav-mode)
(diminish 'flycheck-mode)
(diminish 'global-whitespace-mode)

(add-to-list 'auto-mode-alist
             (cons "\\.hs\\'" 'haskell-mode))
(add-to-list 'auto-mode-alist
             (cons "\\.hcr\\'" 'haskell-core-mode))

;; (global-set-key (kbd "C-c r b") 'nil)
;; (global-set-key (kbd "C-c r b") 'bookmark-jump-other-window)

(global-set-key (kbd "C-x r b") 'helm-filtered-bookmarks)

(use-package tetris
  :config
  ;These new bindings will take some time to get used to. It is
  ;tough, put rotating pieces will get easier over time.
  (define-key tetris-mode-map (kbd "h") 'tetris-rotate-prev)
  (define-key tetris-mode-map (kbd "u") 'tetris-rotate-next)
  (define-key tetris-mode-map (kbd "t") 'tetris-move-right)
  (define-key tetris-mode-map (kbd "e") 'tetris-move-left)
  (define-key tetris-mode-map (kbd "r") 'tetris-move-down)
  (define-key tetris-mode-map (kbd "n") 'nil)
  (define-key tetris-mode-map (kbd "s") 'tetris-start-game))

;; (defun launch-tetris ()
;;   (interactive)
;;   (let (evalstr
;;         "\"(progn
;;   (with-eval-after-load 'tetris
;;     (define-key tetris-mode-map (kbd \"h\") 'tetris-rotate-prev)
;;     (define-key tetris-mode-map (kbd \"u\") 'tetris-rotate-next)
;;     (define-key tetris-mode-map (kbd \"t\") 'tetris-move-right)
;;     (define-key tetris-mode-map (kbd \"e\") 'tetris-move-left)
;;     (define-key tetris-mode-map (kbd \"r\") 'tetris-move-down)
;;     (define-key tetris-mode-map (kbd \"n\") 'nil)
;;     (define-key tetris-mode-map (kbd \"s\") 'tetris-start-game))
;;   (tetris))\""))
;;   (async-shell-command (concat "emacs -Q --execute " evalstr)))

(global-set-key
 (kbd "C-c b h")
 'haskell-buffer-list)

(setq-default display-time-day-and-date t)
(display-time-mode 1)
(display-battery-mode 1)

;; (add-hook 'messages-buffer-mode-hook
;;           (lambda () (turn-off-fci-mode)))
                                        ;I don't want to see that annoying line.

(defadvice push-button
    (around push-button activate)
  (not-if (eq major-mode 'help-mode)
      ad-do-it
    ad-do-it
    (recenter-top-bottom)))

(defadvice elisp-slime-nav-describe-elisp-thing-at-point
    (after switch-to-help-buffer activate)
  (other-window 1))

(global-set-key
 (kbd "C-<tab>")
 'company-complete)

(defadvice view-echo-area-messages
    (after adjust-view activate)
  (save-selected-window
    (other-window 1)
    (recenter)))

(define-key help-map (kbd "C-k") 'describe-key)
(define-key help-map (kbd "t") nil)

;; (global-set-key (kbd "C-x C-b") 'ibuffer-other-window)

(add-to-list 'custom-theme-load-path "~/.emacs.d/elpa/creamsody-theme-0.3.6/")

;; (creamsody-modeline) ;I want a nice modeline!

(setq find-function-C-source-directory "~/Downloads/emacs-26.1/src")

(setq-default indent-tabs-mode nil)

;; (remove-hook 'after-save-hook 'byte-compile-when-byte-compiled-file-exists)

;;; Just some bindings to make help-mode more pleasant.
(define-key help-mode-map (kbd "C-c C-d d") 'elisp-slime-nav-describe-elisp-thing-at-point)
(define-key help-mode-map (kbd "C-c C-d C-d") 'elisp-slime-nav-describe-elisp-thing-at-point)

(setq column-number-mode t)

(global-set-key (kbd "C-. C-.") 'ispell-word)

(with-eval-after-load 'shell-mode
    (define-key shell-mode-map (kbd "C-c d") 'delete-region))

(define-key help-mode-map (kbd "n") 'next-line)
(define-key messages-buffer-mode-map (kbd "n") 'next-line)
(define-key help-mode-map (kbd "p") 'previous-line)
(define-key messages-buffer-mode-map (kbd "p") 'previous-line)

(setq tetris-score-file "~/.emacs.d/tetris-scores")

(defun stupid-string-swap-function (str)
  (if (> (length str ) 3)
      (concat
       (substring str 0 1 )
       (apply 'string
              (shuffle-list
               (string-to-list (substring str 1 (1- (length str))))))
       (substring str (1- (length str)) (length str)))
    str))

(require 'find-func)
(find-function-setup-keys)

;; (defun integer-bounds-of-integer-at-point ()
;;    "Return the start and end points of an integer at the current point.
;; The result is a paired list of character positions for an integer
;; located at the current point in the current buffer. An integer is
;; any decimal digit 0 through 9 with an optional starting minus
;; symbol \(\"-\")."
;;    (save-excursion
;;      (skip-chars-backward "-0123456789")
;;      (if (looking-at "-?[0-9]+")
;;          (cons (point) (1- (match-end 0))) ;bounds of integer
;;        nil)))                              ;no integer at point

;; (put 'integer 'bounds-of-thing-at-point 'integer-bounds-of-integer-at-point)

(global-set-key (kbd "<f9>") 'rgrep)

(global-set-key (kbd "<mouse-3>") 'nil)

(setq visible-bell t
      ring-bell-function 'ignore)

(defvar user-home-directory (concat (expand-file-name "~") "/"))

;; STOP PLAYING GAMES AND DO SOMETHING WORTHWHILE WITH YOUR LIFE.

(defmacro disable-game
    (game)
  "Disable a GAME to improve my concentration, productivity, and workflow."
  `(defadvice ,(intern game) (before no-play activate)
     (message-box (concat "STOP PLAYING " (upcase ,game) "\n"))
     (error (concat "STOP PLAYING " (upcase ,game) "\n"))))

(disable-game "tetris")
(disable-game "5x5")
(disable-game "dunnet")
(disable-game "blackbox")
(disable-game "bubbles")
(disable-game "gomoku")
(disable-game "hanoi")
(disable-game "life")
(disable-game "mpuz")
(disable-game "pong")
(disable-game "snake")
(disable-game "solitaire")
(disable-game "zone")

;; redisplay-dont-pause
(setq redisplay-dont-pause t)

(if (boundp semantic-mode)
    (semantic-mode -1))

(provide 'init-misc)
;; init-misc.el ends here
