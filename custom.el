;;; package --- Summary
;;; Commentary:
;;; Code:

;; maybe load this theme at a later time... it is pretty cool
;; (load-theme 'cobalt t t)
;; (enable-theme 'cobalt)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(Linum-format "%7i ")
 '(ansi-color-faces-vector
   [default bold shadow italic underline bold bold-italic bold])
 '(ansi-color-names-vector
   ["#3C3836" "#FB4933" "#86C9D3" "#8DD1CA" "#419BB0" "#A59FC0" "#3FD7E5" "#EBDBB2"])
 '(ansi-term-color-vector
   [unspecified "#110F13" "#b13120" "#719f34" "#ceae3e" "#7c9fc9" "#7868b5" "#009090" "#F4EAD5"] t)
 '(avy-all-windows (quote all-frames))
 '(avy-dispatch-alist
   (quote
    ((120 . avy-action-kill)
     (109 . avy-action-mark)
     (99 . avy-action-copy))))
 '(company-ghc-show-info t)
 '(compilation-message-face (quote default))
 '(custom-enabled-themes (quote (creamsody)))
 '(custom-safe-themes
   (quote
    ("4570b1b1b2adc38491c06618e65598f6f16ea37ff8c31e8aaf0606a427afaa69" "69831e572dc46ced47c5309bff8fc2f4a9e237e2bad2c76f313da814a4628694" "51f73a06d0f2d5961bb4473cc41609be7c7c99814d1b435afcc5105e18c1301e" "c626e064e512724d0c5ec0f802243fe2f0a4223373df8bb89c1f70ce904c135c" "579e9950513524d8739e08eae289419cfcb64ed9b7cc910dd2e66151c77975c4" "b97a01622103266c1a26a032567e02d920b2c697ff69d40b7d9956821ab666cc" "795d8a0785b16437ee67da091c2c684e4149a12991199c7f5ae4b91637ea0c9c" "d12c2cae6c13a834084e06a3062d5a27cac7627e0872bd1728d203b46ae6a5bb" "787574e2eb71953390ed2fb65c3831849a195fd32dfdd94b8b623c04c7f753f0" "4aee8551b53a43a883cb0b7f3255d6859d766b6c5e14bcb01bed572fcbef4328" "4cf3221feff536e2b3385209e9b9dc4c2e0818a69a1cdb4b522756bcdf4e00a4" default)))
 '(default-input-method "TeX")
 '(evil-jumper-mode t)
 '(evil-want-C-u-scroll t)
 '(fci-rule-character-color "#202020")
 '(fci-rule-color "#eee8d5")
 '(flycheck-disabled-checkers (quote (haskell-stack-ghc)))
 '(fringe-mode 4 nil (fringe))
 '(gfci-rule-color "#d6d6d6")
 '(ghc-report-errors nil t)
 '(global-evil-surround-mode t)
 '(global-evil-visualstar-mode t)
 '(global-prettify-symbols-mode nil)
 '(haskell-complete-module-preferred nil)
 '(haskell-interactive-popup-errors nil)
 '(haskell-process-arg-cabal-repl nil)
 '(haskell-process-auto-import-loaded-modules t)
 '(haskell-process-generate-tags t)
 '(haskell-process-log t)
 '(haskell-process-suggest-haskell-docs-imports t)
 '(haskell-process-suggest-remove-import-lines t)
 '(haskell-process-type (quote ghci))
 '(haskell-stylish-on-save nil)
 '(haskell-tags-on-save nil)
 '(helm-mode t)
 '(highlight-changes-colors (quote ("#FD5FF0" "#AE81FF")))
 '(highlight-tail-colors
   (quote
    (("#3C3D37" . 0)
     ("#679A01" . 20)
     ("#4BBEAE" . 30)
     ("#1DB4D0" . 50)
     ("#9A8F21" . 60)
     ("#A75B00" . 70)
     ("#F309DF" . 85)
     ("#3C3D37" . 100))))
 '(magit-diff-use-overlays nil)
 '(main-line-color1 "#1e1e1e")
 '(main-line-color2 "#111111")
 '(main-line-separator-style (quote chamfer))
 '(menu-bar-mode nil)
 '(org-agenda-files
   (quote
    ("~/Dropbox/org/DumpList.org" "~/Dropbox/org/Journal1/Jan192017Diary.org")))
 '(package-selected-packages
   (quote
    (helm-swoop pdf-tools yas-jit org-ref ox-bibtex elpy monokai-theme elm-mode clojure-here request 4clojure electric-case cider workgroups volatile-highlights vimish-fold use-package soothe-theme smartparens shm shell-pop recentf-ext rainbow-mode rainbow-delimiters powerline pianobar paxedit page-break-lines org-pomodoro org-fstree multiple-cursors magit lorem-ipsum linum-relative keyfreq key-chord jujube-theme jabber info+ hindent highlight-symbol highlight-numbers highlight-escape-sequences help+ helm-unicode helm-projectile helm-ghc helm-descbinds helm-dash helm-bibtex haskell-snippets hackernews guide-key gscholar-bibtex golden-ratio god-mode ghci-completion ggtags flycheck-tip flycheck-hdevtools flycheck-haskell flycheck-ghcmod fill-column-indicator fancy-narrow expand-region evil ercn emacs-xkcd elscreen elisp-slime-nav duplicate-thing drag-stuff dracula-theme discover-my-major dired+ diff-hl darktooth-theme cursor-chg crosshairs creamsody-theme company-ghci company-ghc company-cabal color-theme-sanityinc-tomorrow color-theme-sanityinc-solarized color-theme-modern clean-aindent-mode beacon avy-zap auto-compile anzu ample-theme aggressive-indent ace-window ace-jump-helm-line ac-haskell-process)))
 '(pos-tip-background-color "#1A3734")
 '(pos-tip-foreground-color "#FFFFC8")
 '(powerline-color1 "#1e1e1e")
 '(powerline-color2 "#111111")
 '(semantic-mode t)
 '(send-mail-function (quote smtpmail-send-it))
 '(set-mark-command-repeat-pop t)
 '(shm-indent-point-after-adding-where-clause t)
 '(shm-lambda-indent-style 0)
 '(tool-bar-mode nil)
 '(vc-annotate-background nil)
 '(vc-annotate-color-map
   (quote
    ((20 . "#c82829")
     (40 . "#f5871f")
     (60 . "#eab700")
     (80 . "#718c00")
     (100 . "#3e999f")
     (120 . "#4271ae")
     (140 . "#8959a8")
     (160 . "#c82829")
     (180 . "#f5871f")
     (200 . "#eab700")
     (220 . "#718c00")
     (240 . "#3e999f")
     (260 . "#4271ae")
     (280 . "#8959a8")
     (300 . "#c82829")
     (320 . "#f5871f")
     (340 . "#eab700")
     (360 . "#718c00"))))
 '(vc-annotate-very-old-color nil)
 '(weechat-color-list
   (unspecified "#272822" "#3C3D37" "#F70057" "#F92672" "#86C30D" "#A6E22E" "#BEB244" "#E6DB74" "#40CAE4" "#66D9EF" "#FB35EA" "#FD5FF0" "#74DBCD" "#A1EFE4" "#F8F8F2" "#F8F8F0")))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :stipple nil :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 125 :width normal :foundry "unknown" :family "Source Code Pro" (quote (cursor ((t (:background "#Ff69b4"))))))))))
