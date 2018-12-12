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
 '(LaTeX-command "latex -synctex=1")
 '(Linum-format "%7i ")
 '(Man-notify-method (quote aggressive))
 '(ace-link-setup-default nil t)
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
 '(beacon-color "#dc322f")
 '(blink-cursor-mode nil)
 '(company-ghc-show-info t)
 '(compilation-message-face (quote default))
 '(counsel-mode nil)
 '(custom-enabled-themes (quote (gruvbox)))
 '(custom-safe-themes
   (quote
    ("7f89ec3c988c398b88f7304a75ed225eaac64efa8df3638c815acc563dfd3b55" "6ac7c0f959f0d7853915012e78ff70150bfbe2a69a1b703c3ac4184f9ae3ae02" "39a854967792547c704cbff8ad4f97429f77dfcf7b3b4d2a62679ecd34b608da" "4c8372c68b3eab14516b6ab8233de2f9e0ecac01aaa859e547f902d27310c0c3" "8e7044bfad5a2e70dfc4671337a4f772ee1b41c5677b8318f17f046faa42b16b" "9a3c51c59edfefd53e5de64c9da248c24b628d4e78cc808611abd15b3e58858f" "2caab17a07a40c1427693d630adb45f5d6ec968a1771dcd9ea94a6de5d9f0838" "0973b33d2f15e6eaf88400eee3dc8357ad8ae83d2ca43c125339b25850773a70" "2d5c40e709543f156d3dee750cd9ac580a20a371f1b1e1e3ecbef2b895cf0cd2" "392f19e7788de27faf128a6f56325123c47205f477da227baf6a6a918f73b5dc" "a455366c5cdacebd8adaa99d50e37430b0170326e7640a688e9d9ad406e2edfd" "a5a2954608aac5c4dcf9659c07132eaf0da25a8f298498a7eacf97e2adb71765" "be5b03913a1aaa3709d731e1fcfd4f162db6ca512df9196c8d4693538fa50b86" "2ea9afebc23cca3cd0cd39943b8297ce059e31cb62302568b8fa5c25a22db5bc" "ff8c6c2eb94e776c9eed9299a49e07e70e1b6a6f926dec429b99cf5d1ddca62a" "701b4b4e7989329a0704b92fc17e6600cc18f9df4f2466617ec91c932b5477eb" "5e402ccb94e32d7d09e300fb07a62dc0094bb2f16cd2ab8847b94b01b9d5e866" "aae40caa1c4f1662f7cae1ebfbcbb5aa8cf53558c81f5bc15baefaa2d8da0241" "1a094b79734450a146b0c43afb6c669045d7a8a5c28bc0210aba28d36f85d86f" "8530b2f7b281ea6f263be265dd8c75b502ecd7a30b9a0f28fa9398739e833a35" "da8e6e5b286cbcec4a1a99f273a466de34763eefd0e84a41c71543b16cd2efac" "ed92c27d2d086496b232617213a4e4a28110bdc0730a9457edf74f81b782c5cf" "4eb982b248bf818a72877ecb126a2f95d71eea24680022789b14c3dec7629c1b" "c1de07961a3b5b49bfd50080e7811eea9c949526084df8d64ce1b4e0fdc076ff" "6a674ffa24341f2f129793923d0b5f26d59a8891edd7d9330a258b58e767778a" "2047464bf6781156ebdac9e38a17b97bd2594b39cfeaab561afffcbbe19314e2" "06f0b439b62164c6f8f84fdda32b62fb50b6d00e8b01c2208e55543a6337433a" "82d2cac368ccdec2fcc7573f24c3f79654b78bf133096f9b40c20d97ec1d8016" "4570b1b1b2adc38491c06618e65598f6f16ea37ff8c31e8aaf0606a427afaa69" "69831e572dc46ced47c5309bff8fc2f4a9e237e2bad2c76f313da814a4628694" "51f73a06d0f2d5961bb4473cc41609be7c7c99814d1b435afcc5105e18c1301e" "c626e064e512724d0c5ec0f802243fe2f0a4223373df8bb89c1f70ce904c135c" "579e9950513524d8739e08eae289419cfcb64ed9b7cc910dd2e66151c77975c4" "b97a01622103266c1a26a032567e02d920b2c697ff69d40b7d9956821ab666cc" "795d8a0785b16437ee67da091c2c684e4149a12991199c7f5ae4b91637ea0c9c" "d12c2cae6c13a834084e06a3062d5a27cac7627e0872bd1728d203b46ae6a5bb" "787574e2eb71953390ed2fb65c3831849a195fd32dfdd94b8b623c04c7f753f0" "4aee8551b53a43a883cb0b7f3255d6859d766b6c5e14bcb01bed572fcbef4328" "4cf3221feff536e2b3385209e9b9dc4c2e0818a69a1cdb4b522756bcdf4e00a4" default)))
 '(debug-on-error nil)
 '(debug-on-quit nil)
 '(default-input-method "TeX")
 '(defvar
    (quote
     ((120 aw-delete-window "Ace - Delete Window")
      (109 aw-swap-window " Ace - Swap Window")
      (102 aw-flip-window)
      (118 aw-split-window-vert " Ace - Split Vert Window")
      (98 aw-split-window-horz " Ace - Split Horz Window")
      (99 delete-other-windows " Ace - Maximize Window")
      (67 delete-other-windows))) t)
 '(display-battery-mode t)
 '(display-time-mode t)
 '(electric-pair-mode nil)
 '(elmacro-mode nil)
 '(evil-jumper-mode t)
 '(evil-want-C-u-scroll t)
 '(fci-rule-character-color "#202020")
 '(fci-rule-color "#eee8d5")
 '(flycheck-disabled-checkers (quote (haskell-stack-ghc)))
 '(frame-background-mode (quote dark))
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
 '(haskell-process-type (quote stack-ghci))
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
 '(ivy-mode nil)
 '(magit-diff-use-overlays nil)
 '(main-line-color1 "#1e1e1e")
 '(main-line-color2 "#111111")
 '(main-line-separator-style (quote chamfer))
 '(menu-bar-mode nil)
 '(org-agenda-files
   (quote
    ("~/Dropbox/kadena.org" "~/Dropbox/uvamastersthesis/tex/tasks.org" "~/.notes")))
 '(package-selected-packages
   (quote
    (yaml-mode yaml lsp-haskell lsp-ui lsp-mode writeroom-mode tuareg org-web-tools pocket-reader company-ispell company-math melpa glasses-mode projectile ein company-bibtex session google-translate academic-phrases pdf-tools org diminish webpaste monokai-theme helm-themes ace-link gnuplot "ample-theme" powerline twittering-mode emms markdown-mode wc-mode writegood-mode gruvbox-theme org-bullets all-the-icons auctex ujelly-theme lispy darkroom helm-pass caml helm-ispell helm-firefox helm-swoop yas-jit org-ref ox-bibtex elpy elm-mode clojure-here request 4clojure electric-case cider workgroups volatile-highlights vimish-fold use-package soothe-theme smartparens shm shell-pop recentf-ext rainbow-mode rainbow-delimiters pianobar paxedit page-break-lines org-pomodoro org-fstree multiple-cursors magit lorem-ipsum linum-relative keyfreq key-chord jujube-theme jabber info+ hindent highlight-symbol highlight-numbers highlight-escape-sequences help+ helm-unicode helm-projectile helm-ghc helm-descbinds helm-dash helm-bibtex haskell-snippets hackernews guide-key gscholar-bibtex golden-ratio god-mode ghci-completion ggtags flycheck-tip flycheck-hdevtools flycheck-haskell flycheck-ghcmod fill-column-indicator expand-region evil ercn emacs-xkcd elscreen elisp-slime-nav duplicate-thing drag-stuff dracula-theme discover-my-major dired+ diff-hl darktooth-theme cursor-chg crosshairs creamsody-theme company-ghci company-ghc company-cabal color-theme-sanityinc-tomorrow color-theme-sanityinc-solarized color-theme-modern clean-aindent-mode beacon avy-zap auto-compile anzu ample-theme aggressive-indent ace-window ace-jump-helm-line ac-haskell-process)))
 '(pos-tip-background-color "#1A3734")
 '(pos-tip-foreground-color "#FFFFC8")
 '(powerline-color1 "#1e1e1e")
 '(powerline-color2 "#111111")
 '(projectile-mode nil nil (projectile))
 '(safe-local-variable-values
   (quote
    ((reftex-default-bibliography . "../Bibliography.bib")
     (eval font-lock-add-keywords nil
           (\`
            (((\,
               (concat "("
                       (regexp-opt
                        (quote
                         ("sp-do-move-op" "sp-do-move-cl" "sp-do-put-op" "sp-do-put-cl" "sp-do-del-op" "sp-do-del-cl"))
                        t)
                       "\\_>"))
              1
              (quote font-lock-variable-name-face)))))
     (org-latex-pdf-process "pdflatex -synctex=1 -interaction nonstopmode -output-directory %o %f" "bibtex %b" "pdflatex -synctex=1 -interaction nonstopmode -output-directory %o %f" "pdflatex -synctex=1 -interaction nonstopmode -output-directory %o %f")
     (org-latex-pdf-process "pdflatex -interaction nonstopmode -output-directory %o %f" "bibtex %b" "pdflatex -interaction nonstopmode -output-directory %o %f" "pdflatex -interaction nonstopmode -output-directory %o %f")
     (org-latex-pdf-process "pdflatex -interaction nonstopmode # -output-directory %o %f" "bibtex %b" "pdflatex -interaction nonstopmode -output-directory %o %f" "pdflatex -interaction nonstopmode -output-directory %o %f"))))
 '(semantic-mode nil)
 '(send-mail-function (quote smtpmail-send-it))
 '(session-use-package t nil (session))
 '(set-mark-command-repeat-pop t)
 '(setq "-w" t)
 '(shm-indent-point-after-adding-where-clause t)
 '(shm-lambda-indent-style 0)
 '(tls-program
   (quote
    ("openssl s_client -connect %h:%p -no_ssl2 -ign_eof
                                       -CAfile /home/ootput/.private/certs/CAs.pem
                                       -cert /home/ootput/.private/certs/nick.pem" "gnutls-cli --priority secure256
                                 --x509cafile /home/ootput/.private/certs/CAs.pem
                                 --x509certfile /home/ootput/.private/certs/nick.pem -p %p %h" "gnutls-cli --priority secure256 -p %p %h")))
 '(tool-bar-mode nil)
 '(user-login-name "emmanuel_erc" t)
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
   (unspecified "#272822" "#3C3D37" "#F70057" "#F92672" "#86C30D" "#A6E22E" "#BEB244" "#E6DB74" "#40CAE4" "#66D9EF" "#FB35EA" "#FD5FF0" "#74DBCD" "#A1EFE4" "#F8F8F2" "#F8F8F0"))
 '(winner-mode t))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :stipple nil :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 150 :width normal :foundry "unknown" :family "source code pro" (quote (cursor ((t (:background "#Ff69b4"))))))))))
