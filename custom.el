;;; package --- Summary
;;; Commentary:
;;; Code:

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(Linum-format "%7i ")
 '(ansi-color-faces-vector
   [default bold shadow italic underline bold bold-italic bold])
 '(ansi-color-names-vector
   (vector "#4d4d4c" "#c82829" "#718c00" "#eab700" "#4271ae" "#8959a8" "#3e999f" "#ffffff"))
 '(ansi-term-color-vector
   [unspecified "#110F13" "#b13120" "#719f34" "#ceae3e" "#7c9fc9" "#7868b5" "#009090" "#F4EAD5"] t)
 '(avy-all-windows (quote all-frames))
 '(avy-dispatch-alist
   (quote
    ((120 . avy-action-kill)
     (109 . avy-action-mark)
     (99 . avy-action-copy))))
 '(company-ghc-show-info t)
 '(custom-enabled-themes (quote (sanityinc-solarized-dark)))
 '(custom-safe-themes
   (quote
    ("b97a01622103266c1a26a032567e02d920b2c697ff69d40b7d9956821ab666cc" "795d8a0785b16437ee67da091c2c684e4149a12991199c7f5ae4b91637ea0c9c" "d12c2cae6c13a834084e06a3062d5a27cac7627e0872bd1728d203b46ae6a5bb" "787574e2eb71953390ed2fb65c3831849a195fd32dfdd94b8b623c04c7f753f0" "4aee8551b53a43a883cb0b7f3255d6859d766b6c5e14bcb01bed572fcbef4328" "4cf3221feff536e2b3385209e9b9dc4c2e0818a69a1cdb4b522756bcdf4e00a4" default)))
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
 '(haskell-stylish-on-save t)
 '(haskell-tags-on-save t)
 '(main-line-color1 "#1e1e1e")
 '(main-line-color2 "#111111")
 '(main-line-separator-style (quote chamfer))
 '(menu-bar-mode nil)
 '(powerline-color1 "#1e1e1e")
 '(powerline-color2 "#111111")
 '(save-place t nil (saveplace))
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
 '(vc-annotate-very-old-color nil))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t
             (:inherit nil
                       :stipple nil
                       :inverse-video nil
                       :box nil
                       :strike-through nil
                       :overline nil
                       :underline nil
                       :slant normal
                       :weight normal
                       :height 120
                       :width normal
                       :foundry "unknown"
                       :family "Monaco"))))
 '(cursor ((t (:background "#Ff69b4")))))

;;; I don't really use shm-current-face.
;;; So don't worry about this last bit. It does not really matter!
