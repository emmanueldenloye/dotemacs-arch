(deftheme My Solarized Dark Theme
  "Created 2015-10-08.")

(custom-theme-set-variables
 'My Solarized Dark Theme
 '(ansi-color-faces-vector [default bold shadow italic underline bold bold-italic bold])
 '(ansi-color-names-vector ["#4d4d4c" "#c82829" "#718c00" "#eab700" "#4271ae" "#8959a8" "#3e999f" "#ffffff"])
 '(avy-all-windows (quote all-frames))
 '(custom-safe-themes (quote ("82d2cac368ccdec2fcc7573f24c3f79654b78bf133096f9b40c20d97ec1d8016" "4cf3221feff536e2b3385209e9b9dc4c2e0818a69a1cdb4b522756bcdf4e00a4" "4aee8551b53a43a883cb0b7f3255d6859d766b6c5e14bcb01bed572fcbef4328" "628278136f88aa1a151bb2d6c8a86bf2b7631fbea5f0f76cba2a0079cd910f7d" "bb08c73af94ee74453c90422485b29e5643b73b05e8de029a6909af6a3fb3f58" "1b8d67b43ff1723960eb5e0cba512a2c7a2ad544ddb2533a90101fd1852b426e" default)))
 '(evil-want-C-u-scroll t)
 '(haskell-complete-module-preferred nil)
 '(haskell-stylish-on-save t)
 '(global-prettify-symbols-mode nil)
 '(save-place nil)
 '(haskell-process-suggest-remove-import-lines t)
 '(haskell-process-auto-import-loaded-modules t)
 '(haskell-process-log t)
 '(haskell-process-type (quote cabal-repl))
 '(haskell-process-suggest-haskell-docs-imports t)
 '(haskell-interactive-popup-errors nil)
 '(company-ghc-show-info t)
 '(shm-lambda-indent-style 0)
 '(shm-indent-point-after-adding-where-clause t)
 '(haskell-tags-on-save t)
 '(org-export-backends (quote (ascii beamer html icalendar latex md odt org))))

(custom-theme-set-faces
 'My Solarized Dark Theme
 '(default ((t (:inherit nil :stipple nil :background "#002b36" :foreground "#839496" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 130 :width normal :foundry "unknown" :family "Inconsolata")))))

(provide-theme 'My Solarized Dark Theme)
