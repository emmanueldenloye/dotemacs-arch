(require 'page-break-lines)

(load "auctex.el" nil t t)
(load "preview-latex.el" nil t t)

(setq TeX-auto-save t)
(setq TeX-parse-self t)
(setq-default TeX-master nil)
(add-hook 'LaTeX-mode-hook 'visual-line-mode)
(add-hook 'LaTeX-mode-hook 'auto-fill-mode)
(add-hook 'LaTeX-mode-hook 'flyspell-mode)
(add-hook 'LaTeX-mode-hook 'LaTeX-math-mode)
(add-hook 'LaTeX-mode-hook 'turn-on-reftex)
(add-hook 'reftex-load-hook 'imenu-add-menubar-index)
(add-hook 'reftex-mode-hook 'imenu-add-menubar-index)

(dolist
    (hook
     '(org-mode-hook LaTeX-mode-hook))
  (add-hook hook 'page-break-lines-mode))

(setq reftex-default-bibliography
      '("~/Dropbox/Research_Files/Mutual_Information/Mutual_Information.bib"
        "~/Dropbox/Research_Files/comp_sense/comp_sense.bib"
        "~/Dropbox/Research_Files/functionalData/functionalData.bib"
        "~/Dropbox/Research_Files/mean_shift/mean_shift.bib"
        "~/Dropbox/Research_Files/misc/misc.bib"
        "~/Dropbox/Research_Files/Mutual_Information/Mutual_Information.bib"
        "~/Dropbox/Research_Files/objectRecog/objectRecog.bib"
        "~/Dropbox/Research_Files/OpFlow/OpFlow.bib"
        "~/Dropbox/Research_Files/probMonads/probMonads.bib"
        "~/Dropbox/Research_Files/topology/topology.bib"))

(autoload 'reftex-mode "reftex" "RefTex Minor Mode" t)
(autoload 'turn-on-reftex "reftex" "RefTex Minor Mode" nil)
(autoload 'reftex-citation "reftex-cite" "Make citation" nil)
(autoload 'reftex-index-phrase-mode "reftex-index" "Phrase mode" t)
(add-hook 'LaTeX-mode-hook 'turn-on-reftex) ;with AUCTex LaTeX mode
(add-hook 'latex-mode-hook 'turn-on-reftex) ;with Emacs latex mode
;; (remove-hook 'LaTeX-mode-hook 'server-start)
;; (setq TeX-PDF-mode t)

;;; Make RefTeX faster
(setq font-latex-match-reference-keywords
      '(("cite" "[{")
        ("cites" "[{}]")
        ("autocite" "[{")
        ("footcite" "[{")
        ("footcites" "[{")
        ("parencite" "[{")
        ("textcite" "[{")
        ("fullcite" "[{")
        ("citetitle" "[{")
        ("citetitles" "[{")
        ("headlessfullcite" "[{"))
      reftex-cite-prompt-optional-args nil
      reftex-cite-cleanup-optional-args t
      reftex-cite-format
      '(
        (?\C-m . "\\cite[]{%l}")
        (?t . "\\textcite{%l}")
        (?a . "\\autocite[]{%l}")
        (?p . "\\parencite{%l}")
        (?f . "\\footcite[][]{%l}")
        (?F . "\\fullcite[]{%l}")
        (?x . "[]{%l}")
        (?X . "{%l}"))
      reftex-save-parse-info t
      reftex-use-multiple-selection-buffers t
      reftex-enable-partial-scans t
      reftex-plug-into-AUCTeX t)        ;RefTeX formats for biblatex (not natbib)

(add-hook 'TeX-after-compilation-finished-functions #'TeX-revert-document-buffer)

;; Enable synctex correlation
(setq TeX-source-correlate-method 'synctex
      TeX-source-correlate-start-server t)

;; Enable synctex generation. Even though the command shows as "latex" pdflatex is actually called
(custom-set-variables '(LaTeX-command "latex -synctex=1"))

(setq TeX-view-program-selection
      '((output-pdf "Zathura"))
      TeX-view-program-list
      '(("PDF Viewer" "okular --unique %o#src:%n%b")))

(use-package company-math
  :ensure t
  :defer t)

(provide 'init-latex)
;; init-latex.el ends here
