(use-package page-break-lines
  :config
  (dolist (hook '(org-mode-hook LaTeX-mode-hook))
    (add-hook hook 'page-break-lines-mode)))

(setq reftex-default-bibliography
      '("/home/emmanuel/Dropbox/Research_Files/Mutual_Information/Mutual_Information.bib"
        "/home/emmanuel/Dropbox/Research_Files/comp_sense/comp_sense.bib"
        "/home/emmanuel/Dropbox/Research_Files/functionalData/functionalData.bib"
        "/home/emmanuel/Dropbox/Research_Files/mean_shift/mean_shift.bib"
        "/home/emmanuel/Dropbox/Research_Files/misc/misc.bib"
        "/home/emmanuel/Dropbox/Research_Files/Mutual_Information/Mutual_Information.bib"
        "/home/emmanuel/Dropbox/Research_Files/objectRecog/objectRecog.bib"
        "/home/emmanuel/Dropbox/Research_Files/OpFlow/OpFlow.bib"
        "/home/emmanuel/Dropbox/Research_Files/probMonads/probMonads.bib"
        "/home/emmanuel/Dropbox/Research_Files/topology/topology.bib"))

(add-hook 'LaTeX-mode-hook 'turn-on-reftex) ;with AUCTeX LaTeX mode
(autoload 'reftex-mode "reftex" "RefTex Minor Mode" t)
(autoload 'turn-on-reftex "reftex" "RefTex Minor Mode" nil)
(autoload 'reftex-citation "reftex-cite" "Make citation" nil)
(autoload 'reftex-index-phrase-mode "reftex-index" "Phrase mode" t)
(add-hook 'LaTeX-mode-hook 'turn-on-reftex) ;with AUCTex LaTeX mode
(add-hook 'latex-mode-hook 'turn-on-reftex) ;with Emacs latex mode

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
      reftex-enable-partial-scans t)

;; RefTeX formats for biblatex (not natbib)
(setq reftex-plug-into-AUCTeX t)

(add-hook 'TeX-after-compilation-finished-functions #'TeX-revert-document-buffer)

(provide 'init-latex)
;; init-latex.el ends here
