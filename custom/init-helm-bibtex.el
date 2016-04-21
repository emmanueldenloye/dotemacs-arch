;;; package --- Summary
;;; Commentary:
;;; Personal Items
;;; Code:


(use-package helm-bibtex
  :if helm-mode
  :ensure t
  :init
  (autoload 'helm-bibtex "helm-bibtex" "" t))

(setq
 helm-bibtex-bibliography
 (list
  "~/Dropbox/Research_Files/misc/misc.bib"
  "~/Dropbox/Research_Files/topology/topology.bib"
  "~/Dropbox/Research_Files/comp_sense/comp_sense.bib"
  "~/Dropbox/Research_Files/backgroundSubtraction/backgroundSubtraction.bib"
  "~/Dropbox/Research_Files/functionalData/functionalData.bib"
  "~/Dropbox/Research_Files/Mutual_Information/Mutual_Information.bib"
  "~/Dropbox/Research_Files/mean_shift/mean_shift.bib"
  "~/Dropbox/Research_Files/WienerHopf/WienerHopf.bib"
  "~/Dropbox/Research_Files/OpFlow/OpFlow.bib"
  "~/Dropbox/Research_Files/objectRecog/objectRecog.bib"
  "~/Dropbox/Research_Files/probMonads/probMonads.bib"))
(setq
 helm-bibtex-library-path
 (list
  "~/Dropbox/Research_Files/misc/"
  "~/Dropbox/Research_Files/topology/"
  "~/Dropbox/Research_Files/comp_sense/"
  "~/Dropbox/Research_Files/backgroundSubtraction/"
  "~/Dropbox/Research_Files/functionalData/"
  "~/Dropbox/Research_Files/Mutual_Information/"
  "~/Dropbox/Research_Files/mean_shift/"
  "~/Dropbox/Research_Files/WienerHopf/"
  "~/Dropbox/Research_Files/OpFlow/"
  "~/Dropbox/Research_Files/objectRecog/"
  "~/Dropbox/Research_Files/probMonads/"))

(setq
 helm-bibtex-notes-path
 "~/Dropbox/Research_Files/Notes")
(setq
 helm-bibtex-notes-extension
 ".org")

(setq
 helm-bibtex-pdf-symbol
 "⌘")
(setq
 helm-bibtex-notes-symbol
 "✎")

(setq
 helm-bibtex-pdf-open-function
 (lambda (fpath)
   (start-process "zathura"
                  "*helm-bibtex-zathura*"
                  "/usr/bin/zathura" fpath)))

(setq
 helm-bibtex-browser-function
 (lambda
   (url _)
   (start-process
    "firefox"
    "*firefox"
    "firefox"
    url)))

(setq
 helm-bibtex-format-citation-functions
 '((org-mode      . helm-bibtex-format-citation-org-link-to-PDF)
   (latex-mode    . helm-bibtex-format-citation-cite)
   (markdown-mode . helm-bibtex-format-citation-pandoc-citeproc)
   (default       . helm-bibtex-format-citation-default)))

(provide 'init-helm-bibtex)
;;; init-helm-bibtex.el ends here
