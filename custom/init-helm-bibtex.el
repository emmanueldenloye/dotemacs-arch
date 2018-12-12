;;; package --- Summary
;;; Commentary:
;;; Personal Items
;;; Code:

(use-package helm-bibtex
  :if helm-mode
  :ensure t
  :defer t
  :init
  (add-to-list
   'load-path
   "~/.emacs.d/elpa/biblio")
  (use-package biblio
    :defer t
    :config
    (define-key biblio-selection-mode-map "n" 'biblio--selection-next)
    (define-key biblio-selection-mode-map "p" 'biblio--selection-previous)))

(defun eod-bibtex-pdf-open
    (fpath)
  (let
      ((choice
        (read-char "Zathura (z), Evince(e), Emacs (any other key) : ")))
    (cond
     ((string=
       (string choice)
       "e")
      (start-process "evince"
                     "*helm-bibtex-evince*"
                     "/usr/bin/evince" fpath))
     ((string=
       (string choice)
       "z")
      (start-process "zathura"
                     "*helm-bibtex-zathura*"
                     "/usr/bin/zathura" fpath))
     (t
      (find-file fpath)))))

(defun helm-bibliographies
    ()
  (interactive)
  (find-file
   (helm-comp-read
    "bibliographies: "
    bibtex-completion-bibliography
    :fc-transformer
    (lambda
      (files _source)
      (mapcar
       (lambda
         (file)
         (cons
          (file-name-nondirectory file)
          file))
       files)))))

(defun my-bibtex-completion-format-citation-org-link-to-PDF
    (keys)
  "Formatter for org-links to PDF.  Uses first matching PDF if
several are available.  Entries for which no PDF is available are
omitted."
  (s-join ", "
          (cl-loop
           for key in keys
           for pdfs =
           (bibtex-completion-find-pdf key)
           append
           (--map
            (format "cite:[[%s][%s]]" it key)
            pdfs))))

(setq

 bibtex-completion-bibliography
 (list
  "~/Dropbox/Research_Files/misc/misc.bib"
  "~/Dropbox/Research_Files/topology/topology.bib"
  "~/Dropbox/Research_Files/comp_sense/comp_sense.bib"
  "~/Dropbox/Research_Files/domainAdaptation/domainAdaptation.bib"
  "~/Dropbox/Research_Files/backgroundSubtraction/backgroundSubtraction.bib"
  "~/Dropbox/Research_Files/functionalData/functionalData.bib"
  "~/Dropbox/Research_Files/Mutual_Information/Mutual_Information.bib"
  "~/Dropbox/Research_Files/mean_shift/mean_shift.bib"
  "~/Dropbox/Research_Files/WienerHopf/WienerHopf.bib"
  "~/Dropbox/Research_Files/OpFlow/OpFlow.bib"
  "~/Dropbox/Research_Files/VLC/VLC.bib"
  "~/Dropbox/Research_Files/objectRecog/objectRecog.bib"
  "~/Dropbox/Research_Files/probMonads/probMonads.bib"
  "~/Dropbox/kadenapapers/kadenapapers.bib")
 bibtex-completion-library-path
 (list
  "~/Dropbox/Research_Files/misc/"
  "~/Dropbox/Research_Files/topology/"
  "~/Dropbox/Research_Files/comp_sense/"
  "~/Dropbox/Research_Files/domainAdaptation/"
  "~/Dropbox/Research_Files/backgroundSubtraction/"
  "~/Dropbox/Research_Files/functionalData/"
  "~/Dropbox/Research_Files/Mutual_Information/"
  "~/Dropbox/Research_Files/mean_shift/"
  "~/Dropbox/Research_Files/WienerHopf/"
  "~/Dropbox/Research_Files/OpFlow/"
  "~/Dropbox/Research_Files/VLC"
  "~/Dropbox/Research_Files/objectRecog/"
  "~/Dropbox/Research_Files/probMonads/"
  "~/Dropbox/kadenapapers/")
 bibtex-completion-notes-path "~/Dropbox/Research_Files/Notes"

 bibtex-completion-notes-extension ".org"

 bibtex-completion-pdf-symbol "⌘"

 bibtex-completion-notes-symbol "✎"

 bibtex-completion-pdf-open-function
 'eod-bibtex-pdf-open

 bibtex-completion-browser-function
 (lambda
   (url _)
   (start-process
    "chromium"
    "*chromium*"
    "chromium"
    url))
 bibtex-completion-additional-search-fields
 '(tags)
 bibtex-completion-format-citation-functions
 '(
   ;; (org-mode . bibtex-completion-format-citation-default)
   ;; (org-mode . bibtex-completion-format-citation-org-link-to-PDF)
   (org-mode . my-bibtex-completion-format-citation-org-link-to-PDF)
   (latex-mode . bibtex-completion-format-citation-cite)
   (markdown-mode . bibtex-completion-format-citation-pandoc-citeproc)
   (default . bibtex-completion-format-citation-default)))
;; '((org-mode      . helm-bibtex-format-citation-org-link-to-PDF)
;;   (latex-mode    . helm-bibtex-format-citation-cite)
;;   (markdown-mode . helm-bibtex-format-citation-pandoc-citeproc)
;;   (default       . helm-bibtex-format-citation-default))

(provide 'init-helm-bibtex)
;;; init-helm-bibtex.el ends here
