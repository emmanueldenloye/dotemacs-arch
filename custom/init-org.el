(use-package org
  :bind (("C-c l" . org-store-link)
         ("C-c a a" . org-agenda)
         ("C-c b o" . org-iswitchb))
  :init
  (add-to-list
   'auto-mode-alist '("\\.\\(org\\|org_archive\\|txt\\)$" . org-mode))
  :config
  (defalias 'string-to-int 'string-to-number)
  (setq org-src-fontify-natively t
        org-export-backends '(ascii beamer html icalendar latex md)
        org-todo-keywords '((sequence "TODO" "MAYBE" "STILL WORKING" "DONE"))
        org-export-copy-to-kill-ring 'nil
        org-feed-alist '(("Slashdot"
                          "http://rss.slashdot.org/Slashdot/slashdot"
                          "~/txt/feeds/feeds.org"
                          "Slashdot Entries"))) ;I'll add more to this in time.
  (defun eod-org-set-options ()
    (setq ispell-parser 'tex)
    (load-library "reftex")
    (and (buffer-file-name)
         (file-exists-p (buffer-file-name))
         (reftex-parse-all)))
  (add-hook 'org-mode-hook 'electric-pair-local-mode)
  (add-hook 'org-mode-hook 'flyspell-mode)
  (add-hook 'org-mode-hook 'auto-fill-mode)
  (add-hook 'org-mode-hook 'eod-org-set-options))

;; (require 'ob-haskell)

(use-package org-bullets
  :ensure t
  :init (add-hook 'org-mode-hook
                    (lambda ()
                      (org-bullets-mode 1))))

(use-package ox-latex
  :config
  (add-to-list 'org-latex-classes
               '("beamer"
                 "\\documentclass\[presentation\]\{beamer\}"
                 ("\\section\{%s\}" . "\\section\{%s\}")
                 ("\\subsection\{%s\}" . "\\subsection\{%s\}")
                 ("\\subsubsection\{%s\}" . "\\subsubsection\{%s\}")))
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((emacs-lisp . t)
     (haskell . t)
     (dot . t)
     (gnuplot . t)
     (python . t))))
(use-package org-bullets
  :ensure t
  :init (add-hook 'org-modc-hook (lambda () (org-bullets-mode 1))))

(add-hook 'org-mode-hook 'wc-mode)      ;count the words in the buffer

(setq org-latex-pdf-process
      '("latexmk -pdflatex='pdflatex -interaction nonstopmode' -pdf -bibtex -f %f"))

(setq ieeetran-class
      '("IEEEtran" "\\documentclass[11pt]{IEEEtran}"
        ("\\section{%s}" . "\\section*{%s}")
        ("\\subsection{%s}" . "\\subsection*{%s}")
        ("\\paragraph{%s}" . "\\paragraph*{%s}")
        ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))

(add-to-list 'org-latex-classes ieeetran-class t)

;;; include in org-mode
;;; #+LATEX_CLASS: IEEEtran
;;; #+LATEX_CLASS_OPTIONS: [12pt,draftcls,onecolumn]

(provide 'init-org)
;; init-org.el ends here
