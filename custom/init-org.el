(use-package org
  :init
  (add-to-list
   'auto-mode-alist '("\\.\\(org\\|org_archive\\|txt\\)$" . org-mode))
  (bind-key "C-c l" 'org-store-link)
  (bind-key "C-c a a" 'org-agenda)
  (bind-key "C-c b o" 'org-iswitchb)
  :config
  (setq org-src-fontify-natively t)
  (setq org-export-backends '(ascii beamer html icalendar latex md))
  (setq org-todo-keywords '((sequence "TODO" "MAYBE" "STILL WORKING" "DONE")))
  (add-hook 'org-mode-hook 'electric-pair-mode)
  (add-hook 'org-mode-hook 'flyspell-mode)
  (add-hook 'org-mode-hook 'auto-fill-mode))

;; (require 'ob-haskell)

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
     (dot . t))))

(use-package org-ref
  :if 'helm-bibtex
  :ensure t
  :config

  (defvar org-ref-helm-bibtex-source
    (helm-build-sync-source "BibTeX entries"
      :init 'bibtex-completion-init
      :candidates 'bibtex-completion-candidates
      :filtered-candidate-transformer 'helm-bibtex-candidates-formatter
      :action (helm-make-actions        ;I'm sure there is another way
                                        ;to do this without writing
                                        ;out
                                        ;org-ref-bibtex-completion-actions.
               "Insert citation"            'helm-bibtex-insert-citation
               "Open PDF file (if present)" 'helm-bibtex-open-pdf
               "Open URL or DOI in browser" 'helm-bibtex-open-url-or-doi
               "Insert reference"           'helm-bibtex-insert-reference
               "Insert BibTeX key"          'helm-bibtex-insert-key
               "Attach PDF to email"        'helm-bibtex-add-PDF-attachment
               "Edit notes"                 'helm-bibtex-edit-notes
               "Show entry"                 'helm-bibtex-show-entry
               "Add keywords to entries"    'org-ref-helm-tag-entries
               "Copy entry to clipboard"    'bibtex-completion-copy-candidate))
    "Source for searching in BibTeX files.")

;;;###autoload
(defun org-helm-bibtex (&optional arg)
    "Search BibTeX entries.

With a prefix ARG, the cache is invalidated and the bibliography
reread."
    (interactive "P")
    (when arg
      (bibtex-completion-clear-cache))
    (helm :sources (list org-ref-helm-bibtex-source helm-source-fallback-options)
          :full-frame helm-bibtex-full-frame
          :buffer "*org-ref helm bibtex"
          :candidate-number-limit 500))

;;;###autoload
(defun my-org-ref-helm-insert-cite-link (arg)
    "Insert a citation link with `helm-bibtex'.
With one prefix ARG, insert a ref link.
With two prefix ARGs, insert a label link."
    (interactive "P")
    ;; save all bibtex buffers so we get the most up-to-date selection. I find
    ;; that I often edit a bibliography and forget to save it, so the newest entry
    ;; does not show in helm-bibtex.
    (org-ref-save-all-bibtex-buffers)
    (cond
     ((equal arg nil)
      (let ((bibtex-completion-bibliography (org-ref-find-bibliography)))
        (let ((debug-on-error t))
          (org-helm-bibtex))))
     ((equal arg '(4))
      (org-ref-helm-insert-ref-link))
     ((equal arg '(16))
      (org-ref-helm-insert-label-link))))

  (define-key org-mode-map (kbd "C-c ]") 'my-org-ref-helm-insert-cite-link)

  (setq org-ref-insert-link-function 'my-org-ref-helm-insert-cite-link
        org-ref-insert-cite-function 'my-org-ref-helm-insert-cite-link))


;; (remove-hook 'org-mode-hook 'jcp-org-mode)
;; (remove-hook 'org-mode-hook 'org-mode-reftex-setup)

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
