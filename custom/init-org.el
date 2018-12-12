(use-package org
  :bind
  (("C-c l" . org-store-link)
   ("C-c a a" . org-agenda)
   ("C-c b o" . org-switchb)
   ("C-. C-c" . org-capture)
   :map org-mode-map
   ("C-c p" . org-mark-ring-goto)
   ("C-. C-l" . eod-go-org-pdf-latex-output-buffer)
   ("C-c C-p" . outline-previous-visible-heading))
  :config
  (defalias 'string-to-int 'string-to-number)
  (setq org-src-fontify-natively t
        org-export-backends
        '(ascii beamer html icalendar latex odt md)
        org-todo-keywords
        '((sequence "TODO" "MAYBE" "STILL WORKING" "IGNORE" "DONE"))
        org-export-copy-to-kill-ring 'nil
        org-highlight-latex-and-related
        '(latex script entities)
        org-feed-alist
        '(("Slashdot" "http://rss.slashdot.org/Slashdot/slashdot" "~/txt/feeds/feeds.org" "Slashdot Entries")))
  ;; I'll add more to this in time.
  ;; add an easy template
  (add-to-list
   'org-structure-template-alist
   (list "ba" "#+ATTR_LATEX: :environment ABSTRACT\n#+BEGIN_EXAMPLE\n?\n#+END_EXAMPLE"))
  (setq org-use-speed-commands
        (lambda
          ()
          (and
           (looking-at org-outline-regexp)
           (looking-back "^\**")))
        org-special-ctrl-a/e t)
  ;; (defun eod-org-set-options ()
  ;;   (setq ispell-parser 'tex)
  ;;   (load-library "reftex")
  ;;   (and (buffer-file-name)
  ;;        (file-exists-p (buffer-file-name))
  ;;        (reftex-parse-all)))
  (add-hook 'org-mode-hook 'electric-pair-local-mode)
  ;; (remove-hook 'org-mode-hook 'flyspell-mode)
  (add-hook 'org-mode-hook 'auto-fill-mode)
  (remove-hook 'org-mode-hook 'eod-org-set-options)
  ;; (add-hook 'org-mode-hook 'eod-org-set-options)
  ;; remove comments from org document for use with export hook
  ;; https://emacs.stackexchange.com/questions/22574/orgmode-export-how-to-prevent-a-new-line-for-comment-lines
  (defun delete-org-comments
      (backend)
    (loop for comment in
          (reverse
           (org-element-map
               (org-element-parse-buffer)
               'comment 'identity))
          do
          (setf
           (buffer-substring
            (org-element-property :begin comment)
            (org-element-property :end comment))
           "")))
  ;; add to export hook
  (add-hook 'org-export-before-processing-hook 'delete-org-comments)

  (use-package ox-koma-letter
    :load-path "~/.emacs.d/otherlibs/"
    :config (require 'ox-koma-letter))

  ;; (eval-after-load 'ox '(require 'ox-koma-letter))
  (eval-after-load 'ox-latex
  '(add-to-list 'org-latex-packages-alist '("AUTO" "babel" t) t)))

(require 'ob-haskell)

(use-package org-bullets
  :ensure t
  :init
  (add-hook 'org-mode-hook
            (lambda
              ()
              (org-bullets-mode 1))))

(use-package ox-latex
  :config
  (add-to-list 'org-latex-classes
               '("beamer"
                 "\\documentclass\[presentation\]\{beamer\}"
                 ("\\section\{%s\}" . "\\section\{%s\}")
                 ("\\subsection\{%s\}" . "\\subsection\{%s\}")
                 ("\\subsubsection\{%s\}" . "\\subsubsection\{%s\}")))
  (add-to-list 'org-latex-classes
               '("book-noparts"
                 "\\documentclass{book}"
                 ("\\chapter{%s}" . "\\chapter*{%s}")
                 ("\\section{%s}" . "\\section*{%s}")
                 ("\\subsection{%s}" . "\\subsection*{%s}")
                 ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                 ("\\paragraph{%s}" . "\\paragraph*{%s}")
                 ("\\subparagraph{%s}" . "\\subparagraph*{%s}"))))

(org-babel-do-load-languages
   'org-babel-load-languages
   '((emacs-lisp . t)
     (haskell . t)
     (dot . t)
     (gnuplot . t)
     (python . t)
     (js . t)))

(add-hook 'org-mode-hook 'wc-mode)      ;count the words in the buffer
(add-hook 'org-mode-hook 'org-pretty-table-mode)

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

;; Create sections for papers with ease
(defun org-create-section-with-include-file
    (section-name)
  (interactive "sEnter section name: ")
  (org-insert-heading-respect-content)
  (require 'rx)
  (let
      ((opoint
        (point)))
    (insert section-name)
    (insert
     (concat "\n#+INCLUDE: "
             (file-name-directory
              (buffer-file-name))
             (mapconcat 'downcase (split-string section-name (rx whitespace) t) "-") ".org"))
    (indent-region opoint
                   (point))))

(defun org-popup-todo-list (&optional frame) ;why is this argument here
  (interactive)
  (org-todo-list)
  (delete-other-windows))

;; (remove-hook 'after-make-frame-functions 'org-popup-todo-list)

(setq org-publish-project-alist
      '(("conference2nd"
         :base-directory "~/Dropbox/org/conference2nd"
         :publishing-directory "~/Dropbox/org/conference2nd"
         :publishing-function org-latex-publish-to-pdf
         :base-extension "org"
         :exclude ".*"
         :include ["paper.org"]
         :section-numbers nil
         :with-toc nil)
        ("dissertation"
         :base-directory "~/Dropbox/dissertation"
         :publishing-directory "~/Dropbox/dissertation"
         :publishing-function org-latex-publish-to-pdf
         :base-extension "org"
         :exclude ".*"
         :include ["dissertation.org"]
         :section-numbers t
         :with-toc t)))

;; Just a small convenience to add.
(defadvice org-export-dispatch (around make-point-stay-put activate)
  (save-excursion
    ad-do-it))

;; remove comments from org document for use with export hook
;; https://emacs.stackexchange.com/questions/22574/orgmode-export-how-to-prevent-a-new-line-for-comment-lines
(defun delete-org-comments (backend)
  (loop for comment in
        (reverse
         (org-element-map
             (org-element-parse-buffer)
             'comment 'identity))
        do
        (setf
         (buffer-substring
          (org-element-property :begin comment)
          (org-element-property :end comment))
         "")))
;; add to export hook
(add-hook 'org-export-before-processing-hook 'delete-org-comments)

(defun ap/org-count-words
    ()
  "If region is active, count words in it; otherwise count words in current subtree."
  (interactive)
  (if
      (use-region-p)
      (funcall-interactively #'count-words-region
                             (region-beginning)
                             (region-end))
    (org-with-wide-buffer
     (cl-loop for
              (lines words characters)
              in
              (org-map-entries
               (lambda
                 ()
                 (ap/org-forward-to-entry-content 'unsafe)
                 (let
                     ((end
                       (org-entry-end-position)))
                   (list
                    (count-lines
                     (point)
                     end)
                    (count-words
                     (point)
                     end)
                    (- end
                       (point)))))
               'nil 'tree)
              sum lines into total-lines
              sum words into total-words
              sum characters into total-characters
              finally do
              (message "Subtree \"%s\" has %s lines, %s words and %s characters."
                       (org-get-heading t t)
                       total-lines total-words total-characters)))))

(defun ap/org-forward-to-entry-content
    (&optional unsafe)
  "Skip headline, planning line, and all drawers in current entry.
If UNSAFE is non-nil, assume point is on headline."
  (unless unsafe
    ;; To improve performance in loops (e.g. with `org-map-entries')
    (org-back-to-heading))
  (cl-loop for element =
           (org-element-at-point)
           for pos =
           (pcase element
             (`(headline . ,_)
              (org-element-property :contents-begin element))
             (`(,(or 'planning 'property-drawer 'drawer)
                . ,_)
              (org-element-property :end element)))
           while pos
           do
           (goto-char pos)))

(provide 'init-org)
;; init-org.el ends here
