;;; package --- Summary
;;; Commentary:
;;; Code:
(require 'cl)
(use-package haskell-interactive-mode
  :init
  (add-hook 'haskell-interactive-mode-hook 'turn-on-smartparens-mode)
  (add-hook 'haskell-interactive-mode-hook 'subword-mode)
  (add-hook 'haskell-interactive-mode-hook 'rainbow-delimiters-mode))
(use-package haskell-mode
  :ensure t
  :init
  (autoload 'ghc-init "ghc" nil t)
  (autoload 'ghc-debug "ghc" nil t)
  (require 'shm-case-split)
  :config
  (let ((my-cabal-path (expand-file-name "~/.cabal/bin")))
    (setenv "PATH" (concat my-cabal-path path-separator (getenv "PATH")))
    (add-to-list 'exec-path my-cabal-path))
  ;; To get eta working.
  (let ((my-eta-path (expand-file-name "~/.local/bin")))
    (setenv "PATH" (concat my-eta-path path-separator (getenv "PATH")))
    (add-to-list 'exec-path my-eta-path))
  (setq ghc-ghc-options '("-idir1" "-idir2"))
  (define-abbrev haskell-mode-abbrev-table "im" "import")
  (define-abbrev haskell-mode-abbrev-table "int" "Int")
  (define-abbrev haskell-mode-abbrev-table "inte" "Integer")
  (define-abbrev haskell-mode-abbrev-table "str" "String")
  (define-abbrev haskell-mode-abbrev-table "dou" "Double")
  (define-abbrev haskell-mode-abbrev-table "flt" "Float")
  (define-abbrev haskell-mode-abbrev-table "tr" "True")
  (define-abbrev haskell-mode-abbrev-table "fal" "False")
  (define-abbrev haskell-mode-abbrev-table "maybe" "Maybe")
  (define-abbrev haskell-mode-abbrev-table "just" "Just")
  (define-abbrev haskell-mode-abbrev-table "noth" "Nothing")
  (define-abbrev haskell-mode-abbrev-table "io" "IO ()")
  ;; (add-to-list 'align-rules-list
  ;;              '(haskell-types
  ;;                (regexp . "\\(\\s-+\\)\\(::\\|::\\)\\s-+")
  ;;                (modes quote (haskell-mode literate-haskell-mode))))
  ;; (add-to-list 'align-rules-list
  ;;              '(haskell-assignment
  ;;                (regexp . "\\(\\s-+\\)=\\s-+")
  ;;                (modes quote (haskell-mode literate-haskell-mode))))
  ;; (add-to-list 'align-rules-list
  ;;              '(haskell-arrows
  ;;                (regexp . "\\(\\s-+\\)\\(->\\|→\\)\\s-+")
  ;;                (modes quote (haskell-mode literate-haskell-mode))))
  ;; (add-to-list 'align-rules-list
  ;;              '(haskell-left-arrows
  ;;                (regexp . "\\(\\s-+\\)\\(<-\\|←\\)\\s-+")
  ;;                (modes quote (haskell-mode literate-haskell-mode))))
  (eval-after-load "which-func"
    '(add-to-list 'which-func-modes 'haskell-mode))
  (setq haskell-complete-module-preferred
        '("Control.Monad"
          "Control.Monad.ST"
          "Data.List"
          "Data.Map"
          "Data.Maybe"
          "Data.Ord"
          "Numeric.LinearAlgebra"
          "Numeric.LinearAlgebra.Devel"
          "Data.Function"))
  (autoload 'turn-on-haskell-indent "hindent" "Indentation mode for  Haskell" t)
  (add-to-list 'haskell-language-extensions "--no-force-newline")
  (dolist (hook
           '((lambda () (ghc-init))
             (lambda () (setq-local company-idle-delay 1))
             subword-mode
             auto-fill-mode
             haskell-set-tab-stop-list
             turn-on-haskell-decl-scan
             rainbow-delimiters-mode-enable
             structured-haskell-mode
             company-mode
             haskell-mode-turn-on-shm-case-split
             hindent-mode
             interactive-haskell-mode))
    (add-hook 'haskell-mode-hook hook))

  (add-hook 'find-file-hook 'haskell-auto-insert-module-template)
  ;; (remove-hook 'haskell-mode-hook 'haskell-doc-mode)
  (bind-keys :map haskell-mode-map
             ("C-c C-l" . haskell-process-load-or-reload)
             ("C-c C-z" . haskell-interactive-switch)
             ("C-c C-n C-t" . haskell-process-do-type)
             ("C-c r s" . sp-rewrap-sexp)
             ("C-c C-n C-i" . haskell-process-do-info)
             ("C-c C-n C-c" . haskell-cabal-build)
             ("C-c C-n C-p C-c" . haskell-process-cabal-build)
             ("C-c C-n c" . haskell-process-cabal)
             ("C-M-u" . sp-backward-up-sexp)
             ("C-M-x" . inferior-haskell-send-decl)
             ("C-c C-." . haskell-format-imports)
             ("C-c ?" . helm-ghc-errors)
             ("C-c C-u" . eod-haskell-mode-insert-undefined-at-point)
             ("C-c M-(" . haskell-wrap-with-paren-pair-and-fix-indent)
             ("C-c M-t" . transpose-words)
             ("C-c n i" . eod-haskell-navigate-imports)
             ("C-c n g" . haskell-navigate-imports)
             ("C-M-d" . sp-down-sexp)
             ("C-c v c" . haskell-cabal-visit-file))
  (bind-keys :map haskell-interactive-mode-map
             ("C-c C-l" . helm-haskell-interactive-mode-history)
             ("C-c C-v" . haskell-interactive-toggle-print-mode)
             ("C-c C-b" . haskell-interactive-switch-back)
             ("C-c C-t" . haskell-mode-show-type-at)
             ("M-." . haskell-mode-goto-loc))
  (bind-keys :map haskell-cabal-mode-map
             ("C-c C-z" . haskell-interactive-switch)
             ("C-c C-k" . haskell-interactive-mode-clear)
             ("C-c C-c" . haskell-process-cabal-build)
             ("C-c c" . haskell-process-cabal)
             ("C-c C-o" . haskell-compile))
  ;; (use-package hare
  ;;   :init
  ;;   ;; (add-to-list 'load-path "~/.cabal/share/HaRe-0.8.2.3/elisp")
  ;;   ;; (autoload 'hare-init "hare" nil t)
  ;;   (add-to-list 'load-path "~/.cabal/share/i386-linux-ghc-7.10.3/HaRe-0.8.2.3/elisp")
  ;;   :config ;; (add-hook 'haskell-mode-hook (lambda () (hare-init)))
  ;;   )
  )

(use-package haskell-process)

(use-package haskell-font-lock)

(use-package hindent
  :ensure t
  :config
  (setq hindent-style "chris-done"))

(use-package company-ghc
  :ensure t
  :config
  (add-to-list 'company-backends 'company-ghc))

(use-package company-cabal
  :ensure t
  :config
  (add-to-list 'company-backends 'company-cabal))

;; (defadvice comment-dwim
;;     (after haskell-fix-indentation-empty-line activate)
;;   "Fix the comment so that it starts at the beginning of the line
;; when the region is not active and the current line is empty."
;;   (when (and
;;          (derived-mode-p major-mode 'haskell-parent-mode)
;;          (not (eq major-mode 'literate-haskell-mode))
;;          (not (region-active-p))
;;          ;; (= (point) (line-end-position))
;;          (looking-back "^ +-- "))
;;     (save-excursion
;;       (back-to-indentation)
;;       (delete-region (line-beginning-position) (point)))))

(defun haskell-mode-turn-on-shm-case-split ()
  (when (fboundp 'structured-haskell-mode)
    (require 'shm-case-split)
    (define-key haskell-mode-map (kbd "C-c s") 'shm/case-split)))

(defun shm-reformat-current-node (&optional node start)
  "Reformat the current node with hindent."
  (interactive)
  (let* ((current (or node (shm-current-node))))
    (save-excursion
      (goto-char (or start (shm-node-start current)))
      (push-mark (point) t nil)
      (goto-char (shm-node-end current))
      (hindent-reformat-region))))

(defun haskell-interactive-toggle-print-mode ()
  (interactive)
  (setq haskell-interactive-mode-eval-mode
        (intern
         (ido-completing-read "Eval result mode: "
                              '("fundamental-mode"
                                "haskell-mode"
                                "espresso mode"
                                "ghc-core-mode"
                                "org-mode")))))

(defun eod-hindent-defun ()
  "Indent the current haskell function (including the function's
signature) surround point while not moving the point (as far the
user can tell)."
  (interactive)
  (save-excursion
    (mark-defun)
    (next-line)
    (hindent-reformat-region)))

(defun eod-haskell-navigate-imports ()
  (interactive)
  (haskell-navigate-imports)
  (open-line 1)
  (insert "import "))

(defun haskell-wrap-with-paren-pair-and-fix-indent ()
  (interactive)
  (sp-wrap-with-pair "(")
  (haskell-indentation-indent-backwards))

(defun eod-haskell-mode-insert-undefined-at-point ()
  "Insert undefined at point."
  (interactive)
  (if (region-active-p)
      (progn
        (delete-region (region-beginning) (region-end))
        (insert "undefined"))
    (insert "undefined")))

(defun eod-ghc-insert-template-or-signature (&optional arg)
  (interactive)
  (setq arg (or arg 1))
  (save-excursion
    (let (argg (abs arg))
      (if (>= arg 0)
          (dotimes (i argg) ghc-next-error)
        (dotimes (i argg) ghc-prev-error))
      (ghc-insert-template-or-signature))))

(defun haskell-sp-splice-sexp (&optional arg)
  "Requisite documentation ARG!"
  (interactive "p")
  (sp-splice-sexp arg)
  (haskell-indentation-indent-backwards))

(defun haskell-insert-doc ()
  "Insert the documentation syntax."
  (interactive)
  (insert " -- | "))

(defun haskell-simple-compile-or-run (arg)
  "Run the current haskell file using \"runhaskell\"."
  (interactive "p")
  (let ((file (buffer-file-name)))
    (when (eq major-mode 'haskell-mode)
      (cond
       ((eq 4 arg)
        (let ((arguments
               (format
                "%s"
                (read-from-minibuffer "Enter arguments (if any): "))))
          (async-shell-command
           (concat
            "runhaskell "
            file
            " "
            arguments))))
       ((eq 16 arg)
        (let ((options (format "%s" (read-from-minibuffer "Enter compilation options (if any): "))))
          (async-shell-command (concat "ghc " options " " file))))
       (t (async-shell-command
           (concat "ghc " file)))))))

(define-key haskell-mode-map (kbd "<f12>") 'haskell-simple-compile-or-run)

(defun eod-haskell-split-bracket ()
  (-when-let (ok (sp-get-enclosing-sexp))
    (if (and (equal "[" (sp-get ok :op))
             (equal "]" (sp-get ok :cl))
             (not (eq (sp-get ok :len-in) 0))
             (eq major-mode 'haskell-mode))
        (cons (sp-get ok :beg) (sp-get ok :end))
      nil)))

;;; I work on this later.
(defadvice sp-split-sexp (around haskell-insert-plus-on-split activate)
  (-if-let (bounds (eod-haskell-split-bracket))
      (let ((nsexps (- (length (sp-get-list-items)) 2)))
        ad-do-it
        (if (eq (car (ad-get-arg 0)) 4)
            (message "Work on this later.")
          ;; (progn
          ;;   (save-excursion
          ;;     (goto-char bounds)
          ;;     (setq i 1)
          ;;     (while (<= i nsexps)
          ;;       (sp-forward-sexp)
          ;;       (if (save-excursion
          ;;             (re-search-forward "," nil t))
          ;;           (replace-match " ++" nil nil)
          ;;         (insert " ++"))
          ;;       (setq i (1+ i)))))
          (save-excursion
            (delete-horizontal-space)
            (insert " ++ "))))
    ad-do-it))

;; (ad-deactivate 'sp-split-sexp)
(defadvice sp-join-sexp (before haskell-insert-fix activate)
  (let ((searchbound (save-excursion (search-forward "[" nil t))))
    (when (and (eq major-mode 'haskell-mode)
               searchbound)
      (save-excursion
        (when (search-backward "]" nil t)
          (re-search-forward "++" searchbound t)
          (replace-match "" nil nil)
          (just-one-space))))))

(defun  haskell-auto-insert-module-template ()
  "Insert  a module template for the newly created buffer."
  (interactive)
  (when-let ((name (haskell-guess-module-name)))
    (when (and (= (point-min)
                                 (point-max))
               (buffer-file-name)
               (string= (file-name-extension (buffer-file-name)) "hs"))
            (insert
             "-- | "
             "\n"
             "\n"
             "module "
             )
            (insert name)
            (insert
             " where"
             "\n"
             "\n"
             )
            (goto-char (point-min))
            (forward-char 4))))

(add-hook 'inferior-haskell-mode-hook 'subword-mode)
(add-hook 'inferior-haskell-mode-hook 'turn-on-smartparens-mode)

(add-hook 'interactive-haskell-mode-hook 'subword-mode)
;; (remove-hook 'interactive-haskell-mode-hook 'turn-on-smartparens-mode)
(add-hook 'interactive-haskell-mode-hook 'company-mode)

;; (defun haskell-process-all-types ()
;;   "List all types in a grep buffer."
;;   (interactive)
;;   (let ((session (haskell-session)))
;;     (switch-to-buffer-other-window
;;      (get-buffer-create (format "*%s:all-types*"
;;                                 (haskell-session-name (haskell-session)))))
;;     (setq haskell-session session)
;;     (cd (haskell-session-current-dir session))
;;     (let ((inhibit-read-only t))
;;       (erase-buffer)
;;       (let ((haskell-process-log nil))
;;         (insert (haskell-process-queue-sync-request (haskell-process) ":all-types")))
;;       (unless (eq major-mode 'compilation-mode)
;;         (compilation-mode)
;;         (setq compilation-error-regexp-alist
;;               haskell-compilation-error-regexp-alist)))))


;; (defun haskell-interactive-move-to-prompt ()
;;   "Move the point to prompt if this is a self-insert command and
;;   not haskell-interactive-mode-return."
;;   (when (and haskell-interactive-mode-prompt-start (< (point) haskell-interactive-mode-prompt-start)
;;              (eq 'self-insert-command this-command) (not (eq 'haskell-interactive-mode-return this-command)))
;;     (deactivate-mark)
;;     (push-mark)
;;     (goto-char haskell-interactive-mode-prompt-start)))

;; (remove-hook 'haskell-interactive-mode-hook 'haskell-interactive-move-to-prompt)

(defun shm-reinsert-undefined-slots-after-fill ()
  "Replace any text matching \"\bundefined\b\" with the same text
  and \"evaporating slot\" slot property. Hindent seems to remove
  certain text properties."
  (interactive)
  (let ((bounds (bounds-of-thing-at-point 'defun)))
    (save-excursion
      (save-restriction
        (narrow-to-region (car bounds) (cdr bounds))
        (goto-char (point-min))
        (while (search-forward-regexp "\\bundefined\\b" nil t)
          (progn
            (delete-region (point) (save-excursion (backward-word 1) (point)))
            (shm/insert-undefined)
            (forward-word)))))))

(defadvice shm-reformat-decl (after reinsert-undefined activate)
  (shm-reinsert-undefined-slots-after-fill))

(defadvice shm/backward-node (around in-comment activate)
  (if (or (shm-in-comment) (shm-in-string))
      (sp-backward-sexp)                ;this can be replaced with
                                        ;(backward-sexp) if you don't
                                        ;have smartparens-mode
    ad-do-it))

(defadvice shm/forward-node (around in-comment activate)
  (if (or (shm-in-comment) (shm-in-string))
      (sp-forward-sexp)                 ;this can be replaced with
                                        ;(forward-sexp) if you don't
                                        ;have smartparens-mode
    ad-do-it))

(defadvice hindent-reformat-decl-or-fill (after reinsert-undefined activate)
  (shm-reinsert-undefined-slots-after-fill))

;; (defun shm-push-current-data-constructor-down/up (&optional arg)
;;   (interactive)
;;   (let* ((ev last-command-event)
;;          (echo-keystrokes nil)
;;          (base (event-basic-type ev))
;;          (step
;;           (pcase base
;;             (?p (lambda () (shm-push-current-data-constructor-up-helper arg)))
;;             (?n (lambda () (shm-push-current-data-constructor-down-helper arg))))))
;;     (funcall step)
;;     (message "Use p,n for further adjustment.")
;;     (set-transient-map
;;      (let ((map (make-sparse-keymap)))
;;        (dolist (mods '(() (control)))
;;          (define-key map (vector (append mods (list '(?p))))
;;            `(lambda ()
;;               (interactive)
;;               (shm-push-current-data-constructor-up)))
;;          (define-key map (vector (append mods (list '(?n))))
;;            `(lambda ()
;;               (interactive)
;;               (shm-push-current-data-constructor-down)))))))

;;   )

(defun shm-push-current-data-constructor-down (&optional arg)
  "Transpose the current data constructor with the data
constructor ARG \"constructors\" below (wrap around if
necessary). Here's an example:

data Test = A | (point)B | C

=>

data Test = A | C | (point)B"
  (interactive)
  (setq arg (or arg 1))
  (let* ((regions (cond ((shm-find-current-field-member-index-at-point) (shm-get-field-constructor-members-points))
                        ((shm-find-current-data-member-index-at-point) (shm-get-data-constructor-members-points))
                        (t (error "Not a valid point for transposition."))))
         (end (length regions))
         (ind (or (shm-find-current-field-member-index-at-point)
                  (shm-find-current-data-member-index-at-point))))
    (if ind
        (let ((next (mod (+ ind arg) end)))
          (shm-transpose-data-constructors ind next))
      (message "Point is not in a data constructor."))))

(defun shm-push-current-data-constructor-up (&optional arg)
  "Transpose the current data constructor with the data
constructor ARG \"constructors\" above (wrap around if
necessary). Here's an example:

data Test = A | (point)B | C

=>

data Test = (point)B | A | C"
  (interactive)
  (setq arg (or arg 1))
  (let* ((regions (cond ((shm-find-current-field-member-index-at-point) (shm-get-field-constructor-members-points))
                        ((shm-find-current-data-member-index-at-point) (shm-get-data-constructor-members-points))
                        (t (error "Not a valid point for transposition."))))
         (end (length regions))
         (ind (or (shm-find-current-field-member-index-at-point)
                  (shm-find-current-data-member-index-at-point))))
    (if ind
        (let ((prev (mod (- ind arg) end)))
          (shm-transpose-data-constructors ind prev))
      (message "Point is not in a data constructor."))))

(defun shm-in-data-constructor ()
  "Check if the current node is a Data Declaration (DataDecl) or
the child of one."
  (eq 'DataDecl (elt (elt (shm-decl-ast) 0) 1)))

(defun shm-get-field-constructor-members-points ()
  "Find the region bounds of all the field constructors within
  the current data declaration."
  (let ((field-list (when (shm-in-data-constructor)
                      (mapcar (lambda (ps) (cons (elt ps 2) (elt ps 3)))
                              (remove-if (lambda (n) (eq n 'nil))
                                         (mapcar (lambda (c) (if (eq 'FieldDecl (elt c 1)) c nil))
                                                 (shm-decl-ast)))))))
    (if-let ((data-index (shm-find-current-data-member-index-at-point)))
        (let ((bounds-data-index (elt (shm-get-data-constructor-members-points) data-index)))
          (delq nil
                (mapcar (lambda (b) (if (within-interval bounds-data-index b) b nil))
                        field-list)))
      field-list)))

(defun within-interval (data field)
  "Check if field constructor is within a data constructor and
give the appropriate index."
  (let ((data-start1 (marker-position (car data)))
        (data-end1 (marker-position (cdr data)))
        (field-start1 (marker-position (car field)))
        (field-end1 (marker-position (cdr field))))
   (and (< data-start1 field-start1) (> data-end1 field-start1)
        (< data-start1 field-end1) (> data-end1 field-end1))))

(defun shm-find-current-field-member-index-at-point ()
  "Find the index of the current data field constructor with the
current data declaration."
  (let ((location (point)))
    (if-let ((data-index (shm-find-current-data-member-index-at-point)))
        (let ((bounds-data-index (elt (shm-get-data-constructor-members-points) data-index)))
          (-find-index (lambda (bounds) (and
                                    (>= (marker-position (cdr bounds)) location)
                                    (<= (marker-position (car bounds)) location)))
                       (delq nil
                             (mapcar (lambda (b) (if (within-interval bounds-data-index b) b nil))
                                     (shm-get-field-constructor-members-points)))))
      (-find-index (lambda (bounds) (and
                                (>= (marker-position (cdr bounds)) location)
                                (<= (marker-position (car bounds)) location)))
                   (shm-get-field-constructor-members-points)))))

(defun shm-get-data-constructor-members-points ()
  "Find the region bounds of all the data constructors within the
current data declaration."
  (when (shm-in-data-constructor)
    (mapcar (lambda (ps) (cons (elt ps 2) (elt ps 3)))
            (remove-if (lambda (n) (eq n 'nil))
                       (mapcar (lambda (c) (let ((decl (elt c 1)))
                                        (cond
                                         ((eq decl 'QualConDecl) c)
                                         (t nil))))
                               (shm-decl-ast))))))

(defun shm-find-current-data-member-index-at-point ()
  "Find the index of the current data constructor with the
current data declaration."
  (let ((location (point)))
    (-find-index (lambda (bounds) (and
                              (>= (marker-position (cdr bounds)) location)
                              (<= (marker-position (car bounds)) location)))
                 (shm-get-data-constructor-members-points))))

(defun shm-align-regexp (arg regexp)
  "Align the region covered by the current node with REGEXP. If
ARG is non-nil, then just perform `align-regexp'."
  (interactive "p\nsAlign regexp: ")
  (cond
   ((eq arg 4) (align-regexp
                (region-beginning)
                (region-end)
                (concat "\\(\\s-*\\)" regexp) 1 1 nil))
   (t
    (let ((current (shm-current-node)))
      (align-regexp
       (shm-node-start current)
       (shm-node-end current)
       (concat "\\(\\s-*\\)" regexp) 1 1 nil)))))

(defun shm-transpose-data-constructors (m n)
  "Transpose the mth and nth data constructors. This function
will silently fail if the m or n are not valid indices (the first
index is 0). Here's an example of the function working as
intended. In this example, the 1st and 9th data constructors will
be swapped.

data JSValue
  = JSLetIn String
            JSValue
            JSValue
  | JSLambda [String]
             JSValue (**)
  | JSNull
  | JSIdentifier String
  | JSString String
  | JSArray [JSValue]
  | JSIndexInfo JSValue
                JSValue
  | JSApplication JSValue
                  [JSValue]
  | JSNumber Double
  | JSObject [(String, JSValue)] (**)

data JSValue
  = JSLetIn String
            JSValue
            JSValue
  | JSObject [(String, JSValue)] (**)
  | JSNull
  | JSIdentifier String
  | JSString String
  | JSArray [JSValue]
  | JSIndexInfo JSValue
                JSValue
  | JSApplication JSValue
                  [JSValue]
  | JSNumber Double
  | JSLambda [String]
             JSValue (**)"
  (let* ((bounds (bounds-of-thing-at-point 'defun))
         (regions (cond ((shm-find-current-field-member-index-at-point) (shm-get-field-constructor-members-points))
                        ((shm-find-current-data-member-index-at-point) (shm-get-data-constructor-members-points))
                        (t (error "Not a valid point for transposition."))))
         (m1 (car (elt regions m)))
         (m2 (cdr (elt regions m)))
         (m3 (car (elt regions n)))
         (m4 (cdr (elt regions n))))
    (when (and (shm-in-data-constructor)
               n
               m
               (>= m 0)
               (>= n 0)
               (< n (length regions))
               (< m (length regions))
               (not (= m n)))
      ;; We need to move the point to the Ident node for the transposition to work correctly.
      (let ((x (elt (cdr (shm-current-node-pair)) 1)))
        (cond ((eq x 'DataDecl)
               (progn
                 (forward-word 1)
                 (shm-transpose-data-constructors-helper bounds m1 m2 m3 m4)))
              ((eq x 'Ident) (shm-transpose-data-constructors-helper bounds m1 m2 m3 m4))
              ((eq x 'FieldDecl)
               (progn
                 (if (= (point (shm-node-start (cdr (shm-current-node-pair)))))
                     (shm-set-node-overlay)
                   (progn (shm/backward-node) (shm-set-node-overlay)))
                 (shm-transpose-data-constructors-helper bounds m1 m2 m3 m4)))
              ((member x '(QualConDecl ConDecl))
               (progn
                 (if (= (point) (shm-node-start (cdr (shm-current-node-pair))))
                     (shm-set-node-overlay)
                   (progn (shm/backward-node) (shm-set-node-overlay)))
                 (shm-transpose-data-constructors-helper bounds m1 m2 m3 m4)))
              (t (progn
                   (while (not (eq 'ConDecl (elt (cdr (shm-current-node-pair)) 1)))
                     (shm/goto-parent))
                   (shm-set-node-overlay)
                   (shm-transpose-data-constructors-helper bounds m1 m2 m3 m4))))))))

(defun shm-transpose-data-constructors-helper (bounds m1 m2 m3 m4)
  (let ((commentEndA
         (let ((orig (marker-position m2)))
           (save-excursion
             (goto-char (marker-position m2))
             (if (comment-forward 1)
                 (progn
                   (while (comment-forward))
                   (skip-chars-backward " " nil)
                   (backward-char)
                   (point))
               (if (looking-at "\}")
                   (if (= (+ (save-excursion (skip-chars-backward " ")) (point)) (point-at-bol))
                       (marker-position m2)
                     (progn
                       (delete-horizontal-space)
                       (let* ((start (point))
                              (commentbounds
                               (save-excursion
                                 (progn
                                   (if (search-forward "--" (cdr bounds) t)
                                       (let ((startcommentblock (- (point) 2)))
                                         (backward-char 2)
                                         (while (comment-forward))
                                         (cons startcommentblock (point)))
                                     nil))))
                              (text (if commentbounds
                                        (delete-and-extract-region
                                         (car commentbounds)
                                         (cdr commentbounds))
                                      "")))
                         (goto-char start)
                         (save-excursion
                           (shm/newline-indent-proxy))
                         (insert " ")
                         (insert text)
                         (point)))) ;The text will be unnormalized for now. May fix this later.
                 (let ((sub (buffer-substring-no-properties orig (point))))
                   (if (-all-p (lambda (b)
                                 (or (= 32 b) (= 10 b) (= 9 b)  (= 13 b)))
                               (string-to-list sub))
                       orig
                     (progn
                       (skip-chars-backward " " nil)
                       (backward-char)
                       (point)))))))))
        (commentEndB
         (let ((orig (marker-position m4)))
           (save-excursion
             (goto-char (marker-position m4))
             (if (comment-forward 1)
                 (progn
                   (while (comment-forward))
                   (skip-chars-backward " " nil)
                   (backward-char)
                   (point))
               (if (looking-at "\}")
                   (if (= (+ (save-excursion (skip-chars-backward " ")) (point)) (point-at-bol)) ; The \} might be on a newline.
                       (marker-position m4)
                     (progn
                       (delete-horizontal-space)
                       (let* ((start (point))
                              (commentbounds
                               (save-excursion
                                 (progn
                                   (if (search-forward "--" (cdr bounds) t)
                                       (let ((startcommentblock (- (point) 2)))
                                         (backward-char 2)
                                         (while (comment-forward))
                                         (cons startcommentblock (point)))
                                     nil))))
                              (text (if commentbounds
                                        (delete-and-extract-region
                                         (car commentbounds)
                                         (cdr commentbounds))
                                      "")))
                         (goto-char start)
                         (save-excursion
                           (shm/newline-indent-proxy))
                         (insert " ")
                         (insert text)
                         (point)))) ;The text will be unnormalized for now. May fix this later.
                 (let ((sub (buffer-substring-no-properties orig (point))))
                   (if (-all-p (lambda (b) ;dirty fix
                                 (or (= 32 b) (= 10 b) (= 9 b)  (= 13 b)))
                               (string-to-list sub))
                       orig
                     (progn
                       (skip-chars-backward " " nil)
                       (backward-char)
                       (point))))))))))
    (message "Region A: m1 %s m2 %s commentEndA %s\nRegion B: m3 %s m4 %s commentEndB %s"
             (marker-position m1) (marker-position m2) commentEndA
             (marker-position m3) (marker-position m4) commentEndB)
    (transpose-regions
     (marker-position m1)
     commentEndA
     (marker-position m3)
     commentEndB)
    (let ((parsed-ast (shm-get-ast (if (bound-and-true-p structured-haskell-repl-mode)
                                       "stmt"
                                     "decl")
                                   (car bounds) (cdr bounds))))
      (let ((bail (lambda ()
                    (when shm-display-quarantine
                      (shm-quarantine-overlay (car bounds) (cdr bounds)))
                    (setq shm-lighter " SHM!")
                    nil)))
        (if parsed-ast
            (progn
              (when (bound-and-true-p structured-haskell-repl-mode)
                (shm-font-lock-region (car bounds) (cdr bounds)))
              (let ((ast (shm-get-nodes parsed-ast (car bounds) (cdr bounds))))
                (if ast
                    (progn (setq shm-lighter " SHM")
                           (set-marker m1 nil)
                           (set-marker m2 nil)
                           (set-marker m3 nil)
                           (set-marker m4 nil)
                           (shm-set-decl-ast (car bounds) ast)
                           (shm-delete-overlays (point-min) (point-max) 'shm-quarantine)
                                        ;This was my initial guess for
                                        ;silencing the message at the end. It
                                        ;doesn't work.
                           (let ((inhibit-message t))
                             (shm/init)))
                  (funcall bail))))
          (funcall bail))))))

(define-key haskell-mode-map (kbd "M-g k") 'shm-push-current-data-constructor-up)
(define-key haskell-mode-map (kbd "M-g j") 'shm-push-current-data-constructor-down)

(defun shm/goto-topmost-parent ()
  "Go to the topmost parent of the current node."
  (let ((loc (point)))
    (shm/goto-parent)
    (while (< (point) loc)
      (shm/goto-parent)
      (setq loc (point)))))

(defun shm-add-deriving-clause ()
  "Add a deriving clause to the data type declaration. If successful,
the point should be at the beginning of an evaporating undefined."
  (interactive)
  (shm/goto-topmost-parent)
  (let ((current (shm-current-node))
        (line (line-number-at-pos)))
    (cond ((eq (elt current 1) 'DataDecl)
           (if-let
               ((location (save-excursion
                            (search-forward "deriving " (save-excursion
                                                          (end-of-defun) (point))))))
               (progn
                 (goto-char location)
                 (if (looking-at "(") (forward-char)))
               (progn
                 (shm/forward-node)
              ;; The logic contained within the "if statement" assumes
              ;; that the data declaration at point can be parsed by
              ;; structured-haskell-mode. Specifically, it checks if
              ;; the entire declaration fits on a single line; if so it
              ;; creates space for the deriving clause to fit on the
              ;; same line. Otherwise, it creates a new line for the
              ;; desired clause.
              (if (= line (line-number-at-pos))
                  (insert " ")
                (shm/newline-indent-proxy))
              (insert "deriving (")
              (shm/insert-undefined)
              (save-excursion
                (forward-word)
                (insert ")")))))
          (t (message "The point is not contained within a data type declaration.")))))

(defun haskell-set-tab-stop-list ()
  (setq tab-stop-list '(2 4 8 12)))

(defun shm-narrow-to-current-node ()
  (interactive)
  (when-let ((current (shm-current-node)))
    (narrow-to-region
     (shm-node-start current )
     (shm-node-end current))))

(add-hook 'structured-haskell-mode-hook
          (lambda () (define-key shm-map (kbd "C-x n s") 'shm-narrow-to-current-node)
            (define-key shm-map (kbd "C-$") 'nil)
            (define-key shm-map (kbd "M-]") 'shm/goto-last-point)
            (define-key shm-map (kbd "C-#") 'nil)))

(defun shm-fill-comment ()
  "Fill the comment(s)."
  (cond ((shm-in-comment)
         (let* ((start (save-excursion
                         (comment-beginning)))
                (end (save-excursion
                       (goto-char start)
                       (forward-sexp)
                       (backward-char 3)
                       (point))))
           (fill-individual-paragraphs start end)))
        ((when-let ((beg (save-excursion
                           (comment-beginning))))
           (string= "-- " (buffer-substring-no-properties beg (point))))
         (let* ((start (save-excursion (comment-beginning)))
                (commentStart (save-excursion
                                (goto-char start)
                                (let ((back start))
                                  (while back
                                    (goto-char (- back 1))
                                    (setq back (comment-beginning))))
                                (point)))
                (commentEnd (save-excursion
                              (goto-char start)
                              (while (comment-forward))
                              (point))))
           (fill-region commentStart commentEnd)))))

(defun shm-fill-string  ()
  "Fill the string."
  (when (shm-in-string)
    (let ((beg (save-excursion
                 (backward-up-list)
                 (+ 1 (point))))
          (end (save-excursion
                 (backward-up-list)
                 (forward-sexp)
                 (- (point) 1))))
      (fill-region beg end)
      (save-excursion
        (goto-char beg)
        (goto-char (point-at-eol))
        (while (< (point) end)
          (insert "\\")
          (forward-char)
          (insert "\\")
          (goto-char (point-at-eol)))))))

(defadvice hindent-reformat-decl-or-fill (after fill-string activate)
  (shm-fill-string))

(defadvice shm/double-quote (around wrap-with-quotes activate)
  (if (use-region-p)
      (progn (save-excursion (goto-char (region-beginning)) (insert "\""))
             (save-excursion (goto-char (region-end)) (insert "\"")))
    ad-do-it))

;;; Undo-tree-undo on text with the evaporating text property really
;;; gets on my nerves sometimes. This makes undo-tree-undo function
;;; behave rationally in the scenario.
(defadvice undo-tree-undo (after check-if-word-after-is-undefined activate)
  (when (and (string= (thing-at-point 'word t) "undefined")
             (eq major-mode 'haskell-mode))
    (let ((bds (bounds-of-thing-at-point 'word)))
      (delete-region (car bds) (cdr bds))
      (shm/insert-undefined))))

;;; something to maybe add to structured-haskell-mode
(defun shm/pure-letify-node (&optional node)
  (interactive)
  (let* ((current-node (or node (shm-current-node)))
         (text (delete-and-extract-region (shm-node-start current-node) ;I might want to use shm opinionated killing and yanking functions
                                          (shm-node-end current-node)))
         column)
    (setq column (current-column))
    (insert "let ")
    (shm/insert-underscore)
    (save-excursion
      (insert " = ")
      (shm/insert-undefined)
      (forward-word)
      (forward-char)
      (beginning-of-line)
      (just-one-space column)
      (insert "in ")
      (insert text))))

(defadvice shm/yank (after shm-exchange-point-and-mark activate)
  (exchange-point-and-mark)
  (deactivate-mark))

(defun helm-haskell-interactive-mode-history ()
  "Preconfigured helm for haskell-interactive-mode history."
  (interactive)
  (let* ((end (point))
        (beg (marker-position haskell-interactive-mode-prompt-start))
        (input (buffer-substring beg end))
        flag-empty
        (session-history haskell-interactive-mode-history))
    (when (eq beg end)
      (insert " ")
      (setq flag-empty t)
      (setq end (point)))
    (unwind-protect
        (with-helm-show-completion beg end
          (helm :sources (helm-make-source "Haskell Interactive Mode History"
                             'helm-haskell-interactive-mode-history-source)
                :buffer "*helm haskell-interactive-mode history*"
                :resume 'noresume
                :input input))
      (when (and flag-empty
                 (looking-back " " (1- (point))))
        (delete-char -1)))))

(defclass helm-haskell-interactive-mode-history-source (helm-source-in-buffer)
  ((init :initform (lambda ()
                     (with-current-buffer (helm-candidate-buffer 'global)
                       (mapc (lambda (item) (insert (concat item "\n")))
                             (with-current-buffer
                                 helm-current-buffer
                               haskell-interactive-mode-history)))))
   (nomark :initform t)
   (keymap :initform helm-haskell-interactive-mode-history-map)
   (filtered-candidate-transformer :initform (lambda (candidates sources)
                                               (reverse candidates)))
   (candidate-number-limit :initform 9999)
   (action :initform (lambda (candidate)
                       (haskell-interactive-mode-kill-input)
                       (insert candidate))))
  "Helm class to define source for Haskell Interactive Mode History.")

(defun haskell-interactive-mode-kill-input ()
  "Kill all text at prompt."
  (kill-region
   (marker-position haskell-interactive-mode-prompt-start)
   (point-max)))

(defvar helm-haskell-interactive-mode-history-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map helm-map)
    (define-key map (kbd "M-p") 'helm-next-line)
    (define-key map (kbd "M-n") 'helm-previous-line)
    map)
  "Keymap for `helm-haskell-interactive-mode-history'.")

(provide 'init-haskell)
;;; init-haskell.el ends here
