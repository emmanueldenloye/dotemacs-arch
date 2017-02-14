;;;###autoload
(defun rename-this-file-and-buffer (new-name)
  "Renames both current buffer and file it's visiting to
NEW-NAME."
  (interactive "sNew name: ")
  (let ((name (buffer-name))
        (filename (buffer-file-name)))
    (unless filename
      (error "Buffer '%s' is not visiting a file!" name))
    (if (get-buffer new-name)
        (message "A buffer named '%s' already exists!" new-name)
      (progn
        (when (file-exists-p filename)
          (rename-file filename new-name 1))
        (rename-buffer new-name)
        (set-visited-file-name new-name)))))

;;;###autoload
(defun delete-this-file ()
  "Delete the current file, and kill the buffer."
  (interactive)
  (or
   (buffer-file-name)
   (error "No file is currently being edited"))
  (when (yes-or-no-p
         (format "Really delete '%s'?"
                 (file-name-nondirectory buffer-file-name)))
    (delete-file (buffer-file-name))
    (kill-buffer (current-buffer))))

;;;###autoload
(defun my-goto-scratch-buffer ()
  "Create a new scratch buffer."
  (interactive)
  (switch-to-buffer
   (get-buffer-create
    "*scratch*"))
  (emacs-lisp-mode)
  (insert
   (format
    ";; %s\n\n"
    (replace-regexp-in-string
     "\n" "\n;; " ; comment each line
     (replace-regexp-in-string
      "\n$" "" ; remove trailing linebreak
      (shell-command-to-string "fortune"))))))

;;;###autoload
(defun my-insert-last-kbd-macro ()
  "Insert the last keyboard macro into current buffer."
  (interactive)
  (name-last-kbd-macro 'my-last-macro)
  (insert-kbd-macro 'my-last-macro))

;;;###autoload
(defun add-auto-mode (mode &rest patterns)
  "Add entries to `auto-mode-alist' to use `MODE' for all given
file `PATTERNS'."
  (dolist (pattern patterns)
    (add-to-list 'auto-mode-alist (cons pattern mode))))

;;;###autoload
(defun switch-to-previous-buffer ()
  "Switch to previous buffer."
  (interactive)
  (switch-to-buffer
   (other-buffer (current-buffer) 1)))

;;;###autoload
(defun switch-to-previous-buffer-other-window ()
  (interactive)
  (switch-to-buffer-other-window
   (other-buffer (current-buffer) 1)))

;;;###autoload
(defun eod-insert-buffer-directory ()
  "Insert the directory of the current bufer (FILENAME)"
  (interactive)
  (let ((filename (buffer-file-name)))
    (when (and
           (not (file-directory-p filename))
           (file-exists-p filename)
           (not (eq major-mode 'dired-mode)))
      (insert (file-name-directory filename)))))

(defun eod-open-google-hangouts ()
  "Open Google hangouts."
  (interactive)
  (start-process
   "google-hangouts"
   nil
   "chromium"
   "--app-id=knipolnnllmklapflnccelgolnpehhpl"))

;; This doesn't work the way it should. It opens the signal and
;; google-hangouts apps together.
;; (defun eod-open-signal ()
;;   "Open Signal."
;;   (interactive)
;;   (start-process
;;    "signal"
;;    nil
;;    "chromium"
;;    "--app-id=bikioccmkafdpakkkcpdbppfkghcmihk"))

;;;###autoload
(defun eod-show-buffer-directory ()
  "Show the directory of the current buffer (FILENAME) in the
minibuffer area."
  (interactive)
  (let ((filename (buffer-file-name)))
    (when (and
           (not (file-directory-p filename))
           (file-exists-p filename)
           (not (eq major-mode 'dired-mode)))
      (message (file-name-directory filename)))))

(defun eod-get-buffer-filename ()
  "Place the buffer's filename at the top of the kill ring."
  (let ((filename (buffer-file-name)))
    (when (and
           (not (file-directory-p filename))
           (file-exists-p filename)
           (not (eq major-mode 'dired-mode)))
      (kill-new filename))))

;;;###autoload
(defun dont-kill-emacs ()
  (interactive)
  (error (substitute-command-keys "To exit emacs: \\[kill-emacs]")))

(global-set-key (kbd "C-x C-c") 'dont-kill-emacs)

;;;###autoload
(defun mplayer ()
  "An interface to mplayer."
  (interactive)
  (shell-command (concat "xterm -e mplayer "
                         (shell-quote-argument
                          (expand-file-name
                           (read-file-name "Filename: ")))
                         "& ")))

;;;###autoload
(defun unpop-to-mark-command ()
  "Unpop off mark ring. Does nothing if  ring is empty."
  (interactive)
  (when mark-ring
    (let ((pos (marker-position (car (last mark-ring)))))
      (if (not (= (point) pos))
          (goto-char pos)
        (setq mark-ring (cons (copy-marker (mark-marker)) mark-ring))
        (set-marker (mark-marker) pos)
        (setq mark-ring (nbutlast mark-ring))
        (goto-char (marker-position (car (last mark-ring))))))))

(global-set-key (kbd "C-S-SPC") 'unpop-to-mark-command)

;;;###autoload
(defun date (&optional insert)
  "Display the current date and time.
With a prefix arg, INSERT it into the buffer."
  (interactive "p")
  (funcall (if insert 'insert 'message)
           (format-time-string
            "%a, %d, %b, %Y, %T, %Z"
            (current-time))))

;;;###autoload
(defun reload-init-file ()
  "Reload init.el file"
  (interactive)
  (load user-init-file)
  (message "Reloaded init.el OK."))

;;;###autoload
(defun open-init-file ()
  (interactive)
  (find-file user-init-file))

;;;###autoload
(defun open-as-root (filename)
  "Open FILENAME as root."
  (interactive "f")
  (find-file (concat "/sudo:root@localhost:" filename)))

;;;###autoload
(defun open-buffer-as-root ()
  (interactive)
  (-if-let* ((filename (buffer-file-name)))
      (progn
        (kill-buffer (buffer-name))
        (open-as-root filename))
    (message "Current buffer: %s is not a file." (buffer-name))))

;;;###autoload
(defun eod-send-file-name-to-minibuffer ()
  (interactive)
  (insert
   (buffer-file-name
    (window-buffer
     (minibuffer-selected-window)))))

(define-key
  minibuffer-local-map
  (kbd "C-c f") 'eod-send-file-name-to-minibuffer)
(define-key
  minibuffer-local-map
  (kbd "C-c C-f") 'eod-send-file-name-to-minibuffer)

;;;###autoload
(defun eod-copy-file-name-to-clipboard (&optional arg)
  "Copy the current buffer file name to the clipboard.
When invoked with an argument, strip the directory off the of the
filename."
  (interactive "p")
  (let ((filename
         (if (equal major-mode 'dired-mode)
             default-directory
           (if arg
               (file-name-base (buffer-file-name))
             (buffer-file-name)))))
    (when filename
      (kill-new filename)
      (message
       "Copied buffer file name '%s' to the clipboard. " filename))))

;;;###autoload
(defun eval-sexp-and-replace (value)
  "Evaluate the sexp at point and replace it with its value "
  (interactive (list (eval-last-sexp nil)))
  (mark-sexp -1)
  (delete-region (region-beginning) (region-end))
  (insert (format "%S" value)))

(global-set-key (kbd "C-c e") 'eval-sexp-and-replace)

(global-set-key (kbd "C-c y") 'clipboard-yank)

;;;###autoload
(defun eod-join-next-line (arg)
  "Join the next line with the current one."
  (interactive "p")
  (if (use-region-p)
      (let* ((pl (line-number-at-pos (point)))
             (ml (line-number-at-pos (mark)))
             (difflines (abs (- ml pl))))
        (save-excursion
          (dotimes (i difflines)
            (if (> (point) (mark))
                (join-line nil)
              (join-line 1)))))
    (dotimes (i (abs arg))
      (if (>= arg 0)
          (join-line 1)
        (join-line nil)))))

;;;###autoload
(defun eod-test-net ()
  (interactive)
  (async-shell-command
   "ping -c 3 8.8.8.8"))

;;;###autoload
(defun eod-dropbox-status-message ()
  "Get the current dropbox status."
  (interactive)
  (if (eq 1 (shell-command "dropbox-cli running"))
      (message "Dropbox is running.")
    (message "Dropbox is not running.")))

;;;###autoload
(defun eod-dropbbox-print-puburl-file ()
  "Print the public url of the current file."
  (interactive)
  (when (file-in-directory-p
         (buffer-file-name)
         "~/Dropbox")
    (async-shell-command
     (concat
      "dropbox-cli puburl "
      (buffer-file-name)))))

;;;###autoload
(defun eod-dropbbox-print-filestatus ()
  "Print the status of the current file."
  (interactive)
  (when (file-in-directory-p
         (buffer-file-name)
         "~/Dropbox")
    (async-shell-command
     (concat
      "dropbox-cli filestatus "
      (buffer-file-name)))))

;;;###autoload
(defun eod-dropbox-print-maindir ()
  "Print the status of the Dropbox folder and its contents."
  (interactive)
  (async-shell-command "dropbox-cli filestatus ~/Dropbox/*"))

;;;###autoload
(defun eod-dropbox-status ()
  "Get the current dropbox sync status."
  (interactive)
  (async-shell-command "dropbox-cli status"))

;;;###autoload
(defun eod-dropbox-print-directory-status ()
  "Print the status of the file's current directory.
This still needs some work."
  (interactive)
  (let (file (buffer-file-name))
    (if (and (file-exists-p file)
             (file-in-directory-p file "~/Dropbox"))
        (async-shell-command
         (concat "dropbox-cli filestatus "
                 (file-name-directory file)
                 "*"))
      (message "Check if the current directory is a sub directory of \"~/Dropbox\""))))

;;;###autoload
(defun eod-sudo-edit (&optional arg)
  (interactive "p")
  (if (or arg (not buffer-file-name))
      (find-file
       (concat
        "/sudo:root@localhost:"
        (helm-read-file-name "File: ")))
    (find-alternate-file
     (concat
      "/sudo:root@localhost:"
      buffer-file-name))))

;;;###autoload
(defun prelude-search (query-url prompt)
  "Open the search URL constructed with the QUERY-URL.
PROMPT sets the `read-string prompt."
  (browse-url
   (concat query-url
           (url-hexify-string
            (if mark-active
                (buffer-substring (region-beginning) (region-end))
              (read-string prompt))))))

(defmacro prelude-install-search-engine
    (search-engine-name search-engine-url search-engine-prompt)
  "Given some information regarding a search engine, install the
interactive command to search through them"
  `(defun ,(intern (format "eod-%s" search-engine-name)) ()
     ,(format
       "Search %s with a query or region if any." search-engine-name)
     (interactive)
     (prelude-search ,search-engine-url ,search-engine-prompt)))

(prelude-install-search-engine
 "google"
 "https://www.google.com/?gws_rd=ssl#q=" "Google: ")
(prelude-install-search-engine
 "youtube"
 "https://www.youtube.com/results?search_query=" "Search YouTube: ")
(prelude-install-search-engine
 "github"
 "https://github.com/search?q=" "Search GitHub: ")
(prelude-install-search-engine
 "duckduckgo"
 "https://duckduckgo.com/?q=" "Search DuckDuckgo: ")
(prelude-install-search-engine
 "scholar"
 "https://scholar.google.com/scholar?q=" "Search Google Scholar: ")

(global-set-key (kbd "C-c / g s") 'eod-scholar)

(global-set-key (kbd "C-c / y") 'eod-youtube)

(global-set-key (kbd "C-c / g g") 'eod-google)

(global-set-key (kbd "C-c / G") 'eod-github)

(global-set-key (kbd "C-c / d") 'eod-duckduckgo)

;; (defun eod-search-google nil)
;; (defun eod-search-google ()
;;   "Google the selected region if any, display a query prompt otherwise."
;;   (interactive)
;;   (browse-url
;;    (concat
;;     "http://www.google.com/search?ie=utf-8&oe=utf-8&q="
;;     (url-hexify-string (if mark-active
;;                            (buffer-substring (region-beginning) (region-end))
;;                          (read-string "Search Google: "))))))

;; (defun eod-search-duckduckgo nil)
;; (defun eod-search-duckduckgo ()
;;   "Google the selected region if any, display a query prompt otherwise."
;;   (interactive)
;;   (browse-url
;;    (concat
;;     "https://duckduckgo.com/?q="
;;     (url-hexify-string (if mark-active
;;                            (buffer-substring (region-beginning) (region-end))
;;                          (read-string "Search DuckDuckGo: ")))
;;     "&ia=about")))

;; (defun eod-search-scholar nil)
;; (defun eod-search-scholar ()
;;   "Google (Scholar) the selected region if any, display a query prompt otherwise."
;;   (interactive)
;;   (browse-url
;;    (concat
;;     "https://scholar.google.com/scholar?q="
;;     (url-hexify-string (if mark-active
;;                            (buffer-substring (region-beginning) (region-end))
;;                          (read-string "Search Google Scholar: "))))))

;; toggle vertical/window split
;;;###autoload
(defun toggle-window-split ()
  (interactive)
  (if (= (count-windows) 2)
      (let* ((this-win-buffer (window-buffer))
             (next-win-buffer (window-buffer (next-window)))
             (this-win-edges (window-edges (selected-window)))
             (next-win-edges (window-edges (next-window)))
             (this-win-2nd (not (and (<= (car this-win-edges)
                                         (car next-win-edges))
                                     (<= (cadr this-win-edges)
                                         (cadr next-win-edges)))))
             (splitter
              (if (= (car this-win-edges)
                     (car (window-edges (next-window))))
                  'split-window-horizontally
                'split-window-vertically)))

        (delete-other-windows)
        (let ((first-win (selected-window)))
          (funcall splitter)
          (if this-win-2nd (other-window 1))
          (set-window-buffer (selected-window) this-win-buffer)
          (set-window-buffer (next-window) next-win-buffer)
          (select-window first-win)
          (if this-win-2nd (other-window 1))))))

(add-hook 'emacs-startup-hook 'toggle-window-split)

(global-set-key (kbd "C-c C-)") 'toggle-window-split)
(global-set-key (kbd "C-c )") 'toggle-window-split)

;;;###autoload
(defun prelude-colorize-compilation-buffer ()
  "Colorize a compilation buffer."
  (interactive)
  ;; we don't want to mess with child modes such as grep-mode, ack,
  ;; ag, etc.
  (when (eq major-mode 'compilation-mode)
    (let ((inhibit-read-only t))
      (ansi-color-apply-on-region (point-min) (point-max)))))

;;;###autoload
(defun try-expand-flexible-abbrev (old)
  "Try to complete word using flexible matching.

Flexible matching works by taking the search string and then
 interspering it with a regexp for any character. So, if you try
 to do a flexible match for `foo' it will match the word
 `findOtherOtter' but also `fixTheBoringOrange' and
 `ifthisisboringstopreadingnow'.

The argument OLD has to be nil in the first call of this
function, and t for subsequent calls (for further possible
completions of the same string). It returns t if a new completion
is found, nil otherwise."
  (unless old
    (he-init-string (he-lisp-symbol-beg) (point))
    (if  (not (he-string-member he-search-string he-tried-table))
        (setq he-tried-table (cons he-search-string he-tried-table)))
    (setq he-expand-list
          (and (not (equal he-search-string ""))
               (he-flexible-abbrev-collect he-search-string))))
  (while (and he-expand-list
              (he-string-member (car he-expand-list) he-tried-table))
    (setq he-expand-list (cdr he-expand-list)))
  (cond ((null he-expand-list)
         (if old (he-reset-string))
         nil)
        (t (he-substitute-string (car he-expand-list))
           (setq he-expand-list (cdr he-expand-list))
           t)))

;;;###autoload
(defun he-flexible-abbrev-collect (str)
  "Find and collect all words that flex-matchs STR.
See docstring for `try-expand-flexible-abbrev' for information
about what flexible matching means in this context."
  (let ((collection nil)
        (regexp (he-flexible-abbrev-create-regexp str)))
    (save-excursion
      (goto-char (point-min))
      (while (search-forward-regexp regexp nil t)
        ;; Is there a better or quicker way than using
        ;; `thing-at-point' here?
        (setq collection (cons (thing-at-point 'word) collection)))
      collection)))

;;;###autoload
(defun he-flexible-abbrev-create-regexp (str)
  "Generate regexp for flexible matching of STR.
See docstring for `try-expand-flexible-abbrev' for information
about what flexible matching means in this context."
  (concat "\\b"
          (mapconcat (lambda (x) (concat "\\w*" (list x))) str "")
          "\\w*" "\\b"))

;; This is not the cleanest implementation, perhaps, but it works so whatever. What can ya do?
;;;###autoload
(defun eod-comment-or-uncomment-line (arg)
  "Comment or uncomment the next ARG lines. If ARG is negative,
comment or uncomment the previous ARG lines. This command does
not do anything on empty lines."
  (interactive "p")
  (let (start end)
    (save-excursion
      (if (and (> arg 0) (not (eq arg 0)))
          (progn
            (beginning-of-line)
            (setq start (point)))
        (progn
          (end-of-line)
          (setq start (point))))
      (when (not (eq arg 0))
        (cond
         ((> arg 1) (forward-line arg))
         ((< arg 0) (forward-line (+ arg 1)))
         (t
          (forward-line)))
        (setq end (point))
        (if (>= arg 0)
            (comment-or-uncomment-region start end)
          (comment-or-uncomment-region end start))))))

(global-set-key (kbd "C-c C-;") 'eod-comment-or-uncomment-line)

;;;###autoload
(defun eod-insert-dollar ()
  "Insert a dollar into the buffer as expected, unless the
current major mode is org-mode or latex-mode."
  (interactive)
  (if (member major-mode '(org-mode latex-mode))
      (progn (insert "$$")
             (backward-char))
    (insert "$")))

;; (global-set-key (kbd "$") 'self-insert-command)
(key-chord-define-global ",." 'eod-insert-dollar)

;;;###autoload
(defun avy-move-region ()
  "Select two lines and move the text between them here."
  (interactive)
  (avy-with avy-move-region
    (let ((beg (avy--line))
          (end (avy--line))
          (pad (if (bolp) "" "\n")))
      (move-beginning-of-line nil)
      (insert
       (delete-and-extract-region
        beg
        (save-excursion
          (goto-char end)
          (line-end-position)))
       pad))))

;; (defun zap-up-to-char (arg char)
;;   "Zap up to a character."
;;   (interactive "p\ncZap to char: ")
;;   (save-excursion
;;     (zap-to-char arg char)
;;     (insert char)))

;;;###autoload
(defun zap-to-char-save (arg char)
  "Zap up to a character, but save instead of kill."
  (interactive "p\ncZap to char: ")
  (save-excursion
    (zap-to-char arg char)
    (yank)))

;;;###autoload
(defun shk-yas/helm-prompt (prompt choices &optional display-fn)
  "Use helm to select a snippet. Put this into
`yas-prompt-functions.'"
  (interactive)
  (setq display-fn (or display-fn 'identity))
  (if (require 'helm-config)
      (let (tmpsource cands result rmap)
        (setq cands
              (mapcar (lambda (x) (funcall display-fn x)) choices))
        (setq rmap
              (mapcar (lambda (x)
                        (cons (funcall display-fn x) x)) choices))
        (setq tmpsource
              (list
               (cons 'name prompt)
               (cons 'candidates cands)
               '(action . (("Expand" .
                            (lambda (selection) selection))))))
        (setq result
              (helm-other-buffer
               '(tmpsource) "*helm-select-yasnippet*"))
        (if (null result)
            (signal 'quit "user quit!")
          (cdr (assoc result rmap))))
    nil))

;;; kill all comment in buffer -- Courtesy of Magnar
;;;###autoload
(defun comment-kill-all ()
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (comment-kill
     (save-excursion
       (goto-char (point-max))
       (line-number-at-pos)))))

(global-set-key (kbd "C-c z") 'zap-up-to-char)

;;; Courtesy of Mickey Peterson's blog.
;;;###autoload
(defun push-mark-no-activate ()
  "Pushes `point' to `mark-ring' and does not activate the
  region. Equivalent to \\[set-mark-command] when
  \\[transient-mark-mode] is disabled."
  (interactive)
  (push-mark (point) t nil)
  (message "Pushed mark to ring"))

(global-set-key (kbd "C-c SPC") 'push-mark-no-activate)

;;;###autoload
(defun eod-region-remove-properties ()
  "Remove the current region's properties and reinsert the
\"cleaned\" string."
  (interactive)
  (let ((text
         (delete-and-extract-region
          (region-beginning)
          (region-end))))
    (save-excursion
      (insert
       (with-temp-buffer
         (insert text)
         (buffer-substring-no-properties
          (point-min)
          (point-max)))))))

;;;###autoload
(defun eod-fill-or-unfill ()
  "Like `fill-paragraph', but unfill if used twice."
  (interactive)
  (let ((fill-column
         (if (eq last-command 'eod-fill-or-unfill)
             (progn (setq this-command nil)
                    (point-max))
           fill-column)))
    (call-interactively #'fill-paragraph)))

(global-set-key [remap fill-paragraph] #'eod-fill-or-unfill)

;;;###autoload
(defun eod-duplicate-line (arg)
  "Duplicate the current line ARG times. If ARG is negative,
then replicate the current line ARG times above the current
line."
  (interactive "p")
  (let ((text (string-remove-suffix
               "\n"
               (thing-at-point 'line t)))
        (col (current-column))
                                        ;This is needed because newline acts strangely in modes with
                                        ;auto-fill-mode turned on. TLDR: Newline fucks shit up.
        (auto-fill-on auto-fill-function))
    (unwind-protect
        (when (/= arg 0)
          (when auto-fill-on
            (auto-fill-mode -1))
          (save-excursion
            (end-of-line)
            (dotimes (i (abs arg))
              (newline)
              (insert text))
            (when (< arg 0)
              (forward-line (* -1 arg))
              (forward-char col))))
      (when auto-fill-on
        (auto-fill-mode 1)))))

;;;###autoload
(defun eod-replace-word-with-same-char (char)
  (interactive (list (read-char "GibsMeDat: ")))
  (let ((len (length (thing-at-point 'word)))
        (location (point))
        (bds (bounds-of-thing-at-point 'word)))
    (delete-region (car bds) (cdr bds))
    (insert (make-string len char))
    (goto-char location)))

;;; This can handle the negative sign, but the point still moves. I
;;; understand why it does, but it really annoys me anyway.
;;;###autoload
(defun my-extract-integer-at-point ()
  (let ((beg (save-excursion
               (while (looking-back "[0-9]")
                 (backward-char))
               (if (looking-back "-") (1- (point)) (point))))
        (end (save-excursion
               (when (looking-at "-")
                 (forward-char))
               (while (looking-at "[0-9]")
                 (forward-char))
               (point))))
    (when (not (= beg end))
      (let ((str (buffer-substring-no-properties beg end)))
        (list
         (point)
         beg end
         (string-to-number
          (s-trim str)))))))

;;;###autoload
(defun all-valid-chars-for-integer (string)
  "When valid return parsed integer otherwise return nil."
  (let ((str (string-to-list string))
        (nums (string-to-list "0123556789")))
    (when (cl-reduce
           (lambda (predicate c) (and predicate (member c nums)))
           (cdr str) :initial-value (or (string= (substring string 0 1) "-")
                                        (member (car str) nums)))
      (string-to-number string))))

;; (all-valid-chars-for-integer "-5067")

;;;###autoload
(defun eod-increment-number-at-point-helper (arg)
  (interactive "p")
  (let* ((ok (my-extract-integer-at-point))
         (orig (nth 0 ok))
         (beg (nth 1 ok))
         (end (nth 2 ok))
         (num (nth 3 ok)))
    (when (numberp num)
      (save-restriction
        (narrow-to-region beg end)
        (unwind-protect
            (let ((to-insert (format "%d" (+ arg num))))
              (goto-char (point-min))
              (cond
               ((> (- end beg) (length to-insert))
                (cond ((string= (buffer-substring beg (1+ beg)) "-")
                       (progn
                         (delete-and-extract-region (point-min) (point-max))
                         (insert to-insert)
                         (backward-char)))
                      ((string= (buffer-substring beg (1+ beg)) "0")
                       (progn
                         (delete-and-extract-region beg end)
                         (insert (format "0%d" (+ arg num)))
                         (backward-char)))
                      (t
                       (progn
                         (delete-and-extract-region beg end)
                         (insert to-insert)
                         (backward-char)))))
               ((< (- end beg) (length to-insert))
                (progn
                  (delete-and-extract-region beg end)
                  (let ((start (point)))
                    (insert to-insert)
                    (goto-char (1+ start)))))
               ((= (- end beg) (length to-insert))
                (progn
                  (delete-and-extract-region beg end)
                  (insert to-insert)
                  (backward-char (- (point) orig))))
               (t (error "Something went wrong"))))
          (widen))) ;this is not quite what I want. This function
                    ;should return nil when there is an error.
      t)))

;;;###autoload
(defun eod-increment-number-at-point (arg)
  (interactive "p")
  (let* ((ev last-command-event)
         (echo-keystrokes nil)
         (base (event-basic-type ev))
         (step
          (pcase base
            (?+ arg)
            (?- (- arg))
            ;; (?0 (if orig (lambda (x) orig))) figure this out later
            (_ arg))))
    ;; (unless orig (setq orig (number-at-point)))
    (when (eod-increment-number-at-point-helper step)
      (message "Use +,- for further adjustment")
      (set-transient-map
       (let ((map (make-sparse-keymap)))
         (dolist (mods '(() (control)))
           (dolist (key '(?- ?+))
             (define-key map (vector (append mods (list key)))
               `(lambda ()
                  (interactive)
                  (eod-increment-number-at-point ,(abs arg))))))
         map)))))

;;;###autoload
(defun eod-decrement-number-at-point (arg)
  (interactive "p")
  (eod-increment-number-at-point arg))

(global-set-key (kbd "C-c C-+") 'eod-increment-number-at-point)
(global-set-key (kbd "C-c C--") 'eod-decrement-number-at-point)

;;;###autoload
(defun eod-increment-inner-number (arg)
  (interactive "p")
  (insert
   (format
    "%d"
    (+ arg
       (string-to-number
        (delete-and-extract-region
         (1+ (search-backward-regexp "[^0-9]+"))
         (search-forward-regexp "[0-9]+")))))))

;;; This function is dumb!
;;;###autoload
(defun n-choose-k (arg1 arg2)
  "Calculate n choose k. ARG1 must be greater than or equal to ARG2."
  (if (and (numberp arg1) (numberp arg2)
           (<= arg2 arg1) (>= arg2 0))
      (let ((numerator (apply '* (number-sequence (+ 1 (- arg1 arg2)) arg1)))
            (denominator (apply '* (number-sequence 1 arg2))))
        (/ numerator denominator))
    (message "What have you done?")))

;;;###autoload
(defun eod-delete-region ()
  (unless (with-temp-buffer
            (describe-buffer-bindings (get-buffer-create "*Bindings-Kill-This*") (kbd "C-c"))
            (goto-char (point-min))
            (search-forward-regexp "^C-c d" (point-max) t))
    (local-set-key (kbd "C-c d") 'delete-region)
    (kill-buffer "*Bindings-Kill-This*")))

(add-hook 'find-file-hook 'eod-delete-region)

;; (defun eod-delete-region ()
;;   (interactive)
;;   (when
;;       (and (string-match
;;             "undefined"
;;             (format "%s" (describe-key-briefly (kbd "C-c d"))))
;;            (not
;;             (member major-mode
;;                     '(lisp-interaction-mode emacs-lisp-mode))))
;;     (local-set-key (kbd "C-c d") 'delete-region)))

;; (remove-hook 'prog-mode-hook 'eod-delete-region)

;;;###autoload
(defun concat-strings-separator (strings &optional separator)
  "Requisite documentation"
  (if (or (listp strings) (sequencep strings))
      (mapconcat
       'identity
       strings
       (if separator separator " "))
    (error "You did not provide a list or sequence as the first argument.")))

;;;###autoload
(defun eod-suspend-frame ()
  (interactive)
  (unless (display-graphic-p)
    (suspend-frame)))

(global-set-key (kbd "C-x C-z") 'eod-suspend-frame)
(global-set-key (kbd "C-z") 'eod-suspend-frame)

(defconst eod-important-buffer-names '("*scratch*" "*Messages*")
  "Names of buffers that should not be killed.")

;;;###autoload
(defun eod-do-not-kill-important-buffers ()
  "Inhibit killing of important buffers.

Add this to `kill-buffer-query-functions'."
  (if (not (member (buffer-name) eod-important-buffer-names))
      t
    (message "Not allowed to kill %s, burying instead" (buffer-name))
    (bury-buffer)
    nil))

;;;###autoload
(defun eod-clear-kill-ring ()
  "This does the obvious thing."
  (interactive)
  (setq kill-ring nil))

;;;###autoload
(defun eod-multi-pop-to-mark (orig-fun &rest args)
  "Call ORIG-FUN until the cursor moves.
Try the repeated popping up to 10 times."
  (let ((p (point)))
    (dotimes (i 10)
      (when (= p (point))
        (apply orig-fun args)))))

(advice-add 'pop-to-mark-command :around #'eod-multi-pop-to-mark)

;;;###autoload
(defun eod-ibuffer-other-window (arg)
  "It is annoying accidentally pressing C-x C-b when I really
want helm-mini, so this function will access
`ibuffer-other-window' only when a non-zero argument is specified."
  (interactive "p")
  (message "%s" arg)
  (if (not (= arg 1))
      (ibuffer-other-window)
    (helm-mini)))

(global-set-key (kbd "C-x C-b") 'eod-ibuffer-other-window)

;;;###autoload
(defun eod-delete-blank-lines (arg)
  (interactive "p")
  (cond ((= arg 4) (save-excursion (flush-lines "^$" (region-beginning) (region-end))))
        (t (delete-blank-lines))))

(global-set-key (kbd "C-x C-o") 'eod-delete-blank-lines)

;;;###autoload
(defun eod-refresh-font-lock-buffer ()
  "For when font lock starts to misbehave."
  (interactive)
  (save-restriction
    (font-lock-ensure (point-min) (point-max))))

;;;###autoload
(defun eod-byte-recompile-current-file ()
  (interactive)
  (let* ((filename (buffer-file-name))
         (extension (file-name-extension filename)))
    (when (and (file-exists-p filename) (string= extension "el"))
      (if (byte-recompile-file filename)
          (message "Success")
        (message "Something went wrong")))))

;;;###autoload
(defun get-buffer-major-mode (&optional buffer)
  (let ((buf (or buffer (current-buffer))))
    (when (bufferp buf)
      (with-current-buffer
          (buffer-name buffer)
        major-mode))))

;;;###autoload
(defun eod-byte-recompile-open-init-files ()
  (interactive)
  (mapc (lambda (buffer)
            (byte-recompile-file (buffer-file-name buffer)))
          (-filter (lambda (buffer)
                     (let ((name (buffer-name buffer)))
                       (and (or (and (file-in-directory-p
                                      name
                                      (concat user-emacs-directory "custom/"))
                                     (not (file-directory-p name)))
                             (member name '("init.el" "custom.el")))
                            (eq (get-buffer-major-mode buffer) 'emacs-lisp-mode))))
                   (buffer-list))))

;;;###autoload
(defun eod-byte-recompile-init-files (&optional error-buffer-name)
  "Byte-compile all of the init files.
If there is an error record that information in the buffer. If
ERROR-BUFFER-NAME is non-nil , then record errors in a buffer
named ERROR-BUFFER-NAME. If ERROR-CONDITIONS is non-nil, then
evaluate the conditions in the list to check for an error (Will
write this out later)."
  (interactive)
  (mapc (lambda (file)
          (let ((checktemp (when (with-temp-buffer (insert file)
                                                   (goto-char (point-min))
                                                   (search-forward ".#" nil t))
                             (let ((buf (get-buffer-create (or error-buffer-name "*eod-byte-recompile-init-files-error*"))))
                               (with-current-buffer buf
                                 (insert (format "%s cannot be compiled. Please modify (if necessary) and save the appropriate file." file)))
                               (switch-to-buffer-other-window buf))
                             (error "%s cannot be compiled. Please modify (if necessary) and save the appropriate file." file))))
            (unless (or checktemp
                        (file-directory-p file)
                        (string= (file-name-extension file) "elc"))
              (byte-recompile-file file))))
        (cons (concat user-emacs-directory "init.el")
              (cons (concat user-emacs-directory "custom.el")
                    (mapcar (lambda (f)
                              (concat (concat user-emacs-directory "custom/") f))
                            (directory-files (concat user-emacs-directory "custom/")))))))

;;;###autoload
(defun replace-regexp-whole-buffer (match newtext)
  "This will replace every instance of MATCH with NEWTEXT.
Be careful however! This function will invoke `widen' and work on
the entire buffer."
  (interactive (mapcar
                #'read-from-minibuffer
                '("Match: " "New text: ")))
  (save-excursion
    (save-restriction
      (widen)
      (goto-char (point-min))
      (while (search-forward-regexp
              match
              (point-max) t)
        (replace-match newtext)))))

 ;; Set transparency of emacs
 ;;;###autoload
(defun transparency (value)
   "Sets the transparency of the frame window. 0=transparent/100=opaque"
   (interactive "nTransparency Value 0 - 100 opaque:")
   (set-frame-parameter (selected-frame) 'alpha value))

;;;###autoload
(defun increase-transparency (arg)
  "Increases the transparency of the current frame window."
  (interactive)
  (let ((cur (assoc 'alpha (frame-parameters))))
    (modify-frame-parameters
     (selected-frame)
     (list (cons (car cur) (+ arg (cdr cur)))))))

;;;###autoload
(defun decrease-transparency (arg)
  "Decreases the transparency of the current frame window."
  (interactive)
  (increase-transparency (* -1 arg)))

(defvar to-ask-delete-trailing-whitespace-major-modes
  '(java-mode
    haskell-mode
    emacs-lisp-mode))

;;;###autoload
(defun ask-to-delete-trailing-whitespace ()
  "Ask to delete trailing white space when file is opened."
  (let ((decision
         (when (and
                (member major-mode
                        to-ask-delete-trailing-whitespace-major-modes)
                (not
                 (or
                  (= (buffer-size) 0)
                  (not
                   (save-excursion
                     (search-forward-regexp "\\s-+$" nil t))) ;don't bother if file has whitespace already removed.
                  (s-contains? "elpa" (buffer-file-name))
                  (s-contains? "gz" (buffer-name)))))
           (y-or-n-p "Delete trailing white space:"))))
    (when decision
      (delete-trailing-whitespace))))

(add-hook 'find-file-hook #'ask-to-delete-trailing-whitespace)

;;;###autoload
(defun compose (&rest funs)
  "Return function composed of FUNS."
  (lexical-let ((lex-funs  funs))
    (lambda (&rest args)
      (reduce 'funcall (butlast lex-funs)
              :from-end t
              :initial-value (apply (car (last lex-funs)) args)))))

;;; I should make this a proper helm source. With the bell and
;;; whistles baby.
;;;###autoload
(defun haskell-buffer-list (arg)
  "Return a list of Haskell buffers and open the selected buffer.
If this function is called with the `universal-argument'
prepended to its invocation, the retrieved buffer is opened in a
new window."
  (interactive "P")
  (let ((choices
         (mapcar (lambda (b)
                   (file-name-nondirectory
                    (buffer-file-name b)))
                 (let ((filter
                        (lambda (b)
                          (with-current-buffer b
                            (derived-mode-p 'haskell-mode)))))
                   (delq nil (mapcar
                              (lambda (b)
                                (if (and (funcall filter b)
                                         (not
                                          (string-match
                                           "tmp" (buffer-name b))))
                                    b
                                  nil))
                              (buffer-list))))))
        (sb (lambda (b) (switch-to-buffer
                         (completing-read "Go to Haskell file: " b))))
        (sw (lambda (b) (switch-to-buffer-other-window
                         (completing-read "Go to Haskell file: " b)))))
    (cond
     ((not (eq arg nil)) (funcall sw choices))
     (t (funcall sb choices)))))


;; courtesy of http://emacsredux.com/blog/2013/05/22/smarter-navigation-to-the-beginning-of-a-line/
;;;###autoload
(defun smarter-move-beginning-of-line (arg)
  "Move point back to indentation of beginning of line.

Move point to the first non-whitespace character on this line.
If point is already there, move to the beginning of the line.
Effectively toggle between the first non-whitespace character and
the beginning of the line.

If ARG is not nil or 1, move forward ARG - 1 lines first. If
point reaches the beginning or end of the buffer, stop there."
  (interactive "^p")
  (setq arg (or arg 1))

  ;; Move lines first
  (when (/= arg 1)
    (let ((line-move-visual nil))
      (forward-line (1- arg))))

  (let ((orig-point (point)))
    (back-to-indentation)
    (when (= orig-point (point))
      (move-beginning-of-line 1))))
;;;###autoload
(defun byte-compile-when-byte-compiled-file-exists () ;This long ass name!
  (-when-let ((file-name (buffer-file-name)))
    (when (and (file-exists-p (concat file-name "c"))
               (string= (file-name-extension (concat file-name "c" )) "elc"))
      (byte-recompile-file file-name)
      (message "Byte-recompiled %s" file-name))))
;; ;; I may use this.
;; ;;; This automatically re-indents pasted text in certain major modes.
;; (dolist (command '(yank yank-pop))
;;   (eval `(defadvice ,command (after indent-region activate)
;;            (and (not current-prefix-arg)
;;                 (member major-mode '(emacs-lisp-mode lisp-mode ;taking out haskell-mode
;;                                                      clojure-mode scheme-mode
;;                                                      ruby-mode rspec-mode
;;                                                      python-mode c-mode
;;                                                      c++-mode objc-mode
;;                                                      latex-mode plain-tex-mode))
;;                 (let ((mark-even-if-inactive transient-mark-mode))
;;                   (indent-region (region-beginning) (region-end) nil))))))

(provide 'init-defuns)
;; init-defuns.el ends here
