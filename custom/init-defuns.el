(defun rename-this-file-and-buffer (new-name)
  "Renames both current buffer and file it's visiting to
NEW-NAME."
  (interactive "sNew name: ")
  (let ((name (buffer-name))
        (filename (buffer-file-name)))
    (unless filename
      (error "Buffer '%s' is not visiting a file!" name))
    (if (get-buffer-new-name)
        (message "A buffer named '%s' already exists!" new-name)
      (progn
        (when (file-exists-p filename)
          (rename-file filename new-name 1))
        (rename-buffer new-name)
        (set-visited-file-name new-name)))))

(defun delete-this-file ()
  "Delete the current file, and kill the buffer."
  (interactive)
  (or (buffer-file-name) (error "No file is currently being edited"))
  (when (yes-or-no-p
         (format "Really delete '%s'?"
                 (file-name-nondirectory buffer-file-name)))
    (delete-file (buffer-file-name))
    (kill-buffer (current-buffer))))

(defun my-goto-scratch-buffer ()
  "Create a new scratch buffer."
  (interactive)
  (switch-to-buffer (get-buffer-create "*scratch*"))
  (emacs-lisp-mode)
  (insert
   (format
    ";; %s\n\n"
    (replace-regexp-in-string
     "\n" "\n;; " ; comment each line
     (replace-regexp-in-string
      "\n$" "" ; remove trailing linebreak
      (shell-command-to-string "fortune"))))))

(defun my-insert-last-kbd-macro ()
  "Insert the last keyboard macro into current buffer."
  (interactive)
  (name-last-kbd-macro 'my-last-macro)
  (insert-kbd-macro 'my-last-macro))



(defun add-auto-mode (mode &rest patterns)
  "Add entries to `auto-mode-alist' to use `MODE' for all given
file `PATTERNS'."
  (dolist (pattern patterns)
    (add-to-list 'auto-mode-alist (cons pattern mode))))

(defun switch-to-previous-buffer ()
  "Switch to previous buffer."
  (interactive)
  (switch-to-buffer
   (other-buffer (current-buffer) 1)))

(defun switch-to-previous-buffer-other-window ()
  (interactive)
  (switch-to-buffer-other-window
   (other-buffer (current-buffer) 1)))

(defun eod-kill-buffer ()
  "Kill the current buffer."
  (interactive)
  (kill-buffer (current-buffer)))

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

(defun dont-kill-emacs ()
  (interactive)
  (error (substitute-command-keys "To exit emacs: \\[kill-emacs]")))

(global-set-key "\C-x\C-c" 'dont-kill-emacs)

(defun mplayer ()
  "An interface to mplayer."
  (interactive)
  (shell-command (concat "xterm -e mplayer "
                         (shell-quote-argument
                          (expand-file-name
                           (read-file-name "Filename: ")))
                         "& ")))

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

(defun date (&optional insert)
  "Display the current date and time.
With a prefix arg, INSERT it into the buffer."
  (interactive "p")
  (funcall (if insert 'insert 'message)
           (format-time-string
            "%a, %d, %b, %Y, %T, %Z"
            (current-time))))

(defun reload-init-file ()
  "Reload init.el file"
  (interactive)
  (load user-init-file)
  (message "Reloaded init.el OK."))

(defun open-init-file ()
  (interactive)
  (find-file user-init-file))

(defun open-as-root ()
  "Open a file as root."
  (find-file (concat "/sudo:root@localhost:" filename)))

(defun open-buffer-as-root ()
  (interactive)
  (let ((filename (buffer-file-name (current-buffer)))
        (bufname (buffer-name (current-buffer))))
    (progn
      (kill-buffer bufname)
      (open-as-root filename))))


(defun eod-send-file-name-to-minibuffer ()
  (interactive)
  (insert (buffer-file-name (window-buffer (minibuffer-selected-window)))))

(define-key
  minibuffer-local-map
  (kbd "C-c f") 'eod-send-file-name-to-minibuffer)

(defun eod-copy-file-name-to-clipboard ()
  "Copy the current buffer file name to the clipboard."
  (interactive)
  (let ((filename (if (equal major-mode 'dired-mode)
                      default-directory
                    (buffer-file-name))))
    (when filename
      (kill-new filename)
      (message
       "Copied buffer file name '%s' to the clipboard. " filename))))

(defun eval-sexp-and-replace (value)
  "Evaluate the sexp at point and replace it with its value "
  (interactive (list (eval-last-sexp nil)))
  (mark-sexp -1)
  (delete-region (region-beginning) (region-end))
  (insert (format "%S" value)))

(global-set-key (kbd "C-c e") 'eval-sexp-and-replace)

(global-set-key (kbd "C-c y") 'x-clipboard-yank)

(defun eod-join-next-line (arg)
  "Join the next line with the current one."
  (interactive "p")
  (if (region-active-p)
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

(defun eod-test-net ()
  (interactive)
  (async-shell-command
   "ping -c 3 8.8.8.8"))

(defun eod-dropbox-status-message ()
  "Get the current dropbox status."
  (interactive)
  (if (eq 1 (shell-command "dropbox-cli running"))
      (message "Dropbox is running.")
    (message "Dropbox is not running.")))

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

(defun eod-dropbox-print-maindir ()
  "Print the status of the Dropbox folder and its contents."
  (interactive)
  (async-shell-command "dropbox-cli filestatus ~/Dropbox/*"))

(defun eod-dropbox-status ()
  "Get the current dropbox sync status."
  (interactive)
  (async-shell-command "dropbox-cli status"))

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

(defun prelude-colorize-compilation-buffer ()
  "Colorize a compilation buffer."
  (interacitve)
  ;; we don't want to mess with child modes such as grep-mode, ack,
  ;; ag, etc.
  (when (eq major-mode 'compilation-mode)
    (let ((inhibit-read-only t))
      (ansi-color-apply-on-region (point-min) (point-max)))))

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
  (if (null he-expand-list)
      (progn
        (if old (he-reset-string))
        ())
    (progn
      (he-substitute-string (car he-expand-list))
      (setq he-expand-list (cdr he-expand-list))
      t)))

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

(defun he-flexible-abbrev-create-regexp (str)
  "Generate regexp for flexible matching of STR.
See docstring for `try-expand-flexible-abbrev' for information
about what flexible matching means in this context."
  (concat "\\b"
          (mapconcat (lambda (x) (concat "\\w*" (list x))) str "")
          "\\w*" "\\b"))

;;; This is not the cleanest implementation, but it works so whatever.
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

(defun eod-insert-dollar ()
  "Insert a dollar into the buffer as expected, unless the
current major mode is org-mode or latex-mode."
  (interactive)
  (if (member major-mode '(org-mode latex-mode))
      (progn (insert "$$")
             (backward-char))
    (insert "$")))

(global-set-key (kbd "$") 'eod-insert-dollar)

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

;;; This isn't very satisfying. When `keyboard quit' is executed
;;; during avy-goto-line, then fci-mode is not re-enabled.
(defadvice avy-goto-line (around avy-fci activate)
  (global-fci-mode -1)
  ad-do-it
  (global-fci-mode 1))

;; (defun zap-up-to-char (arg char)
;;   "Zap up to a character."
;;   (interactive "p\ncZap to char: ")
;;   (save-excursion
;;     (zap-to-char arg char)
;;     (insert char)))

(defun zap-to-char-save (arg char)
  "Zap up to a character, but save instead of kill."
  (interactive "p\ncZap to char: ")
  (save-excursion
    (zap-to-char arg char)
    (yank)))

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
               '(tmpsource) "*helm-select-yasnippet"))
        (if (null result)
            (signal 'quit "user quit!")
          (cdr (assoc result rmap))))
    nil))

;;; kill all comment in buffer -- Courtesy of Magnar
(defun comment-kill-all ()
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (comment-kill (save-excursion
                    (goto-char (point-max))
                    (line-number-at-pos)))))

(global-set-key (kbd "C-c z") 'zap-up-to-char)

;;; Courtesy of Mickey Peterson's blog.
(defun push-mark-no-activate ()
  "Pushes `point' to `mark-ring' and does not activate the
  region. Equivalent to \\[set-mark-command] when
  \\[transient-mark-mode] is disabled."
  (interactive)
  (push-mark (point) t nil)
  (message "Pushed mark to ring"))

(global-set-key (kbd "C-c SPC") 'push-mark-no-activate)

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

(defun eod-duplicate-line (arg)
  "Duplicate the current line ARG times. If ARG is negative,
then replicate the current line ARG times above the current
line."
  (interactive "p")
  (let ((text (string-remove-suffix
               "\n"
               (thing-at-point 'line t)))
        (col (current-column)))
    (when (/= arg 0)
      (save-excursion
        (end-of-line)
        (dotimes (i (abs arg))
          (newline)
          (insert text)))
      (when (< arg 0)
        (forward-line (* -1 arg))
        (forward-char col)))))

;; (defun eod-replace-string-with-same-char ()
;;   (interactive)
;;   (let ((len (length (thing-at-point 'word)))
;;         (location (point))
;;         (char (read-string "GIVE ME DAT CHAR: "))
;;         (bds (bounds-of-thing-at-point 'word)))
;;     (delete-region (car bds) (cdr bds))
;;     (insert (make-string len arg))
;;     (goto-char location)))

(defun eod-increment-number-at-point-helper (inc)
  (interactive "p")
  (let* ((num
          (number-at-point))
         (start
          (car (bounds-of-thing-at-point 'word)))
         (m (make-marker))
         (end
          (cdr (bounds-of-thing-at-point 'word))))
    (when num
      (set-marker m start)
      (delete-and-extract-region start end)
      (insert (format "%d" (+ num inc)))
      (goto-char m))))

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
            (t arg))))
    ;; (unless orig (setq orig (number-at-point)))
    (eod-increment-number-at-point-helper step)
    (message "Use +,- for further adjustment")
    (set-transient-map
     (let ((map (make-sparse-keymap)))
       (dolist (mods '(() (control)))
         (dolist (key '(?- ?+))
           (define-key map (vector (append mods (list key)))
             `(lambda ()
                (interactive)
                (eod-increment-number-at-point ,(abs arg))))))
       map))))

(defun eod-decrement-number-at-point (arg)
  (interactive "p")
  (eod-increment-number-at-point arg))

(global-set-key (kbd "C-c C-+") 'eod-increment-number-at-point)
(global-set-key (kbd "C-c C--") 'eod-decrement-number-at-point)

(defun eod-increment-inner-number (arg)
  (interactive "p")
  (insert
   (format
    "%d"
    (+ arg
       (string-to-int
        (delete-and-extract-region
         (1+ (search-backward-regexp "[^0-9]+"))
         (search-forward-regexp "[0-9]+")))))))

;;; This function is dumb....HAHAHAHA!!!!
(defun n-choose-k (arg1 arg2)
  "Calculate n choose k. ARG1 must be greater than or equal to ARG2."
  (if (and (numberp arg1) (numberp arg2)
           (<= arg2 arg1) (>= arg2 0))
      (let ((numerator (apply '* (number-sequence (+ 1 (- arg1 arg2)) arg1)))
            (denominator (apply '* (number-sequence 1 arg2))))
        (/ numerator denominator))
    (message "What have you done?")))

(defun eod-delete-region ()
  (interactive)
  (when
      (and (string-match
            "undefined"
            (format "%s" (describe-key-briefly (kbd "C-c d"))))
           (not
            (member major-mode
                    '(lisp-interaction-mode emacs-lisp-mode))))
    (local-set-key (kbd "C-c d") 'delete-region)))

(defun concat-strings-separator (strings &optional separator)
  "Requisite documentation"
  (if (or (listp strings) (sequencep strings))
      (mapconcat
       'identity
       strings
       (if separator separator " "))
    (error "You did not provide a list or sequence as the first argument.")))

(defun message-length-kill-ring ()
  (interactive)
  (message "The length of the kill ring is %s" (length kill-ring)))

(provide 'init-defuns)
;; init-defuns.el ends here
