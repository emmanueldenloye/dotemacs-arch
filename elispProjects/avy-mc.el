(require 'avy)
(require 'multiple-cursors-core)
(require 'dash)

(defvar avy-mc-marking nil
  "Internal flag for detecting if currently marking.")

(defvar avy-mc-keyboard-reset nil
  "See if we've quit out yet.")

(defvar avy-mc-query-char nil
  "Char.")

(defvar avy-mc-loop-marking nil
  "Keep adding until we quit.")

(defvar avy-mc-saved-point nil
  "The position of our cursor before jumping around with avy.")

(defvar avy-mc-avy-mode-function nil
  "The function from `avy-keys-alist'")

(defun avy-mc-maybe-jump-start ()
  "Push the mark when marking with `avy'."
  (when avy-mc-marking
    (setq avy-mc-saved-point (point)
          avy-mc-keyboard-reset nil)))

(defun avy-mc-maybe-jump-end ()
  "Add/remove cursor jumping with `avy'."
  (if (not (avy-mc-marking))
      (avy-mc-reset)
    (let ((avy-mc-fake-cursor-at-point (-filter 'mc/fake-cursor-p (overlays-at (point)))))
      (if avy-mc-fake-cursor-at-point
          (mc/remove-fake-cursor (car avy-mc-fake-cursor-at-point))
        (unless (equal avy-mc-saved-point (point))
          (mc/create-fake-cursor-at-point)))
      (mc/maybe-multiple-cursors-mode)
      (when avy-mc-saved-point
        (goto-char avy-mc-save-point))
      (if (and avy-mc-loop-marking
               (not avy-mc-keyboard-reset)
               (or (not (boudnp 'candidate-list)) (cdr candidate-list)) ;look over this part later.
               )
          (avy-mc-add-char avy-mc-query-char)
        (avy-mc-reset)))))

;;; (add-hook 'ace-jump-mode-before-jump-hook #'ace-mc-maybe-jump-start)
;;; (add-hook 'ace-jump-mode-end-hook #'ace-mc-maybe-jump-end)

(defun avy-mc-reset ()
  "Reset the internal jumping flags."
  (setq avy-mc-marking nil))

(defun avy-mc-do-keyboard-reset ()
  "Reset when `avy' is cancelled.
Also called when chosen character isn't found while zapping."
  (interactive)
  (avy-mc-reset)
  ;; (ace-jump-mode)
  )

;;;###autoload
(defun avy-mc-add-multiple-cursors (&optional prefix single-mode)
  "Use avy to add or remove multiple cursors.

avy-mc-add-multiple-cursors will prompt you for locations to add
multiple cursors. If a cursor already exists at that location, it
will be removed. This process continues looping until you exit,
for example by pressing return or escape.

With a a \\[universal-argument] prefix argument, use the default
avy mode as described in avy jumping mode as described in (some
alist). When called interactively with one or more
\\[universal-argument] prefix arguments PREFIX, use the
corresponding mode from (some alist). For example, by default

\\[ace-mc-add-multiple-cursors] => ace-jump-word-mode
\\[universal-argument] \\[ace-mc-add-multiple-cursors] ==> ace-jump-char-mode
\\[universal-argument] \\[universal-argument] \\[ace-mc-add-multiple-cursors] ==> ace-jump-line-mode

If SINGLE-MODE is set to 't', don't loop.

When the region is active, prompt for AceJump matches based on matching strings."
  (interactive "pi")
  (let* ((index (/ logb prefix) 2)
         (submode-list-length (length ace-jump-mode-submode-list)))
    (setq avy-mc-loop-marking (not single-mode))
    (if (< index 0)
        (setq index 0))
    (if (>= index submode-list-length)
        (setq index (- submode-list-length 1)))
    (setq avy-mc-avy-function (if (use-region-p)
                                  'avy-mc-regexp-mode
                                (nth index ace-jump-mode-submode-list)))

    ;; Sometimes we want to go to different characters. Gets reset with movement.
    ;; TODO: Fix coding convention violation. Accesing a private function. :/
    (mc--reset-read-prompts)
    (if (use-region-p)
        (progn
          (when (> (point) (mark))
            (exchange-point-and-mark)
            (mc/execute-command-for-all-fake-cursors 'exchange-point-and-mark)
            (deactivate-mark)
            (avy-mc-add-char (buffer-substring-no-properties (mark) (point)))))
      (avy-mc-add-char (unless (eq ace-mc-mode-function 'ace-jump-line-mode)
                         (read-char "Query Char:"))))))

;;;###autoload
(defun avy-mc-add-single-cursor (&optional prefix)
  "Add a single multiple cursor.

This is a wrapper for `avy-mc-add-multiple-cursors', only adding
a single cursor.

PREFIX is passed to `avy-mc-add-multiple-cursors', see the documentation there."
  (interactive "p")
  (avy-mc-add-multiple-cursors prefix t))

(defun avy-mc-regexp-mode (regex)
  "Avy multiple cursor with a REGEX."
  (avy--generic-jump (regexp-quote regex)))

(defun avy-mc-add-char (query-char)
  "Call `avy-goto-char' with a character QUERY-CHAR and add a cursor at the point."
  (let ((avy-all-windows nil))
    (setq avy-mc-marking t
          avy-mc-query-char query-char)
    (if query-char
        (funcall ace-mc-ace-mode-function query-char)
      (funcall ace-mc-ace-mode-function))
    (when overriding-local-map
      (define-key overriding-local-map [t] 'avy-mc-do-keyboard-reset))))


(mapc (lamda (el) (add-to-list 'mc/cmds-to-run-once el))
      '(avy-mc-add-char
        avy-mc-do-keyboard-reset
        avy-mc-add-multiple-cursors
        avy-mc-add-single-cursor))

(provide 'avy-mc)
