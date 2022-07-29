;;; hl-scope.el --- Highlighting nested blocks -*- lexical-binding: t -*-

;; SPDX-License-Identifier: GPL-2.0-or-later
;; Copyright (C) 2019-2021  Campbell Barton

;; Author: Campbell Barton <ideasman42@gmail.com>

;; URL: https://codeberg.org/ideasman42/emacs-hl-scope-mode
;; Version: 0.1
;; Package-Requires: ((emacs "26.1"))

;;; Commentary:

;; Highlight indentation by syntax (or user configurable methods).

;;; Usage

;; (hl-scope-mode)        ; activate in the current buffer.
;; (global-hl-scope-mode) ; activate globally for all buffers.


;;; Code:

;;

;; ---------------------------------------------------------------------------
;; Custom Variables

(defgroup hl-scope nil "Highlight nested blocks or brackets." :group 'convenience)

(defcustom hl-scope-fixed-width nil
  "Idle time to wait before highlighting (in seconds)."
  :type 'boolean)

(defcustom hl-scope-fill-empty-lines t "Fill in the empty lines (experimental)." :type 'boolean)
(defcustom hl-scope-fill-over-text nil "Fill in the empty lines (experimental)." :type 'boolean)
(defcustom hl-scope-highlight-on-motion nil
  "Fill in the empty lines (experimental)."
  :type 'boolean)


;; ---------------------------------------------------------------------------
;; Custom Variables


(defvar-local hl-scope-next-sexp-fn 'hl-scope-next-sexp-by-syntax
  "Skip forward to the next opening expression, bound by END.

Built in options are:
- `hl-scope-next-sexp-by-syntax'
- `hl-scope-next-sexp-by-search'
Or you may define your own.")

(defvar-local hl-scope-top-sexp-fn 'hl-scope-top-sexp-by-syntax
  "Skip to the top-level s-expression.

Built in options are:
- `top-sexp-by-syntax'
- `top-sexp-by-search'
Or you may define your own.")

;; ---------------------------------------------------------------------------
;; Internal Variables

(defvar-local hl-scope--font-lock-extend-multi-line-added nil)

;; ---------------------------------------------------------------------------
;; Internal Refresh Function

(defun hl-scope--overlay-clear ()
  "Clear all overlays."
  (remove-overlays (point-min) (point-max) 'hl-scope t))


;; ---------------------------------------------------------------------------
;; Callback Implementations

(defun hl-scope-next-sexp-by-search (end)
  "Skip forward by search, bound by END."
  (let ((found nil))
    (save-match-data
      (while (and (null found) (search-forward "{" end t))
        ;; Ensure the bracket character is seen as a bracket by the syntax tree.
        ;; And not some kind of escaped bracket (for e.g.).
        (when (eq ?\( (char-syntax (char-before)))
          (setq found t))))
    found))

(defun hl-scope--search-forward-open-sexp (end)
  "Search forward syntax table for an opening bracket until END.
This has the same behavior as `search-forward'."
  (skip-syntax-forward "^(" (1- end))
  (cond
    ((eq ?\( (char-syntax (char-after)))
      (forward-char 1)
      t)
    (t
      nil)))

(defun hl-scope-next-sexp-by-syntax (end)
  "Skip forward by syntax, bound by END."
  (let ((found nil))
    (while (and (null found) (hl-scope--search-forward-open-sexp end))
      (when (eq ?\( (char-syntax (char-before)))
        (setq found t)))
    found))

(defun hl-scope-top-sexp-by-syntax (beg)
  "Seek BEG backwards to encompass the outer-most s-expression.
If we are not already inside an s-expression, leave all-beg as-is."
  (let ((beg-next nil))
    (while (setq beg-next (ignore-errors (nth 1 (syntax-ppss beg))))
      (setq beg beg-next)))
  beg)


;; Both of these functions work, we may want to select syntax for lisp,
;; and search for C/C++ as way may not want to treat literal braces
;; the same way as other kinds of parenthesis.

;; ---------------------------------------------------------------------------
;; Internal Bracket Functions
;;

(defun hl-scope--tree-from-buffer-impl (all-beg all-end _beg end)
  "Return a tree from the buffer.

The format is ((start . end) children-or-nil)
Arguments ALL-BEG, ALL-END are the full range.
Arguments _BEG END are the range to use."
  (let
    (
      (range-tree nil)
      (end-bound (min end all-end)))
    (while (and (< (point) end-bound) (funcall hl-scope-next-sexp-fn end-bound))
      (let ((state (syntax-ppss)))
        (unless (or (nth 3 state) (nth 4 state))
          (let ((pos-beg (point)))
            (let ((pos-end (ignore-errors (scan-sexps (1- (point)) 1))))
              (unless pos-end
                (setq pos-end end))
              (unless (or (<= all-end pos-beg) (<= pos-end all-beg))
                (push
                  (cons
                    (cons (point) pos-end)
                    (hl-scope--tree-from-buffer-impl all-beg all-end pos-beg pos-end))
                  range-tree))
              (goto-char pos-end))))))
    range-tree))

(defun hl-scope--tree-from-buffer (all-beg all-end)
  "Return a tree in range ALL-BEG, ALL-END."
  (save-excursion
    (goto-char all-beg)
    (setq all-beg (funcall hl-scope-top-sexp-fn all-beg))
    (hl-scope--tree-from-buffer-impl all-beg all-end all-beg all-end)))

(defconst hl-scope--face-odd (list :background "#2e2e3f"))
(defconst hl-scope--face-even (list :background "#3e3e5e"))
(defconst hl-scope--face-hi (list :background "#7e7e9e"))

(cond
  ((display-graphic-p)
    (defun hl-scope--face-from-level (level)
      "Return a face from the indentation level."
      (cond
        ((zerop (mod level 2))
          hl-scope--face-odd)
        (t
          hl-scope--face-even))))
  (t
    (defun hl-scope--face-from-level (level)
      "Return a face from the indentation level."
      (cond
        ((zerop (mod level 2))
          (list :background "#AA0000"))
        (t
          (list :background "#0000AA"))))))

(defun hl-scope--detect-indent (range-beg range-end stop)
  "Detect the next indentation level in (RANGE-BEG RANGE-END).
Argument STOP is the current indentation level, use for reference."
  (cond
    (hl-scope-fixed-width
      (+ stop tab-width))
    (t
      (let
        (
          (stop-next (+ stop tab-width))
          (found nil))
        (save-excursion
          (let ((pos-next nil))
            (goto-char range-beg)
            (setq pos-next (1+ (line-end-position)))
            (while (and (null found) (< pos-next range-end))
              (goto-char pos-next)
              (let ((eol (line-end-position)))
                (let ((skip (skip-syntax-forward " " eol)))
                  (cond
                    ((< stop skip)
                      (setq stop-next skip)
                      (setq found t))
                    (t
                      (setq pos-next (1+ eol)))))))))
        stop-next))))

(defun hl-scope--propertize-stops (level stops empty-line-str &optional E)
  "It's assumed the point is at the line start.
Argument LEVEL is the depth of the integer.
Argument STOPS are the list of integer large to zero.
Argument EMPTY-LINE-STR stores the empty string."
  (let*
    ( ;; It's assumed (point) is at the beginning of the line.
      (pos-bol (point))
      (pos-eol (line-end-position)))
    (cond
      ;; Empty line.
      ((eq pos-bol pos-eol)

        ;; Otherwise do nothing.
        (when hl-scope-fill-empty-lines

          ;; Empty line, add overlay.
          (when (null (car empty-line-str))
            (let ((stops-max (car stops)))
              (let ((ov-str (make-string stops-max ?\s)))
                ;; Properties.
                (let ((pos-end (pop stops)))
                  (while stops
                    (let ((pos-beg (pop stops)))
                      (let ((face (hl-scope--face-from-level level)))
                        (when E
                          (setq face (list :background "#FF0000")))
                        (put-text-property pos-beg pos-end 'font-lock-face face ov-str))
                      (setq pos-end pos-beg))
                    (setq level (1- level))))
                (setcar empty-line-str ov-str))))

          (let ((ov (make-overlay pos-bol pos-bol)))
            (overlay-put ov 'hl-scope t)
            ;; For some reason sharing strings is NOT working (use `concat').
            (overlay-put ov 'after-string (car empty-line-str)))))
      (t
        (let
          (
            (pos-whitespace
              (cond
                ;; Fill in background over any exiting text.
                (hl-scope-fill-over-text
                  pos-eol)
                ;; Fill in background until white-space ends.
                (t
                  (save-excursion
                    (skip-syntax-forward " " pos-eol)
                    (point))))))
          (unless (eq pos-whitespace pos-bol)
            (let ((pos-end (+ (pop stops) pos-bol)))
              (while stops
                (let ((pos-beg (+ (pop stops) pos-bol)))
                  (when (<= pos-end pos-whitespace)
                    (setq pos-end (min pos-whitespace pos-end))
                    (let ((face (hl-scope--face-from-level level)))
                      ;; Also works.
                      ;; (put-text-property pos-beg pos-end 'font-lock-face face)
                      (when E
                        (setq face (list :background "#FF0000")))

                      (let ((ov (make-overlay pos-beg pos-end)))
                        (overlay-put ov 'face face)
                        (overlay-put ov 'evaporate t)
                        (overlay-put ov 'hl-scope t))))
                  (setq pos-end pos-beg))

                (setq level (1- level))))))))))

(defun hl-scope--font-lock-tree-impl (all-beg all-end tree level stops)
  "Implement full buffer font locking of indentation levels.

Argument TREE is the nested tree to lock.

Argument LEVEL an integer representing the depth of the tree.

Argument STOPS is a list of indentation widths, ordered largest to smallest,
always ending in zero: e.g. (list 8 4 0).

Arguments ALL-BEG, ALL-END are the full range."
  (while tree
    (pcase-let ((`(,range . ,children) (pop tree)))
      (pcase-let ((`(,range-beg . ,range-end) range))
        (let
          (
            (level-next (1+ level))
            (stops-next (cons (hl-scope--detect-indent range-beg range-end (car stops)) stops))
            (empty-line-str (cons nil nil))
            (empty-line-str-next (cons nil nil)))
          ;; Only jump to the next range when level is zero so we don't miss coloring indentation
          ;; for nested blocks:
          ;;    }
          ;;    else /* <- indentation before this line for e.g. */
          ;;    {

          ;; For zero level indentation there is nothing to do in-between members of the tree.
          (when (zerop level)
            (goto-char range-end)
            (goto-char (line-beginning-position)))

          ;; TODO: avoid forward-line when we're jumping over values out of all{beg/end}
          (cond
            ((zerop level)
              (goto-char range-end)
              (goto-char (line-beginning-position)))
            (t
              (while (<= range-end (point))
                (when (and (< (point) all-end) (<= all-beg (point)))
                  (hl-scope--propertize-stops level stops empty-line-str))
                (forward-line -1))))

          (when children
            (hl-scope--font-lock-tree-impl all-beg all-end children level-next stops-next))

          (while (<= range-beg (point))
            (when (and (< (point) all-end) (<= all-beg (point)))
              (hl-scope--propertize-stops level-next stops-next empty-line-str-next))
            (forward-line -1)))))))

(defun hl-scope--font-lock-tree (all-beg all-end)
  "Lock tree.
Arguments ALL-BEG, ALL-END are the full range.
Note that this function moves the point, caller may wish to use `save-excursion'"

  (remove-overlays all-beg all-end 'hl-scope t)
  (save-excursion
    (let ((tree (hl-scope--tree-from-buffer all-beg all-end)))
      (when tree

        ;; Go to the end of the tree's range.
        (goto-char (cdr (car (car tree))))
        (goto-char (line-beginning-position))

        (hl-scope--font-lock-tree-impl all-beg all-end tree 0 (list 0))))))

(defun hl-scope--font-lock-fontify-region (pos-beg pos-end)
  "Update highlighting for POS-BEG & POS-END to the queue, checking all text."
  (save-excursion
    (goto-char pos-beg)
    (setq pos-beg (line-beginning-position))
    (goto-char pos-end)
    (setq pos-end (line-end-position)))

  (hl-scope--font-lock-tree pos-beg pos-end)

  (when hl-scope-highlight-on-motion
    (when (and (<= (point) pos-end) (<= (point) pos-end))
      (hl-scope--block-hl-on-idle))))

(defun hl-scope--font-lock-extend-region (pos-beg pos-end)
  "Update highlighting for POS-BEG & POS-END to the queue, checking all text."
  (hl-scope--font-lock-tree pos-beg pos-end))


;; ---------------------------------------------------------------------------
;; Internal Block Highlighting Support

(defvar-local hl-scope--block-hl-overlay-list nil)

(defun hl-scope--block-hl-fwd-skip-blank (dir)
  (forward-line dir)
  (while (eq (line-end-position) (line-beginning-position))
    (forward-line dir)))

(defun hl-scope--block-hl-clr (overlay-list)
  (while overlay-list
    (let ((ov (pop overlay-list)))
      (let ((face-prev (overlay-get ov 'face-prev)))
        (when face-prev
          (overlay-put ov 'face-prev nil)
          (overlay-put ov 'face face-prev))))))

(defun hl-scope--block-hl-set (overlay-list)
  (while overlay-list
    (let ((ov (pop overlay-list)))
      (unless (overlay-get ov 'face-prev)
        (overlay-put ov 'face-prev (overlay-get ov 'face))
        (overlay-put ov 'face hl-scope--face-hi)))))


(defun hl-scope--block-hl-find-overlay-at-point-indent (indent)
  (let
    (
      (pos-eol (line-end-position))
      (pos-bol (line-beginning-position))
      (ov-found nil))
    (unless (eq pos-bol pos-eol)
      (goto-char pos-bol)
      (unless
        (zerop
          (cond
            (hl-scope-fill-over-text
              (goto-char (min (+ pos-bol indent 1) pos-eol))
              1)
            (t
              (skip-syntax-forward " " (min pos-eol (+ pos-bol indent 1))))))
        (let ((overlay-list (overlays-at (1- (point)) nil)))
          (while overlay-list
            (let ((ov (pop overlay-list)))
              (when (overlay-get ov 'hl-scope)
                (when (eq (- (overlay-start ov) pos-bol) indent)
                  (setq ov-found ov))
                (setq overlay-list nil))

              ;;
              )))))
    ov-found))

(defun hl-scope--block-hl-find-indent-at-point ()
  (let
    (
      (pos-bol (line-beginning-position))
      (pos-eol (line-end-position))
      (indent-at-point nil))

    (cond
      ;; Blank line.
      ((eq pos-bol pos-eol)
        (when hl-scope-fill-empty-lines
          (let ((overlay-list (overlays-in pos-bol pos-bol)))
            (while overlay-list
              (let ((ov (pop overlay-list)))
                (when (overlay-get ov 'hl-scope)
                  (setq indent-at-point (length (overlay-get ov 'after-string)))
                  ;; Break.
                  (setq overlay-list nil)))))))
      ;; None blank.
      (hl-scope-fill-over-text
        (let ((overlay-list (overlays-in pos-bol pos-eol)))
          (while overlay-list
            (let ((ov (pop overlay-list)))
              (when (overlay-get ov 'hl-scope)
                (let ((next-indent (overlay-start ov)))
                  (cond
                    (indent-at-point
                      (when (< indent-at-point next-indent)
                        (setq indent-at-point next-indent)))
                    (t
                      (setq indent-at-point next-indent)))))))
          (when indent-at-point
            (setq indent-at-point (- indent-at-point pos-bol)))))
      (t
        (goto-char pos-bol)
        (unless (zerop (skip-syntax-forward " " pos-eol))
          (let ((overlay-list (overlays-at (1- (point)) nil)))
            (while overlay-list
              (let ((ov (pop overlay-list)))
                (when (overlay-get ov 'hl-scope)
                  (setq indent-at-point (- (overlay-start ov) pos-bol))
                  ;; Break.
                  (setq overlay-list nil))))))))
    indent-at-point))


(defun hl-scope--block-hl-find-overlays-at-indent (indent)
  ""
  (goto-char (line-beginning-position))
  (let
    (
      (pos-beg (point))
      (overlay-list-at-indent nil))
    (dolist (dir (list -1 1))
      (goto-char pos-beg)
      (when (eq dir 1)
        ;; Don't handle the same line twice.
        (hl-scope--block-hl-fwd-skip-blank 1))
      (let ((keep-looking t))
        (while keep-looking
          (let ((ov (hl-scope--block-hl-find-overlay-at-point-indent indent)))
            (setq keep-looking nil)
            (when ov
              (setq keep-looking t)
              (push ov overlay-list-at-indent)))
          (hl-scope--block-hl-fwd-skip-blank dir))))
    overlay-list-at-indent))

(defun hl-scope--block-hl-on-idle ()
  (save-excursion
    (let ((indent (hl-scope--block-hl-find-indent-at-point)))
      (cond
        (indent
          (let ((overlay-list-at-indent (hl-scope--block-hl-find-overlays-at-indent indent)))
            (when overlay-list-at-indent
              (hl-scope--block-hl-clr hl-scope--block-hl-overlay-list)
              (hl-scope--block-hl-set overlay-list-at-indent)
              (setq hl-scope--block-hl-overlay-list overlay-list-at-indent))))
        (t
          (hl-scope--block-hl-clr hl-scope--block-hl-overlay-list)
          (setq hl-scope--block-hl-overlay-list nil))))))

;; ---------------------------------------------------------------------------
;; Internal Mode Management

(defun hl-scope-mode-enable ()
  "Turn on `hl-scope-mode' for the current buffer."

  (when (member major-mode (list 'c-mode 'c++-mode 'glsl-mode))
    (setq hl-scope-next-sexp-fn 'hl-scope-next-sexp-by-search))

  ;; Warning, this is not fool-proof, if users happen to use a temporary mode that
  ;; enables this and then disable that mode while this one runs,
  ;; partial lines may be used. Accept this limitation since it's a real corner case.
  (unless (memq 'font-lock-extend-region-wholelines font-lock-extend-region-functions)
    (add-hook 'font-lock-extend-region-functions #'font-lock-extend-region-wholelines 100 t)
    (setq hl-scope--font-lock-extend-multi-line-added t))

  ;; Contextual locking is needed since lines need updating when s-expressions are modified.
  (jit-lock-register #'hl-scope--font-lock-fontify-region t)

  (when hl-scope-highlight-on-motion
    (run-with-idle-timer 0.127 t #'hl-scope--block-hl-on-idle)))

(defun hl-scope-mode-disable ()
  "Turn off `hl-scope-mode' for the current buffer."

  (when hl-scope--font-lock-extend-multi-line-added
    (remove-hook 'font-lock-extend-region-functions #'font-lock-extend-region-wholelines nil))

  (jit-lock-unregister #'hl-scope--font-lock-fontify-region)
  (hl-scope--overlay-clear))

(defun hl-scope-mode-turn-on ()
  "Enable command `hl-scope-mode'."
  (when (and (not (minibufferp)) (not (bound-and-true-p hl-scope-mode)))
    (hl-scope-mode 1)))

;; ---------------------------------------------------------------------------
;; Public API

;;;###autoload
(defun hl-scope-buffer ()
  "One off syntax highlighting of indentation."
  (interactive)
  (save-excursion (hl-scope--font-lock-tree (point-min) (point-max))))

;;;###autoload
(define-minor-mode hl-scope-mode
  "Highlight block under the cursor."
  :global nil
  (cond
    (hl-scope-mode
      (hl-scope-mode-enable))
    (t
      (hl-scope-mode-disable))))

;;;###autoload
(define-globalized-minor-mode
  global-hl-scope-mode

  hl-scope-mode hl-scope-mode-turn-on)

(provide 'hl-scope)
;; Local Variables:
;; indent-tabs-mode: nil
;; End:
;;; hl-scope.el ends here
