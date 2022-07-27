;;; hl-scope.el --- Highlighting nested blocks -*- lexical-binding: t -*-

;; SPDX-License-Identifier: GPL-2.0-or-later
;; Copyright (C) 2019-2021  Campbell Barton

;; Author: Campbell Barton <ideasman42@gmail.com>

;; URL: https://codeberg.org/ideasman42/emacs-hl-scope-mode
;; Version: 0.1
;; Package-Requires: ((emacs "26.1"))

;;; Commentary:

;; Highlight blocks surrounding the cursor.

;;; Usage

;; (hl-scope-mode)        ; activate in the current buffer.
;; (global-hl-scope-mode) ; activate globally for all buffers.


;;; Code:

;;

;; ---------------------------------------------------------------------------
;; Custom Variables

(defgroup hl-scope nil "Highlight nested blocks or brackets." :group 'convenience)

(defcustom hl-scope-bracket "{"
  "Characters to use as a starting bracket (set to nil to use all brackets)."
  :type '(or null string))

;; (defcustom hl-scope-delay 0.2 "Idle time to wait before highlighting (in seconds)." :type 'float)
(defcustom hl-scope-fixed-width nil
  "Idle time to wait before highlighting (in seconds)."
  :type 'boolean)


;; ---------------------------------------------------------------------------
;; Internal Variables


;; ---------------------------------------------------------------------------
;; Internal Refresh Function

(defun hl-scope--overlay-clear ()
  "Clear all overlays."
  (remove-overlays (point-min) (point-max) 'hl-scope t))


;; ---------------------------------------------------------------------------
;; Internal Mode Management

(defun printf (&rest args) (princ (apply #'format args) #'external-debugging-output))

(defun hl-block--find-range (pt)
  "Return range around PT or nil."
  (let ((beg (ignore-errors (nth 1 (syntax-ppss pt)))))
    (when beg
      ;; Note that `end' may be nil for un-matched brackets.
      ;; The caller must handle this case.
      (let ()
        end))))


;; ---------------------------------------------------------------------------
;; Internal Bracket Functions
;;

;; If we are not already inside an s-expression, leave all-beg as-is.
(defun hl-scope--prev-sexp-expand (beg)
  (let ((beg-next nil))
    (while (setq beg-next (ignore-errors (nth 1 (syntax-ppss beg))))
      (setq beg beg-next)))
  beg)

(defun hl-scope--next-sexp-by-search (end)
  "Skip forward by search, bound by END."
  (let ((found nil))
    (while (and (null found) (search-forward "{" end t))
      ;; Ensure the bracket character is seen as a bracket by the syntax tree.
      ;; And not some kind of escaped bracket (for e.g.).
      (when (eq ?\( (char-syntax (char-before)))
        (setq found t)))
    found))

(defun hl-scope--next-sexp-by-syntax (end)
  "Skip forward by syntax, bound by END."
  (let ((found nil))
    (while (and (null found) (not (zerop (skip-syntax-forward "^(" end))) (< (point) end))
      (forward-char 1)
      (when (eq ?\( (char-syntax (char-before)))
        (setq found t)))
    found))

(defun hl-scope--next-sexp-generic (end)
  "Skip forward to the next opening expression, bound by END."
  ;; Both of these functions work, we may want to select syntax for lisp,
  ;; and search for C/C++ as way may not want to treat literal braces
  ;; the same way as other kinds of parenthesis.
  (hl-scope--next-sexp-by-search end)
  ;; (hl-scope--next-sexp-by-syntax end)
  ;;
  )

(defun hl-scope--tree-from-buffer-impl (all-beg all-end beg end)
  "Return a tree from the buffer.

The format is ((start . end) children-or-nil)
Test."
  (let
    (
      (range-tree nil)
      (end-bound (min end all-end)))
    (while (and (< (point) end-bound) (hl-scope--next-sexp-generic end-bound))
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
  (save-excursion
    (goto-char all-beg)
    (setq all-beg (hl-scope--prev-sexp-expand all-beg))
    (save-match-data (hl-scope--tree-from-buffer-impl all-beg all-end all-beg all-end))))

(cond
  ((display-graphic-p)
    (defun hl-scope--face-from-level (level)
      "Return a face from the indentation level."
      (cond
        ((zerop (mod level 2))
          (list :background "#2e2e3f"))
        (t
          (list :background "#3e3e5e")))))
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

(defun hl-scope--propertize-stops (level stops)
  "It's assumed the point is at the line start."
  (let
    ( ;; It's assumed (point) is at the beginning of the line.
      (pos-bol (point))
      (pos-whitespace
        (progn
          (skip-syntax-forward " " (line-end-position))
          (point))))
    (goto-char pos-bol)
    (let ((pos-end (+ (pop stops) pos-bol)))
      (while stops
        (let ((pos-beg (+ (pop stops) pos-bol)))
          (when (<= pos-end pos-whitespace)
            (setq pos-end (min pos-whitespace pos-end))
            (let ((face (hl-scope--face-from-level level)))
              ;; WORKS.
              ;; (put-text-property pos-beg pos-end 'font-lock-face face)

              (let ((ov (make-overlay pos-beg pos-end)))
                (overlay-put ov 'face face)
                (overlay-put ov 'hl-scope t))))
          (setq pos-end pos-beg))

        (setq level (1- level))))))

(defun hl-scope--font-lock-tree-impl (all-beg all-end tree level stops)
  "Implement full buffer font locking of indentation levels.

Argument STOPS is a list of indentation widths, ordered largest to smallest,
always ending in zero: e.g. (list 8 4 0)."
  (while tree
    (pcase-let ((`(,range . ,children) (pop tree)))
      (pcase-let ((`(,range-beg . ,range-end) range))
        (let ((stops-next (cons (hl-scope--detect-indent range-beg range-end (car stops)) stops)))
          ;; Only jump to the next range when level is zero so we don't miss coloring indentation
          ;; for nested blocks:
          ;;    }
          ;;    else /* <- indentation before this line for e.g. */
          ;;    {
          (when (zerop level)
            (goto-char (min all-end range-end))
            (goto-char (line-beginning-position)))
          (cond
            (children
              (let ((child-end (cdr (car (car children)))))
                (while (> (point) child-end)
                  (when (and (<= (point) all-end) (<= all-beg (point)))
                    (hl-scope--propertize-stops level stops-next))
                  (forward-line -1)))

              (hl-scope--font-lock-tree-impl all-beg all-end children (1+ level) stops-next)

              (let ((range-beg-bound (max range-beg all-beg)))
                (while (> (point) range-beg-bound)
                  (hl-scope--propertize-stops level stops-next)
                  (forward-line -1))))
            (t
              (while (> (point) range-beg)
                (when (and (<= (point) all-end) (<= all-beg (point)))
                  (hl-scope--propertize-stops level stops-next))
                (forward-line -1)))))))))

(defun hl-scope--font-lock-tree (all-beg all-end)
  "Lock tree."
  (save-excursion
    (let ((tree (hl-scope--tree-from-buffer all-beg all-end)))
      (hl-scope--font-lock-tree-impl all-beg all-end tree 0 (list 0)))))

(defun hl-scope--font-lock-fontify-region (pos-beg pos-end)
  "Update spelling for POS-BEG & POS-END to the queue, checking all text."
  (hl-scope--font-lock-tree pos-beg pos-end))

(defun hl-scope-mode-enable ()
  "Turn on `hl-scope-mode' for the current buffer."
  ;; (hl-scope--font-lock-tree)
  (jit-lock-register #'hl-scope--font-lock-fontify-region))

(defun hl-scope-mode-disable ()
  "Turn off `hl-scope-mode' for the current buffer."
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
  (hl-scope--font-lock-tree))

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
;;; hl-scope-.el ends here
