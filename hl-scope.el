;;; hl-scope-.el --- Highlighting nested blocks -*- lexical-binding: t -*-

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

(defcustom hl-scope-delay 0.2 "Idle time to wait before highlighting (in seconds)." :type 'float)


;; ---------------------------------------------------------------------------
;; Internal Variables

(defvar-local hl-scope--overlay nil)

;; ---------------------------------------------------------------------------
;; Internal Bracket Functions

(defun hl-scope--syntax-prev-bracket (pt)
  "A version of `syntax-ppss' to match curly braces.
PT is typically the `(point)'."
  (let ((beg (ignore-errors (elt (syntax-ppss pt) 1))))
    (when beg
      (cond
        ((memq (char-after beg) hl-scope-bracket)
          beg)
        (t
          (hl-scope--syntax-prev-bracket (1- beg)))))))


(defun hl-scope--find-range (pt)
  "Return range around PT or nil."
  (let
    (
      (beg
        (cond
          (hl-scope-bracket
            (hl-scope--syntax-prev-bracket pt))
          (t
            (ignore-errors (elt (syntax-ppss pt) 1))))))
    (when beg
      ;; Note that `end' may be nil for un-matched brackets.
      ;; The caller must handle this case.
      (let ((end (ignore-errors (scan-sexps beg 1))))
        (cons beg end)))))


(defun hl-scope--find-all-ranges (pt)
  "Return ranges starting from PT, outer-most to inner-most."
  (let ((range (hl-scope--find-range pt)))
    (when range
      ;; When the previous range is nil, this simply terminates the list.
      (cons range (hl-scope--find-all-ranges (car range))))))


(defun hl-scope--find-single-range (pt)
  "Return ranges starting from PT, only a single level."
  (let ((range (hl-scope--find-range pt)))
    (when range
      (list range))))


(defun hl-scope--syntax-skip-to-multi-line ()
  "Move point to the first multi-line block.

The point will only ever be moved backward."
  (let
    (
      (line-min (line-beginning-position))
      (line-max (line-end-position))
      (beg (point))
      (end (point)))
    (while (and beg (>= beg line-min) end (<= end line-max))
      (setq beg (ignore-errors (elt (syntax-ppss beg) 1)))
      (when beg
        (setq end (ignore-errors (scan-sexps beg 1)))))))


;; ---------------------------------------------------------------------------
;; Internal Color Tint (Draw Style)

(defun hl-scope--color-values-as-string (color)
  "Build a color from COLOR.
Inverse of `color-values'."
  (format "#%02x%02x%02x" (ash (aref color 0) -8) (ash (aref color 1) -8) (ash (aref color 2) -8)))


(defun hl-scope--color-tint-add (a b tint)
  "Tint color lighter from A to B by TINT amount."
  (vector
    (+ (aref a 0) (* tint (aref b 0)))
    (+ (aref a 1) (* tint (aref b 1)))
    (+ (aref a 2) (* tint (aref b 2)))))


(defun hl-scope--color-tint-sub (a b tint)
  "Tint colors darker from A to B by TINT amount."
  (vector
    (- (aref a 0) (* tint (aref b 0)))
    (- (aref a 1) (* tint (aref b 1)))
    (- (aref a 2) (* tint (aref b 2)))))


(defun hl-scope--overlay-create-color-tint (block-list end-fallback)
  "Update the overlays based on the cursor location.
Argument BLOCK-LIST represents start-end ranges of braces.
Argument END-FALLBACK is the point used when no matching end bracket is found,
typically `(point)'."
  (let*
    (
      (block-list-len (length block-list))
      (bg-color (apply 'vector (color-values (face-attribute 'default :background))))
      (bg-color-tint (apply 'vector (color-values hl-scope-color-tint)))
      ;; Check dark background is light/dark.
      (do-highlight (> 98304 (+ (aref bg-color 0) (aref bg-color 1) (aref bg-color 2))))
      ;; Iterator.
      (i 0))
    (pcase-let ((`(,beg-prev . ,end-prev) (pop block-list)))
      (unless end-prev ;; May be `nil' for un-matched brackets.
        (setq end-prev end-fallback))
      (while block-list
        (pcase-let ((`(,beg . ,end) (pop block-list)))
          (unless end ;; May be `nil' for un-matched brackets.
            (setq end end-fallback))
          (let
            (
              (elem-overlay-beg (make-overlay beg beg-prev))
              (elem-overlay-end (make-overlay end-prev end)))

            (let
              ( ;; Calculate the face with the tint color at this highlight level.
                (hl-face
                  (list
                    :background
                    (hl-scope--color-values-as-string
                      (let ((i-tint (- block-list-len i)))
                        (cond
                          (do-highlight
                            (hl-scope--color-tint-add bg-color bg-color-tint i-tint))
                          (t
                            (hl-scope--color-tint-sub bg-color bg-color-tint i-tint)))))
                    :extend t)))

              (overlay-put elem-overlay-beg 'face hl-face)
              (overlay-put elem-overlay-end 'face hl-face))

            (push elem-overlay-beg hl-scope--overlay)
            (push elem-overlay-end hl-scope--overlay)
            (setq beg-prev beg)
            (setq end-prev end))
          (setq i (1+ i)))))))


;; ---------------------------------------------------------------------------
;; Internal Color Tint (Draw Style)

(defun hl-scope--overlay-create-bracket (block-list)
  "Update the overlays based on the cursor location.
Argument BLOCK-LIST represents start-end ranges of braces."
  ;; hl-scope-bracket-face
  (pcase-dolist (`(,beg . ,end) block-list)
    (let ((elem-overlay-beg (make-overlay beg (1+ beg))))
      (overlay-put elem-overlay-beg 'face hl-scope-bracket-face)
      (push elem-overlay-beg hl-scope--overlay)
      (when end ;; May be `nil' for un-matched brackets.
        (let ((elem-overlay-end (make-overlay (1- end) end)))
          (overlay-put elem-overlay-end 'face hl-scope-bracket-face)
          (push elem-overlay-end hl-scope--overlay))))))


;; ---------------------------------------------------------------------------
;; Internal Refresh Function

(defun hl-scope--overlay-clear ()
  "Clear all overlays."
  (mapc 'delete-overlay hl-scope--overlay)
  (setq hl-scope--overlay nil))


(defun hl-scope--overlay-refresh ()
  "Update the overlays based on the cursor location."
  (hl-scope--overlay-clear)
  (let
    (
      (block-list
        (save-excursion
          (when hl-scope-multi-line
            (hl-scope--syntax-skip-to-multi-line))
          (cond
            (hl-scope-single-level
              (hl-scope--find-single-range (point)))
            (t
              (hl-scope--find-all-ranges (point)))))))

    (when block-list
      (cond
        ((eq hl-scope-style 'color-tint)
          ;; Ensure outer bounds (when only one pair exists).
          (setq block-list
            (cond
              ((cdr block-list)
                (reverse block-list))
              (t
                (cons (cons (point-min) (point-max)) block-list))))
          (hl-scope--overlay-create-color-tint block-list (point)))
        ((eq hl-scope-style 'bracket)
          (hl-scope--overlay-create-bracket block-list))
        (t
          (error "Unknown style %S" hl-scope-style))))))


;; ---------------------------------------------------------------------------
;; Internal Mode Management

(defun printf (&rest args) (princ (apply #'format args) #'external-debugging-output))


(defun hl-block--find-range (pt)
  "Return range around PT or nil."
  (let ((beg (ignore-errors (elt (syntax-ppss pt) 1))))
    (when beg
      ;; Note that `end' may be nil for un-matched brackets.
      ;; The caller must handle this case.
      (let ()
        end))))


;; ---------------------------------------------------------------------------
;; Internal Bracket Functions

(defun hl-scope---tree-from-buffer-impl (beg end)
  "Return a tree from the buffer.

The format is ((start . end) children-or-nil)
Test."
  (let ((range-tree nil))
    (while (search-forward "{" end t)
      (let ((state (syntax-ppss)))
        (unless (or (nth 3 state) (nth 4 state))
          (let ((pos-beg (point)))
            (let ((pos-end (ignore-errors (scan-sexps (1- (point)) 1))))
              (unless pos-end
                (setq pos-end end))
              (push
                (cons (cons (point) pos-end) (hl-scope---tree-from-buffer-impl pos-beg pos-end))
                range-tree)
              (goto-char pos-end))))))
    range-tree))

(defun hl-scope---tree-from-buffer ()
  (save-excursion
    (goto-char (point-min))
    (save-match-data (hl-scope---tree-from-buffer-impl (point-min) (point-max)))))


(defun hl-scope-mode-enable ()
  "Turn on `hl-scope-mode' for the current buffer."

  ;; (put-text-property 1 8 'font-lock-face 'warning)
  (toggle-debug-on-error)
  (put-text-property 1 6 'font-lock-face 'warning)

  (goto-char (point-min))

  (printf "TREE: %S\n" (hl-scope---tree-from-buffer))

  (goto-char (point-min))
  (let
    (
      (aa-face (list :background "#00AA00"))
      (xx-face (list :background "#AA0000"))
      (yy-face (list :background "#0000AA")))

    (save-match-data
      (while (search-forward "{" (point-max) t)
        (let
          (
            (state (syntax-ppss))
            (pos-end nil)
            (var nil))
          (cond
            ((or (nth 3 state) (nth 4 state))
              nil)
            (t
              ;; (insert "|")

              (setq pos-end (ignore-errors (scan-sexps (1- (point)) 1)))
              (unless pos-end
                (setq pos-end (point-max)))

              (printf "%S -\n" pos-end)

              ;; (setq pos-end
              ;;   (save-excursion
              ;;     (forward-char -1)
              ;;     (ignore-errors (forward-sexp) (1- (point)))))

              (put-text-property (point) pos-end 'font-lock-face xx-face)
              ;; (goto-char (point-max))

              (setq var xx-face)
              (setq xx-face aa-face)
              (setq aa-face yy-face)
              (setq yy-face var)

              ;; (forward-char 1)
              )))))))


(defun hl-scope-mode-disable ()
  "Turn off `hl-scope-mode' for the current buffer."
  (hl-scope--overlay-clear)
  (kill-local-variable 'hl-scope--overlay)
  (kill-local-variable 'hl-scope-bracket)
  (hl-scope--time-buffer-local-disable))

(defun hl-scope-mode-turn-on ()
  "Enable command `hl-scope-mode'."
  (when (and (not (minibufferp)) (not (bound-and-true-p hl-scope-mode)))
    (hl-scope-mode 1)))

;; ---------------------------------------------------------------------------
;; Public API

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
