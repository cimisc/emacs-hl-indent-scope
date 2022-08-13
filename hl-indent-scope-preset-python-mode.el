;;; hl-indent-scope-preset-python-mode.el --- Python preset -*- lexical-binding: t -*-

;; SPDX-License-Identifier: GPL-3.0-or-later
;; Copyright (C) 2022  Campbell Barton

;; Author: Campbell Barton <ideasman42@gmail.com>

;; URL: https://codeberg.org/ideasman42/emacs-hl-indent-scope-preset
;; Version: 0.1
;; Package-Requires: ((emacs "26.2"))

;;; Commentary:
;; Preset for Python mode.

;;; Code:

(eval-when-compile (require 'hl-indent-scope))

(defun printf (&rest args) (princ (apply #'format args) #'external-debugging-output))

(defconst hl-indent-scope-preset-python--block-commands
  (concat
    "\\_<"
    ;; Commands with arguments:
    "\\(if\\|elif\\|while\\|for\\|def\\|class\\|except\\|match\\|case\\)"
    ;; Trailing space & parenthesis.
    "\\_>"

    "\\|"
    ;; Commands without arguments:
    "\\(else\\|try\\)"
    ;; Trailing space & parenthesis.
    "\\_>"))

(defun hl-indent-scope-preset-python--command-list (beg end)
  "XX."
  (let ((result (list)))
    (save-excursion
      ;; TODO:
      ;; (goto-char beg)
      (goto-char (point-max))

      ;; (goto-char (line-end-position))

      (while (re-search-backward hl-indent-scope-preset-python--block-commands beg t)
        (let ((state (syntax-ppss)))
          ;; Skip strings & comments.
          (unless (or (nth 3 state) (nth 4 state))
            (let
              (
                (match-beg (match-beginning 0))
                (bol (line-beginning-position)))
              ;; Ensure the command is at the line beginning.
              (when
                (save-excursion
                  (goto-char bol)
                  (skip-syntax-forward " " match-beg)
                  (eq (point) match-beg))
                ;; TODO: `cmd-end' should be the point directly after `:'.
                ;; for now use line end position.
                (let ((cmd-end (line-end-position)))
                  (push (cons (- match-beg bol) (cons match-beg cmd-end)) result))))))))
    result))

;;

(defun hl-indent-scope-preset-python--tree-impl-REF (beg end use-match)
  "Recursive tree extraction for Python in range BEG END.
Argument USE-MATCH uses an existing match instead of a new search."
  (let
    (
      (span (cons nil nil))
      (span-beg-fallback nil)
      (children nil))
    (while
      (and
        (null (cdr span))
        (cond
          (use-match
            ;; Only ever use once!
            (setq use-match nil)
            t)
          (t
            (re-search-forward hl-indent-scope-preset-python--block-commands (point-max) t))))
      (let ((state (syntax-ppss)))
        ;; Skip strings & comments.
        (unless (or (nth 3 state) (nth 4 state))
          (let*
            (
              (str-open (match-string 1))
              (str-close (and (null str-open) (match-string 2))))

            (cond
              (str-open
                (cond
                  ((null (car span))
                    (setcar span (match-end 1)))
                  (t
                    (let ((child (hl-indent-scope-preset-python--tree-impl beg end t)))
                      (when child
                        (unless span-beg-fallback
                          (setq span-beg-fallback (car (car child))))
                        (unless (eq t (cdr child))
                          (push child children)))))))
              (str-close
                ;; Break.
                (setcdr span (match-beginning 2))))))))
    (cond
      ((and (car span) (cdr span))
        (cond
          ((or (< (cdr span) beg) (< end (car span)))
            ;; Return the span so it's possible to know the bounds,
            ;; but this is out of range.
            (cons span t))
          (t
            (cons span children))))
      (children
        (cons (cons span-beg-fallback (cdr (car (car children)))) children))
      (t
        nil))))

(defun hl-indent-scope-preset-python--tree-fn--REF (beg end)
  "Callback for `hl-indent-scope-tree-fn'.
Return a tree in range BEG END."
  (let
    (
      (tree nil)
      ;; Python uses case insensitive commands.
      (case-fold-search t))
    (goto-char (point-min))
    (save-match-data
      (while (< (point) end)
        ;; Stop searching once end is exceeded.
        (let ((child (hl-indent-scope-preset-python--tree-impl beg end nil)))
          (cond
            ((null child)
              ;; Exit.
              (goto-char end))
            ((not (eq t (cdr child)))
              ;; Skip t (out of range).
              (push child tree))))))
    tree))


(defsubst py-line-indent-is-comment-or-space (pos)
  "XXX."
  ;; See: Syntax Table Internals.
  (memq
    (car (syntax-after pos))
    (list
      ;; White-space.
      0
      ;; Comment start, end
      11 12
      ;; Generic comment.
      14)))

(defsubst py-line-indent-is-atleast-or-ignore (ident-limit)
  "Line beginning position indentation level."
  (save-excursion
    (let ((abs-limit (+ (point) ident-limit)))
      (let ((ident-curr (skip-syntax-forward " " abs-limit)))
        ;; (printf "%d %d\n" ident-limit ident-curr)
        (cond
          ((eq ident-limit ident-curr)
            ;; Atleast at limit.
            t)
          ;; Ignore comment or space.
          ((py-line-indent-is-comment-or-space (point))
            t)
          (t
            nil))))))

;; Function that returns the tree.

;; This takes two arguments representing the range to return BEG & END.

;; The resulting list is in the format: ((start . end) children-or-nil)
;; All items in the lists (including children) should be ordered

(defun hl-indent-scope-preset-python--tree-fn-impl (beg end flat-tree this-ident)
  "Return the remaining flat-tree as well as the resulting tree."

  (let
    (
      (flat-tree-next flat-tree)
      (tree-siblings (list)))
    (while flat-tree
      (pcase-let ((`(,ident-ofs . (,cmd-beg . ,cmd-end)) (pop flat-tree)))
        (cond
          ((< ident-ofs this-ident)
            ;; Break
            (printf "BREAK?\n")
            (setq flat-tree nil))
          ((> ident-ofs this-ident)
            (printf "CHILD?\n")
            (let ((last-sibling (car tree-siblings)))
              (pcase-let
                (
                  (`(,child-tree . ,child-result)
                    (hl-indent-scope-preset-python--tree-fn-impl
                      beg end flat-tree-next ident-ofs)))
                ;; Consume the child tree.
                (setq flat-tree child-tree)

                (printf "CHILD? - child-result %S\n" child-result)
                (setcdr last-sibling child-result)
                (when child-result
                  (let
                    (
                      (last-sibling-end (cdr (car last-sibling)))
                      (child-end (cdr (car (car child-result)))))
                    (setcdr (car last-sibling) (max last-sibling-end child-end))))))

            ;; Step.
            (setq flat-tree-next flat-tree))
          (t
            (printf "SIBLING?\n")
            ;; Add sibling.
            (goto-char cmd-end)
            (forward-line 1)
            (goto-char (line-beginning-position))
            (let
              (
                (block-end-step (point))
                (limit end))
              (when flat-tree
                (pcase-let ((`(,_ident-ofs . (,cmd-beg . ,_cmd-end)) (car flat-tree)))
                  (setq limit (min limit cmd-beg))))

              (while (and (< (point) limit) (py-line-indent-is-atleast-or-ignore ident-ofs))
                (setq block-end-step (point))
                ;; (printf "Stepping\n")
                (unless (zerop (forward-line 1))
                  (goto-char end)))
              (let
                (
                  (block-beg
                    (save-excursion
                      (goto-char cmd-end)
                      (forward-line 1)
                      (line-beginning-position)))
                  (block-end
                    (save-excursion
                      (goto-char block-end-step)
                      (line-end-position))))
                (push (cons (cons block-beg block-end) nil) tree-siblings)))
            ;; Step.
            (setq flat-tree-next flat-tree)))))

    (cons flat-tree-next tree-siblings)))


;; (defun hl-indent-scope-preset-python--tree-fn-expand (tree)
;;   (let ((last-end (point-max)))
;;     (dolist (x tree)
;;       (cond
;;         ;; No children.
;;         ((null (cdr x))
;;           (goto-char (car (car x)))
;;           (while (and (< (point) last-end) (py-line-indent-is-atleast-or-ignore ident-ofs))
;;             (setq block-end-step (point))
;;             ;; (printf "Stepping\n")
;;             (unless (zerop (forward-line 1))
;;               (goto-char end)))))
;;       ;;
;;       )))

(defun hl-indent-scope-preset-python--tree-fn (beg end)
  "XXX."
  ;;
  (let ((flat-tree (hl-indent-scope-preset-python--command-list beg end)))

    ;; (printf "%s\n" (buffer-substring-no-properties (point-min) (point-max)))
    ;; (printf "BEGIN >>>>\n")
    ;; ;; Order first to last.
    ;; ;; (setq flat-tree (reverse flat-tree))
    ;; (dolist (x flat-tree)
    ;;   (printf "- %S\n" x)
    ;;   (printf "= '%s'\n" (buffer-substring-no-properties (car (cdr x)) (cdr (cdr x)))))
    ;; (printf "END >>>>\n")

    (goto-char (point-max))
    (let ((tree (cdr (hl-indent-scope-preset-python--tree-fn-impl beg end flat-tree 0))))

      ;; (hl-indent-scope-preset-python--tree-fn-expand tree)

      (printf "== %S\n" tree)
      tree

      ;; (list (cons (cons (point-min) (point-max)) tree)))))
      )

    ;; (printf "%S" children)
    ;;
    ))


;;;###autoload
(defun hl-indent-scope-preset-python-mode (&rest args)
  "Presets for `python-mode' with optional ARGS keyword arguments."
  (when args
    (message "Currently ARGS isn't used!"))
  (setq hl-indent-scope-fixed-width t)
  (setq hl-indent-scope-tree-fn 'hl-indent-scope-preset-python--tree-fn))

(provide 'hl-indent-scope-preset-python-mode)
;;; hl-indent-scope-preset-python-mode.el ends here
