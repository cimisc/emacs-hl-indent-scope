;;; hl-scope-test.el --- Undo-fu session test -*- lexical-binding: t -*-

;; SPDX-License-Identifier: GPL-2.0-or-later
;; Copyright (C) 2020  Campbell Barton
;; Copyright (C) 2009-2015  Tomohiro Matsuyama

;; Author: Campbell Barton <ideasman42@gmail.com>

;; URL: https://codeberg.org/ideasman42/emacs-hl-scope
;; Keywords: convenience
;; Version: 0.1
;; Package-Requires: ((emacs "24.1"))

;;; Commentary:

;; This is a test for `hl-scope'.
;;

;;; Usage

;;
;; To test this file run:
;;
;;     `emacs -batch -l tests/hl-scope-test.el -f hl-scope-test-run-all'
;;

;;; Code:

;; ---------------------------------------------------------------------------
;; Setup Environment

(setq hl-scope-basedir (concat (file-name-directory load-file-name) ".."))
(add-to-list 'load-path hl-scope-basedir)
(require 'hl-scope)


;; ---------------------------------------------------------------------------
;; Internal Macros

(defmacro hl-scope-test--with-temp-dir (temp-dir &rest body)
  "Run BODY with TEMP-DIR directory."
  `
  (let ((,temp-dir (make-temp-file "" t)))
    (unwind-protect
      (progn
        ,@body)
      (delete-directory ,temp-dir t))))


;; ---------------------------------------------------------------------------
;; Tests

;; (defun hl-scope-test-run-all-impl ()
;;   "Run each test and exit."
;;   (hl-scope-test--with-temp-dir
;;     ;; Don't touch the users home directory.
;;     hl-scope-directory
;;     (dotimes (f 100)
;;       (let*
;;         ( ;; While the session file wouldn't typically
;;           ;; be in the same directory as the undo session data, it's harmless.
;;           (filename (concat hl-scope-directory "/hl-scope-test"))
;;           (filename-session (hl-scope--make-file-name filename)))
;;         (when (file-exists-p filename)
;;           (delete-file filename))
;;         (when (file-exists-p filename-session)
;;           (delete-file filename-session))
;;         (with-current-buffer (find-file-literally filename)
;;           (dotimes (_i 1000)
;;             (ignore-errors
;;               (pcase (random 3)
;;                 (`0
;;                   (dotimes (_j 10)
;;                     (insert (make-string (1+ (random 20)) (+ (random 26) 65)))))
;;                 (`1 (newline))
;;                 (`2 (insert "\t"))
;;                 (`3 (forward-line))
;;                 (`4 (forward-line -1))
;;                 (`5 (kill-line))
;;                 (`6 (kill-paragraph -1))
;;                 (`7 (yank))
;;                 (`8
;;                   (kill-region
;;                     (+ (point-min) (random (point-max)))
;;                     (+ (point-min) (random (point-max))))))))
;;           (save-buffer)
;;           (hl-scope-save)
;;           (kill-buffer (current-buffer)))
;;         (with-current-buffer (find-file-literally filename)
;;           (hl-scope-recover)
;;           (ignore-errors
;;             (while
;;               (prog1 t
;;                 (undo))))
;;           (let ((contents (buffer-string)))
;;             (set-buffer-modified-p nil)
;;             (kill-buffer (current-buffer))
;;             (cond
;;               ((string-equal contents "")
;;                 (message "Test succeeded #%s" f))
;;               (t
;;                 (error "Test failed #%s" f))))))))
;;   (message "Done"))
;; (require 'face-explorer)

(defun hl-scope-preview ()
  "Run every test."
  ;; (load-theme 'tango-dark)
  ;; (load-theme ')
  ;; (require 'color-theme)
  ;; (color-theme-initialize)

  (defun display-graphic-p (&rest _) t)
  (defun window-system (&rest _) 'pgtk)

  ;; (defun modify-frame-parameters (&rest _) nil)

  ;; (defun x-get-resource (a b)
  ;;   (message "A: %S" a)
  ;;   (cond
  ;;     ((string-equal a "background")
  ;;       "black")))


  (load-theme 'wombat)

  (let ((buf (generate-new-buffer "untitled.c")))
    (with-current-buffer buf
      (c-mode)
      (message "Hello %S\n" (list-colors-display))
      (insert
        "/* This is a comment {}. */\n"
        "#include \"test{}.h\"\n"
        "\n"
        "int main(void)\n"
        "{\n"
        "  if (foo) {\n"
        "    this_is_foo();\n"
        "    that_is_foo();\n"
        "  }\n"
        "  else {\n"
        "    testme();\n"
        "    if (bar) {\n"
        "      baz();\n"
        "      tas();\n"
        "    }\n"
        "  }\n"
        "  return 1;"
        "\n}\n"
        "\n"
        "static void test_me(int a)\n"
        "{\n"
        "  foo_bar();\n"
        "\n"
        "  if (foo) {\n"
        "    this_is_foo();\n"
        "    that_is_foo();\n"
        "  }\n"
        "  else {\n"
        "    testme();\n"
        "    if (bar) {\n"
        "      baz();\n"
        "      tas();\n"
        "    }\n"
        "  }\n"
        "}\n"
        "\n"
        "\n"
        "/* Test foo. */\n")

      (font-lock-mode 1)
      (font-lock-flush (point-min) (point-max))
      (font-lock-ensure (point-min) (point-max))
      (font-lock-fontify-region (point-min) (point-max))

      (hl-scope-mode)

      (let ((buf-html (htmlfontify-buffer)))
        (with-current-buffer buf-html
          (write-region (point-min) (point-max) (file-name-concat hl-scope-basedir "file.html"))))

      (kill-emacs))))

(provide 'hl-scope-test)
;;; hl-scope-test.el ends here
