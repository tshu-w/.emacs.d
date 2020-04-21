;;; lang-emacs-lisp.el --- -*- lexical-binding: t; -*-

;; Copyright (C) 2020  Tianshu Wang

;; Author: Tianshu Wang <volekingsg@gmail.com>

;;; Commentary:

;;; Code:

;; Idea from http://www.reddit.com/r/emacs/comments/312ge1/i_created_this_function_because_i_was_tired_of/
(defun eval-current-form ()
  "Find and evaluate the current def* or set* command.
Unlike `eval-defun', this does not go to topmost function."
  (interactive)
  (save-excursion
    (search-backward-regexp "(def\\|(set")
    (forward-list)
    (call-interactively 'eval-last-sexp)))

(defun nav-find-elisp-thing-at-point-other-window ()
  "Find thing under point and go to it another window."
  (interactive)
  (let ((symb (variable-at-point)))
    (if (and symb
             (not (equal symb 0))
             (not (fboundp symb)))
        (find-variable-other-window symb)
      (find-function-at-point))))

(defun find-ert-test-buffer (ert-test)
  "Return the buffer where ERT-TEST is defined."
  (save-excursion
    (car (find-definition-noselect (ert-test-name ert-test) 'ert-deftest))))

(defun ert-run-tests-buffer ()
  "Run all the tests in the current buffer."
  (interactive)
  (save-buffer)
  (load-file (buffer-file-name))
  (ert '(satisfies (lambda (test)
                     (eq (current-buffer) (find-ert-test-buffer test))))))

(despot-def :keymaps '(emacs-lisp-mode-map lisp-interaction-mode-map)
  ","  'lisp-state-toggle-lisp-state
  "c"  '(:ignore t :which-key "compile")
  "cc" 'emacs-lisp-byte-compile
  "e"  '(:ignore t :which-key "eval")
  "e$" 'lisp-state-eval-sexp-end-of-line
  "eb" 'eval-buffer
  "eC" 'eval-current-form
  "ee" 'eval-last-sexp
  "er" 'eval-region
  "ef" 'eval-defun
  "el" 'lisp-state-eval-sexp-end-of-line
  "g"  '(:ignore t :which-key "find-symbol")
  "gb" 'xref-pop-marker-stack
  "gG" 'nav-find-elisp-thing-at-point-other-window
  "t"  '(:ignore t :which-key "tests")
  "tb" 'ert-run-tests-buffer
  "tq" 'ert)

(use-package ielm
  :general
  (despot-def :keymaps '(emacs-lisp-mode-map lisp-interaction-mode-map)
    "'" 'ielm))

(use-package debug
  :config
  (defun elisp-toggle-debug-expr-and-eval-func ()
    "Insert or remove debug expression, evaluate function and save buffer."
    (interactive)
    (let ((trace "(debug)")
          (line (thing-at-point 'line)))
      (if (and line (string-match trace line))
          (kill-whole-line)
        (progn
          (back-to-indentation)
          (insert trace)
          (newline-and-indent))))
    (eval-defun nil)
    (save-buffer))
  :general
  (despot-def :keymaps '(emacs-lisp-mode-map lisp-interaction-mode-map)
    "d"  '(:ignore t :which-key "debug")
    "dt" 'elisp-toggle-debug-expr-and-eval-func))

(use-package edebug
  :config
  (defun edebug-instrument-defun-on ()
    "Toggle on instrumentalisation for the function under `defun'."
    (interactive)
    (eval-defun 'edebugit))

  (defun edebug-instrument-defun-off ()
    "Toggle off instrumentalisation for the function under `defun'."
    (interactive)
    (eval-defun nil))
  :general
  (despot-def :keymaps '(emacs-lisp-mode-map lisp-interaction-mode-map)
    "d"  '(:ignore t :which-key "debug")
    "df" 'edebug-instrument-defun-on
    "dF" 'edebug-instrument-defun-off))

(use-package emr
  :ensure t
  :general
  (despot-def :keymaps 'emacs-lisp-mode-map
    "ew"  'emr-el-eval-and-replace
    "r"   '(:ignore t :which-key "refactor")
    "rd"  '(:ignore t :which-key "delete")
    "rdl" 'emr-el-delete-let-binding-form
    "rdd" 'emr-el-delete-unused-definition
    "re"  '(:ignore t :which-key "extract/expand")
    "ref" 'emr-el-extract-function
    "rev" 'emr-el-extract-variable
    "rel" 'emr-el-extract-to-let
    "rec" 'emr-el-extract-constant
    "rea" 'emr-el-extract-autoload
    "rf"  '(:ignore t :which-key "find/function")
    "rfe" 'emr-el-implement-function
    "rfd" 'emr-el-find-unused-definitions
    "ri"  '(:ignore t :which-key "insert/inline")
    "riv" 'emr-el-inline-variable
    "ris" 'emr-el-inline-let-variable
    "rif" 'emr-el-inline-function
    "ria" 'emr-el-insert-autoload-directive))


(provide 'lang-emacs-lisp)
;;; lang-emacs-lisp.el ends here
