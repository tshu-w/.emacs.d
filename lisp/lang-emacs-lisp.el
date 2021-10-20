;;; lang-emacs-lisp.el --- -*- lexical-binding: t; -*-

;; Copyright (C) 2020-2021  Tianshu Wang

;; Author: Tianshu Wang <wang@tianshu.me>

;;; Commentary:

;;; Code:

(use-package elisp-mode
  :config
  ;; Idea from http://www.reddit.com/r/emacs/comments/312ge1/i_created_this_function_because_i_was_tired_of/
  (defun eval-current-form ()
    "Find and evaluate the current def* or set* command.
  Unlike `eval-defun', this does not go to topmost function."
    (interactive)
    (save-excursion
      (search-backward-regexp "(def\\|(set")
      (forward-list)
      (call-interactively 'eval-last-sexp)))

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
    "c"  '(:ignore t :which-key "compile")
    "cc" 'emacs-lisp-byte-compile
    "e"  '(:ignore t :which-key "eval")
    "eb" 'eval-buffer
    "ec" 'eval-current-form
    "ee" 'eval-last-sexp
    "er" 'eval-region
    "ef" 'eval-defun
    "t"  '(:ignore t :which-key "tests")
    "tb" 'ert-run-tests-buffer
    "tq" 'ert))

(use-package ielm
  :general
  (despot-def :keymaps '(emacs-lisp-mode-map lisp-interaction-mode-map)
    "'" 'ielm))


(provide 'lang-emacs-lisp)
;;; lang-emacs-lisp.el ends here
