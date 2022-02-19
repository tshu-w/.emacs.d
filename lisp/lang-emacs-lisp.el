;;; lang-emacs-lisp.el --- -*- lexical-binding: t; -*-

;; Copyright (C) 2020-2022  Tianshu Wang

;; Author: Tianshu Wang <wang@tianshu.me>

;;; Commentary:

;;; Code:

(use-package elisp-mode
  :config
  (despot-def :keymaps '(emacs-lisp-mode-map lisp-interaction-mode-map)
    "'"  'ielm
    "c"  '(:ignore t :which-key "compile")
    "cc" 'emacs-lisp-byte-compile
    "e"  '(:ignore t :which-key "eval")
    "eb" 'eval-buffer
    "ee" 'eval-last-sexp
    "er" 'eval-region
    "ef" 'eval-defun
    "t"  '(:ignore t :which-key "tests")
    "tb" 'ert-run-tests-buffer
    "tq" 'ert))

(use-package eval-sexp-fu
  :straight t
  :hook (emacs-lisp-mode . eval-sexp-fu-flash-mode))


(provide 'lang-emacs-lisp)
;;; lang-emacs-lisp.el ends here
