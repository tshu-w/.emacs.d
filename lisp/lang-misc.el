;;; lang-misc.el --- -*- lexical-binding: t; -*-

;; Copyright (C) 2020-2021  Tianshu Wang

;; Author: Tianshu Wang <wang@tianshu.me>

;;; Commentary:

;;; Code:

(use-package dockerfile-mode :straight t :defer t)

(use-package js
  :defer t
  :mode ("\\.json\\'" . json-mode)
  :config
  (defvar json-mode-map
    (make-sparse-keymap)
    "Keymap for `json-mode'.")

  (define-derived-mode json-mode js-mode "JSON"
    (make-local-variable 'js-indent-level)
    (setq js-indent-level 2))

  (despot-def json-mode-map
    "=" 'json-pretty-print-buffer
    "+" 'json-pretty-print-buffer-ordered))

(use-package jsonnet-mode
  :straight t
  :defer t
  :config
  (despot-def jsonnet-mode-map
    "=" 'jsonnet-reformat-buffer))

(use-package web-mode :straight t :defer t)

(use-package yaml-mode :straight t :defer t)

(provide 'lang-misc)
;;; lang-misc.el ends here
