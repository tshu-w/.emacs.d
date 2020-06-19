;;; lang-misc.el --- -*- lexical-binding: t; -*-

;; Copyright (C) 2020  Tianshu Wang

;; Author: Tianshu Wang <volekingsg@gmail.com>

;;; Commentary:

;;; Code:

(use-package dockerfile-mode
  :ensure t
  :mode ("Dockerfile\\(?:\\..*\\)?\\'" . dockerfile-mode))

(use-package json-mode
  :ensure t
  :mode ("\\.json\\'" . json-mode)
  :config
  (add-hook 'json-mode-hook
            (lambda ()
              (make-local-variable 'js-indent-level)
              (setq js-indent-level 2)))
  :general
  (despot-def json-mode-map
    "=" 'json-mode-beautify
    "p" 'json-mode-show-path
    "P" 'json-mode-kill-path
    "t" 'json-toggle-boolean
    "n" 'json-nullify-sexp))

(use-package web-mode
  :ensure t
  :mode ("\\.html\\'"))

(use-package yaml-mode
  :ensure t
  :mode ("\\.\\(e?ya?\\|ra\\)ml\\'" . yaml-mode))


(provide 'lang-misc)
;;; lang-misc.el ends here
