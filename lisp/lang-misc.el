;;; lang-misc.el --- -*- lexical-binding: t; -*-

;; Copyright (C) 2020  Tianshu Wang

;; Author: Tianshu Wang <volekingsg@gmail.com>

;;; Commentary:

;;; Code:

(use-package dockerfile-mode :ensure t :defer t)

(use-package json-mode
  :ensure t
  :defer t
  :config
  (add-hook 'json-mode-hook
            (lambda ()
              (make-local-variable 'js-indent-level)
              (setq js-indent-level 2)))

  (despot-def json-mode-map
    "=" 'json-mode-beautify
    "p" 'json-mode-show-path
    "P" 'json-mode-kill-path
    "t" 'json-toggle-boolean
    "n" 'json-nullify-sexp))

(use-package web-mode :ensure t :defer t)

(use-package yaml-mode :ensure t :defer t)


(provide 'lang-misc)
;;; lang-misc.el ends here
