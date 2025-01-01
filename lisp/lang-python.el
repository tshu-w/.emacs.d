;;; lang-python.el --- -*- lexical-binding: t; -*-

;; Copyright (C) 2020-2025  Tianshu Wang

;; Author: Tianshu Wang <wang@tianshu.me>

;;; Commentary:

;;; Code:

(use-package python
  :defer t
  :config
  (setq python-indent-def-block-scale 1
        python-indent-guess-indent-offset-verbose nil)

  (add-hook 'python-base-mode
            (defun init-python-mode ()
              "Stuff to do when opening `python-mode' files."
              (set (make-local-variable 'comment-inline-offset) 2)
              (setq fill-column 88))))


(provide 'lang-python)
;;; lang-python.el ends here
