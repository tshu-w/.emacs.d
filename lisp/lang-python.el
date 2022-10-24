;;; lang-python.el --- -*- lexical-binding: t; -*-

;; Copyright (C) 2020-2022  Tianshu Wang

;; Author: Tianshu Wang <wang@tianshu.me>

;;; Commentary:

;;; Code:

(use-package python
  :init
  (add-hook 'python-mode-hook
            (defun enable-eglot-unless-tramp ()
              (unless (file-remote-p default-directory)
                         eglot-ensure)))
  :config
  (setq python-indent-def-block-scale 1
        python-indent-guess-indent-offset-verbose nil)

  (add-hook 'python-mode-hook
            (defun init-python-mode ()
              "Stuff to do when opening `python-mode' files."
              (set (make-local-variable 'comment-inline-offset) 2)
              (setq fill-column 88
                    tab-width 4))))


(provide 'lang-python)
;;; lang-python.el ends here
