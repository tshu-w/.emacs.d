;;; core-packages.el --- -*- lexical-binding: t; -*-

;; Copyright (C) 2020-2021  Tianshu Wang

;; Author: Tianshu Wang <wang@tianshu.me>

;; initialize package

;;; Commentary:

;;; Code:

;; bootstrap `straight.el'
(defvar bootstrap-version)
(setq straight-check-for-modifications '(find-when-checking)
      straight-host-usernames '((github . "tshu-w"))
      straight-vc-git-default-clone-depth 1)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(straight-use-package 'use-package)

(use-package exec-path-from-shell
  :if (memq window-system '(mac ns))
  :straight t
  :defer t
  :init
  (setq exec-path-from-shell-arguments '("-l")
        exec-path-from-shell-variables '("PATH" "MANPATH"
                                         "GNUPGHOME"
                                         "WAKATIME_HOME"
                                         "SSH_AUTH_SOCK"))
  (exec-path-from-shell-initialize))

(use-package no-littering :straight t :defer t)

(use-package esup :straight t :defer t)

(use-package restart-emacs :straight t :defer t)

(provide 'core-packages)
;;; core-packages.el ends here
