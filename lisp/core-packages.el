;;; core-packages.el --- -*- lexical-binding: t; -*-

;; Copyright (C) 2020-2025  Tianshu Wang

;; Author: Tianshu Wang <wang@tianshu.me>

;; initialize package

;;; Commentary:

;;; Code:

(setq package-archives '(("gnu"          . "https://elpa.gnu.org/packages/")
                         ("melpa"        . "https://melpa.org/packages/")
                         ("nongnu"       . "https://elpa.nongnu.org/nongnu/")))

;; bootstrap `straight.el'
(defvar bootstrap-version)
(setq straight-check-for-modifications '(find-when-checking)
      straight-host-usernames '((github . "tshu-w"))
      straight-vc-git-default-clone-depth 1
      straight-build-dir (format "build/%d%s%d"
                                 emacs-major-version
                                 version-separator
                                 emacs-minor-version))

(let ((bootstrap-file
       (expand-file-name
        "straight/repos/straight.el/bootstrap.el"
        (or (bound-and-true-p straight-base-dir)
            user-emacs-directory)))
      (bootstrap-version 7))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(if init-file-debug
    (setq use-package-verbose t
          use-package-minimum-reported-time 0
          use-package-compute-statistics t
          use-package-inject-hooks t
          debug-on-error t)
  (setq use-package-expand-minimally t))
(straight-use-package 'use-package)

(use-package exec-path-from-shell
  :if (eq system-type 'darwin)
  :straight t
  :defer t
  :init
  (setq exec-path-from-shell-arguments nil
        exec-path-from-shell-variables '("PATH" "MANPATH" "GNUPGHOME" "SSH_AUTH_SOCK"
                                         "XDG_CACHE_HOME" "XDG_DATA_HOME" "XDG_CONFG_HOME" "XDG_STATE_HOME"))
  (exec-path-from-shell-initialize))

(use-package no-littering :straight t :defer t)

(use-package restart-emacs
  :straight t
  :commands (restart-emacs-debug-init restart-emacs-without-desktop)
  :init
  (with-eval-after-load 'files
    ;; unbind `restart-emacs' and declare it from package
    (fmakunbound 'restart-emacs)
    (autoload 'restart-emacs "restart-emacs"))
  :config
  (defun restart-emacs-debug-init (&optional args)
    "Restart emacs and enable debug-init."
    (interactive)
    (restart-emacs (cons "--debug-init" args)))

  (defun restart-emacs-without-desktop (&optional args)
    "Restart emacs without desktop."
    (interactive)
    (restart-emacs (cons "--no-desktop" args))))

(provide 'core-packages)
;;; core-packages.el ends here
