;;; init.el --- Tianshu Wang Personal Emacs Configuration. -*- lexical-binding: t; -*-

;; Copyright (C) 2020  Tianshu Wang

;; Author: Tianshu Wang <volekingsg@gmail.com>

(setq gc-cons-threshold most-positive-fixnum gc-cons-percentage 0.6)
(defvar default-file-name-handler-alist file-name-handler-alist)
(setq file-name-handler-alist nil)

(add-hook 'emacs-startup-hook
          (lambda ()
            (setq file-name-handler-alist default-file-name-handler-alist)
            (setq gc-cons-threshold 100000000 gc-cons-percentage 0.1)

            (defun my-minibuffer-setup-hook ()
              (setq gc-cons-threshold 402653184 gc-cons-percentage 0.6))

            (defun my-minibuffer-exit-hook ()
              (setq gc-cons-threshold 100000000 gc-cons-percentage 0.1))

            (add-hook 'minibuffer-setup-hook #'my-minibuffer-setup-hook)
            (add-hook 'minibuffer-exit-hook #'my-minibuffer-exit-hook)))

;;; Load path
;; optimize: force "lisp"" and "site-lisp" at the head to reduce the startup time.
(defun update-load-path (&rest _)
  "Update `load-path'."
  (push (expand-file-name "site-lisp" user-emacs-directory) load-path)
  (push (expand-file-name "lisp" user-emacs-directory) load-path))

(defun add-subdirs-to-load-path (&rest _)
  "Add subdirectories to `load-path'."
  (let ((default-directory
          (expand-file-name "site-lisp" user-emacs-directory)))
    (normal-top-level-add-subdirs-to-load-path)))

(advice-add #'package-initialize :after #'update-load-path)
(advice-add #'package-initialize :after #'add-subdirs-to-load-path)

(update-load-path)

(require 'core-packages)
(require 'core-config)
(require 'core-funcs)
(require 'core-keybinds)

(require 'editor-ui)
(require 'editor-completion)
(require 'editor-projectile)
(require 'editor-layouts)
(require 'editor-vsc)
(require 'editor-shell)
(require 'editor-misc)

(require 'lang-org)
(require 'lang-latex)
(require 'lang-python)
(require 'lang-emacs-lisp)
(require 'lang-markdown)
(require 'lang-misc)

(defun display-startup-echo-area-message ()
  (message (concat "Startup time: " (emacs-init-time))))
