;;; init.el --- Tianshu Wang Personal Emacs Configuration. -*- lexical-binding: t; -*-

;; Copyright (C) 2020  Tianshu Wang

;; Author: Tianshu Wang <volekingsg@gmail.com>

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;;; Code:

(setq gc-cons-threshold most-positive-fixnum gc-cons-percentage 0.6)
(defvar default-file-name-handler-alist file-name-handler-alist)
(setq file-name-handler-alist nil)

(add-hook 'emacs-startup-hook
          (lambda ()
            (setq file-name-handler-alist default-file-name-handler-alist)
            (setq gc-cons-threshold 100000000 gc-cons-percentage 0.1)

            (defun minibuffer-setup ()
              (setq gc-cons-threshold 402653184 gc-cons-percentage 0.6))

            (defun minibuffer-exit ()
              (setq gc-cons-threshold 100000000 gc-cons-percentage 0.1))

            (add-hook 'minibuffer-setup-hook #'minibuffer-setup)
            (add-hook 'minibuffer-exit-hook  #'minibuffer-exit)))

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

(advice-add 'package-initialize :after #'update-load-path)
(advice-add 'package-initialize :after #'add-subdirs-to-load-path)

(update-load-path)

(setq byte-compile-warnings '(cl-functions))

(require 'core-packages)
(require 'core-config)
(require 'core-funcs)
(require 'core-keybinds)

(require 'editor-ui)
(require 'editor-completion)
(require 'editor-projectile)
(require 'editor-checker)
(require 'editor-vsc)
(require 'editor-shell)
(require 'editor-mail)
(require 'editor-misc)

(require 'lang-org)
(require 'lang-latex)
(require 'lang-python)
(require 'lang-emacs-lisp)
(require 'lang-markdown)
(require 'lang-misc)

(defun display-startup-echo-area-message ()
  "Display startup message."
  (message (concat "Startup time: " (emacs-init-time))))

;;; init.el ends here
