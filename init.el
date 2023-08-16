;;; init.el --- Tianshu Wang Personal Emacs Configuration. -*- lexical-binding: t; -*-

;; Copyright (C) 2020-2023  Tianshu Wang

;; Author: Tianshu Wang <wang@tianshu.me>

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

;;; Load path
;; optimize: force "lisp"" and "site-lisp" at the head to reduce the startup time.
(dolist (dir '("site-lisp" "lisp"))
  (push (expand-file-name dir user-emacs-directory) load-path))

(require 'core-packages)
(require 'core-config)
(require 'core-funcs)
(require 'core-keybinds)

(require 'editor-ui)
(require 'editor-completion)
(require 'editor-vsc)
(require 'editor-mail)
(require 'editor-misc)

(require 'lang-org)
(require 'lang-tex)
(require 'lang-python)
(require 'lang-emacs-lisp)
(require 'lang-misc)

(defun display-startup-echo-area-message ()
  "Display startup message."
  (message (concat "Startup time: " (emacs-init-time))))

;;; init.el ends here
