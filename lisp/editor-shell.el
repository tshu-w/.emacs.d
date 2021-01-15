;;; editor-shell.el --- -*- lexical-binding: t; -*-

;; Copyright (C) 2020  Tianshu Wang

;; Author: Tianshu Wang <volekingsg@gmail.com>

;;; Commentary:

;;; Code:

(use-package shell-pop
  :ensure t
  :init
  (setq shell-pop-window-position 'bottom
        shell-pop-window-size     50
        shell-pop-term-shell      shell-file-name
        shell-pop-shell-type      '("eshell" "*eshell*" (lambda () (eshell)))
        shell-pop-full-span       t)
  :general
  (tyrant-def "'" '(shell-pop :which-key "open shell")))

(use-package terminal-here
  :ensure t
  :init
  (setq terminal-here-terminal-command '("open" "-a" "iterm.app" "."))
  :general
  (tyrant-def
    "\""   'terminal-here-launch
    "p \"" 'terminal-here-project-launch))


(provide 'editor-shell)
;;; editor-shell.el ends here
