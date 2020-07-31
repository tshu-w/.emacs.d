;;; editor-shell.el --- -*- lexical-binding: t; -*-

;; Copyright (C) 2020  Tianshu Wang

;; Author: Tianshu Wang <volekingsg@gmail.com>

;;; Commentary:

;;; Code:

(use-package vterm
  :ensure t
  :hook (vterm-mode . (lambda () (setq-local global-hl-line-mode nil)))
  :commands (vterm vterm-other-window)
  :config
  (setq vterm-buffer-name-string "vterm %s"
        vterm-clear-scrollback-when-clearing t
        vterm-kill-buffer-on-exit t)

  (with-eval-after-load 'writeroom-mode
    (add-to-list 'writeroom-major-modes-exceptions 'vterm-mode))

  (with-eval-after-load 'centered-cursor-mode
    (add-hook 'vterm-mode-hook
              '(lambda ()
                 (add-hook 'after-change-major-mode-hook
                           (lambda () (centered-cursor-mode 0))
                           :append
                           :local))))

  (evil-set-initial-state 'vterm-mode 'emacs)
  (general-def 'emacs vterm-mode-map
    "C-y"      'vterm-yank
    "C-c C-d"  'vterm-send-C-d
    "C-c C-z"  'vterm-send-C-z))

(use-package shell-pop
  :ensure t
  :init
  (setq shell-pop-window-position 'bottom
        shell-pop-window-size     50
        shell-pop-term-shell      shell-file-name
        shell-pop-shell-type      '("vterm" "*vterm*"
                                    (lambda nil (vterm)))
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
