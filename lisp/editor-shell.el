;;; editor-shell.el --- -*- lexical-binding: t; -*-

;; Copyright (C) 2020  Tianshu Wang

;; Author: Tianshu Wang <volekingsg@gmail.com>

;;; Commentary:

;;; Code:

(use-package comint
  :defer t
  :config
  (setq comint-prompt-read-only t)
  (add-hook 'comint-mode-hook (lambda () (setq-local global-hl-line-mode nil))))

(use-package eshell
  :defer t
  :config
  (setq eshell-hist-ignoredups t
        eshell-plain-echo-behavior t)

  (add-hook 'eshell-mode-hook
            (defun init-eshell-mode ()
              "Stuff to do when enabling `eshell-mode'."
              (setq-local global-hl-line-mode nil)
              (company-mode)
              (company-box-mode -1)))

  (add-hook 'eshell-output-filter-functions #'eshell-truncate-buffer)

  ;; quick commands
  (defalias 'eshell/ec    'find-file-other-window)
  (defalias 'eshell/dired 'dired)
  (defalias 'eshell/magit 'magit-status)

  (use-package em-term
    :config
    (mapc (lambda (x) (add-to-list 'eshell-visual-commands x))
          '("el" "elinks" "htop" "less" "ssh" "tmux" "top")))

  (defun projectile-eshell ()
    "Open a term buffer at projectile project root."
    (interactive)
    (when (get-buffer "*eshell*") (kill-buffer "*eshell*"))
    (let ((default-directory (projectile-project-root)))
      (call-interactively 'eshell)))

  (defun eshell/clear ()
    (interactive)
    (let ((inhibit-read-only t))
      (erase-buffer)
      (eshell-send-input)))

  (defun eshell-yank-last-arg ()
    "Insert the last arg of the previous command."
    (interactive)
    (insert "$_")
    (pcomplete-expand))

  (autoload 'eshell-delchar-or-maybe-eof "em-rebind")

  (general-def 'insert eshell-mode-map
    "C-d"   'eshell-delchar-or-maybe-eof
    "C-l"   'eshell/clear
    "C-r"   'counsel-esh-history
    "M-."   'eshell-yank-last-arg)
  :general
  (tyrant-def
    "'"  'eshell
    "p'" 'projectile-eshell))

(use-package term
  :defer t
  :config
  (add-hook 'term-mode-hook (lambda () (setq-local global-hl-line-mode nil))))

(use-package terminal-here
  :ensure t
  :config
  (setq terminal-here-mac-terminal-command 'iterm2)
  :general
  (tyrant-def
    "\""   'terminal-here-launch
    "p \"" 'terminal-here-project-launch))

(use-package xterm-color
  :ensure t
  :after eshell
  :config
  (add-hook 'eshell-before-prompt-hook
            (lambda () (setq xterm-color-preserve-properties t)))

  (add-to-list 'eshell-preoutput-filter-functions 'xterm-color-filter)
  (setq eshell-output-filter-functions (remove 'eshell-handle-ansi-color eshell-output-filter-functions))
  (setenv "TERM" "xterm-256color"))


(provide 'editor-shell)
;;; editor-shell.el ends here
