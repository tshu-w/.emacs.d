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

  (add-hook 'eshell-mode-hook (lambda () (setq-local global-hl-line-mode nil)))
  (add-hook 'eshell-output-filter-functions #'eshell-truncate-buffer)

  (add-hook 'eshell-mode-hook
            (lambda ()
              (company-mode)
              (setq-local company-frontends '(company-preview-frontend))))

  ;; quick commands
  (defalias 'eshell/ec    'find-file-other-window)
  (defalias 'eshell/dired 'dired)
  (defalias 'eshell/magit 'magit-status)

  (use-package em-term
    :defer t
    :config
    (mapc (lambda (x) (add-to-list 'eshell-visual-commands x))
          '("el" "elinks" "htop" "less" "ssh" "tmux" "top")))

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
  (tyrant-def "'" '(shell-pop :which-key "open shell")))
(use-package eshell-prompt-extras
  :ensure t
  :after eshell
  :config
  (setq eshell-highlight-prompt nil
        eshell-prompt-function 'epe-theme-lambda)

  (defun protect-eshell-prompt ()
    "Protect Eshell's prompt like Comint's prompts.

E.g. `evil-change-whole-line' won't wipe the prompt. This
is achieved by adding the relevant text properties."
    (let ((inhibit-field-text-motion t))
      (add-text-properties
       (point-at-bol)
       (point)
       '(rear-nonsticky t
                        inhibit-line-move-field-capture t
                        field output
                        read-only t
                        front-sticky (field inhibit-line-move-field-capture)))))

  (add-hook 'eshell-after-prompt-hook #'protect-eshell-prompt))

(use-package term
  :defer t
  :config
  (add-hook 'term-mode-hook (lambda () (setq-local global-hl-line-mode nil))))

(use-package terminal-here
  :ensure t
  :init
  (setq terminal-here-terminal-command '("open" "-a" "iterm.app" "."))
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
