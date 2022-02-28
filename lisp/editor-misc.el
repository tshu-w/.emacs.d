;;; editor-misc.el --- -*- lexical-binding: t; -*-

;; Copyright (C) 2020-2022  Tianshu Wang

;; Author: Tianshu Wang <wang@tianshu.me>

;;; Commentary:

;;; Code:

(use-package aggressive-indent
  :straight t
  :hook (emacs-lisp-mode . aggressive-indent-mode)
  :init
  (setq aggressive-indent-mode-map nil)
  :config
  (add-hook 'diff-auto-refine-mode-hook (lambda () (aggressive-indent-mode -1))))

(use-package alert
  :straight t
  :defer t
  :config
  (when (memq window-system '(mac ns))
    (defun alert-notifier-notify (info)
      (if alert-notifier-command
          (let ((args
                 (list "-group" "Emacs"
                       "-sender"  "org.gnu.Emacs"
                       "-activate" "org.gnu.Emacs"
                       "-title"   (alert-encode-string (plist-get info :title))
                       "-message" (alert-encode-string (plist-get info :message)))))
            (apply #'call-process alert-notifier-command nil nil nil args)))
      (alert-message-notify info))

    (setq alert-default-style 'notifier)))

(use-package dumb-jump
  :straight t
  :defer t
  :init
  (add-hook 'xref-backend-functions #'dumb-jump-xref-activate)
  (setq dumb-jump-selector 'completing-read))

(use-package editorconfig
  :disabled t
  :straight t
  :init (editorconfig-mode))

(use-package elfeed
  :straight t
  :init
  (with-eval-after-load 'writeroom-mode
    (add-to-list 'writeroom-major-modes 'elfeed-search-mode)
    (add-to-list 'writeroom-major-modes 'elfeed-show-mode))
  :general
  (tyrant-def "af" 'elfeed))

(use-package elfeed-org
  :straight t
  :after elfeed
  :config
  (elfeed-org)
  (setq rmh-elfeed-org-files `(,(no-littering-expand-etc-file-name "elfeed/elfeed.org"))))

(use-package esup :straight t :defer t)

(use-package fcitx
  :straight t
  :hook (after-init . fcitx-setup)
  :config
  (defun fcitx-setup ()
    "Setup for `fcitx'."
    (interactive)
    (when (fcitx-check-status)
      (fcitx-read-funcs-turn-on)
      (fcitx-evil-turn-on)
      (fcitx-aggressive-minibuffer-turn-on))))

(use-package gcmh
  :straight t
  :hook (after-init . gcmh-mode)
  :config
  (setq gcmh-high-cons-threshold #x6400000))

(use-package helpful
  :straight t
  :config
  (defun helpful-reuse-window (buffer-or-name)
    "Switch to helpful BUFFER-OR-NAME.

The logic is simple, if we are currently in the helpful buffer,
reuse it's window, otherwise create new one."
    (if (eq major-mode 'helpful-mode)
        (pop-to-buffer-same-window buffer-or-name)
      (pop-to-buffer buffer-or-name)))

  (setq helpful-max-buffers 3
        helpful-switch-buffer-function #'helpful-reuse-window)

  (with-eval-after-load 'ibuffer
    (add-to-list 'ibuffer-help-buffer-modes 'helpful-mode))
  :general
  ([remap describe-variable] 'helpful-variable
   [remap describe-function] 'helpful-callable
   [remap describe-symbol]   'helpful-symbol
   [remap describe-key]      'helpful-key))

(use-package link-hint
  :straight t
  :config
  (setq link-hint-restore nil)
  :general
  (general-def
    :keymaps '(compilation-mode-map
               custom-mode-map
               eww-link-keymap
               eww-mode-map
               help-mode-map
               helpful-mode-map
               Info-mode-map
               mu4e-view-mode-map
               xref--xref-buffer-mode-map
               woman-mode-map)
    :states  'normal
    "o"      'link-hint-open-link)

  (tyrant-def
    "jl" 'link-hint-open-link
    "jL" 'link-hint-open-multiple-links
    "jy" 'link-hint-copy-link))

(use-package nov
  :straight t
  :commands (nov-org-link-follow nov-org-link-store)
  :mode ("\\.epub\\'" . nov-mode)
  :init
  (with-eval-after-load 'org
    (org-link-set-parameters "nov"
                             :follow 'nov-org-link-follow
                             :store 'nov-org-link-store)))

(use-package pandoc-mode
  :straight t
  :hook (pandoc-mode . pandoc-load-default-settings)
  :commands pandoc
  :config
  (defun pandoc ()
    "Start pandoc for the buffer and open the menu"
    (interactive)
    ;; only run pandoc-mode if not active, as it resets pandoc--local-settings
    (if (not (bound-and-true-p pandoc-mode)) (pandoc-mode))
    (pandoc-main-hydra/body)))

(use-package pangu-spacing
  :straight t
  :hook (org-mode . pangu-spacing-mode)
  :config
  (setq pangu-spacing-real-insert-separtor t))

(use-package popper
  :straight t
  :hook ((after-init . popper-mode)
         (after-init . popper-echo-mode))
  :config
  (setq popper-display-control nil
        popper-reference-buffers
        '("\\*Messages\\*"
          "Output\\*$"
          "\\*Async Shell Command\\*"
          help-mode
          helpful-mode
          compilation-mode
          process-menu-mode
          special-mode
          "\\*Flycheck errors\\*"))
  :general
  (tyrant-def ";" 'popper-toggle-latest))

(use-package reveal-in-osx-finder
  :if (memq window-system '(mac ns))
  :straight t
  :general (tyrant-def "bf" 'reveal-in-osx-finder))

(use-package reformatter :straight t :defer t)

(use-package rime
  :straight t
  :defer t
  :custom-face (rime-preedit-face ((t nil)))
  :init
  (setq default-input-method "rime")
  :config
  (setq rime-librime-root (no-littering-expand-etc-file-name "librime/dist")
        rime-user-data-dir (no-littering-expand-etc-file-name "rime/")
        rime-show-candidate 'posframe
        rime-show-preedit 'inline
        rime-posframe-properties (list :internal-border-width 2))

  (add-to-list 'rime-translate-keybindings "C-`")

  (add-hook 'kill-emacs-hook (lambda ()
                               (when (fboundp 'rime-lib-sync-user-data)
                                 (ignore-errors (rime-sync)))))

  (general-def rime-mode-map "C-`" 'rime-send-keybinding))

(use-package rime-regexp
  :straight (:host github :repo "colawithsauce/rime-regexp.el")
  :commands rime-regexp-build-regexp-string
  :init
  (with-eval-after-load 'orderless
    (defun orderless-pinyin-regexp (component)
      "Match COMPONENT as a pinyin regexp with `pyim-cregexp-build'."
      (rime-regexp-build-regexp-string (orderless-regexp component)))

    (defun pinyin-if-ampersand (pattern _index _total)
      (when (string-suffix-p "&" pattern)
        `(orderless-pinyin-regexp . ,(substring pattern 0 -1))))

    (add-to-list 'orderless-style-dispatchers 'pinyin-if-ampersand))

  (with-eval-after-load 'avy
    (defun avy--regex-candidates@around (fun regex &optional beg end pred group)
      (let ((regex (rime-regexp-build-regexp-string regex)))
        (funcall fun regex beg end pred group)))
    (advice-add 'avy--regex-candidates :around #'avy--regex-candidates@around))
  :config
  (rime-regexp-load-rime))

(use-package terminal-here
  :straight t
  :config
  (setq terminal-here-mac-terminal-command 'iterm2
        terminal-here-project-root-function (lambda () (project-root (project-current t))))
  :general
  (tyrant-def
    "\""   'terminal-here-launch
    "p \"" 'terminal-here-project-launch))

(use-package undohist
  :straight t
  :hook (after-init . undohist-initialize)
  :config
  (setq undohist-ignored-files '("EDITMSG")))

(use-package winum
  :straight t
  :hook (after-init . winum-mode)
  :config
  (setq winum-auto-assign-0-to-minibuffer t
        winum-auto-setup-mode-line t
        winum-scope 'frame-local)

  (defun move-buffer-to-window (windownum follow-focus-p)
    "Moves a buffer to a window. follow-focus-p controls
whether focus moves to new window (with buffer), or stays on current"
    (interactive)
    (let ((b (current-buffer))
          (w1 (selected-window))
          (w2 (winum-get-window-by-number windownum)))
      (unless (eq w1 w2)
        (set-window-buffer w2 b)
        (switch-to-prev-buffer)
        (unrecord-window-buffer w1 b)))
    (when follow-focus-p (select-window (winum-get-window-by-number windownum))))

  (defun swap-buffers-to-window (windownum follow-focus-p)
    "Swaps visible buffers between active window and selected window.
follow-focus-p controls whether focus moves to new window (with buffer), or
stays on current"
    (interactive)
    (let* ((b1 (current-buffer))
           (w1 (selected-window))
           (w2 (winum-get-window-by-number windownum))
           (b2 (window-buffer w2)))
      (unless (eq w1 w2)
        (set-window-buffer w1 b2)
        (set-window-buffer w2 b1)
        (unrecord-window-buffer w1 b1)
        (unrecord-window-buffer w2 b2)))
    (when follow-focus-p (winum-select-window-by-number windownum)))

  (dotimes (i 9)
    (let ((n (+ i 1)))
      (eval `(defun ,(intern (format "buffer-to-window-%s" n)) (&optional arg)
               ,(format "Move buffer to the window with number %i." n)
               (interactive "P")
               (if arg
                   (move-buffer-to-window ,n t)
                 (swap-buffers-to-window ,n t))))))
  :general
  (tyrant-def
    "0" '(winum-select-window-0-or-10   :which-key "select window 0 or 10")
    "1" '(winum-select-window-1         :which-key ("1\.\.9" . "select window 1..9"))
    "2" '(winum-select-window-2         :which-key t)
    "3" '(winum-select-window-3         :which-key t)
    "4" '(winum-select-window-4         :which-key t)
    "5" '(winum-select-window-5         :which-key t)
    "6" '(winum-select-window-6         :which-key t)
    "7" '(winum-select-window-7         :which-key t)
    "8" '(winum-select-window-8         :which-key t)
    "9" '(winum-select-window-9         :which-key t)
    "b1" '(buffer-to-window-1           :which-key ("1\.\.9" . "Move buffer to window 1..9"))
    "b2" '(buffer-to-window-2           :which-key t)
    "b3" '(buffer-to-window-3           :which-key t)
    "b4" '(buffer-to-window-4           :which-key t)
    "b5" '(buffer-to-window-5           :which-key t)
    "b6" '(buffer-to-window-6           :which-key t)
    "b7" '(buffer-to-window-7           :which-key t)
    "b8" '(buffer-to-window-8           :which-key t)
    "b9" '(buffer-to-window-9           :which-key t)))

(use-package wakatime-mode
  :straight t
  :hook (prog-mode . wakatime-mode)
  :config
  (setq wakatime-cli-path "wakatime-cli")

  (defun wakatime-dashboard ()
    (interactive)
    (browse-url "https://wakatime.com/dashboard")))


(provide 'editor-misc)
;;; editor-misc.el ends here
