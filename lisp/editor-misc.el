;;; editor-misc.el --- -*- lexical-binding: t; -*-

;; Copyright (C) 2020-2023  Tianshu Wang

;; Author: Tianshu Wang <wang@tianshu.me>

;;; Commentary:

;;; Code:

(use-package alert
  :straight t
  :defer t
  :config
  (when (eq system-type 'darwin)
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

(use-package atomic-chrome
  :straight t
  :hook (after-init . atomic-chrome-start-server)
  :config
  (setq atomic-chrome-extension-type-list '(ghost-text)
        atomic-chrome-url-major-mode-alist
        '(("overleaf\\.com" . TeX-tex-mode)
          ("github\\.com" . markdown-mode))))

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

(use-package flyspell-correct
  :straight t
  :general
  (tyrant-def
    "sc" 'flyspell-correct-wrapper
    "ss" 'flyspell-correct-at-point))

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
  ([remap describe-command]  'helpful-command
   [remap describe-function] 'helpful-callable
   [remap describe-key]      'helpful-key
   [remap describe-symbol]   'helpful-symbol
   [remap describe-variable] 'helpful-variable))

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
          "\\*eldoc\\*"
          "^\\*EGLOT"
          help-mode
          helpful-mode
          compilation-mode
          process-menu-mode
          special-mode
          flymake-diagnostics-buffer-mode))
  :general
  (tyrant-def
    ";" 'popper-toggle-latest
    ":" 'popper-kill-latest-popup))

(use-package reveal-in-osx-finder
  :if (eq system-type 'darwin)
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
        rime-emacs-module-header-root (expand-file-name "include" (locate-dominating-file invocation-directory "include"))
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
    (defun avy--regex-candidates@around (fn regex &optional beg end pred group)
      (let ((regex (rime-regexp-build-regexp-string regex)))
        (funcall fn regex beg end pred group)))
    (advice-add 'avy--regex-candidates :around #'avy--regex-candidates@around))
  :config
  (rime-regexp-load-rime))

(use-package sideline
  :straight t
  :init
  (use-package sideline-flymake
  :straight t
  :hook (flymake-mode . sideline-mode)
  :init
  (setq sideline-backends-right '(sideline-flymake))
  (add-hook 'flymake-mode-hook
            (lambda () (remove-hook 'eldoc-documentation-functions 'flymake-eldoc-function t)))))

(use-package terminal-here
  :straight t
  :config
  (setq terminal-here-mac-terminal-command 'iterm2
        terminal-here-project-root-function (lambda () (project-root (project-current t))))
  :general
  (tyrant-def
    "\""   'terminal-here-launch
    "p \"" 'terminal-here-project-launch))

(use-package treesit-auto
  :when (and (fboundp 'treesit-available-p) (treesit-available-p))
  :straight t
  :hook (after-init . global-treesit-auto-mode))

(use-package undohist
  :straight t
  :hook (after-init . undohist-initialize)
  :config
  (setq undohist-ignored-files '("EDITMSG"))

  (defun undohist-recover-safe@around (fn)
    (cl-letf (((symbol-function 'yes-or-no-p) (lambda (p) nil)))
      (funcall fn)))
  (advice-add #'undohist-recover-safe :around #'undohist-recover-safe@around))

(use-package xr :straight t :defer t)

(use-package winum
  :straight t
  :hook (after-init . winum-mode)
  :init
  (with-eval-after-load 'which-key
    (push '((nil . "winum-select-window-[1-9]") . t) which-key-replacement-alist)
    (push '((nil . "buffer-to-window-[1-9]") . t) which-key-replacement-alist))
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
    "0"  '("select window 0 or 10" . winum-select-window-0-or-10)
    "1"  '("select window 1..9" . winum-select-window-1)
    "2"  'winum-select-window-2
    "3"  'winum-select-window-3
    "4"  'winum-select-window-4
    "5"  'winum-select-window-5
    "6"  'winum-select-window-6
    "7"  'winum-select-window-7
    "8"  'winum-select-window-8
    "9"  'winum-select-window-9
    "b1" '("Move buffer to window 1..9" . buffer-to-window-1)
    "b2" 'buffer-to-window-2
    "b3" 'buffer-to-window-3
    "b4" 'buffer-to-window-4
    "b5" 'buffer-to-window-5
    "b6" 'buffer-to-window-6
    "b7" 'buffer-to-window-7
    "b8" 'buffer-to-window-8
    "b9" 'buffer-to-window-9))

;; (use-package wakatime-mode
;;   :straight t
;;   :hook (prog-mode . wakatime-mode)
;;   :config
;;   (setq wakatime-cli-path "wakatime-cli")

;;   (defun wakatime-dashboard ()
;;     (interactive)
;;     (browse-url "https://wakatime.com/dashboard")))


(provide 'editor-misc)
;;; editor-misc.el ends here
