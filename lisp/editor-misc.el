;;; editor-misc.el --- -*- lexical-binding: t; -*-

;; Copyright (C) 2020  Tianshu Wang

;; Author: Tianshu Wang <volekingsg@gmail.com>

;;; Commentary:

;;; Code:

(use-package aggressive-indent
  :ensure t
  :hook (emacs-lisp-mode . aggressive-indent-mode)
  :config
  (add-hook 'diff-auto-refine-mode-hook (lambda () (aggressive-indent-mode -1)))
  :general
  (tyrant-def "tA" 'aggressive-indent-mode))

(use-package alert
  :ensure t
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

(use-package calibredb
  :ensure t
  :config
  (setq calibredb-root-dir "~/Documents/Calibre"
        calibredb-db-dir (expand-file-name "metadata.db" calibredb-root-dir)
        calibredb-library-alist '(("~/Documents/Calibre")))
  (evil-set-initial-state 'calibredb-search-mode 'motion)
  (general-def 'motion calibredb-search-mode-map "/" 'calibredb-search-live-filter)
  :general
  (tyrant-def "aC" 'calibredb))

(use-package cal-china-x
  :ensure t
  :after calendar
  :config
  (setq calendar-mark-holidays-flag nil
        cal-china-x-important-holidays cal-china-x-chinese-holidays
        cal-china-x-general-holidays '((holiday-lunar 1 15 "元宵节")
                                       (holiday-lunar 7 7 "七夕节")
                                       (holiday-fixed 3 8 "妇女节")
                                       (holiday-fixed 3 12 "植树节")
                                       (holiday-fixed 5 4 "青年节")
                                       (holiday-fixed 6 1 "儿童节")
                                       (holiday-fixed 9 10 "教师节"))
        holiday-other-holidays '((holiday-fixed 2 14 "情人节")
                                 (holiday-fixed 4 1 "愚人节")
                                 (holiday-fixed 12 25 "圣诞节")
                                 (holiday-float 5 0 2 "母亲节")
                                 (holiday-float 6 0 3 "父亲节")
                                 (holiday-float 11 4 4 "感恩节"))
        calendar-holidays (append cal-china-x-important-holidays
                                  cal-china-x-general-holidays
                                  holiday-other-holidays))

  (cal-china-x-setup))

(use-package dash-at-point
  :if (memq window-system '(mac ns))
  :ensure t
  :general
  (tyrant-def
    "d" '(:ignore t :which-key "docs")
    "dd" 'dash-at-point
    "dD" 'dash-at-point-with-docset))

(use-package direnv
  :ensure t
  :hook (after-init . direnv-mode))

(use-package dumb-jump
  :ensure t
  :defer t
  :init
  (add-hook 'xref-backend-functions #'dumb-jump-xref-activate)
  (setq dumb-jump-selector 'ivy))

(use-package editorconfig
  :disabled t
  :ensure t
  :init (editorconfig-mode))

(use-package helpful
  :ensure t
  :config
  (defun helpful-reuse-window (buf)
    (if-let ((window (display-buffer-reuse-mode-window buf '((mode . helpful-mode)))))
        (select-window window)
      (pop-to-buffer buf)))

  (setq helpful-max-buffers 3
        helpful-switch-buffer-function #'helpful-reuse-window)
  :general
  (tyrant-def
    "hk" 'helpful-key
    "hf" 'helpful-callable
    "hv" 'helpful-variable))

(use-package link-hint
  :ensure t
  :general
  (general-def
    :keymaps '(compilation-mode-map
               custom-mode-map
               eww-link-keymap
               eww-mode-map
               help-mode-map
               helpful-mode-map
               Info-mode-map
               xref--xref-buffer-mode-map
               woman-mode-map)
    :states  'normal
    "o"      'link-hint-open-link)

  (tyrant-def
    "xo" 'link-hint-open-link
    "xl" 'link-hint-open-multiple-links
    "xy" 'link-hint-copy-link))

(use-package nov
  :ensure t
  :commands (nov-org-link-follow nov-org-link-store)
  :mode ("\\.epub\\'" . nov-mode)
  :init
  (with-eval-after-load 'org
    (org-link-set-parameters "nov"
                             :follow 'nov-org-link-follow
                             :store 'nov-org-link-store)))

(use-package pandoc-mode
  :ensure t
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
  :ensure t
  :hook (org-mode . pangu-spacing-mode)
  :config
  (setq pangu-spacing-real-insert-separtor t))

(use-package reveal-in-osx-finder
  :if (memq window-system '(mac ns))
  :ensure t
  :general (tyrant-def "bf" 'reveal-in-osx-finder))

(use-package rime
  :ensure t
  :defer t
  :custom-face (rime-preedit-face ((t nil)))
  :hook ((kill-emacs . (lambda ()
                         (when (fboundp 'rime-lib-sync-user-data)
                           (ignore-errors (rime-sync))))))
  :config
  (setq default-input-method "rime"
        rime-librime-root "~/.emacs.d/etc/librime/dist"
        rime-user-data-dir "~/.emacs.d/etc/rime/"
        rime-show-candidate 'posframe
        rime-show-preedit 'inline
        rime-posframe-properties (list :internal-border-width 2))

  (general-def rime-mode-map
    "M-j"   'rime-force-enable
    "C-`"   'rime-send-keybinding
    "C-~"   'rime-send-keybinding
    "C-S-`" 'rime-send-keybinding))

(use-package sis
  :ensure t
  :hook ((after-init . sis-global-respect-mode)
         (text-mode . sis-context-mode)
         (text-mode . sis-inline-mode))
  :config
  (sis-ism-lazyman-config nil "rime" 'native)

  (setq sis-prefix-override-keys '("C-c" "C-x" "C-h" "M-SPC"))

  (setq-default sis-inline-tighten-head-rule 0
                sis-inline-tighten-tail-rule 1))

(use-package winum
  :ensure t
  :hook (after-init . winum-mode)
  :config
  (setq winum-auto-assign-0-to-minibuffer nil
        winum-auto-setup-mode-line t)

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
    "`" '(winum-select-window-by-number :which-key "select window by number")
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
  :ensure t
  :hook (prog-mode . wakatime-mode)
  :config
  (setq wakatime-cli-path (executable-find "wakatime")
        wakatime-api-key "3fd63845-ecde-47ea-bd1a-7042221d1046")

  (defun wakatime-dashboard ()
    (interactive)
    (browse-url "https://wakatime.com/dashboard")))


(provide 'editor-misc)
;;; editor-misc.el ends here
