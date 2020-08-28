;;; editor-misc.el --- -*- lexical-binding: t; -*-

;; Copyright (C) 2020  Tianshu Wang

;; Author: Tianshu Wang <volekingsg@gmail.com>

;;; Commentary:

;;; Code:

(use-package ace-link
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
    "o"      'ace-link))

(use-package aggressive-indent
  :ensure t
  :hook (emacs-lisp-mode . aggressive-indent-mode)
  :config
  (add-hook 'diff-auto-refine-mode-hook (lambda () (aggressive-indent-mode -1)))
  :general
  (tyrant-def "tA" 'aggressive-indent-mode))

(use-package avy
  :ensure t
  :config
  (setq avy-all-windows 'all-frames
        avy-background t)
  :general
  (tyrant-def
    "jb" 'avy-pop-mark
    "jj" '(avy-goto-char-timer :which-key "avy timer")
    "jl" '(avy-goto-line :which-key "avy line")
    "jw" '(avy-goto-word-or-subword-1 :which-key "avy word")))

(use-package calibredb
  :ensure t
  :config
  (setq calibredb-root-dir "~/Documents/Calibre"
        calibredb-db-dir (expand-file-name "metadata.db" calibredb-root-dir)
        calibredb-library-alist '(("~/Documents/Calibre")))
  (evil-set-initial-state 'calibredb-search-mode 'motion)
  (general-def 'motion calibredb-search-mode-map "/" 'calibredb-search-live-filter)
  :general
  (tyrant-def
    "aC" 'calibredb
    "sc" 'calibredb-find-counsel))

(use-package cal-china-x
  :ensure t
  :after calendar
  :config
  (setq calendar-mark-holidays-flag nil)
  (setq cal-china-x-important-holidays cal-china-x-chinese-holidays)
  (setq cal-china-x-general-holidays
        '((holiday-lunar 1 15 "元宵节")
          (holiday-lunar 7 7 "七夕节")
          (holiday-fixed 3 8 "妇女节")
          (holiday-fixed 3 12 "植树节")
          (holiday-fixed 5 4 "青年节")
          (holiday-fixed 6 1 "儿童节")
          (holiday-fixed 9 10 "教师节")))
  (setq holiday-other-holidays
        '((holiday-fixed 2 14 "情人节")
          (holiday-fixed 4 1 "愚人节")
          (holiday-fixed 12 25 "圣诞节")
          (holiday-float 5 0 2 "母亲节")
          (holiday-float 6 0 3 "父亲节")
          (holiday-float 11 4 4 "感恩节")))
  (setq calendar-holidays
        (append cal-china-x-important-holidays
                cal-china-x-general-holidays
                holiday-other-holidays))

  (cal-china-x-setup))


(use-package counsel-dash
  :ensure t
  :disabled t
  :init
  (add-hook 'python-mode-hook (lambda () (setq-local dash-docs-docsets '("Python 3" "SciPy" "NumPy" "Pandas" "Matplotlib"))))
  (add-hook 'LaTeX-mode-hook  (lambda () (setq-local dash-docs-docsets '("LaTeX"))))
  (add-hook 'sh-mode-hook     (lambda () (setq-local dash-docs-docsets '("Bash"))))
  (add-hook 'c++-mode-hook    (lambda () (setq-local dash-docs-docsets '("C++"))))
  :config
  (setq dash-docs-browser-func 'eww)
  (add-to-list 'ivy-re-builders-alist '(counsel-dash-at-point . ivy--regex-ignore-order))
  (add-to-list 'ivy-re-builders-alist '(counsel-dash . ivy--regex-ignore-order))
  :general
  (tyrant-def
    "d"  '(:ignore t :which-key "docs")
    "da" 'dash-docs-activate-docset
    "dh" 'counsel-dash-at-point
    "dH" 'counsel-dash))

(use-package devdocs
  :ensure t
  :disabled t
  :general (tyrant-def "db" 'devdocs-search))

(use-package direnv
  :ensure t
  :hook (after-init . direnv-mode))

(use-package dotenv-mode :ensure t :defer t)

(use-package dumb-jump
  :ensure t
  :init
  (setq dumb-jump-selector 'ivy)
  (add-to-list 'default-jump-handlers 'dumb-jump-go 'append)
  :general (tyrant-def "jq" 'dumb-jump-quick-look))

(use-package editorconfig
  :disabled t
  :ensure t
  :init (editorconfig-mode))

(use-package google-translate
  :disabled t
  :ensure t
  :init
  (setq google-translate-enable-ido-completion t
        google-translate-show-phonetic t
        google-translate-default-source-language "en"
        google-translate-default-target-language "zh-CN")
  :config
  (defun set-google-translate-languages (source target)
    "Set source language for google translate.
     For instance pass En as source for English."
    (interactive
     "sEnter source language (ie. en): \nsEnter target language (ie. en): "
     source target)
    (message
     (format "Set google translate source language to %s and target to %s"
             source target))
    (setq google-translate-default-source-language (downcase source))
    (setq google-translate-default-target-language (downcase target)))
  :general
  (tyrant-def
    "xg"  '(:ignore t :which-key "google-translate")
    "xgl" 'set-google-translate-languages
    "xgQ" 'google-translate-query-translate-reverse
    "xgq" 'google-translate-query-translate
    "xgT" 'google-translate-at-point-reverse
    "xgt" 'google-translate-at-point))

(use-package helpful
  :ensure t
  :config
  (defun helpful-reuse-window (buf)
    (if-let ((window (display-buffer-reuse-mode-window buf '((mode . helpful-mode)))))
        (select-window window)
      (pop-to-buffer buf)))

  (setq helpful-switch-buffer-function #'helpful-reuse-window)
  :general
  (tyrant-def
    "hk" 'helpful-key
    "hf" 'helpful-callable
    "hv" 'helpful-variable))

(use-package insert-translated-name
  :quelpa (insert-translated-name :fetcher github :repo "manateelazycat/insert-translated-name")
  :general ("H-t" 'insert-translated-name-insert))

(use-package link-hint
  :ensure t
  :general
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
  :config
  (defun run-pandoc ()
    "Start pandoc for the buffer and open the menu"
    (interactive)
    ;; only run pandoc-mode if not active, as it resets pandoc--local-settings
    (if (not (bound-and-true-p pandoc-mode)) (pandoc-mode))
    (pandoc-main-hydra/body))
  :general
  (tyrant-def "a C-P" 'run-pandoc))

(use-package pangu-spacing
  :ensure t
  :hook (org-mode . pangu-spacing-mode)
  :config
  (setq pangu-spacing-real-insert-separtor t))

(use-package pdf-tools
  :ensure t
  :defer t
  :config
  (pdf-tools-install :no-query)
  (setq pdf-view-use-scaling t)

  (despot-def pdf-view-mode-map
    ;; Annotations
    "a"              '(:ignore t :which-key "annotations")
    "aD"              'pdf-annot-delete
    "at"              'pdf-annot-attachment-dired
    "ah"              'pdf-annot-add-highlight-markup-annotation
    "al"              'pdf-annot-list-annotations
    "am"              'pdf-annot-add-markup-annotation
    "ao"              'pdf-annot-add-strikeout-markup-annotation
    "as"              'pdf-annot-add-squiggly-markup-annotation
    "at"              'pdf-annot-add-text-annotation
    "au"              'pdf-annot-add-underline-markup-annotation
    "f"              '(:ignore t :which-key "fit")
    ;; Fit image to window
    "fw"              'pdf-view-fit-width-to-window
    "fh"              'pdf-view-fit-height-to-window
    "fp"              'pdf-view-fit-page-to-window
    "s"              '(:ignore t :which-key "slice/search")
    ;; Slicing image
    "sm"              'pdf-view-set-slice-using-mouse
    "sb"              'pdf-view-set-slice-froquelpam-bounding-box
    "sr"              'pdf-view-reset-slice
    ;; Other
    "ss"              'pdf-occur
    "p"               'pdf-misc-print-document
    "O"               'pdf-outline
    "n"               'pdf-view-midnight-minor-mode)

  (general-def 'visual pdf-view-mode-map
    "y"        'pdf-view-kill-ring-save))

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
         (text-mode . sis-follow-context-mode)
         (text-mode . sis-inline-mode))
  :config
  (sis-ism-lazyman-config nil "rime" 'native)

  (setq sis-prefix-override-keys '("C-c" "C-x" "C-h" "M-SPC"))

  (setq-default sis-inline-tighten-head-rule 0
                sis-inline-tighten-tail-rule 1))

(use-package string-inflection
  :ensure t
  :general
  (tyrant-def
    "xi"  '(:ignore t :which-key "inflection")
    "xic" 'string-inflection-lower-camelcase
    "xiC" 'string-inflection-camelcase
    "xi-" 'string-inflection-kebab-case
    "xik" 'string-inflection-kebab-case
    "xi_" 'string-inflection-underscore
    "xiu" 'string-inflection-underscore
    "xiU" 'string-inflection-upcase))

(use-package xwidget
  :if (featurep 'xwidget-internal)
  :defer t
  :init
  ;; (setq browse-url-browser-function 'xwidget-webkit-browse-url)
  :config
  (defun xwidget-webkit ()
    "Switch to xwidget webkit buffer or open https://google.com."
    (interactive)
    (if (buffer-live-p xwidget-webkit-last-session-buffer)
        (switch-to-buffer xwidget-webkit-last-session-buffer)
      (xwidget-webkit-browse-url "https://google.com")))

  (defun xwidget-webkit-browse-url@after (url &optional new-session)
    "Switch to xwidget webkit buffer after open URL."
    (ignore url new-session)
    (switch-to-buffer xwidget-webkit-last-session-buffer))
  (advice-add 'xwidget-webkit-browse-url :after #'xwidget-webkit-browse-url@after)

  (defun xwidget-webkit-browse-with-external-browser (&optional url)
    "Browse the current URL with an external browser.
The browser to used is specified by the
`browse-url-secondary-browser-function' variable."
    (interactive)
    (funcall browse-url-secondary-browser-function
             (or url (xwidget-webkit-current-url))))

  (with-eval-after-load 'writeroom-mode
    (add-to-list 'writeroom-major-modes-exceptions 'xwidget-webkit-mode))

  (general-def 'normal xwidget-webkit-mode-map
    "&" 'xwidget-webkit-browse-with-external-browser)
  :general
  (tyrant-def "ab" 'xwidget-webkit))

(use-package xwwp-full
  :quelpa (xwwp :fetcher github :repo "BlueFlo0d/xwwp" :files ("*.el" "*.css" "*.js"))
  :defer t
  :init
  (with-eval-after-load 'xwidget (require 'xwwp-full))
  :config
  (setq xwwp-follow-link-completion-system 'ivy
        xwwp-ace-label-style '(("z-index" . "2147483647")
                               ("color" . "red")
                               ("opacity" . "0.7")
                               ("background-color" . "yellow")
                               ("font-family" . "monospace")
                               ("font-size" . "1em")))

  (general-def 'normal xwidget-webkit-mode-map
    "H-c" 'xwidget-webkit-copy-selection-as-kill
    "o"   'xwwp-ace-toggle
    "O"   'xwwp-follow-link)
  :general
  (tyrant-def "aB" 'xwwp))

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
    (browse-url "https://wakatime.com/dashboard"))
  :general
  (tyrant-def "aw" 'wakatime-dashboard))


(provide 'editor-misc)
;;; editor-misc.el ends here
