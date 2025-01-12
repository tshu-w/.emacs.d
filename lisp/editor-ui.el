;;; editor-ui.el --- -*- lexical-binding: t; -*-

;; Copyright (C) 2020-2025  Tianshu Wang

;; Author: Tianshu Wang <wang@tianshu.me>

;;; Commentary:

;;; Code:

(defvar after-load-theme-hook nil
  "Hook run after a color theme is loaded using `load-theme'.")

(defun load-theme@run-hooks (&rest _)
  "Run `after-load-theme-hook'."
  (run-hooks 'after-load-theme-hook))
(advice-add 'load-theme :after #'load-theme@run-hooks)

(defun load-theme@theme-dont-propagate (&rest _)
  "Discard all themes before loading new."
  (mapc #'disable-theme custom-enabled-themes))
(advice-add #'load-theme :before #'load-theme@theme-dont-propagate)

(add-hook 'after-load-theme-hook
          (defun bolder-faces ()
            (set-face-attribute 'font-lock-function-name-face nil :weight 'semi-bold)
            (set-face-attribute 'font-lock-keyword-face nil :weight 'semi-bold)))

(add-hook 'after-load-theme-hook
          (defun unscale-outlines ()
            (with-eval-after-load 'org
              (dolist (outline-number (number-sequence 1 8))
                (let ((outline (intern (format "outline-%d" outline-number))))
                  (set-face-attribute outline nil :height 1.0))))))

(add-hook 'after-load-theme-hook
          (defun customize-tab-bar ()
            "Customize tab-bar faces."
            (set-face-attribute 'tab-bar nil
                                :foreground 'unspecified
                                :background 'unspecified
                                :box `(:line-width (-1 . 4) :color ,(face-background 'default))
                                :inherit 'unspecified)
            (set-face-attribute 'tab-bar-tab nil
                                :weight 'bold
                                :box 'unspecified
                                :foreground 'unspecified
                                :background 'unspecified
                                :inherit 'unspecified)
            (set-face-attribute 'tab-bar-tab-inactive nil
                                :box 'unspecified
                                :foreground 'unspecified
                                :background 'unspecified
                                :inherit 'unspecified)))

(use-package doom-themes
  :straight t
  :defer t
  :custom-face
  ;; fix doom-theme diff-hl face
  (diff-hl-change ((t (:background unspecified))))
  (diff-hl-delete ((t (:background unspecified))))
  (diff-hl-insert ((t (:background unspecified)))))

(use-package ef-themes
  :straight t
  :defer t
  :config
  (dotimes (n 5)
    (set (intern (format "ef-themes-height-%s" n)) 1.0)))

(use-package flucui-themes :straight t :defer t)

(use-package humanoid-themes
  :straight t
  :defer t
  :config
  (setq humanoid-themes-org-height nil
        humanoid-themes-org-bold nil
        humanoid-themes-org-priority-bold nil))

(use-package kaolin-themes
  :straight t
  :defer t
  :config
  (setq kaolin-themes-org-scale-headings nil
        kaolin-themes-modeline-border nil))

(use-package lab-themes
  :straight t
  :defer t
  :custom-face
  (font-lock-keyword-face ((t (:slant normal))))
  (font-lock-constant-face ((t (:slant normal)))))

(use-package modus-themes
  :straight t
  :init
  (setq modus-themes-common-palette-overrides
        '((fringe unspecified))))

(use-package spacemacs-common
  :straight spacemacs-theme
  :defer t
  :init
  (setq spacemacs-theme-org-height nil
        spacemacs-theme-comment-bg nil))

(use-package solo-jazz-theme :straight t :defer t)

(defvar light-themes '(doom-acario-light
                       ;; doom-ayu-light
                       doom-earl-grey
                       doom-feather-light
                       doom-flatwhite
                       doom-gruvbox-light
                       doom-homage-white
                       doom-nord-light
                       doom-one-light
                       doom-opera-light
                       ;; doom-plain
                       doom-solarized-light
                       doom-tomorrow-day
                       ef-arbutus
                       ef-cyprus
                       ef-day
                       ef-eagle
                       ef-elea-light
                       ;; ef-deuteranopia-light
                       ef-duo-light
                       ef-frost
                       ef-kassio
                       ef-light
                       ef-maris-light
                       ef-melissa-light
                       ef-reverie
                       ef-spring
                       ef-summer
                       ef-trio-light
                       ;; ef-tritanopia-light
                       flucui-light
                       ;; humanoid-light
                       kaolin-breeze
                       kaolin-light
                       kaolin-mono-light
                       kaolin-valley-light
                       lab-light
                       spacemacs-light
                       solo-jazz
                       modus-operandi)
  "Light themes to switch.")

(defvar dark-themes '(;; doom-1337
                      ;; doom-acario-dark
                      ;; doom-ayu-dark
                      ;; doom-ayu-mirage
                      ;; doom-badger
                      doom-challenger-deep
                      doom-city-lights
                      doom-dark+
                      ;; doom-dracula
                      ;; doom-ephemeral
                      ;; doom-fairy-floss
                      doom-feather-dark
                      doom-gruvbox
                      ;; doom-henna
                      ;; doom-homage-black
                      doom-horizon
                      doom-Iosvkem
                      ;; doom-ir-black
                      doom-lantern
                      ;; doom-laserwave
                      ;; doom-manegarm
                      doom-material
                      doom-material-dark
                      ;; doom-meltbus
                      ;; doom-miramare
                      ;; doom-molokai
                      ;; doom-monokai-classic
                      ;; doom-monokai-machine
                      ;; doom-monokai-octagon
                      doom-monokai-pro
                      ;; doom-monokai-ristretto
                      ;; doom-monokai-spectrum
                      doom-moonlight
                      doom-nord
                      ;; doom-nord-aurora
                      ;; doom-nova
                      doom-oceanic-next
                      ;; doom-old-hope
                      doom-one
                      doom-opera
                      ;; doom-outrun-electric
                      doom-palenight
                      ;; doom-pine
                      ;; doom-plain-dark
                      doom-peacock
                      ;; doom-plain-dark
                      doom-rouge
                      ;; doom-shades-of-purple
                      doom-snazzy
                      doom-solarized-dark
                      doom-solarized-dark-high-contrast
                      doom-sourcerer
                      doom-spacegrey
                      doom-tokyo-night
                      doom-tomorrow-night
                      doom-vibrant
                      doom-wilmersdorf
                      doom-xcode
                      doom-zenburn
                      ef-autumn
                      ef-bio
                      ef-cherie
                      ef-dark
                      ;; ef-deuteranopia-dark
                      ef-dream
                      ef-duo-dark
                      ef-elea-dark
                      ef-maris-dark
                      ;; ef-melissa-dark
                      ef-night
                      ef-owl
                      ef-rosa
                      ef-symbiosis
                      ef-trio-dark
                      ;; ef-tritanopia-dark
                      ef-winter
                      ;; flucui-dark
                      ;; humanoid-dark
                      ;; kaolin-blossom
                      kaolin-bubblegum
                      kaolin-galaxy
                      kaolin-ocean
                      kaolin-temple
                      kaolin-valley-dark
                      ;; lab-dark
                      spacemacs-dark
                      modus-vivendi)
  "Dark themes to switch.")

(if (boundp 'ns-system-appearance-change-functions)
    (add-hook 'ns-system-appearance-change-functions
              (defun load-theme-randomly (appearance)
                (let* ((day (string-to-number
                             (format-time-string "%j" (current-time))))
                       (themes (pcase appearance
                                 ('light light-themes)
                                 ('dark dark-themes)))
                       (themes-length (length themes))
                       (theme (nth (% day themes-length) themes)))
                  (load-theme theme t)))))

(use-package doom-modeline
  :straight t
  :hook (after-init . doom-modeline-mode)
  :config
  (setq inhibit-compacting-font-caches t

        doom-modeline-height 0
        doom-modeline-buffer-file-name-style 'auto
        doom-modeline-project-detection 'project

        doom-modeline-gnus nil
        doom-modeline-icon nil
        doom-modeline-irc nil
        doom-modeline-persp-name nil))

(use-package popper
  :straight t
  :hook ((after-init . popper-mode)
         (after-init . popper-echo-mode))
  :config
  (setq popper-display-control nil
        popper-group-function #'popper-group-by-directory
        popper-reference-buffers
        '("\\*Messages\\*"
          "Output\\*$"
          "\\*Async Shell Command\\*"
          "\\*eldoc\\*"
          "^\\*EGLOT"
          help-mode
          helpful-mode
          ("compilation\\*" . hide)
          process-menu-mode
          special-mode
          flymake-diagnostics-buffer-mode))

  (with-eval-after-load 'compile
    (defun show-compile-buffer-on-failure (buffer msg)
      "Show compilation BUFFER if compilation failed."
      (when (and
             (string-match "compilation" (buffer-name buffer))
             (or (string-match "exited abnormally" msg)
                 (not (string-match "finished" msg))
                 (with-current-buffer buffer
                   (search-forward "error" nil t))))
        (display-buffer buffer)))

    (add-hook 'compilation-finish-functions 'show-compile-buffer-on-failure))
  :general
  (tyrant-def
    ";" 'popper-toggle
    ":" 'popper-kill-latest-popup))

(use-package shackle
  :straight t
  :hook (after-init . shackle-mode)
  :config
  (setq shackle-default-size 0.4
        shackle-rules
        `((help-mode                       :select t :align right :size ,fill-column)
          (helpful-mode                    :select t :align right :size ,fill-column)
          ("*Messages*"                    :select t :align t)
          ("*eldoc*"                       :align t)
          (special-mode                    :align t)
          (process-menu-mode               :align t)
          (compilation-mode                :align t)
          ("compilation\\*"                :align t :regexp t)
          (flymake-diagnostics-buffer-mode :align t)
          ("*Shell Command Output*"        :align t)
          ("*Async Shell Command*"         :align t)
          ("\\*EGLOT.*"                    :select t :align right :size ,fill-column :regexp t))))

(use-package visual-fill-column
  :straight t
  :hook (after-init . global-visual-fill-column-mode)
  :init
  (setq visual-fill-column-center-text t
        visual-fill-column-enable-sensible-window-split t
        visual-fill-column-width 128
        visual-fill-column-major-modes '(text-mode prog-mode conf-mode special-mode Info-mode dired-mode)
        visual-fill-column-major-modes-exceptions '(process-menu-mode proced-mode backtrace-mode))
  :config
  (advice-add 'text-scale-adjust :after #'visual-fill-column-adjust)

  (defcustom visual-fill-column-major-modes '(text-mode)
    "List of major modes in which `visual-fill-column-mode' is activated."
    :group 'visual-fill-column
    :type '(repeat (choice (symbol :tag "Major mode")
                           (string :tag "Regular expression"))))

  (defcustom visual-fill-column-use-derived-modes t
    "Activate `visual-fill-column-mode' in derived modes as well.'."
    :group 'visual-fill-column
    :type '(choice (const :tag "Use derived modes" t)
                   (const :tag "Do not use derived modes" nil)))

  (defcustom visual-fill-column-major-modes-exceptions nil
    "List of major modes in which `visual-fill-column-mode' should not be activated."
    :group 'visual-fill-column
    :type '(repeat (choice (symbol :tag "Major mode exception")
                           (string :tag "Regular expression"))))

  (defun visual-fill-column--match-major-mode (modes &optional derived)
    "Match the current buffer's major mode against MODES.
MODES a list of mode names (symbols) or regular expressions.
Return t if the current major mode matches one of the elements of
MODES, nil otherwise.  Comparison is done with `eq` (for symbols
in MODES) or with `string-match-p' (for strings in MODES).  That
is, if the major mode is e.g., `emacs-lisp-mode', it will not
match the symbol `lisp-mode', but it will match the string
\"lisp-mode\".

If DERIVED is non-nil, also return t if the current buffer's
major mode is a derived mode of one of the major mode symbols in
MODES."
    (catch 'match
      (dolist (elem modes)
        (if (cond ((symbolp elem)
                   (or (eq elem major-mode)
                       (and derived (derived-mode-p elem))))
                  ((string-match-p elem (symbol-name major-mode))))
            (throw 'match t)))))

  (defun turn-on-visual-fill-column-mode@override ()
    "Turn on `visual-fill-column-mode'."
    (unless (visual-fill-column--match-major-mode visual-fill-column-major-modes-exceptions)
      (if (visual-fill-column--match-major-mode
           visual-fill-column-major-modes visual-fill-column-use-derived-modes)
          (visual-fill-column-mode 1))))

  (advice-add 'turn-on-visual-fill-column-mode :override #'turn-on-visual-fill-column-mode@override)
  :general
  (tyrant-def
    "wc" 'visual-fill-column-mode
    "wC" 'global-visual-fill-column-mode))

(use-package hl-todo
  :straight t
  :hook (after-init . global-hl-todo-mode))

(use-package highlight-parentheses
  :straight t
  :hook (prog-mode . highlight-parentheses-mode)
  :config
  (setq highlight-parentheses-colors '("Springgreen3"
                                       "IndianRed1"
                                       "IndianRed3"
                                       "IndianRed4"))
  (set-face-attribute 'highlight-parentheses-highlight nil :weight 'ultra-bold))

(use-package rainbow-delimiters
  :straight t
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package xterm-color
  :straight t
  :defer t
  :init
  (setq compilation-environment '("TERM=xterm-256color"))

  (defun compilation-filter@around (fn proc string)
    (funcall fn proc (xterm-color-filter string)))
  (advice-add 'compilation-filter :around #'compilation-filter@around))


(provide 'editor-ui)
;;; editor-ui.el ends here
