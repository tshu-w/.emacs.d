;;; editor-ui.el --- -*- lexical-binding: t; -*-

;; Copyright (C) 2020-2022  Tianshu Wang

;; Author: Tianshu Wang <wang@tianshu.me>

;;; Commentary:

;;; Code:

(defvar after-load-theme-hook nil
  "Hook run after a color theme is loaded using `load-theme'.")

(defun load-theme@after (&rest _)
  "Run `after-load-theme-hook'."
  (run-hooks 'after-load-theme-hook))
(advice-add 'load-theme :after #'load-theme@after)

(add-hook 'after-load-theme-hook
          (defun bolder-faces ()
            (set-face-attribute 'font-lock-function-name-face nil :weight 'semi-bold)
            (set-face-attribute 'font-lock-keyword-face nil :weight 'semi-bold)))

(use-package doom-themes
  :straight t
  :defer t
  :custom-face
  ;; fix doom-theme diff-hl face
  (diff-hl-change ((t (:background nil))))
  (diff-hl-delete ((t (:background nil))))
  (diff-hl-insert ((t (:background nil)))))

(use-package berrys-theme
  :straight t
  :defer t
  :custom-face
  (fringe ((t nil))))

(use-package flucui-themes :straight t :defer t)

(use-package humanoid-themes
  :straight t
  :defer t
  :config
  (setq humanoid-themes-org-height nil
        humanoid-themes-org-bold nil))

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

(use-package spacemacs-common
  :straight spacemacs-theme
  :defer t
  :init
  (setq spacemacs-theme-org-height nil
        spacemacs-theme-comment-bg nil))

(use-package solo-jazz-theme :straight t :defer t)

(defvar light-themes '(doom-acario-light
                       ;; doom-ayu-light
                       doom-flatwhite
                       doom-gruvbox-light
                       doom-homage-white
                       doom-nord-light
                       doom-one-light
                       doom-opera-light
                       ;; doom-plain
                       doom-solarized-light
                       doom-tomorrow-day
                       berrys
                       flucui-light
                       humanoid-light
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
                      ;; doom-ayu-mirage
                      ;; doom-badger
                      doom-challenger-deep
                      doom-city-lights
                      doom-dark+
                      ;; doom-dracula
                      ;; doom-ephemeral
                      ;; doom-fairy-floss
                      doom-gruvbox
                      ;; doom-henna
                      doom-homage-black
                      doom-horizon
                      doom-Iosvkem
                      ;; doom-ir-black
                      ;; doom-laserwave
                      ;; doom-manegarm
                      doom-material
                      doom-material-dark
                      ;; doom-meltbus
                      ;; doom-miramare
                      ;; doom-molokai
                      ;; doom-monokai-classic
                      doom-monokai-pro
                      ;; doom-monokai-machine
                      ;; doom-monokai-octagon
                      ;; doom-monokai-ristretto
                      ;; doom-monokai-spectrum
                      doom-moonlight
                      doom-nord
                      ;; doom-nova
                      doom-oceanic-next
                      ;; doom-old-hope
                      doom-one
                      doom-opera
                      ;; doom-outrun-electric
                      doom-palenight
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
                      ;; flucui-dark
                      humanoid-dark
                      ;; kaolin-blossom
                      kaolin-bubblegum
                      kaolin-galaxy
                      kaolin-ocean
                      kaolin-temple
                      kaolin-valley-dark
                      lab-dark
                      spacemacs-dark
                      modus-vivendi)
  "Dark themes to switch.")

(if (boundp 'ns-system-appearance-change-functions)
    (add-hook 'ns-system-appearance-change-functions
              (defun load-theme-randomly (appearance)
                (mapc #'disable-theme custom-enabled-themes)
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
  :hook ((after-load-theme . doom-modeline-mode)
         (after-load-theme . smaller-modeline))
  :config
  (setq inhibit-compacting-font-caches t

        doom-modeline-height 0
        doom-modeline-buffer-file-name-style 'auto
        doom-modeline-project-detection 'project

        doom-modeline-gnus nil
        doom-modeline-icon nil
        doom-modeline-irc nil
        doom-modeline-persp-name nil)

  (defun smaller-modeline ()
    "Make doom-modeline smaller."
    (set-face-attribute 'mode-line nil :height 0.85 :inherit 'default)
    (set-face-attribute 'mode-line-inactive nil :inherit 'mode-line)))

(use-package shackle
  :straight t
  :hook (after-init . shackle-mode)
  :config
  (setq shackle-default-size 0.4
        shackle-rules `((help-mode                       :select t :align right :size ,fill-column)
                        (helpful-mode                    :select t :align right :size ,fill-column)
                        ("*Messages*"                    :select t :align t)
                        ("*eldoc*"                       :align t)
                        (special-mode                    :align t)
                        (process-menu-mode               :align t)
                        (compilation-mode                :align t)
                        (flymake-diagnostics-buffer-mode :align t)
                        ("*Shell Command Output*"        :align t)
                        ("*Async Shell Command*"         :align t)
                        ("\\*EGLOT.*"                    :select t :align right :size ,fill-column :regexp t))))

(use-package tree-sitter :straight t :defer t)

(use-package tree-sitter-langs
  :straight t
  :after tree-sitter)

(use-package writeroom-mode
  :straight t
  :hook (after-init . global-writeroom-mode)
  :config
  (setq split-width-threshold 120

        writeroom-width 128
        writeroom-bottom-divider-width 0
        writeroom-fringes-outside-margins nil
        writeroom-fullscreen-effect nil
        writeroom-major-modes '(text-mode prog-mode conf-mode special-mode Info-mode dired-mode)
        writeroom-major-modes-exceptions '(process-menu-mode proced-mode)
        writeroom-maximize-window nil
        writeroom-mode-line t
        writeroom-mode-line-toggle-position 'mode-line-format)
  :general
  (tyrant-def
    "wc" 'writeroom-mode
    "wC" 'global-writeroom-mode))

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

  (defun compilation-filter@around (f proc string)
    (funcall f proc (xterm-color-filter string)))
  (advice-add 'compilation-filter :around #'compilation-filter@around))


(provide 'editor-ui)
;;; editor-ui.el ends here
