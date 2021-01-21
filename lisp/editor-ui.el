;;; editor-ui.el --- -*- lexical-binding: t; -*-

;; Copyright (C) 2020  Tianshu Wang

;; Author: Tianshu Wang <volekingsg@gmail.com>

;;; Commentary:

;;; Code:

(defvar after-load-theme-hook nil
  "Hook run after a color theme is loaded using `load-theme'.")

(defun load-theme@after (&rest r)
  "Run `after-load-theme-hook'."
  (ignore r)
  (run-hooks 'after-load-theme-hook))
(advice-add 'load-theme :after #'load-theme@after)

(add-hook 'after-load-theme-hook
          (lambda () (set-face-attribute 'fringe nil :background nil)))

(add-hook 'after-load-theme-hook
          (lambda ()
            ;; (set-face-italic-p 'italic nil)
            (set-face-italic font-lock-constant-face nil)
            (set-face-italic font-lock-keyword-face nil)))

(use-package doom-themes
  :ensure t
  :defer t
  :custom-face
  ;; fix doom-theme diff-hl face
  (diff-hl-change ((t (:background nil))))
  (diff-hl-delete ((t (:background nil))))
  (diff-hl-insert ((t (:background nil)))))

(use-package flucui-themes :ensure t :defer t)

(use-package gruvbox-theme
  :ensure t
  :defer t
  :custom-face
  (internal-border ((t (:background nil)))))

(use-package humanoid-themes
  :ensure t
  :defer t
  :config
  (setq humanoid-org-height nil
        humanoid-org-bold nil))

(use-package kaolin-themes
  :ensure t
  :defer t
  :config
  (setq kaolin-themes-org-scale-headings nil
        kaolin-themes-modeline-border nil))

(use-package lab-themes :ensure t :defer t)

(use-package solo-jazz-theme
  :quelpa (solo-jazz-theme :fetcher github :repo "cstby/solo-jazz-emacs-theme")
  :defer t)

(use-package spacemacs-common
  :ensure spacemacs-theme
  :defer t
  :init
  (setq spacemacs-theme-org-height nil
        spacemacs-theme-comment-bg nil))

(defvar light-themes '(doom-acario-light
                       doom-nord-light
                       doom-one-light
                       doom-opera-light
                       doom-solarized-light
                       doom-tomorrow-day
                       flucui-light
                       gruvbox-light-medium
                       gruvbox-light-soft
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

(defvar dark-themes '(doom-city-lights
                      doom-dark+
                      doom-ephemeral
                      doom-Iosvkem
                      doom-material
                      doom-nord
                      doom-nova
                      doom-oceanic-next
                      doom-one
                      doom-opera
                      doom-palenight
                      doom-peacock
                      doom-rouge
                      doom-solarized-dark
                      doom-sourcerer
                      doom-spacegrey
                      doom-tomorrow-night
                      doom-wilmersdorf
                      doom-vibrant
                      doom-zenburn
                      flucui-dark
                      gruvbox-dark-hard
                      gruvbox-dark-medium
                      gruvbox-dark-soft
                      humanoid-dark
                      kaolin-blossom
                      kaolin-bubblegum
                      kaolin-galaxy
                      kaolin-ocean
                      kaolin-temple
                      kaolin-valley-dark
                      lab-dark
                      spacemacs-dark
                      modus-vivendi)
  "Dark themes to switch.")

(add-hook 'ns-system-appearance-change-functions
          #'(lambda (appearance)
              (mapc #'disable-theme custom-enabled-themes)
              (pcase appearance
                ('light (load-theme (seq-random-elt light-themes) t))
                ('dark (load-theme (seq-random-elt dark-themes) t)))))

(use-package doom-modeline
  :ensure t
  :hook (emacs-startup . doom-modeline-mode)
  :config
  (setq inhibit-compacting-font-caches t
        doom-modeline-height 1
        doom-modeline-buffer-file-name-style 'auto
        doom-modeline-icon nil
        doom-modeline-project-detection 'project)

  (defun smaller-modeline ()
    "Make doom-modeline smaller."
    (when window-system
      (create-fontset-from-ascii-font "Source Code Pro:medium" nil "modeline")
      (set-face-attribute 'mode-line nil
                          :height 120 :fontset "fontset-modeline")
      (set-face-attribute 'mode-line-inactive nil
                          :height 120 :fontset "fontset-modeline")))
  (smaller-modeline)
  (add-hook 'after-load-theme-hook 'smaller-modeline))

(use-package hide-mode-line
  :ensure t
  :general
  (tyrant-def "tm" 'hide-mode-line-mode))

(use-package writeroom-mode
  :ensure t
  :hook (emacs-startup . global-writeroom-mode)
  :config
  (setq writeroom-width 128
        writeroom-bottom-divider-width 0
        writeroom-fringes-outside-margins t
        writeroom-fullscreen-effect nil
        writeroom-major-modes '(text-mode prog-mode conf-mode special-mode Info-mode dired-mode)
        writeroom-maximize-window nil
        writeroom-mode-line t
        writeroom-mode-line-toggle-position 'mode-line-format)
  :general
  (tyrant-def
    "wc" 'writeroom-mode
    "wC" 'global-writeroom-mode))

(use-package popwin
  :ensure t
  :hook (after-init . popwin-mode)
  :config
  ;; don't use default value but manage it ourselves
  (setq popwin:special-display-config nil)

  ;; buffers that we manage
  (push '("*Help*"
          :dedicated t :position bottom :stick t :noselect t   :height 0.4) popwin:special-display-config)
  (push '("^\*helpful.+\*$"
          :dedicated t :position bottom :stick t :noselect t   :height 0.4) popwin:special-display-config)
  (push '("*Process List*"
          :dedicated t :position bottom :stick t :noselect nil :height 0.4) popwin:special-display-config)
  (push '("*compilation*"
          :dedicated t :position bottom :stick t :noselect t   :height 0.4) popwin:special-display-config)
  (push '("*Shell Command Output*"
          :dedicated t :position bottom :stick t :noselect nil            ) popwin:special-display-config)
  (push '("*Async Shell Command*"
          :dedicated t :position bottom :stick t :noselect nil            ) popwin:special-display-config)
  (push '("*ert*"
          :dedicated t :position bottom :stick t :noselect nil            ) popwin:special-display-config)
  (push '("*grep*"
          :dedicated t :position bottom :stick t :noselect nil            ) popwin:special-display-config)
  (push '("*nosetests*"
          :dedicated t :position bottom :stick t :noselect nil            ) popwin:special-display-config)
  (push '("^\*WoMan.+\*$"
          :regexp t    :position bottom                                   ) popwin:special-display-config)
  (push '("^\\*Flycheck.+\\*$"
          :dedicated t :position bottom :stick t :noselect t   :regexp t  ) popwin:special-display-config)
  (push '("*lsp-help*"
          :dedicated t :position bottom :stick t :noselect t   :height 0.4) popwin:special-display-config)
  :general
  (tyrant-def
    "bm" 'popwin:messages
    "bM" 'switch-to-messages-buffer
    "wp" 'popwin:close-popup-window))

(use-package hl-todo
  :ensure t
  ;; global hook activates hl-todo-mode for prog-mode, text-mode
  ;; mode can be explicitly defined using hl-todo-activate-in-modes variable
  :hook (after-init . global-hl-todo-mode))

(use-package highlight-parentheses
  :ensure t
  :hook (prog-mode . highlight-parentheses-mode)
  :config
  (setq hl-paren-delay 0.2
        hl-paren-colors '("Springgreen3"
                          "IndianRed1"
                          "IndianRed3"
                          "IndianRed4"))
  (set-face-attribute 'hl-paren-face nil :weight 'ultra-bold)
  :general (tyrant-def "tp" 'highlight-parentheses-mode))

(use-package rainbow-delimiters
  :ensure t
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package highlight-numbers
  :ensure t
  :hook (prog-mode . highlight-numbers-mode))

(use-package eval-sexp-fu
  :ensure t
  :hook (emacs-lisp-mode . eval-sexp-fu-flash-mode))

(use-package hide-comnt
  :general
  (tyrant-def "t c" '(hide/show-comments-toggle :which-key "toggle-comments")))


(provide 'editor-ui)
;;; editor-ui.el ends here
