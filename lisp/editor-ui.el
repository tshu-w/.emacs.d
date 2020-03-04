;;; editor-ui.el -*- lexical-binding: t; -*-

;; Copyright (C) 2020  Tianshu Wang

;; Author: Tianshu Wang <volekingsg@gmail.com>

(use-package doom-themes
  :init
  (load-theme 'doom-nord-light t)
  (load-theme 'doom-nord t t)
  :config
  ;; fix doom-theme diff-hl face
  (custom-set-faces '(diff-hl-change ((t (:background nil))))
                    '(diff-hl-delete ((t (:background nil))))
                    '(diff-hl-insert ((t (:background nil))))))

(use-package doom-modeline
  :init
  (doom-modeline-mode)
  :config
  (setq inhibit-compacting-font-caches t
        doom-modeline-buffer-file-name-style 'auto
        doom-modeline-project-detection 'project))

(use-package theme-changer
  :commands change-theme
  :hook (emacs-startup . (lambda () (change-theme 'doom-nord-light 'doom-nord))))

(use-package all-the-icons)

(use-package writeroom-mode
  :hook (after-init . global-writeroom-mode)
  :config
  (setq writeroom-width 128
        writeroom-bottom-divider-width 0
        writeroom-fringes-outside-margins t
        writeroom-fullscreen-effect nil
        writeroom-major-modes '(text-mode prog-mode)
        writeroom-maximize-window nil
        writeroom-mode-line t
        writeroom-mode-line-toggle-position 'mode-line-format)
  :general
  (tyrant-def
    "wc" 'writeroom-mode
    "wC" 'global-writeroom-mode))

(use-package popwin
  :config
  (popwin-mode)
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
  (push '("*undo-tree*"
          :dedicated t :position right  :stick t :noselect nil :width   60) popwin:special-display-config)
  (push '("*undo-tree Diff*"
          :dedicated t :position bottom :stick t :noselect nil :height 0.3) popwin:special-display-config)
  (push '("*ert*"
          :dedicated t :position bottom :stick t :noselect nil            ) popwin:special-display-config)
  (push '("*grep*"
          :dedicated t :position bottom :stick t :noselect nil            ) popwin:special-display-config)
  (push '("*nosetests*"
          :dedicated t :position bottom :stick t :noselect nil            ) popwin:special-display-config)
  (push '("^\*WoMan.+\*$"
          :regexp t :position bottom                                      ) popwin:special-display-config)
  (push '("*Google Translate*"
          :dedicated t :position bottom :stick t :noselect t   :height 0.4) popwin:special-display-config)
  :general
  (tyrant-def
    "bm" 'popwin:messages
    "bM" 'switch-to-messages-buffer
    "wp" 'popwin:close-popup-window))

(use-package golden-ratio
  :config
  (defun toggle-golden-ratio ()
    (interactive)
    (if (bound-and-true-p golden-ratio-mode)
        (progn (golden-ratio-mode -1) (balance-windows))
      (progn (golden-ratio-mode) (golden-ratio))))

  ;; golden-ratio-exclude-modes
  (dolist (m '("bs-mode"
               "calc-mode"
               "ediff-mode"
               "dired-mode"
               "gud-mode"
               "gdb-locals-mode"
               "gdb-registers-mode"
               "gdb-breakpoints-mode"
               "gdb-threads-mode"
               "gdb-frames-mode"
               "gdb-inferior-io-mode"
               "gdb-disassembly-mode"
               "gdb-memory-mode"
               "ranger-mode"
               "speedbar-mode"
               " *transient*"))
    (add-to-list 'golden-ratio-exclude-modes m))

  ;; golden-ratio-extra-commands
  (dolist (f '(ace-window
               ace-delete-window
               ace-select-window
               ace-swap-window
               ace-maximize-window
               avy-pop-mark
               buf-move-left
               buf-move-right
               buf-move-up
               buf-move-down
               evil-avy-goto-word-or-subword-1
               evil-avy-goto-line
               evil-window-delete
               evil-window-split
               evil-window-vsplit
               evil-window-left
               evil-window-right
               evil-window-up
               evil-window-down
               evil-window-bottom-right
               evil-window-top-left
               evil-window-mru
               evil-window-next
               evil-window-prev
               evil-window-new
               evil-window-vnew
               evil-window-rotate-upwards
               evil-window-rotate-downwards
               evil-window-move-very-top
               evil-window-move-far-left
               evil-window-move-far-right
               evil-window-move-very-bottom
               next-multiframe-window
               previous-multiframe-window
               quit-window
               winum-select-window-0-or-10
               winum-select-window-1
               winum-select-window-2
               winum-select-window-3
               winum-select-window-4
               winum-select-window-5
               winum-select-window-6
               winum-select-window-7
               winum-select-window-8
               winum-select-window-9
               windmove-left
               windmove-right
               windmove-up
               windmove-down))
    (add-to-list 'golden-ratio-extra-commands f))

  ;; golden-ratio-exclude-buffer-names
  (dolist (n '(" *NeoTree*"
               "*LV*"
               " *which-key*"))
    (add-to-list 'golden-ratio-exclude-buffer-names n))
  :general (tyrant-def "tg" 'toggle-golden-ratio))

(use-package hl-todo
  ;; global hook activates hl-todo-mode for prog-mode, text-mode
  ;; mode can be explicitly defined using hl-todo-activate-in-modes variable
  :hook (after-init . global-hl-todo-mode))

(use-package highlight-parentheses
  :hook (prog-mode . highlight-parentheses-mode)
  :config
  (setq hl-paren-delay 0.2
        hl-paren-colors '("Springgreen3"
                          "IndianRed1"
                          "IndianRed3"
                          "IndianRed4"))
  (set-face-attribute 'hl-paren-face nil :weight 'ultra-bold)
  :general (tyrant-def "tp" 'highlight-parentheses-mode))

(use-package indent-guide
  :config
  (setq indent-guide-delay 0.3)
  :general
  (tyrant-def
    "ti" 'indent-guide-mode
    "tI" 'indent-guide-global-mode))

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package highlight-numbers
  :hook (prog-mode . highlight-numbers-mode))

(use-package clean-aindent-mode
  :hook (prog-mode . clean-aindent-mode))

(use-package eval-sexp-fu
  :hook (emacs-lisp-mode . eval-sexp-fu-flash-mode))

(use-package volatile-highlights
  :disabled t
  :defer 2
  :config
  (require 'volatile-highlights)
  ;; additional extensions
  ;; evil
  (vhl/define-extension 'evil
                        'evil-move
                        'evil-paste-after
                        'evil-paste-before
                        'evil-paste-pop)
  (with-eval-after-load 'evil
    (vhl/install-extension 'evil)
    (vhl/load-extension 'evil))
  ;; undo-tree
  (vhl/define-extension 'undo-tree
                        'undo-tree-move
                        'undo-tree-yank)
  (with-eval-after-load 'undo-tree
    (vhl/install-extension 'undo-tree)
    (vhl/load-extension 'undo-tree))
  (volatile-highlights-mode))

(use-package hide-comnt
  :ensure nil
  :commands hide/show-comments-toggle
  :general (tyrant-def "t c" '(hide/show-comments-toggle :which-key "toggle-comments")))

(use-package centered-cursor-mode
  :config
  (setq ccm-recenter-at-end-of-file t
        ccm-ignored-commands '(mouse-drag-region
                               mouse-set-point
                               mouse-set-region
                               widget-button-click
                               scroll-bar-toolkit-scroll
                               evil-mouse-drag-region))
  :general (tyrant-def
             "t-"    'centered-cursor-mode
             "t C--" 'global-centered-cursor-mode))


(provide 'editor-ui)
