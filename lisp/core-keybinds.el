;;; core-keybinds.el -*- lexical-binding: t; -*-

;; Copyright (C) 2020  Tianshu Wang

;; Author: Tianshu Wang <volekingsg@gmail.com>


(use-package which-key
  :hook (after-init . which-key-mode)
  :config
  (setq echo-keystrokes 0.02)
  (setq which-key-echo-keystrokes 0.02
        which-key-idle-delay 0.4
        which-key-idle-secondary-delay 0.01
        which-key-max-description-length 32
        which-key-sort-order 'which-key-key-order-alpha
        which-key-allow-evil-operators t))

(use-package general
  :config
  (general-create-definer tyrant-def
    :states '(normal insert visual motion emacs evilified)
    :keymaps 'override
    :prefix "SPC"
    :non-normal-prefix "M-SPC")
  (tyrant-def "" nil)

  (general-create-definer despot-def
    :states '(normal insert visual motion emacs evilified)
    :keymaps 'override
    :major-modes t
    :prefix ","
    :non-normal-prefix "M-,")
  (despot-def "" nil)

  (general-def universal-argument-map
    "SPC u" 'universal-argument-more)

  (tyrant-def
    "SPC"     '(execute-extended-command :which-key "M-x")
    "TAB"     '(alternate-buffer :whick-key "last buffer")
    "!"       '(shell-command :which-key "shell cmd")

    "a"       '(:ignore t :which-key "applications")
    "ac"      'calc-dispatch
    "ad"      'dired
    "ap"      'list-processes
    "aP"      'proced
    "au"      'undo-tree-visualize

    "b"       '(:ignore t :which-key "buffers")
    "bb"      'mode-line-other-buffer
    "bB"      'ibuffer
    "bd"      'kill-current-buffer
    "bh"      'switch-to-help-buffer
    "bm"      'switch-to-messages-buffer
    "bn"      'next-buffer
    "bp"      'previous-buffer
    "bs"      'switch-to-scratch-buffer
    "bu"      'reopen-killed-buffer
    "bx"      'kill-buffer-and-window
    "br"      'read-only-mode
    "b C-d"   'kill-other-buffers
    "b C-S-d" 'kill-matching-buffers-rudely

    "c"       '(:ignore t :which-key "compile")
    "cb"      'switch-to-compilation-buffer
    "cC"      'compile
    "ck"      'kill-compilation
    "cr"      'recompile
    "ct"      'toggle-compilation-window

    "e"       '(:ignore t :which-key "errors")
    "en"      'next-error
    "eN"      'previous-error
    "ep"      'previous-error

    "f"       '(:ignore t :which-key "files")
    "fA"      '(find-alternate-file :which-key "find-file-and-replace-buffer")
    "fb"      'bookmark-jump
    "fc"      '(write-file :which-key "copy-file")
    "fd"      'dired-jump
    "fD"      'delete-current-buffer-file
    "fe"      '(:ignore t :which-key "emacs")
    "fed"     'find-user-init-file
    "feR"     'load-user-init-file
    "fel"     'find-library
    "fE"      'sudo-edit
    "ff"      'find-file
    "fl"      'find-file-literally
    "fo"      'open-file-or-directory-in-external-app
    "fR"      'rename-current-buffer-file
    "fs"      'save-buffer
    "fv"      '(:ignore t :which-key "variables")
    "fvd"     'add-dir-local-variable
    "fvf"     'add-file-local-variable
    "fvp"     'add-file-local-variable-prop-line

    "F"       '(:ignore t :which-key "frame")
    "Fd"      'delete-frame
    "FD"      'delete-other-frames
    "Fn"      'make-frame
    "Fo"      'other-frame

    "n"       '(:ignore t :which-key "narrow")
    "nr"      'narrow-to-region
    "np"      'narrow-to-page
    "nf"      'narrow-to-defun
    "nw"      'widen

    "m"       (general-simulate-key "," :which-key "major mode" :state 'normal)

    "h"       '(:ignore t :which-key "help")
    "ha"      'apropos-command
    "hb"      'describe-bindings
    "hc"      'describe-char
    "hf"      'describe-function
    "hF"      'describe-face
    "hi"      'info-lookup-symbol
    "hk"      'describe-key
    "hK"      'describe-keymap
    "h C-K"   'which-key-show-top-level
    "hm"      'describe-mode
    "hM"      'man
    "hp"      'describe-package
    "ht"      'describe-theme
    "hv"      'describe-variable
    "hP"      '(:ignore t :which-key "profiler")
    "hPs"     'profiler-start
    "hPk"     'profiler-stop
    "hPr"     'profiler-report

    "j"       '(:ignore t :which-key "jump/join/split")
    "j="      'indent-region-or-buffer
    "j+"      'iwb-region-or-buffer
    "jc"      'goto-last-change
    "jd"      'dired-jump
    "jD"      'dired-jump-other-window
    "jf"      'find-function
    "ji"      'imenu
    "jv"      'find-variable
    "jx"      'xref-find-definitions
    "jX"      'xref-find-references

    "q"       '(:ignore t :which-key "quit")
    "qd"      'restart-emacs-debug-init
    "qf"      'kill-frame
    "qr"      'restart-emacs
    "qq"      'save-buffers-kill-terminal
    "qQ"      'kill-emacs

    "r"       '(:ignore t :which-key "register/ring")
    "ry"      'yank-pop
    "rm"      'pop-to-mark-command

    "s"       '(:ignore t :which-key "search")

    "t"       '(:ignore t :which-key "toggles")
    "ta"      'auto-fill-mode
    "td"      'toggle-debug-on-error
    "tf"      'toggle-frame-fullscreen
    "tF"      'fringe-mode
    ;; "tF"      'display-fill-column-indicator-mode
    ;; "t C-f"   'global-display-fill-column-indicator-mode
    "th"      'global-hl-line-mode
    "tk"      'which-key-mode
    "tl"      'toggle-truncate-lines
    "tn"      'linum-mode
    "tm"      'hidden-mode-line-mode
    "ts"      'font-lock-mode
    "tt"      'load-theme
    "tw"      'whitespace-mode
    "tW"      'toggle-word-wrap

    "u"       '(universal-argument :which-key "universal arg")

    "x"       '(:ignore t :which-key "text")
    "xc"      'count-words-region
    "xC"      'count-words-analysis
    "xd"      '(:ignore t :which-key "delete")
    "xdl"     'delete-blank-lines
    "xdw"     'delete-trailing-whitespace
    "xj"      '(:ignore t :which-key "justification")
    "xjc"     'set-justification-center
    "xjf"     'set-justification-full
    "xjl"     'set-justification-left
    "xjn"     'set-justification-none
    "xjr"     'set-justification-right
    "xt"      '(:ignore t :which-key "transpose")
    "xtc"     'transpose-chars
    "xte"     'transpose-sexps
    "xtl"     'transpose-lines
    "xtp"     'transpose-paragraphs
    "xts"     'transpose-sentences
    "xtw"     'transpose-words

    "w"       '(:ignore t :which-key "windows")
    "w TAB"   'alternate-window
    "w+"      'window-layout-toggle
    "wb"      'switch-to-minibuffer-window
    "wd"      'delete-window
    "wD"      'delete-other-windows
    "wm"      'toggle-maximize-buffer
    "wf"      'follow-mode
    "wr"      'rotate-windows-forward
    "wR"      'rotate-windows-backward
    "ws"      'split-window-vertically
    "wS"      'split-window-vertically-and-focus
    "wt"      'toggle-current-window-dedication
    "wu"      'winner-undo
    "wU"      'winner-redo
    "wv"      'split-window-horizontally
    "wV"      'split-window-horizontally-and-focus
    "ww"      'other-window)

  (when (memq window-system '(mac ns))
    (general-def
      "H-q" 'save-buffers-kill-terminal
      "H-v" 'yank
      "H-c" 'evil-yank
      "H-a" 'mark-whole-buffer
      "H-x" 'kill-region
      "H-w" 'delete-window
      "H-W" 'delete-frame
      "H-n" 'make-frame
      "H-`" 'other-frame
      "H-z" 'undo-tree-undo
      "H-Z" 'undo-tree-redo
      "H-s" (lambda () (interactive) (call-interactively (key-binding "\C-x\C-s")))
      "H-<backspace>" (lambda () (interactive) (kill-line 0) (indent-according-to-mode)))))

(use-package hydra)


(provide 'core-keybinds)
