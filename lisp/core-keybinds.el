;;; core-keybinds.el --- -*- lexical-binding: t; -*-

;; Copyright (C) 2020-2021  Tianshu Wang

;; Author: Tianshu Wang <wang@tianshu.me>

;;; Commentary:

;;; Code:

(use-package which-key
  :ensure t
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
  :ensure t
  :after evil
  :config
  (general-create-definer tyrant-def
    :states '(normal insert visual motion emacs)
    :keymaps 'override
    :prefix "SPC"
    :non-normal-prefix "M-SPC")
  (tyrant-def "" nil)

  (general-create-definer despot-def
    :states '(normal insert visual motion emacs)
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
    "ap"      'list-processes
    "aP"      'proced

    "b"       '(:ignore t :which-key "buffers")
    "bb"      'switch-to-buffer
    "bB"      'switch-to-buffer-other-window
    "bd"      'kill-current-buffer
    "bh"      'switch-to-help-buffer
    "bj"      'bookmark-jump
    "bJ"      'bookmark-jump-other-window
    "bm"      'switch-to-messages-buffer
    "bs"      'switch-to-scratch-buffer
    "bS"      'bookmark-set
    "bu"      'reopen-killed-buffer
    "bx"      'kill-buffer-and-window
    "b C-d"   'kill-other-buffers
    "b C-S-d" 'kill-matching-buffers-rudely

    "c"       '(:ignore t :which-key "code")
    "cb"      'switch-to-compilation-buffer
    "cc"      'compile
    "cn"      'next-error
    "cp"      'previous-error
    "cr"      'recompile
    "ct"      'toggle-compilation-window
    "cx"      'kill-compilation

    "f"       '(:ignore t :which-key "files")
    "fC"      '(write-file :which-key "copy-file")
    "fD"      'delete-current-buffer-file
    "fe"      '(:ignore t :which-key "emacs")
    "fed"     'find-user-init-file
    "fel"     'find-library
    "fE"      'sudo-edit
    "ff"      'find-file
    "fj"      'dired-jump
    "fJ"      'dired-jump-other-window
    "fl"      'find-file-literally
    "fo"      'open-file-or-directory-in-external-app
    "fr"      'read-only-mode
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
    "hM"      'woman
    "hp"      'describe-package
    "ht"      'describe-text-properties
    "hT"      'describe-theme
    "hv"      'describe-variable
    "hP"      '(:ignore t :which-key "profiler")
    "hPs"     'profiler-start
    "hPk"     'profiler-stop
    "hPr"     'profiler-report

    "q"       '(:ignore t :which-key "quit")
    "qd"      'restart-emacs-debug-init
    "qf"      'kill-frame
    "qr"      'restart-emacs
    "qR"      'restart-emacs-without-desktop
    "qq"      'save-buffers-kill-terminal
    "qQ"      'kill-emacs

    "s"       '(:ignore t :which-key "search")
    "si"      'imenu

    "t"       '(:ignore t :which-key "toggles")
    "ta"      'auto-fill-mode
    "td"      'toggle-debug-on-error
    "tf"      'display-fill-column-indicator-mode
    "th"      'global-hl-line-mode
    "tH"      'font-lock-mode
    "tl"      'toggle-truncate-lines
    "tn"      'display-line-numbers-mode
    "tt"      'load-theme
    "tw"      'whitespace-mode
    "tW"      'toggle-word-wrap

    "u"       '(universal-argument :which-key "universal arg")

    "x"       '(:ignore t :which-key "text")
    "x="      'indent-region-or-buffer
    "xc"      'count-words-region
    "xd"      'delete-trailing-whitespace
    "xs"      'text-scale-adjust
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
    "wh"      'evil-window-left
    "wH"      'evil-window-move-far-left
    "wj"      'evil-window-down
    "wJ"      'evil-window-move-very-bottom
    "wk"      'evil-window-up
    "wK"      'evil-window-move-very-top
    "wl"      'evil-window-right
    "wL"      'evil-window-move-far-right
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

  (general-def
    [remap comment-dwim] 'comment-or-uncomment
    "M-j" (defun scroll-other-window-next-line (&optional arg)
            (interactive "P")
            (scroll-other-window (or arg 1)))
    "M-k" (defun scroll-other-window-previous-line (&optional arg)
            (interactive "P")
            (scroll-other-window (- (or arg 1)))))

  (when (memq window-system '(mac ns))
    (general-def
      "H-`"   'other-frame
      "H-a"   'mark-whole-buffer
      "H-c"   'evil-yank
      "H-n"   'make-frame
      "H-m"   'iconify-frame
      "H-q"   'save-buffers-kill-terminal
      "H-v"   'yank
      "H-x"   'kill-region
      "H-w"   'delete-window
      "H-W"   'delete-frame
      "H-z"   'evil-undo
      "H-Z"   'evil-redo
      "H-C-F" 'toggle-frame-fullscreen
      "H-s"   'save-buffer
      "H-<backspace>" (defun delete-line-before-point ()
                        (interactive)
                        (let ((prev-pos (point)))
                          (forward-visible-line 0)
                          (delete-region (point) prev-pos)
                          (indent-according-to-mode))))))

(use-package evil
  :ensure t
  :hook (prog-mode . hs-minor-mode)
  :init
  (setq evil-want-keybinding nil
        evil-ex-search-vim-style-regexp t
        evil-search-module 'evil-search
        evil-symbol-word-search t
        evil-magic 'very-magic)
  (evil-mode)
  :config
  (setq evil-cross-lines t
        evil-kill-on-visual-paste nil
        evil-move-beyond-eol t
        evil-shift-width 2
        evil-want-C-i-jump t
        evil-want-fine-undo t
        evil-want-visual-char-semi-exclusive t)

  (setq evil-normal-state-cursor  '("DarkGoldenrod2" box)
        evil-insert-state-cursor  '("chartreuse3" (bar . 2))
        evil-emacs-state-cursor   '("SkyBlue2" box)
        evil-replace-state-cursor '("chocolate" (hbar . 2))
        evil-visual-state-cursor  '("gray" (hbar . 2))
        evil-motion-state-cursor  '("plum3" box))

  (evil-set-undo-system 'undo-redo)

  ;; override default-jump-handlers
  (setq default-jump-handlers '(evil-goto-definition))
  (evil-set-command-property 'jump-to-definition :jump t)
  (evil-set-command-property 'jump-to-reference :jump t)

  (progn
    ;; Thanks to `editorconfig-emacs' for many of these
    (defvar evil-indent-variable-alist
      ;; Note that derived modes must come before their sources
      '(((awk-mode c-mode c++-mode java-mode
                   idl-mode java-mode objc-mode pike-mode) . c-basic-offset)
        (groovy-mode . groovy-indent-offset)
        (python-mode . python-indent-offset)
        (cmake-mode . cmake-tab-width)
        (coffee-mode . coffee-tab-width)
        (cperl-mode . cperl-indent-level)
        (css-mode . css-indent-offset)
        (elixir-mode . elixir-smie-indent-basic)
        ((emacs-lisp-mode lisp-mode) . lisp-indent-offset)
        (enh-ruby-mode . enh-ruby-indent-level)
        (erlang-mode . erlang-indent-level)
        (js2-mode . js2-basic-offset)
        (js3-mode . js3-indent-level)
        ((js-mode json-mode) . js-indent-level)
        (latex-mode . (LaTeX-indent-level tex-indent-basic))
        (livescript-mode . livescript-tab-width)
        (mustache-mode . mustache-basic-offset)
        (nxml-mode . nxml-child-indent)
        (perl-mode . perl-indent-level)
        (puppet-mode . puppet-indent-level)
        (ruby-mode . ruby-indent-level)
        (rust-mode . rust-indent-offset)
        (scala-mode . scala-indent:step)
        (sgml-mode . sgml-basic-offset)
        (sh-mode . sh-basic-offset)
        (typescript-mode . typescript-indent-level)
        (web-mode . web-mode-markup-indent-offset)
        (yaml-mode . yaml-indent-offset))
      "An alist where each key is either a symbol corresponding
  to a major mode, a list of such symbols, or the symbol t,
  acting as default. The values are either integers, symbols
  or lists of these.")

    (defun set-evil-shift-width ()
      "Set the value of `evil-shift-width' based on the indentation settings of the
  current major mode."
      (let ((shift-width
             (catch 'break
               (dolist (test evil-indent-variable-alist)
                 (let ((mode (car test))
                       (val (cdr test)))
                   (when (or (and (symbolp mode) (derived-mode-p mode))
                             (and (listp mode) (apply 'derived-mode-p mode))
                             (eq 't mode))
                     (when (not (listp val))
                       (setq val (list val)))
                     (dolist (v val)
                       (cond
                        ((integerp v) (throw 'break v))
                        ((and (symbolp v) (boundp v))
                         (throw 'break (symbol-value v))))))))
               (throw 'break (default-value 'evil-shift-width)))))
        (when (and (integerp shift-width)
                   (< 0 shift-width))
          (setq-local evil-shift-width shift-width))))

    ;; after major mode has changed, reset evil-shift-width
    (add-hook 'after-change-major-mode-hook #'set-evil-shift-width 'append))

  (progn
    (defmacro define-text-object-regexp (key name start-regexp end-regexp)
      "Define a text object.
  START-REGEXP and END-REGEXP are the boundaries of the text object."
      (let ((inner-name (make-symbol (concat "evil-inner-" name)))
            (outer-name (make-symbol (concat "evil-outer-" name))))
        `(progn
           (evil-define-text-object ,inner-name (count &optional beg end type)
             (evil-select-paren ,start-regexp ,end-regexp beg end type count nil))
           (evil-define-text-object ,outer-name (count &optional beg end type)
             (evil-select-paren ,start-regexp ,end-regexp beg end type count t))
           (define-key evil-inner-text-objects-map ,key (quote ,inner-name))
           (define-key evil-outer-text-objects-map ,key (quote ,outer-name)))))

    (defmacro define-text-object (key name start end)
      "Define a text object and a surround pair.
  START and END are strings (not regular expressions) that define
  the boundaries of the text object."
      `(progn
         (define-text-object-regexp ,key ,name
           ,(regexp-quote start)
           ,(regexp-quote end))
         (with-eval-after-load 'evil-surround
           (add-to-list 'evil-surround-pairs-alist
                        (cons (string-to-char ,key)
                              (if ,end
                                  (cons ,start ,end)
                                ,start))))))

    (define-text-object "$" "dollar" "$" "$")
    (define-text-object "*" "star" "*" "*")
    (define-text-object "8" "block-star" "/*" "*/")
    (define-text-object "|" "bar" "|" "|")
    (define-text-object "%" "percent" "%" "%")
    (define-text-object "/" "slash" "/" "/")
    (define-text-object "_" "underscore" "_" "_")
    (define-text-object "-" "hyphen" "-" "-")
    (define-text-object "~" "tilde" "~" "~")
    (define-text-object "=" "equal" "=" "=")
    (define-text-object "«" "double-angle-bracket" "«" "»")
    (define-text-object "｢" "corner-bracket" "｢" "｣")
    (define-text-object "‘" "single-quotation-mark" "‘" "’")
    (define-text-object "“" "double-quotation-mark" "“" "”")
    (define-text-object ";" "elisp-comment" ";; " "")

    (evil-define-text-object evil-pasted (count &rest args)
      (list (save-excursion (evil-goto-mark ?\[) (point))
            (save-excursion (evil-goto-mark ?\]) (1+ (point)))))
    (define-key evil-inner-text-objects-map "P" 'evil-pasted)

    ;; define text-object for entire buffer
    (evil-define-text-object evil-inner-buffer (count &optional beg end type)
      (list (point-min) (point-max)))
    (define-key evil-inner-text-objects-map "g" 'evil-inner-buffer))

  ;; This will keep eldoc active when you are in a method and you go in insert mode.
  (with-eval-after-load 'eldoc
    (eldoc-add-command #'evil-insert)
    (eldoc-add-command #'evil-insert-line)
    (eldoc-add-command #'evil-append)
    (eldoc-add-command #'evil-append-line)
    (eldoc-add-command #'evil-force-normal-state)
    (eldoc-add-command #'evil-cp-insert)
    (eldoc-add-command #'evil-cp-insert-at-end-of-form)
    (eldoc-add-command #'evil-cp-insert-at-beginning-of-form)
    (eldoc-add-command #'evil-cp-append))

  (add-hook 'evil-normal-state-exit-hook #'evil-ex-nohighlight)

  (general-def 'normal
    "C-,"      'evil-repeat-find-char-reverse
    "zf"       'reposition-window
    "gd"       'jump-to-definition
    "gD"       'jump-to-definition-other-window
    "gr"       'jump-to-reference
    "gR"       'jump-to-reference-other-window)

  (general-def 'insert [remap evil-complete-previous] 'hippie-expand))

(use-package evil-textobj-syntax
  :ensure t
  :general
  (general-def evil-inner-text-objects-map "h" 'evil-i-syntax)
  (general-def evil-outer-text-objects-map "h" 'evil-a-syntax))

(use-package evil-collection
  :ensure t
  :init
  (evil-collection-init)
  (add-hook 'org-agenda-mode-hook
            (lambda ()
              (evil-collection-unimpaired-mode -1))))

(use-package evil-indent-plus
  :ensure t
  :hook (after-init . evil-indent-plus-default-bindings))

(use-package evil-matchit
  :ensure t
  :hook ((prog-mode LaTeX-mode) . turn-on-evil-matchit-mode)
  :config
  (setq evilmi-forward-chars (string-to-list "[{(<")
        evilmi-backward-chars (string-to-list "]})>")))

(use-package evil-owl
  :ensure t
  :hook (after-init . evil-owl-mode)
  :config
  (setq evil-owl-display-method 'posframe
        evil-owl-idle-delay 0.5
        evil-owl-max-string-length 50))

(use-package evil-pinyin
  :ensure t
  :hook (after-init . global-evil-pinyin-mode)
  :init
  (setq evil-pinyin-scheme 'simplified-xiaohe-all))

(use-package evil-snipe
  :ensure t
  :hook ((after-init . evil-snipe-mode)
         (after-init . evil-snipe-override-mode))
  :config
  (setq evil-snipe-scope 'buffer
        evil-snipe-show-prompt t)

  (general-def 'visual evil-snipe-local-mode-map
    "z" 'evil-snipe-s
    "Z" 'evil-snipe-S))

(use-package evil-surround
  :ensure t
  :hook (after-init . global-evil-surround-mode)
  :config
  (add-hook 'emacs-lisp-mode-hook
            (lambda ()
              (push '(?` . ("`" . "'")) evil-surround-pairs-alist)))
  ;; `s' for surround instead of `subtitute'
  (general-def 'visual evil-surround-mode-map
    "s" 'evil-surround-region
    "S" 'evil-substitute))

(provide 'core-keybinds)
;;; core-keybinds.el ends here
