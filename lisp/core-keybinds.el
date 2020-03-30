;;; core-keybinds.el -*- lexical-binding: t; -*-

;; Copyright (C) 2020  Tianshu Wang

;; Author: Tianshu Wang <volekingsg@gmail.com>

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


(use-package evil
  :ensure t
  :hook (prog-mode . hs-minor-mode)
  :init
  (setq evil-want-keybinding nil)
  (evil-mode)
  :config
  (setq evil-want-C-i-jump t
        evil-want-visual-char-semi-exclusive t
        evil-move-beyond-eol t
        evil-move-cursor-back t
        evil-shift-width 2)

  (setq evil-normal-state-cursor       '("DarkGoldenrod2" box)
        evil-insert-state-cursor       '("chartreuse3" (bar . 2))
        evil-emacs-state-cursor        '("SkyBlue2" box)
        evil-hybrid-state-cursor       '("SkyBlue2" (bar . 2))
        evil-replace-state-cursor      '("chocolate" (hbar . 2))
        evil-evilified-state-cursor    '("LightGoldenrod3" box)
        evil-visual-state-cursor       '("gray" (hbar . 2))
        evil-motion-state-cursor       '("plum3" box)
        evil-lisp-state-cursor         '("HotPink1" box)
        evil-iedit-state-cursor        '("firebrick1" box)
        evil-iedit-insert-state-cursor '("firebrick1" (bar . 2)))

  (add-to-list 'default-jump-handlers 'evil-goto-definition)
  (evil-set-command-property 'jump-to-definition :jump t)
  (evil-set-command-property 'jump-to-reference :jump t)


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
  (add-hook 'after-change-major-mode-hook 'set-evil-shift-width 'append)


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
  (define-key evil-inner-text-objects-map "g" 'evil-inner-buffer)

  (defun evil-smart-doc-lookup ()
    "Run documentation lookup command specific to the major mode.
Use command bound to `SPC m h h` if defined, otherwise fall back
to `evil-lookup'"
    (interactive)
    (let ((binding (key-binding (kbd ",hh"))))
      (if (commandp binding)
          (call-interactively binding)
        (evil-lookup))))

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

  (tyrant-def
    "bN"  'evil-buffer-new
    "fS"  'evil-write-all
    "rM"  'evil-show-marks
    "rr"  'evil-show-registers
    "wh"  'evil-window-left
    "wH"  'evil-window-move-far-left
    "wl"  'evil-window-right
    "wL"  'evil-window-move-far-right
    "wj"  'evil-window-down
    "wJ"  'evil-window-move-very-bottom
    "wk"  'evil-window-up
    "wK"  'evil-window-move-very-top)

  (general-def 'normal
    "C-,"   'evil-repeat-find-char-reverse
    "K"     'evil-smart-doc-lookup
    "zf"    'reposition-window
    "gd"    'jump-to-definition
    "gD"    'jump-to-definition-other-window
    "gr"    'jump-to-reference)

  (general-def 'insert
    [remap evil-complete-previous] 'hippie-expand)

  ;; make <escape> quit as much as possible
  (general-def :keymaps '(minibuffer-local-map
                          minibuffer-local-ns-map
                          minibuffer-local-completion-map
                          minibuffer-local-must-match-map
                          minibuffer-local-isearch-map)
    "<escape>" 'abort-recursive-edit))

(use-package evil-evilified-state
  :commands (evilified-state-evilify-map))

(use-package evil-args
  :ensure t
  :after evil
  :init
  ;; bind evil-args text objects
  (general-def evil-inner-text-objects-map "a" 'evil-inner-arg)
  (general-def evil-outer-text-objects-map "a" 'evil-outer-arg))

(use-package evil-collection
  :ensure t
  :after evil
  :init
  (setq evil-collection-company-use-tng nil)
  (evil-collection-init))

(use-package evil-commentary
  :ensure t
  :after evil
  :init (evil-commentary-mode))

(use-package evil-indent-plus
  :ensure t
  :after evil
  :init (evil-indent-plus-default-bindings))

(use-package evil-lion
  :ensure t
  :after evil
  :init (evil-lion-mode))

(use-package evil-numbers
  :ensure t
  :after evil
  :init
  (general-def evil-normal-state-map
    "C-c +" 'evil-numbers/inc-at-pt
    "C-c -" 'evil-numbers/dec-at-pt
    "C-c C-+" 'evil-numbers/inc-at-pt-incremental
    "C-c C--" 'evil-numbers/dec-at-pt-incremental))

(use-package evil-matchit
  :ensure t
  :after evil
  :hook ((prog-mode LaTeX-mode) . turn-on-evil-matchit-mode))

(use-package evil-surround
  :ensure t
  :after evil
  :init (global-evil-surround-mode)
  :config
  ;; TODO: make this work
  (add-hook 'emacs-lisp-mode-hook (lambda ()
                                    (push '(?` . ("`" . "'")) evil-surround-pairs-alist)))

  ;; `s' for surround instead of `substitute'
  (general-def 'visual evil-surround-mode-map
    "s" 'evil-surround-region
    "S" 'evil-substitute))

(use-package evil-visualstar
  :ensure t
  :after evil
  :init
  (general-def evil-visual-state-map
    "*" 'evil-visualstar/begin-search-forward
    "#" 'evil-visualstar/begin-search-backward))

(use-package linum-relative
  :ensure t
  :after evil
  :config (setq linum-relative-current-symbol "")
  :general (tyrant-def "tr" 'linum-relative-toggle))


(provide 'core-keybinds)
