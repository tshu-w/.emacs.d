;;; lang-misc.el --- -*- lexical-binding: t; -*-

;; Copyright (C) 2020-2022  Tianshu Wang

;; Author: Tianshu Wang <wang@tianshu.me>

;;; Commentary:

;;; Code:

(use-package dockerfile-mode :straight t :defer t)

(use-package js
  :mode ("\\.json\\'" . json-mode)
  :config
  (defvar json-mode-map
    (make-sparse-keymap)
    "Keymap for `json-mode'.")

  (define-derived-mode json-mode js-mode "JSON"
    (make-local-variable 'js-indent-level)
    (setq js-indent-level 2))

  (despot-def json-mode-map
    "=" 'json-pretty-print-buffer
    "+" 'json-pretty-print-buffer-ordered))

(use-package jsonnet-mode
  :straight t
  :defer t
  :config
  (despot-def jsonnet-mode-map
    "=" 'jsonnet-reformat-buffer))

(use-package web-mode :straight t :defer t)

(use-package yaml-mode :straight t :defer t)

(use-package markdown-mode
  :straight t
  :defer t
  :config
  (setq markdown-fontify-code-blocks-natively t)

  (despot-def markdown-mode-map
    "RET"   'markdown-do
    ;; rebind this so terminal users can use it
    "M-RET" 'markdown-insert-list-item
    "{"     'markdown-backward-paragraph
    "}"     'markdown-forward-paragraph
    "]"     'markdown-complete
    ">"     'markdown-indent-region
    "<"     'markdown-outdent-region
    "-"     'markdown-insert-hr
    "c"     '(:ignore t :which-key "command")
    "c]"    'markdown-complete-buffer
    "cc"    'markdown-check-refs
    "ce"    'markdown-export
    "cm"    'markdown-other-window
    "cn"    'markdown-cleanup-list-numbers
    "co"    'markdown-open
    "cp"    'markdown-preview
    "cv"    'markdown-export-and-preview
    "cw"    'markdown-kill-ring-save
    "f"     'markdown-follow-thing-at-point
    "h"     '(:ignore t :which-key "header")
    "hi"    'markdown-insert-header-dwim
    "hI"    'markdown-insert-header-setext-dwim
    "h1"    'markdown-insert-header-atx-1
    "h2"    'markdown-insert-header-atx-2
    "h3"    'markdown-insert-header-atx-3
    "h4"    'markdown-insert-header-atx-4
    "h5"    'markdown-insert-header-atx-5
    "h6"    'markdown-insert-header-atx-6
    "h!"    'markdown-insert-header-setext-1
    "h@"    'markdown-insert-header-setext-2
    "i"     '(:ignore t :which-key "insert")
    "if"    'markdown-insert-footnote
    "ii"    'markdown-insert-image
    "il"    'markdown-insert-link
    "iw"    'markdown-insert-wiki-link
    "iu"    'markdown-insert-uri
    "k"     'markdown-kill-thing-at-point
    "N"     'markdown-next-link
    "l"     '(:ignore t :which-key "lists")
    "li"    'markdown-insert-list-item
    "P"     'markdown-previous-link
    "t"     '(:ignore t :which-key "table")
    "tp"    'markdown-table-move-row-up
    "tn"    'markdown-table-move-row-down
    "tf"    'markdown-table-move-column-right
    "tb"    'markdown-table-move-column-left
    "tr"    'markdown-table-insert-row
    "tR"    'markdown-table-delete-row
    "tc"    'markdown-table-insert-column
    "tC"    'markdown-table-delete-column
    "ts"    'markdown-table-sort-lines
    "td"    'markdown-table-convert-region
    "tt"    'markdown-table-transpose
    "T"     '(:ignore t :which-key "toggle")
    "Ti"    'markdown-toggle-inline-images
    "Tl"    'markdown-toggle-url-hiding
    "Tm"    'markdown-toggle-markup-hiding
    "Tt"    'markdown-toggle-gfm-checkbox
    "Tw"    'markdown-toggle-wiki-links
    "x"     '(:ignore t :which-key "text")
    "xb"    'markdown-insert-bold
    "xB"    'markdown-insert-gfm-checkbox
    "xc"    'markdown-insert-code
    "xC"    'markdown-insert-gfm-code-block
    "xi"    'markdown-insert-italic
    "xk"    'markdown-insert-kbd
    "xp"    'markdown-insert-pre
    "xq"    'markdown-insert-blockquote
    "xs"    'markdown-insert-strike-through
    "xQ"    'markdown-blockquote-region
    "xP"    'markdown-pre-region)

  (general-def '(normal insert) markdown-mode-map
    "M-h"      'markdown-promote
    "M-j"      'markdown-move-down
    "M-k"      'markdown-move-up
    "M-l"      'markdown-demote))

(provide 'lang-misc)
;;; lang-misc.el ends here
