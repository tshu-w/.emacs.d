;;; lang-markdown.el --- -*- lexical-binding: t; -*-

;; Copyright (C) 2020-2021  Tianshu Wang

;; Author: Tianshu Wang <wang@tianshu.me>

;;; Commentary:

;;; Code:

(use-package markdown-mode
  :ensure t
  :defer t
  :config
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

  ;; Header navigation in normal state movements
  (general-def 'normal markdown-mode-map
    "gj"       'outline-forward-same-level
    "gk"       'outline-backward-same-level
    "gh"       'outline-up-heading
    ;; next visible heading is not exactly what we want but close enough
    "gl"       'outline-next-visible-heading)

  (general-def '(normal insert) markdown-mode-map
    "M-h"      'markdown-promote
    "M-j"      'markdown-move-down
    "M-k"      'markdown-move-up
    "M-l"      'markdown-demote))

(use-package mmm-mode
  :ensure t
  :hook (markdown-mode . mmm-mode)
  :config
  (defvar markdown-mmm-auto-modes
    '(;; in alphabetical order, symbols first then lists
      "c"
      "c++"
      "css"
      "java"
      "javascript"
      "python"
      "ruby"
      "rust"
      "scala"

      ("elisp" "emacs-lisp")
      ("ess" "R")
      ("ini" "conf-unix")
      ("html" "web"))
    "List of language names or lists of language and mode names for which to
  generate mmm classes.")

  ;; from Jason Blevins http://jblevins.org/log/mmm
  (defun markdown-mmm-auto-class (lang)
    (let* ((l (if (listp lang) (car lang) lang))
           (s (if (listp lang) (cadr lang) lang))
           (class (intern (concat "markdown-" l)))
           (submode (intern (concat s "-mode")))
           (front (concat "^```" l "[\n\r]+"))
           (back "^```$"))
      (mmm-add-classes (list (list class
                                   :submode submode
                                   :front front
                                   :back back)))
      (dolist (mode '(markdown-mode gfm-mode))
        (mmm-add-mode-ext-class mode nil class))))

  (mapc 'markdown-mmm-auto-class markdown-mmm-auto-modes))


(provide 'lang-markdown)
;;; lang-markdown.el ends here
