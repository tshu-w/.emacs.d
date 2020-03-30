;;; lang-latex.el -*- lexical-binding: t; -*-

;; Copyright (C) 2020  Tianshu Wang

;; Author: Tianshu Wang <volekingsg@gmail.com>

(define-jump-handlers latex-mode)

(use-package tex
  :ensure auctex
  :config
  (setq-default TeX-engine 'xetex)
  (setq latex-build-command "LatexMk"
        TeX-auto-save t
        TeX-command-default latex-build-command
        TeX-master t
        TeX-parse-self t
        TeX-save-query nil
        TeX-syntactic-comment t
        TeX-source-correlate-start-server t
        TeX-source-correlate-mode t
        ;; Don't insert line-break at inline math
        LaTeX-fill-break-at-separators nil)

  (setq TeX-view-program-list
        '(("Preview.app" "open -a Preview.app %o")
          ("Skim" "open -a Skim.app %o")
          ("displayline" "displayline -b %n %o %b")
          ("open" "open %o"))
        TeX-view-program-selection
        '((output-dvi "open")
          (output-pdf "displayline")
          (output-html "open")))

  (add-hook 'doc-view-mode-hook 'auto-revert-mode)

  (custom-set-faces '(preview-reference-face ((t (:foreground "black")))))

  (defun latex/build ()
    (interactive)
    (progn
      (let ((TeX-save-query nil))
        (TeX-save-document (TeX-master-file)))
      (TeX-command latex-build-command 'TeX-master-file -1)))

  (add-hook 'LaTeX-mode-hook 'TeX-fold-mode)
  (add-hook 'LaTeX-mode-hook 'LaTeX-math-mode)
  (add-hook 'LaTeX-mode-hook 'TeX-source-correlate-mode)
  (add-hook 'LaTeX-mode-hook 'TeX-PDF-mode)

  ;; Rebindings for TeX-font
  (defun font-bold () (interactive) (TeX-font nil ?\C-b))
  (defun font-medium () (interactive) (TeX-font nil ?\C-m))
  (defun font-code () (interactive) (TeX-font nil ?\C-t))
  (defun font-emphasis () (interactive) (TeX-font nil ?\C-e))
  (defun font-italic () (interactive) (TeX-font nil ?\C-i))
  (defun font-clear () (interactive) (TeX-font nil ?\C-d))
  (defun font-calligraphic () (interactive) (TeX-font nil ?\C-a))
  (defun font-small-caps () (interactive) (TeX-font nil ?\C-c))
  (defun font-sans-serif () (interactive) (TeX-font nil ?\C-f))
  (defun font-normal () (interactive) (TeX-font nil ?\C-n))
  (defun font-serif () (interactive) (TeX-font nil ?\C-r))
  (defun font-oblique () (interactive) (TeX-font nil ?\C-s))
  (defun font-upright () (interactive) (TeX-font nil ?\C-u))

  (general-def TeX-mode-map "<H-S-mouse-1>" 'TeX-view)
  (despot-def :keymaps '(tex-mode-map TeX-mode-map latex-mode-map LaTeX-mode-map)
    ","             'TeX-command-master
    "\\"            'TeX-insert-macro                            ;; C-c C-m
    "-"             'TeX-recenter-output-buffer                  ;; C-c C-l
    "%"             'TeX-comment-or-uncomment-paragraph          ;; C-c %
    ";"             'comment-or-uncomment-region                 ;; C-c ; or C-c :
    ;; TeX-command-run-all runs compile and open the viewer
    "a"             'TeX-command-run-all                         ;; C-c C-a
    "b"             'latex/build
    ;; TeX-doc is a very slow function
    "h"             '(:ignore t :which-key "help")
    "hd"            'TeX-doc
    "k"             'TeX-kill-job                                ;; C-c C-k
    "l"             'TeX-recenter-output-buffer                  ;; C-c C-l
    "m"             'TeX-insert-macro                            ;; C-c C-m
    "n"             'TeX-next-error                              ;; C-c `
    "N"             'TeX-previous-error                          ;; M-g p
    "v"             'TeX-view                                    ;; C-c C-v
    "x"             '(:ignore t :which-key "text/fonts")
    "xb"            'font-bold
    "xc"            'font-code
    "xe"            'font-emphasis
    "xi"            'font-italic
    "xr"            'font-clear
    "xo"            'font-oblique
    "xf"            '(:ignore t :which-key "fonts")
    "xfc"           'font-small-caps
    "xff"           'font-sans-serif
    "xfr"           'font-serif
    "z"             '(:ignore t :which-key "fold")
    "z="            'TeX-fold-math
    "zb"            'TeX-fold-buffer
    "zB"            'TeX-fold-clearout-buffer
    "ze"            'TeX-fold-env
    "zI"            'TeX-fold-clearout-item
    "zm"            'TeX-fold-macro
    "zp"            'TeX-fold-paragraph
    "zP"            'TeX-fold-clearout-paragraph
    "zr"            'TeX-fold-region
    "zR"            'TeX-fold-clearout-region
    "zz"            'TeX-fold-dwim)

  (despot-def :keymaps '(latex-mode-map LaTeX-mode-map)
    "*"             'LaTeX-mark-section                  ;; C-c *
    "."             'LaTeX-mark-environment              ;; C-c .
    "c"             'LaTeX-close-environment             ;; C-c ]
    "e"             'LaTeX-environment                   ;; C-c C-e
    "f"             '(:ignore t :which-key "fill")
    "fe"            'LaTeX-fill-environment              ;; C-c C-q C-e
    "fp"            'LaTeX-fill-paragraph                ;; C-c C-q C-p
    "fr"            'LaTeX-fill-region                   ;; C-c C-q C-r
    "fs"            'LaTeX-fill-section                  ;; C-c C-q C-s
    "i"             '(:ignore t :which-key "insert")
    "ii"            'LaTeX-insert-item                   ;; C-c C-j
    "p"             '(:ignore t :which-key "preview")
    "pb"            'preview-buffer
    "pc"            'preview-clearout
    "pd"            'preview-document
    "pe"            'preview-environment
    "pf"            'preview-cache-preamble
    "pp"            'preview-at-point
    "pr"            'preview-region
    "ps"            'preview-section
    "s"             'LaTeX-section                       ;; C-c C-s
    "x"             '(:ignore t :which-key "text/fonts")
    "xb"            'font-bold
    "xB"            'font-medium
    "xc"            'font-code
    "xe"            'font-emphasis
    "xf"            '(:ignore t :which-key "fonts")
    "xfa"           'font-calligraphic
    "xfc"           'font-small-caps
    "xff"           'font-sans-serif
    "xfn"           'font-normal
    "xfr"           'font-serif
    "xfu"           'font-upright
    "xi"            'font-italic
    "xr"            'font-clear
    "xo"            'font-oblique))

(use-package reftex
  :hook (TeX-mode . turn-on-reftex)
  :config
  (setq reftex-plug-into-AUCTeX '(nil nil t t t)
        reftex-use-fonts t)

  (despot-def :keymaps '(latex-mode-map LaTeX-mode-map)
    "r"     '(:ignore t :which-key "reftex")
    "rc"    'reftex-citation
    "rg"    'reftex-grep-document
    "ri"    'reftex-index-selection-or-word
    "rI"    'reftex-display-index
    "r TAB" 'reftex-index
    "rl"    'reftex-label
    "rp"    'reftex-index-phrase-selection-or-word
    "rP"    'reftex-index-visit-phrases-buffer
    "rr"    'reftex-reference
    "rs"    'reftex-search-document
    "rt"    'reftex-toc
    "rT"    'reftex-toc-recenter
    "rv"    'reftex-view-crossref))

(use-package cdlatex
  :ensure t
  :hook ((LaTeX-mode . turn-on-cdlatex)
         (org-mode   . turn-on-org-cdlatex))
  :config
  (setq cdlatex-command-alist
        '(("ct" "Insert \\citet" "\\citet{?}" cdlatex-position-cursor nil t nil)
          ("cp" "Insert \\citep" "\\citep{?}" cdlatex-position-cursor nil t nil)
          ("eref" "Insert \\eqref" "\\eqref{eq?}" cdlatex-position-cursor nil t nil)
          ("fref" "Insert \\ref" "\\ref{fig?}" cdlatex-position-cursor nil t nil)
          ("sref" "Insert \\ref" "\\ref{sec?}" cdlatex-position-cursor nil t nil))))

(use-package auctex-latexmk
  :ensure t
  :hook (LaTeX-mode . auctex-latexmk-setup)
  :config (setq auctex-latexmk-inherit-TeX-PDF-mode t))

(use-package company-auctex
  :ensure t
  :after (auctex company)
  :config (company-auctex-init))

(use-package company-reftex
  :ensure t
  :after company
  :hook (reftex-mode . (lambda ()
                         (add-to-list (make-local-variable 'company-backends)
                                      '(company-reftex-labels company-reftex-citations)))))


(provide 'lang-latex)
