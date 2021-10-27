;;; lang-latex.el --- -*- lexical-binding: t; -*-

;; Copyright (C) 2020-2021  Tianshu Wang

;; Author: Tianshu Wang <wang@tianshu.me>

;;; Commentary:

;;; Code:

(use-package tex
  :straight auctex
  :defer t
  :mode ("\\.[tT]e[xX]\\'" . TeX-tex-mode)
  :config
  (setq TeX-auto-save t
        TeX-command-default "LatexMk"
        TeX-master t
        TeX-parse-self t
        TeX-save-query nil
        TeX-source-correlate-start-server t
        TeX-view-program-list '(("Preview.app" "open -a Preview.app %o")
                                ("Skim" "open -a Skim.app %o")
                                ("displayline" "displayline -b %n %o %b")
                                ("open" "open %o"))
        TeX-view-program-selection '((output-dvi "open")
                                     (output-pdf "displayline")
                                     (output-html "open"))
        ;; Don't insert line-break at inline math
        LaTeX-fill-break-at-separators nil)

  (defvar latex-nofill-env '("equation"
                             "equation*"
                             "align"
                             "align*"
                             "tabular"
                             "tabular*"
                             "tabu"
                             "tabu*"
                             "tikzpicture")
    "List of environment names in which `auto-fill-mode' will be inhibited.")

  (defun latex-autofill ()
    "Check whether the pointer is currently inside one of the
environments described in `latex-nofill-env' and if so, inhibits
the automatic filling of the current paragraph."
    (let ((do-auto-fill t)
          (current-environment "")
          (level 0))
      (while (and do-auto-fill (not (string= current-environment "document")))
        (setq level (1+ level)
              current-environment (LaTeX-current-environment level)
              do-auto-fill (not (member current-environment latex-nofill-env))))
      (when do-auto-fill
        (do-auto-fill))))

  (defun latex-auto-fill-mode ()
    "Toggle auto-fill-mode using the custom auto-fill function."
    (interactive)
    (auto-fill-mode)
    (setq auto-fill-function #'latex-autofill))

  (add-hook 'LaTeX-mode-hook 'latex-auto-fill-mode)
  (add-hook 'LaTeX-mode-hook 'TeX-fold-mode)
  (add-hook 'LaTeX-mode-hook 'LaTeX-math-mode)
  (add-hook 'LaTeX-mode-hook 'TeX-source-correlate-mode)
  (add-hook 'LaTeX-mode-hook 'TeX-PDF-mode)

  (defun TeX-build ()
    (interactive)
    (TeX-save-document (TeX-master-file))
    (TeX-command TeX-command-default 'TeX-master-file -1))

  (add-hook 'LaTeX-mode-hook
            (lambda ()
              (add-hook 'after-save-hook 'TeX-build nil 'local)))

  (defun TeX-process-check@around (fun name)
    (cl-flet ((yes-or-no-p (&rest _args) t)
              (y-or-n-p (&rest _args) t))
      (funcall fun name)))
  (advice-add 'TeX-process-check :around #'TeX-process-check@around)

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

  (despot-def (TeX-mode-map LaTeX-mode-map)
    :major-modes '(tex-mode latex-mode)
    ","             'TeX-command-master
    "\\"            'TeX-insert-macro                          ;; C-c C-m
    "-"             'TeX-recenter-output-buffer                ;; C-c C-l
    "%"             'TeX-comment-or-uncomment-paragraph        ;; C-c %
    ";"             'comment-or-uncomment-region               ;; C-c ; or C-c :
    ;; TeX-command-run-all runs compile and open the viewer
    "a"             'TeX-command-run-all                       ;; C-c C-a
    "b"             'TeX-build
    ;; TeX-doc is a very slow function
    "h"             '(:ignore t :which-key "help")
    "hd"            'TeX-doc
    "k"             'TeX-kill-job                              ;; C-c C-k
    "l"             'TeX-recenter-output-buffer                ;; C-c C-l
    "m"             'TeX-insert-macro                          ;; C-c C-m
    "n"             'TeX-next-error                            ;; C-c `
    "N"             'TeX-previous-error                        ;; M-g p
    "v"             'TeX-view                                  ;; C-c C-v
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

  (despot-def LaTeX-mode-map
    :major-modes '(latex-mode)
    "*"                'LaTeX-mark-section                  ;; C-c *
    "."                'LaTeX-mark-environment              ;; C-c .
    "c"                'LaTeX-close-environment             ;; C-c ]
    "e"                'LaTeX-environment                   ;; C-c C-e
    "f"                '(:ignore t :which-key "fill")
    "fe"               'LaTeX-fill-environment              ;; C-c C-q C-e
    "fp"               'LaTeX-fill-paragraph                ;; C-c C-q C-p
    "fr"               'LaTeX-fill-region                   ;; C-c C-q C-r
    "fs"               'LaTeX-fill-section                  ;; C-c C-q C-s
    "i"                '(:ignore t :which-key "insert")
    "ii"               'LaTeX-insert-item                   ;; C-c C-j
    "p"                '(:ignore t :which-key "preview")
    "pb"               'preview-buffer
    "pc"               'preview-clearout
    "pd"               'preview-document
    "pe"               'preview-environment
    "pf"               'preview-cache-preamble
    "pp"               'preview-at-point
    "pr"               'preview-region
    "ps"               'preview-section
    "s"                'LaTeX-section                       ;; C-c C-s
    "x"                '(:ignore t :which-key "text/fonts")
    "xB"               'font-medium
    "xf"               '(:ignore t :which-key "fonts")
    "xfa"              'font-calligraphic
    "xfn"              'font-normal
    "xfu"              'font-upright
    "xi"               'font-italic
    "xr"               'font-clear
    "xo"               'font-oblique))

(use-package reftex
  :hook (TeX-mode . reftex-mode)
  :config
  (setq reftex-default-bibliography '("~/Documents/Bibliography/references.bib")
        reftex-plug-into-AUCTeX '(nil nil t t t)
        reftex-use-fonts t)

  (despot-def (TeX-mode-map LaTeX-mode-map)
    :major-modes '(tex-mode latex-mode)
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

(use-package auctex-latexmk
  :straight t
  :after tex
  :config
  (auctex-latexmk-setup)
  (setq-default TeX-command-default "LatexMk")
  (setq auctex-latexmk-inherit-TeX-PDF-mode t))

(use-package aas
  :straight t
  :hook ((TeX-mode org-mode) . aas-activate-for-major-mode))

(use-package laas
  :straight t
  :hook (TeX-mode . laas-mode))

(use-package evil-tex
  :straight t
  :after evil
  :hook (TeX-mode . evil-tex-mode))


(use-package bibtex
  :defer t
  :config
  (setq bibtex-file-path "~/Documents/Bibliography/"
        bibtex-files '("references.bib")
        bibtex-notes-path "~/Documents/Org/notes/refs/"

        bibtex-align-at-equal-sign t
        bibtex-autokey-titleword-separator "-"
        bibtex-autokey-year-title-separator "-"
        bibtex-autokey-name-year-separator "-"
        bibtex-dialect 'biblatex))

(use-package bibtex-completion
  :straight (:files ("bibtex-completion.el"))
  :defer t
  :config
  (setq bibtex-autokey-year-length 4
        bibtex-completion-additional-search-fields '(keywords)
        bibtex-completion-bibliography (mapcar (lambda (file) (concat bibtex-file-path file)) bibtex-files)
        bibtex-completion-library-path (concat bibtex-file-path "files/")
        bibtex-completion-notes-path bibtex-notes-path
        bibtex-completion-pdf-field "file"))

(use-package bibtex-actions
  :straight (:includes oc-bibtex-actions)
  :defer t
  :config
  (with-eval-after-load 'embark
    (setq bibtex-actions-at-point-function 'embark-act)

    ;; Make the 'bibtex-actions' bindings and targets available to `embark'.
    (add-to-list 'embark-target-finders 'bibtex-actions-citation-key-at-point)
    (add-to-list 'embark-keymap-alist '(bib-reference . bibtex-actions-map))
    (add-to-list 'embark-keymap-alist '(citation-key . bibtex-actions-buffer-map))))

(use-package oc-bibtex-actions
  :straight citeproc
  :after oc
  :defer t
  :commands (oc-bibtex-actions-insert
             oc-bibtex-actions-select-style
             oc-bibtex-actions-follow
             oc-bibtex-actions-activate)
  :init
  (org-cite-register-processor 'oc-bibtex-actions
    :insert (org-cite-make-insert-processor
             #'oc-bibtex-actions-insert
             #'oc-bibtex-actions-select-style)
    :follow #'oc-bibtex-actions-follow
    :activate #'oc-bibtex-actions-activate)

  (setq org-cite-insert-processor 'oc-bibtex-actions
        org-cite-follow-processor 'oc-bibtex-actions
        org-cite-activate-processor 'oc-bibtex-actions))

(use-package ebib
  :straight t
  :config
  (setq ebib-default-directory bibtex-file-path
        ebib-bib-search-dirs `(,bibtex-file-path)
        ebib-file-search-dirs `(,(concat bibtex-file-path "files/"))
        ebib-notes-directory bibtex-notes-path
        ebib-reading-list-file "~/Documents/Org/reading_list.org"

        ebib-bibtex-dialect bibtex-dialect
        ebib-file-associations '(("pdf" . "open"))
        ebib-index-default-sort '("timestamp" . descend)
        ebib-notes-template ":PROPERTIES:
:ID:       %i
:ROAM_REFS: @%k
:END:
#+title: %t
#+description: %d
#+date: %s

%%?"
        ebib-notes-template-specifiers '((?k . ebib-create-key)
                                         (?i . ebib-create-id)
                                         (?t . ebib-create-org-title)
					                               (?d . ebib-create-org-description)
                                         (?l . ebib-create-org-link)
                                         (?s . ebib-create-org-time-stamp))
        ebib-preload-bib-files bibtex-files
        ebib-use-timestamp t)

  (defun ebib-create-key (key _db)
    "Return the KEY in DB for the Org mode note."
    (format "%s" key))

  (defun ebib-create-id (_key _db)
    "Create an ID for the Org mode note."
    (org-id-new))

  (defun ebib-create-org-time-stamp (_key _db)
    "Create timestamp for the Org mode note."
    (format "%s" (with-temp-buffer (org-insert-time-stamp nil))))

  (defcustom ebib-zotero-translation-server "https://translate.manubot.org"
    "The address of Zotero translation server."
    :group 'ebib
    :type 'string)

  (defun ebib-zotero-translate (item server-path &optional export-format)
    "Convert item to EXPORT-FORMAT entry through `ebib-zotero-translation-server'."
    (let ((export-format (or export-format
                             (downcase (symbol-name (intern-soft bibtex-dialect))))))
      (shell-command-to-string
       (format "curl -s -d '%s' -H 'Content-Type: text/plain' '%s/%s' | curl -s -d @- -H 'Content-Type: application/json' '%s/export?format=%s'" item ebib-zotero-translation-server server-path ebib-zotero-translation-server export-format))))

  (defun ebib-zotero-import-url (url)
    "Fetch a entry from zotero translation server via a URL.
The entry is stored in the current database."
    (interactive "MURL: ")
    (with-temp-buffer
      (insert (ebib-zotero-translate url "web"))
      (ebib-import-entries ebib--cur-db)))

  (defun ebib-zotero-import-identifier (identifier)
    "Fetch a entry from zotero translation server via an IDENTIFIER.
The entry is stored in the current database,
and the identifier can be DOI, ISBN, PMID, or arXiv ID."
    (interactive "MIDENTIFIER: ")
    (with-temp-buffer
      (insert (ebib-zotero-translate identifier "search"))
      (ebib-import-entries ebib--cur-db)))

  (general-def ebib-index-mode-map
    "/" 'ebib-jump-to-entry
    [remap ebib-quit] 'ebib-force-quit)

  (general-def '(ebib-index-mode-map ebib-entry-mode-map)
    [remap save-buffer] 'ebib-save-current-database)
  :general
  (tyrant-def "ab" 'ebib))


(provide 'lang-latex)
;;; lang-latex.el ends here
