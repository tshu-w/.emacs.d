;;; editor-misc.el --- -*- lexical-binding: t; -*-

;; Copyright (C) 2020  Tianshu Wang

;; Author: Tianshu Wang <volekingsg@gmail.com>

;;; Commentary:

;;; Code:

(use-package ace-link
  :ensure t
  :general
  (general-def
    :keymaps '(compilation-mode-map
               custom-mode-map
               eww-link-keymap
               eww-mode-map
               help-mode-map
               helpful-mode-map
               Info-mode-map
               xref--xref-buffer-mode-map
               woman-mode-map)
    :states  'normal
    "o"      'ace-link))

(use-package ace-pinyin
  :ensure t
  :init
  (setq ace-pinyin-use-avy t)
  (ace-pinyin-global-mode))

(use-package aggressive-indent
  :ensure t
  :hook (emacs-lisp-mode . aggressive-indent-mode)
  :config
  (add-hook 'diff-auto-refine-mode-hook (lambda () (aggressive-indent-mode -1)))
  :general
  (tyrant-def
    "tA" 'aggressive-indent-mode
    "t C-a" 'global-aggressive-indent-mode))

(use-package avy
  :ensure t
  :config
  (setq avy-all-windows 'all-frames
        avy-background t)
  :general
  (tyrant-def
    "jb" 'avy-pop-mark
    "jj" '(avy-goto-char-timer :which-key "avy timer")
    "jl" '(avy-goto-line :which-key "avy line")
    "jw" '(avy-goto-word-or-subword-1 :which-key "avy word")))

(use-package calibredb
  :ensure t
  :config
  (setq calibredb-root-dir "~/Documents/Calibre"
        calibredb-db-dir (expand-file-name "metadata.db" calibredb-root-dir)
        calibredb-library-alist '(("~/Documents/Calibre")))
  (evil-set-initial-state 'calibredb-search-mode 'motion)
  (general-def 'motion calibredb-search-mode-map "/" 'calibredb-search-live-filter)
  :general
  (tyrant-def
    "aC" 'calibredb
    "sc" 'calibredb-find-counsel))

(use-package cal-china-x
  :ensure t
  :after calendar
  :config
  (cal-china-x-setup)
  (setq calendar-mark-holidays-flag nil)
  (setq cal-china-x-important-holidays cal-china-x-chinese-holidays)
  (setq cal-china-x-general-holidays
        '((holiday-lunar 1 15 "元宵节")
          (holiday-lunar 7 7 "七夕节")
          (holiday-fixed 3 8 "妇女节")
          (holiday-fixed 3 12 "植树节")
          (holiday-fixed 5 4 "青年节")
          (holiday-fixed 6 1 "儿童节")
          (holiday-fixed 9 10 "教师节")))
  (setq holiday-other-holidays
        '((holiday-fixed 2 14 "情人节")
          (holiday-fixed 4 1 "愚人节")
          (holiday-fixed 12 25 "圣诞节")
          (holiday-float 5 0 2 "母亲节")
          (holiday-float 6 0 3 "父亲节")
          (holiday-float 11 4 4 "感恩节")))
  (setq calendar-holidays
        (append cal-china-x-important-holidays
                cal-china-x-general-holidays
                holiday-other-holidays)))

(use-package counsel-dash
  :ensure t
  :init
  (add-hook 'python-mode-hook     '(lambda () (setq-local dash-docs-docsets '("Python 3" "SciPy" "NumPy" "Pandas" "Matplotlib"))))
  (add-hook 'LaTeX-mode-hook      '(lambda () (setq-local dash-docs-docsets '("LaTeX"))))
  (add-hook 'sh-mode-hook         '(lambda () (setq-local dash-docs-docsets '("Bash"))))
  (setq dash-docs-browser-func 'eww)
  (with-eval-after-load 'ivy
    (add-to-list 'ivy-re-builders-alist '(counsel-dash-at-point . ivy--regex-ignore-order))
    (add-to-list 'ivy-re-builders-alist '(counsel-dash . ivy--regex-ignore-order)))
  :general
  (tyrant-def
    "d"  '(:ignore t :which-key "docs")
    "da" 'dash-docs-activate-docset
    "dh" 'counsel-dash-at-point
    "dH" 'counsel-dash))

(use-package devdocs
  :ensure t
  :general (tyrant-def "db" 'devdocs-search))

(use-package dotenv-mode :ensure t)

(use-package dumb-jump
  :ensure t
  :init
  (setq dumb-jump-selector 'ivy)
  ;; Since it's dumb, we add it to the end of the default jump handlers. At
  ;; the time of writing it is the only default jump handler. (gtags remains
  ;; mode-local)
  (add-to-list 'default-jump-handlers 'dumb-jump-go 'append)
  :general (tyrant-def "jq" 'dumb-jump-quick-look))

(use-package editorconfig
  :disabled t
  :ensure t
  :int (editorconfig-mode))

(use-package edit-indirect :ensure t)

(use-package expand-region
  :disabled t
  :ensure t
  :config
  (setq expand-region-contract-fast-key "V"
        expand-region-reset-fast-key "r")
  :general
  (tyrant-def "v" '(er/expand-region :which-key "expand region")))

(use-package fcitx
  :ensure t
  :after exec-path-from-shell
  :init
  (fcitx-aggressive-setup)
  (fcitx-prefix-keys-turn-off))

(use-package google-translate
  :ensure t
  :init
  (setq google-translate-enable-ido-completion t
        google-translate-show-phonetic t
        google-translate-default-source-language "en"
        google-translate-default-target-language "zh-CN")
  (defun set-google-translate-languages (source target)
    "Set source language for google translate.
     For instance pass En as source for English."
    (interactive
     "sEnter source language (ie. en): \nsEnter target language (ie. en): "
     source target)
    (message
     (format "Set google translate source language to %s and target to %s"
             source target))
    (setq google-translate-default-source-language (downcase source))
    (setq google-translate-default-target-language (downcase target)))

  ;; https://github.com/atykhonov/google-translate/issues/98#issuecomment-562870854
  (defun google-translate-json-suggestion@override (json)
    "Retrieve from JSON (which returns by the `google-translate-request'
     function) suggestion. This function does matter when translating misspelled
     word. So instead of translation it is possible to get suggestion."
    (let ((info (aref json 7)))
      (if (and info (> (length info) 0))
          (aref info 1) nil)))
  (advice-add #'google-translate-json-suggestion :override #'google-translate-json-suggestion@override)
  :general
  (tyrant-def
    "xg"  '(:ignore t :which-key "google-translate")
    "xgl" 'set-google-translate-languages
    "xgQ" 'google-translate-query-translate-reverse
    "xgq" 'google-translate-query-translate
    "xgT" 'google-translate-at-point-reverse
    "xgt" 'google-translate-at-point))

(use-package helpful
  :ensure t
  :config
  (defun helpful-reuse-window (buf)
    (if-let ((window (display-buffer-reuse-mode-window buf '((mode . helpful-mode)))))
        (select-window window)
      (pop-to-buffer buf)))

  (setq helpful-switch-buffer-function #'helpful-reuse-window)
  :general
  (tyrant-def
    "hk" 'helpful-key
    "hf" 'helpful-callable
    "hv" 'helpful-variable))

(use-package insert-translated-name
  :quelpa (insert-translated-name :fetcher github :repo "manateelazycat/insert-translated-name")
  :general ("H-t" 'insert-translated-name-insert))

(use-package link-hint
  :ensure t
  :general
  (tyrant-def
    "xo" 'link-hint-open-link
    "xl" 'link-hint-open-multiple-links
    "xy" 'link-hint-copy-link))

(use-package nov
  :ensure t
  :mode ("\\.epub\\'" . nov-mode))

(use-package pandoc-mode
  :ensure t
  :config
  (defun run-pandoc ()
    "Start pandoc for the buffer and open the menu"
    (interactive)
    ;; only run pandoc-mode if not active, as it resets pandoc--local-settings
    (if (not (bound-and-true-p pandoc-mode)) (pandoc-mode))
    (pandoc-main-hydra/body))

  (add-hook 'pandoc-mode-hook 'pandoc-load-default-settings)
  :general
  (tyrant-def "a C-P" 'run-pandoc))

(use-package pdf-tools
  :ensure t
  :mode (("\\.pdf\\'" . pdf-view-mode))
  :config
  (add-hook 'pdf-view-mode-hook (lambda () (pdf-view-midnight-minor-mode)))
  (pdf-tools-install :no-query)
  (setq pdf-view-use-scaling t)

  (despot-def pdf-view-mode-map
    ;; Annotations
    "a"              '(:ignore t :which-key "annotations")
    "aD"              'pdf-annot-delete
    "at"              'pdf-annot-attachment-dired
    "ah"              'pdf-annot-add-highlight-markup-annotation
    "al"              'pdf-annot-list-annotations
    "am"              'pdf-annot-add-markup-annotation
    "ao"              'pdf-annot-add-strikeout-markup-annotation
    "as"              'pdf-annot-add-squiggly-markup-annotation
    "at"              'pdf-annot-add-text-annotation
    "au"              'pdf-annot-add-underline-markup-annotation
    "f"              '(:ignore t :which-key "fit")
    ;; Fit image to window
    "fw"              'pdf-view-fit-width-to-window
    "fh"              'pdf-view-fit-height-to-window
    "fp"              'pdf-view-fit-page-to-window
    "s"              '(:ignore t :which-key "slice/search")
    ;; Slicing image
    "sm"              'pdf-view-set-slice-using-mouse
    "sb"              'pdf-view-set-slice-froquelpam-bounding-box
    "sr"              'pdf-view-reset-slice
    ;; Other
    "ss"              'pdf-occur
    "p"               'pdf-misc-print-document
    "O"               'pdf-outline
    "n"               'pdf-view-midnight-minor-mode)

  (general-def 'visual pdf-view-mode-map
    "y"        'pdf-view-kill-ring-save))

(use-package request :ensure t)

(use-package reveal-in-osx-finder
  :if (memq window-system '(mac ns))
  :ensure t
  :general (tyrant-def "bf" 'reveal-in-osx-finder))

(use-package rime
  :ensure t
  :custom-face
  (rime-preedit-face ((t nil)))
  :hook ((after-init . (lambda ()
                         (when (fboundp 'rime-lib-sync-user-data)
                           (ignore-errors (rime-sync)))))
         (kill-emacs . (lambda ()
                         (when (fboundp 'rime-lib-sync-user-data)
                           (ignore-errors (rime-sync))))))
  :init
  (setq default-input-method "rime"
        rime-librime-root "~/.emacs.d/var/librime/dist"
        rime-user-data-dir "~/.emacs.d/etc/rime/"
        rime-show-candidate 'posframe
        rime-show-preedit 'inline
        rime-posframe-properties (list :internal-border-width 2)
        rime-disable-predicates '(rime-predicate-evil-mode-p
                                  ;; rime-predicate-after-alphabet-char-p
                                  rime-predicate-after-ascii-char-p
                                  +rime-predicate-prog-in-code-p
                                  rime-predicate-punctuation-after-space-cc-p
                                  rime-predicate-punctuation-line-begin-p
                                  rime-predicate-auto-english-p
                                  rime-predicate-space-after-cc-p
                                  rime-predicate-org-latex-mode-p
                                  rime-predicate-org-in-src-block-p))
  :config
  (defun +rime-predicate-prog-in-code-p ()
    "If cursor is in code.
Can be used in `rime-disable-predicates' and `rime-inline-predicates'."
    (and (derived-mode-p 'prog-mode 'conf-mode)
         ;; (not (or (nth 3 (syntax-ppss))
         ;;          (nth 4 (syntax-ppss))))
         ;; insert-translated-name
         (not (and (boundp 'insert-translated-name-active-overlay)
                   insert-translated-name-active-overlay))))
  :general
  (general-def rime-mode-map
    "M-j"   'rime-force-enable
    "C-`"   'rime-send-keybinding
    "C-~"   'rime-send-keybinding
    "C-S-`" 'rime-send-keybinding))

(use-package smart-input-source
	:ensure t
  :hook (text-mode . smart-input-source-mode)
  :config
  (setq smart-input-source-english-input-source
        "com.apple.keylayout.US")
  (setq smart-input-source-other-input-source
        "im.rime.inputmethod.Squirrel.Rime"))

(use-package string-inflection
  :ensure t
  :general
  (tyrant-def
    "xi"  '(:ignore t :which-key "inflection")
    "xic" 'string-inflection-lower-camelcase
    "xiC" 'string-inflection-camelcase
    "xi-" 'string-inflection-kebab-case
    "xik" 'string-inflection-kebab-case
    "xi_" 'string-inflection-underscore
    "xiu" 'string-inflection-underscore
    "xiU" 'string-inflection-upcase))

(use-package vlf
  :ensure t
  :init (require 'vlf-setup))

(use-package wakatime-mode
  :ensure t
  :hook (prog-mode . wakatime-mode)
  :init
  (setq wakatime-cli-path (executable-find "wakatime")
        wakatime-api-key "3fd63845-ecde-47ea-bd1a-7042221d1046")
  :config
  (defun wakatime-dashboard ()
    (interactive)
    (browse-url "https://wakatime.com/dashboard"))
  :general
  (tyrant-def "aw" 'wakatime-dashboard))


(provide 'editor-misc)
;;; editor-misc.el ends here
