;;; editor-completion.el -*- lexical-binding: t; -*-

;; Copyright (C) 2020  Tianshu Wang

;; Author: Tianshu Wang <volekingsg@gmail.com>


(use-package ivy
  :hook (after-init . ivy-mode)
  :init
  (setq ivy-height 15
        ivy-use-virtual-buffers t
        ivy-initial-inputs-alist nil ;; it will change after counsel load
        ivy-use-selectable-prompt t)
  :config
  (defun ivy-tab ()
    (interactive)
    (let ((dir ivy--directory))
      (ivy-partial-or-done)
      (when (string= dir ivy--directory)
        (ivy-insert-current)
        (when (and (eq (ivy-state-collection ivy-last) #'read-file-name-internal)
                   (setq dir (ivy-expand-file-if-directory (ivy-state-current ivy-last))))
          (ivy--cd dir)
          (setq this-command 'ivy-cd)))))

  (defun ivy-c-h ()
    (interactive)
    (if (eq (ivy-state-collection ivy-last) #'read-file-name-internal)
        (if (string-equal (ivy--input) "")
            (counsel-up-directory)
          (delete-minibuffer-contents))
      (ivy-backward-delete-char)))

  (defun ivy-edit ()
    "Edit the current search results in a buffer using wgrep."
    (interactive)
    (run-with-idle-timer 0 nil 'ivy-wgrep-change-to-wgrep-mode)
    (ivy-occur))

  :general
  (tyrant-def
    "bb" 'ivy-switch-buffer
    "rl" 'ivy-resume)
  (despot-def ivy-occur-grep-mode-map
    "w" 'ivy-wgrep-change-to-wgrep-mode
    "s" 'wgrep-save-all-buffers)
  (general-def :keymaps '(ivy-minibuffer-map ivy-switch-buffer-map)
    "<escape>" 'abort-recursive-edit
    "<tab>"    'ivy-tab
    "C-h"      'ivy-c-h
    "C-S-h"    'help-map
    "C-c C-e"  'ivy-edit))

(use-package ivy-hydra
  :general
  (general-def hydra-ivy/keymap
    "<escape>" 'hydra-ivy/keyboard-escape-quit-and-exit))

(use-package ivy-rich
  ;; if `counsel' loads after `ivy-rich', it overrides some of `ivy-rich''s
  ;; transformers
  :after counsel
  :hook (counsel-mode . ivy-rich-mode)
  :config
  (setq ivy-rich-path-style 'abbrev
        ivy-virtual-abbreviate 'full))

(use-package ivy-posframe
  :hook (ivy-mode . ivy-posframe-mode)
  :init
  (setq ivy-posframe-parameters '((left-fringe . 8)
                                  (right-fringe . 8))
        ivy-posframe-display-functions-alist '((swiper . nil)
                                               (complete-symbol . ivy-posframe-display-at-point)
                                               (t . ivy-posframe-display-at-frame-center))))

(use-package ivy-xref
  :init
  (setq xref-prompt-for-identifier '(not xref-find-definitions
                                         xref-find-definitions-other-window
                                         xref-find-definitions-other-frame
                                         xref-find-references
                                         jump-to-definition
                                         jump-to-reference))
  ;; Use ivy-xref to display `xref.el' results.
  (setq xref-show-xrefs-function #'ivy-xref-show-xrefs))

(use-package counsel
  :hook (ivy-mode . counsel-mode)
  :config
  ;; Enable better auto completion of counsel-find-file
  ;; by recognizing file at point.
  (setq counsel-find-file-at-point t)

  (defun counsel-rg-region-or-symbol ()
    "Use `counsel-rg' to search for
    the selected region or the symbol around point in the current
    directory ."
    (interactive)
    (counsel-rg (if (region-active-p)
                    (buffer-substring-no-properties
                     (region-beginning) (region-end))
                  (thing-at-point 'symbol t))))

  ;; append ivy actions
  (ivy-add-actions 'counsel-recentf'(("r" (lambda (arg) (interactive)
                                            (recentf-cleanup)
                                            (counsel-recentf))
                                      "refresh list")))
  :general
  (tyrant-def
    "fr" 'counsel-recentf
    "fL" 'counsel-locate
    "sf" 'counsel-rg
    "sF" 'counsel-rg-region-or-symbol)
  (general-def read-expression-map
    "C-r" 'counsel-minibuffer-history))

(use-package swiper
  :config
  (defun counsel-current-region-or-symbol ()
    "Return contents of the region or symbol at point.

If region is active, mark will be deactivated in order to prevent region
expansion when jumping around the buffer with counsel. See `deactivate-mark'."
    (if (region-active-p)
        (prog1
            (buffer-substring-no-properties (region-beginning) (region-end))
          (deactivate-mark))
      (thing-at-point 'symbol t)))

  (defun swiper-region-or-symbol ()
    "Run `swiper' with the selected region or the symbol
around point as the initial input."
    (interactive)
    (let ((input (counsel-current-region-or-symbol)))
      (swiper input)))

  (defun swiper-all-region-or-symbol ()
    "Run `swiper-all' with the selected region or the symbol
around point as the initial input."
    (interactive)
    (let ((input (counsel-current-region-or-symbol)))
      (swiper-all input)))
  :general
  (tyrant-def
    "ss"  'swiper
    "sS"  'swiper-region-or-symbol
    "sb"  'swiper-all
    "sB"  'swiper-all-region-or-symbol))

(use-package pyim
  :after ivy
  :commands pyim-cregexp-build
  :init
  (defun eh-ivy-cregexp (str)
    (let ((x (ivy--regex-plus str))
          (case-fold-search nil))
      (if (listp x)
          (mapcar (lambda (y)
                    (if (cdr y)
                        (list (if (equal (car y) "")
                                  ""
                                (pyim-cregexp-build (car y)))
                              (cdr y))
                      (list (pyim-cregexp-build (car y)))))
                  x)
        (pyim-cregexp-build x))))

  (setq ivy-re-builders-alist '((t . eh-ivy-cregexp))
        pyim-dcache-directory (concat cache-dir "pyim/dcache/")))

(use-package wgrep
  :general
  (despot-def wgrep-mode-map
    "," 'wgrep-finish-edit
    "c" 'wgrep-finish-edit
    "a" 'wgrep-abort-changes
    "k" 'wgrep-abort-changes))

(use-package smex
  :after ivy
  :config
  (setq smex-history-length 32
        smex-save-file (concat cache-dir ".smex-items")))

(use-package company
  :init
  (setq company-idle-delay 0.2
        company-minimum-prefix-length 2
        company-require-match nil
        company-selection-wrap-around t
        company-tooltip-align-annotations t
        company-transformers '(company-sort-by-occurrence)
        company-dabbrev-ignore-case nil
        company-dabbrev-downcase nil
        company-global-modes '(not erc-mode message-mode help-mode gud-mode eshell-mode shell-mode))
  (global-company-mode))

(use-package company-box
  :hook (company-mode . company-box-mode)
  :config
  (setq company-box-enable-icon nil
        company-box-show-single-candidate t
        company-box-max-candidates 50
        company-box-doc-enable nil)

  ;; https://github.com/seagle0128/.emacs.d/blob/master/lisp/init-company.el#L76
  (when (require 'all-the-icons nil 'noerror)
    (declare-function all-the-icons-faicon 'all-the-icons)
    (declare-function all-the-icons-material 'all-the-icons)
    (declare-function all-the-icons-octicon 'all-the-icons)
    (setq company-box-enable-icon t
          company-box-icons-all-the-icons
          `((Unknown . ,(all-the-icons-material "find_in_page" :height 0.85 :v-adjust -0.2))
            (Text . ,(all-the-icons-faicon "text-width" :height 0.8 :v-adjust -0.05))
            (Method . ,(all-the-icons-faicon "cube" :height 0.8 :v-adjust -0.05 :face 'all-the-icons-purple))
            (Function . ,(all-the-icons-faicon "cube" :height 0.8 :v-adjust -0.05 :face 'all-the-icons-purple))
            (Constructor . ,(all-the-icons-faicon "cube" :height 0.8 :v-adjust -0.05 :face 'all-the-icons-purple))
            (Field . ,(all-the-icons-octicon "tag" :height 0.8 :v-adjust 0 :face 'all-the-icons-lblue))
            (Variable . ,(all-the-icons-octicon "tag" :height 0.8 :v-adjust 0 :face 'all-the-icons-lblue))
            (Class . ,(all-the-icons-material "settings_input_component" :height 0.85 :v-adjust -0.2 :face 'all-the-icons-orange))
            (Interface . ,(all-the-icons-material "share" :height 0.85 :v-adjust -0.2 :face 'all-the-icons-lblue))
            (Module . ,(all-the-icons-material "view_module" :height 0.85 :v-adjust -0.2 :face 'all-the-icons-lblue))
            (Property . ,(all-the-icons-faicon "wrench" :height 0.8 :v-adjust -0.05))
            (Unit . ,(all-the-icons-material "settings_system_daydream" :height 0.85 :v-adjust -0.2))
            (Value . ,(all-the-icons-material "format_align_right" :height 0.85 :v-adjust -0.2 :face 'all-the-icons-lblue))
            (Enum . ,(all-the-icons-material "storage" :height 0.85 :v-adjust -0.2 :face 'all-the-icons-orange))
            (Keyword . ,(all-the-icons-material "filter_center_focus" :height 0.85 :v-adjust -0.2))
            (Snippet . ,(all-the-icons-material "format_align_center" :height 0.85 :v-adjust -0.2))
            (Color . ,(all-the-icons-material "palette" :height 0.85 :v-adjust -0.2))
            (File . ,(all-the-icons-faicon "file-o" :height 0.85 :v-adjust -0.05))
            (Reference . ,(all-the-icons-material "collections_bookmark" :height 0.85 :v-adjust -0.2))
            (Folder . ,(all-the-icons-faicon "folder-open" :height 0.85 :v-adjust -0.05))
            (EnumMember . ,(all-the-icons-material "format_align_right" :height 0.85 :v-adjust -0.2 :face 'all-the-icons-lblue))
            (Constant . ,(all-the-icons-faicon "square-o" :height 0.85 :v-adjust -0.1))
            (Struct . ,(all-the-icons-material "settings_input_component" :height 0.85 :v-adjust -0.2 :face 'all-the-icons-orange))
            (Event . ,(all-the-icons-octicon "zap" :height 0.8 :v-adjust 0 :face 'all-the-icons-orange))
            (Operator . ,(all-the-icons-material "control_point" :height 0.85 :v-adjust -0.2))
            (TypeParameter . ,(all-the-icons-faicon "arrows" :height 0.8 :v-adjust -0.05))
            (Template . ,(all-the-icons-material "format_align_left" :height 0.85 :v-adjust -0.2)))
          company-box-icons-alist 'company-box-icons-all-the-icons)))

(use-package company-statistics
  :defer 2
  :config
  (setq company-statistics-file (concat cache-dir "company-statistics-cache.el"))
  (company-statistics-mode))

(use-package fuzzy)

(use-package yasnippet
  :defer 2
  :init
  (add-to-list 'hippie-expand-try-functions-list 'yas-hippie-try-expand)
  :config
  (setq yas-triggers-in-field t
        yas-wrap-around-region t
        yas-snippet-dirs (list (concat user-emacs-directory "snippets")))
  (yas-global-mode)

  ;; disable yas minor mode map
  ;; use hippie-expand instead
  (setq yas-minor-mode-map (make-sparse-keymap))
  :general
  (tyrant-def "ty" 'yas-minor-mode)
  (general-def 'insert yas-minor-mode-map
    "\t"       'yas-maybe-expand))

(use-package yasnippet-snippets
  :after yasnippet
  :config
  (yasnippet-snippets-initialize)
  (yas-reload-all))

(use-package ivy-yasnippet
  :general
  (general-def '(insert normal)
    "M-/"      'ivy-yasnippet))

(use-package yatemplate
  :defer 2
  :config
  (yatemplate-fill-alist)
  (auto-insert-mode))


(provide 'editor-completion)
