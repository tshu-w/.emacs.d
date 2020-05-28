;;; core-config.el --- -*- lexical-binding: t; -*-

;; Copyright (C) 2020  Tianshu Wang

;; Author: Tianshu Wang <volekingsg@gmail.com>

;;; Commentary:

;;; Code:

(set-language-environment 'utf-8)
(set-default-coding-systems 'utf-8)

(setq user-mail-address "volekingsg@gmail.com")

(when (memq window-system '(mac ns))
  (setq url-proxy-services
        '(("no_proxy" . "^\\(localhost\\|10\\..*\\|192\\.168\\..*\\)")
          ("http" . "127.0.0.1:6152")
          ("https" . "127.0.0.1:6152")))
  (setq mac-command-modifier 'hyper
        mac-option-modifier  'meta)
  (setq insert-directory-program "/usr/local/bin/gls"
        dired-listing-switches "-aBhl --group-directories-first"))

(defun set-monospaced-font (english chinese english-size chinese-size)
  "Set the monospaced font size when mixed CHINESE and ENGLISH words."
  (set-face-attribute 'default nil :font
                      (format   "%s:pixelsize=%d"  english english-size))
  (dolist (charset '(kana han cjk-misc bopomofo))
    (set-fontset-font (frame-parameter nil 'font) charset
                      (font-spec :family chinese :size chinese-size))))
(when (display-graphic-p)
  (set-monospaced-font "Source Code Pro" "PingFang SC" 14 16))

(setq initial-major-mode 'text-mode ;; "start with the *scratch* buffer in text mode"
      initial-scratch-message nil   ;; "make scratch buffer empty"
      inhibit-startup-message t)    ;; "disable splash screen"

(setq-default indent-tabs-mode nil ;; use only spaces and no tabs
              tab-width 2
              standard-indent 2)

;; auto fill breaks line beyond buffer's fill-column
(setq-default fill-column 80)

;; no beep and visual blinking
(setq ring-bell-function 'ignore
      visible-bell nil)

;; Highlight and allow to open http link at point in programming buffers
;; goto-address-prog-mode only highlights links in strings and comments
(add-hook 'prog-mode-hook 'goto-address-prog-mode)
;; Highlight and follow bug references in comments and strings
(add-hook 'prog-mode-hook 'bug-reference-prog-mode)

;; important for golden-ratio to better work
(setq window-combination-resize t)

;; scroll compilation to first error or end
;; (setq compilation-scroll-output 'first-error)

;; Use system trash for file deletion.
;; This should work on Windows and Linux distros.
(setq delete-by-moving-to-trash t)
;; Enable built-in trash support via finder API if available
;; (only on Emacs macOS Port)
(when (boundp 'mac-system-move-file-to-trash-use-finder)
  (setq mac-system-move-file-to-trash-use-finder t))

;; remove the GUI elements
(menu-bar-mode -1)
(scroll-bar-mode -1)
(tool-bar-mode -1)
(tooltip-mode -1)
;; highlight current line
(global-hl-line-mode t)
;; no blink
(blink-cursor-mode 0)

(global-prettify-symbols-mode 1)

;; Single space between sentences is more widespread than double
(setq sentence-end-double-space nil)

;; smooth scrolling
(setq scroll-conservatively 101
      scroll-margin 5)

;; draw underline lower
(setq x-underline-at-descent-line t)

;; When emacs asks for "yes" or "no", let "y" or "n" suffice
(fset 'yes-or-no-p 'y-or-n-p)

(defun server-remove-kill-buffer-hook ()
  "Remove prompt if the file is opened in other clients."
  (remove-hook 'kill-buffer-query-functions 'server-kill-buffer-query-function))
(add-hook 'server-visit-hook 'server-remove-kill-buffer-hook)

;; don't load outdated compiled files.
(setq load-prefer-newer t)

(defun make-directory-maybe ()
  "Create parent directory if not exists while visiting file."
  (let ((dir (file-name-directory buffer-file-name)))
    (unless (file-exists-p dir)
      (if (y-or-n-p (format "Directory %s does not exist,do you want you create it? " dir))
          (make-directory dir t)
        (keyboard-quit)))))
(add-to-list 'find-file-not-found-functions 'make-directory-maybe nil #'eq)

(defun check-large-file ()
  "Check when opening large files - literal file open."
  (let* ((filename (buffer-file-name))
         (size (nth 7 (file-attributes filename))))
    (when (and
           (not (memq major-mode '(archive-mode tar-mode jka-compr git-commit-mode image-mode
                                                doc-view-mode doc-view-mode-maybe ebrowse-tree-mode
                                                pdf-view-mode tags-table-mode fundamental-mode)))
           size (> size (* 1024 1024 1))
           (y-or-n-p (format (concat "%s is a large file, open literally to "
                                     "avoid performance issues?")
                             filename)))
      (setq buffer-read-only t)
      (buffer-disable-undo)
      (fundamental-mode))))
;; Prompt to open file literally if large file.
(add-hook 'find-file-hook 'check-large-file)

;; seems pointless to warn. There's always undo.
(put 'narrow-to-region 'disabled nil)
(put 'narrow-to-page 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)
(put 'dired-find-alternate-file 'disabled nil)

;; (setq ad-redefinition-action 'accept)

(custom-set-faces '(nobreak-space ((t nil))))

;; don't let the cursor go into minibuffer prompt
;; Tip taken from Xah Lee: http://ergoemacs.org/emacs/emacs_stop_cursor_enter_prompt.html
(setq minibuffer-prompt-properties
      '(read-only t point-entered minibuffer-avoid-prompt face minibuffer-prompt))

(use-package autorevert
  :init
  (setq global-auto-revert-non-file-buffers t
        auto-revert-verbose nil)
  (global-auto-revert-mode)
  (add-to-list 'global-auto-revert-ignore-modes 'Buffer-menu-mode))

(use-package bookmark
  :config
  (setq bookmark-save-flag 1))

(use-package calendar
  :init
  (setq calendar-location-name "Beijing"
        calendar-latitude 39.90
        calendar-longitude 116.40))

(use-package desktop
  :init
  (when (member "--restore-desktop" command-line-args)
    (add-hook 'emacs-startup-hook 'desktop-read)
    (delete "--restore-desktop" command-line-args))

  (defun restart-emacs-restore-desktop (&optional args)
    "Restart emacs and restore desktop."
    (interactive)
    (restart-emacs (cons "--restore-desktop" args)))
  :config
  (setq desktop-save t
        desktop-load-locked-desktop t)

  (defun desktop-save-without-prompts ()
    "Save desktop without annoying prompts."
    (interactive)
    (setq desktop-file-modtime (nth 5 (file-attributes (desktop-full-file-name))))
    (desktop-save-in-desktop-dir))

  (add-hook 'auto-save-hook 'desktop-save-without-prompts)
  (add-hook 'kill-emacs-hook 'desktop-save-without-prompts))

(use-package dired
  :commands (dired dired-jump dired-jump-other-window)
  :config (setq dired-dwim-target t))

(use-package electric
  :config
  ;; (setq electric-pair-inhibit-predicate 'electric-pair-conservative-inhibit)
  (electric-pair-mode))

(use-package ediff
  :hook (ediff-quit . winner-undo)
  :config
  ;; first we set some sane defaults
  (setq-default ediff-window-setup-function 'ediff-setup-windows-plain
                ;; emacs is evil and decrees that vertical shall henceforth be horizontal
                ediff-split-window-function 'split-window-horizontally
                ediff-merge-split-window-function 'split-window-horizontally)
  ;; show org ediffs unfolded
  (require 'outline)
  (add-hook 'ediff-prepare-buffer-hook #'show-all)
  ;; restore window layout when done
  (add-hook 'ediff-quit-hook #'winner-undo))

(use-package eldoc
  :config
  ;; enable eldoc in `eval-expression'
  (add-hook 'eval-expression-minibuffer-setup-hook #'eldoc-mode)
  ;; enable eldoc in IELM
  (add-hook 'ielm-mode-hook #'eldoc-mode))

(use-package epa
  :init
  (epa-file-enable))

(use-package ffap
  :init
  (setq ffap-machine-p-known 'reject))

(use-package files
  :config
  (setq make-backup-files nil ;; don't create backup~ files
        revert-without-query '(".*") ;; disable revert query
        auto-save-default t
        auto-save-file-name-transforms
        `((".*" ,(no-littering-expand-var-file-name "auto-save/") t))))

(use-package help
  :config
  ;; keep focus while navigating help buffers
  (setq help-window-select 't))

(use-package imenu
  :commands imenu)

(use-package recentf
  :commands recentf-save-list
  :hook (after-init . recentf-mode)
  :config
  (setq recentf-max-saved-items 100
        recentf-auto-cleanup 'never)

  (add-to-list 'recentf-exclude (recentf-expand-file-name package-user-dir))
  (add-to-list 'recentf-exclude "COMMIT_EDITMSG\\'"))

(use-package savehist
  :hook (after-init . savehist-mode)
  :config
  ;; minibuffer history
  (setq enable-recursive-minibuffers t ; allow commands in minibuffers
        history-length 100
        savehist-additional-variables '(evil-jumps-history
                                        mark-ring
                                        global-mark-ring
                                        search-ring
                                        regexp-search-ring
                                        extended-command-history)
        savehist-autosave-interval 60))

(use-package saveplace
  :hook (after-init . save-place-mode))

(use-package server
  :hook (after-init . (lambda () (ignore-errors (server-mode)))))

(use-package simple
  :config
  (add-hook 'before-save-hook #'delete-trailing-whitespace)

  (setq column-number-mode t
        save-interprogram-paste-before-kill t ;; save clipboard contents into kill-ring before replace them
        eval-expression-print-length nil
        eval-expression-print-level nil))

(use-package subword
  :hook (prog-mode . subword-mode))

(use-package time
  :config
  (setq display-time-24hr-format t
        display-time-default-load-average nil))

(use-package tramp
  :init
  (setq remote-file-name-inhibit-cache nil
        tramp-default-method "ssh"
        tramp-verbose 1
        tramp-ssh-controlmaster-options ""))

(use-package undo-tree
  :ensure t
  :init
  (setq undo-tree-visualizer-timestamps t
        undo-tree-visualizer-diff t
        ;; 10X bump of the undo limits to avoid issues with premature
        ;; Emacs GC which truncages the undo history very aggresively
        undo-limit 800000
        undo-strong-limit 12000000
        undo-outer-limit 120000000)

  (setq undo-tree-auto-save-history t)
  :config
  ;; TODO fix upstream
  (defun undo-tree-restore-default ()
    "Restore diff window after quit."
    (setq undo-tree-visualizer-diff t))
  (advice-add #'undo-tree-visualizer-quit :after #'undo-tree-restore-default))


(use-package whitespace
  :init
  (defun show-trailing-whitespace ()
    (set-face-attribute 'trailing-whitespace nil
                        :background
                        (face-attribute 'font-lock-comment-face
                                        :foreground))
    (setq show-trailing-whitespace t))

  (add-hook 'prog-mode-hook #'show-trailing-whitespace)

  (defun set-whitespace-style-for-diff ()
    "Whitespace configuration for `diff-mode'"
    (setq-local whitespace-style '(face
                                   tabs
                                   tab-mark
                                   spaces
                                   space-mark
                                   trailing
                                   indentation::space
                                   indentation::tab
                                   newline
                                   newline-mark)))

  (add-hook 'diff-mode-hook   #'whitespace-mode)
  (add-hook 'diff-mode-hook   #'set-whitespace-style-for-diff)
  :config
  (set-face-attribute 'whitespace-space nil
                      :background nil
                      :foreground (face-attribute 'font-lock-warning-face
                                                  :foreground))
  (set-face-attribute 'whitespace-tab nil
                      :background nil)
  (set-face-attribute 'whitespace-indentation nil
                      :background nil))

(use-package winner
  :commands (winner-undo winner-redo)
  :hook (after-init . winner-mode)
  :config
  (setq winner-boring-buffers '("*Completions*"
                                "*Compile-Log*"
                                "*inferior-lisp*"
                                "*Fuzzy Completions*"
                                "*Apropos*"
                                "*Help*"
                                "*cvs*"
                                "*Buffer List*"
                                "*Ibuffer*"
                                "*esh command on file*")))


(setq-default custom-file (no-littering-expand-var-file-name "custom.el"))
(when (file-exists-p custom-file)
  (load custom-file))


(provide 'core-config)
;;; core-config.el ends here
