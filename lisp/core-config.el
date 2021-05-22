;;; core-config.el --- -*- lexical-binding: t; -*-

;; Copyright (C) 2020-2021  Tianshu Wang

;; Author: Tianshu Wang <wang@tianshu.me>

;;; Commentary:

;;; Code:

(set-language-environment 'utf-8)
(set-default-coding-systems 'utf-8)

(setq user-full-name "Tianshu Wang"
      user-mail-address "wang@tianshu.me")

(when (memq window-system '(mac ns))
  (setq url-proxy-services
        '(("no_proxy" . "^\\(localhost\\|10\\..*\\|192\\.168\\..*\\)")
          ("http" . "127.0.0.1:6152")
          ("https" . "127.0.0.1:6152")))

  (setq mac-command-modifier 'hyper
        mac-option-modifier  'meta)

  (setq ns-pop-up-frames nil)
  (setq insert-directory-program "/usr/local/bin/gls"
        dired-listing-switches "-aBhl --group-directories-first")

  (let ((spec (font-spec :family "Apple Color Emoji")))
    (set-fontset-font t nil spec nil 'append)
    ;; Work around lots of font lookups in emoji compositions.
    (set-fontset-font t #xFE0E spec)	; Variation Selector 15
    (set-fontset-font t #xFE0F spec)	; Variation Selector 16
    (set-fontset-font t '(#x1F1E6 . #x1F1FF) spec) ; Regional Indicator Syms
    (set-fontset-font t '(#x1F3FB . #x1F3FF) spec)) ; Emoji Modifiers
  )

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

(setq-default c-basic-offset 4)

;; auto fill breaks line beyond buffer's fill-column
(setq-default fill-column 80)

;; no beep and visual blinking
(setq ring-bell-function 'ignore
      visible-bell nil)

(setq frame-resize-pixelwise t)

;; Highlight and allow to open http link at point in programming buffers
;; goto-address-prog-mode only highlights links in strings and comments
(add-hook 'prog-mode-hook #'goto-address-prog-mode)
;; Highlight and follow bug references in comments and strings
(add-hook 'prog-mode-hook #'bug-reference-prog-mode)

;; important for golden-ratio to better work
(setq window-combination-resize t)

;; scroll compilation to first error or end
(setq compilation-scroll-output 'first-error)

;; Don't try to ping things that look like domain names
(setq ffap-machine-p-known 'reject)

;; Use system trash for file deletion.
(setq delete-by-moving-to-trash t)

;; autosave each change
(setq bookmark-save-flag 1)

;; disable bookmark fontify
(setq bookmark-fontify nil)

;; keep focus while navigating help buffers
(setq help-window-select t)

;; highlight current line
(global-hl-line-mode 1)
;; no blink
(blink-cursor-mode 0)
;; prettify symbols
(global-prettify-symbols-mode 1)

;; Single space between sentences is more widespread than double
(setq sentence-end-double-space nil)

;; smooth scrolling
(setq scroll-conservatively 101
      scroll-margin 2)

;; draw underline lower
(setq x-underline-at-descent-line t)

;; When emacs asks for "yes" or "no", let "y" or "n" suffice
;; (fset 'yes-or-no-p 'y-or-n-p)
(setq use-short-answers t)

(defun server-remove-kill-buffer-hook ()
  "Remove prompt if the file is opened in other clients."
  (remove-hook 'kill-buffer-query-functions 'server-kill-buffer-query-function))
(add-hook 'server-visit-hook #'server-remove-kill-buffer-hook)

;; don't load outdated compiled files.
(setq load-prefer-newer t)

;; seems pointless to warn. There's always undo.
(put 'narrow-to-region 'disabled nil)
(put 'narrow-to-page 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)
(put 'dired-find-alternate-file 'disabled nil)

;; inhibit annoying warnings
(setq warning-minimum-log-level :error)

;; don't let the cursor go into minibuffer prompt
;; Tip taken from Xah Lee: http://ergoemacs.org/emacs/emacs_stop_cursor_enter_prompt.html
;; (setq minibuffer-prompt-properties
;;       '(read-only t point-entered minibuffer-avoid-prompt face minibuffer-prompt))

(use-package autorevert
  :hook (after-init . global-auto-revert-mode)
  :config
  (setq global-auto-revert-non-file-buffers t
        auto-revert-verbose nil)
  (add-to-list 'global-auto-revert-ignore-modes 'Buffer-menu-mode))

(use-package desktop
  :hook ((auto-save kill-emacs) . desktop-save-without-prompts)
  :commands (restart-emacs-restore-desktop desktop-save-without-prompts)
  :init
  (when (member "--restore-desktop" command-line-args)
    (add-hook 'emacs-startup-hook 'desktop-read)
    (delete "--restore-desktop" command-line-args))

  (setq desktop-buffers-not-to-save "^$"
        desktop-load-locked-desktop t
        desktop-restore-frames nil
        desktop-save t)
  :config
  (defun restart-emacs-restore-desktop (&optional args)
    "Restart emacs and restore desktop."
    (interactive)
    (restart-emacs (cons "--restore-desktop" args)))

  (defun desktop-save-without-prompts ()
    "Save desktop without annoying prompts."
    (interactive)
    (setq desktop-file-modtime (nth 5 (file-attributes (desktop-full-file-name))))
    (let ((inhibit-message t))
      (desktop-save-in-desktop-dir))))

(use-package dired
  :commands (dired dired-jump dired-jump-other-window)
  :config
  (setq dired-dwim-target t)

  (defun dired-show-hide-dotfile ()
    "Show/hide dotfiles."
    (interactive)
    (if (or (not (boundp 'dired-dotfiles-show-p)) dired-dotfiles-show-p)
        (progn
          (setq-local dired-dotfiles-show-p nil)
          (dired-mark-files-regexp "^\\.")
          (dired-do-kill-lines))
      (revert-buffer)
      (setq-local dired-dotfiles-show-p t)))
  (advice-add 'dired-do-print :override #'dired-show-file-type))

(use-package electric
  :hook (after-init . electric-pair-mode))

(use-package ediff
  :hook ((ediff-quit . winner-undo)
         (ediff-prepare-buffer . outline-show-all))
  :config
  ;; first we set some sane defaults
  (setq-default ediff-window-setup-function 'ediff-setup-windows-plain
                ;; emacs is evil and decrees that vertical shall henceforth be horizontal
                ediff-split-window-function 'split-window-horizontally
                ediff-merge-split-window-function 'split-window-horizontally))

(use-package eldoc
  ;; enable eldoc in `eval-expression' and IELM
  :hook ((eval-expression-minibuffer-setup ielm-mode) . eldoc-mode))

(use-package files
  :init
  (setq auto-save-default t
        auto-save-file-name-transforms `(("\\`/[^/]*:\\([^/]*/\\)*\\([^/]*\\)\\'"
                                          ,(no-littering-expand-var-file-name "auto-save/") t))
        auto-save-visited-interval 60
        make-backup-files nil        ;; don't create backup~ files
        revert-without-query '(".*") ;; disable revert query
        enable-remote-dir-locals t)
  :config
  ;; Prompt to open file literally if large file.
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
  (add-hook 'find-file-hook #'check-large-file)

  ;; TODO: find a better implementation or report issue for it
  ;; https://github.com/emacs-mirror/emacs/commit/06585bb939ed61574a4b79455c58cab02f11f0fc
  (defun system-move-file-to-trash (filename)
    (if (file-remote-p filename)
        (with-parsed-tramp-file-name
         filename nil
         (tramp-flush-file-properties v localname)
         (tramp-send-command
          v (format "%s %s"
	                  (tramp-get-remote-trash v)
	                  (tramp-shell-quote-argument localname))))
      (call-process "trash" nil nil nil filename)))

  (defun make-directory-maybe ()
    "Create parent directory if not exists while visiting file."
    (let ((dir (file-name-directory buffer-file-name)))
      (unless (file-exists-p dir)
        (if (y-or-n-p (format "Directory %s does not exist,do you want you create it? " dir))
            (make-directory dir t)
          (keyboard-quit)))))
  (add-to-list 'find-file-not-found-functions 'make-directory-maybe nil #'eq))

(use-package imenu)

(use-package newcomment
  :commands comment-or-uncomment
  :config
  (defun comment-or-uncomment ()
    (interactive)
    (if (region-active-p)
        (comment-or-uncomment-region
         (region-beginning) (region-end))
      (if (save-excursion
            (beginning-of-line)
            (looking-at "\\s-*$"))
          (call-interactively 'comment-dwim)
        (comment-or-uncomment-region
         (line-beginning-position) (line-end-position))))))

(use-package recentf
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
  :hook (after-init . server-mode))

(use-package simple
  :hook (before-save . delete-trailing-whitespace)
  :config
  (setq column-number-mode t
        delete-trailing-lines nil
        eval-expression-print-length nil
        eval-expression-print-level nil
        ;; save clipboard contents into kill-ring before replace them
        save-interprogram-paste-before-kill t))

(use-package so-long
  :hook (after-init . global-so-long-mode))

(use-package subword
  :hook (prog-mode . subword-mode))

(use-package tramp
  :defer t
  :config
  (setq remote-file-name-inhibit-cache 60
        tramp-default-method "ssh"
        tramp-verbose 1
        tramp-use-ssh-controlmaster-options nil
        vc-handled-backends '(SVN Git))

  (add-to-list 'tramp-remote-path 'tramp-own-remote-path))

(use-package xref
  :ensure nil
  :defer t
  :config
  (setq xref-prompt-for-identifier '(not xref-find-definitions
                                         xref-find-definitions-other-window
                                         xref-find-definitions-other-frame
                                         xref-find-references
                                         jump-to-definition
                                         jump-to-definition-other-window
                                         jump-to-reference
                                         jump-to-reference-other-window))

  (setq xref-show-definitions-function #'xref-show-definitions-completing-read
        xref-show-xrefs-function       #'xref-show-definitions-completing-read))

(use-package whitespace
  :hook ((prog-mode . show-trailing-whitespace)
         (diff-mode . whitespace-mode)
         (diff-mode . set-whitespace-style-for-diff))
  :config
  (set-face-attribute 'whitespace-space nil :background nil)
  (set-face-attribute 'whitespace-tab nil :background nil)
  (set-face-attribute 'whitespace-indentation nil :background nil)

  (defun show-trailing-whitespace ()
    (set-face-attribute 'trailing-whitespace nil :background
                        (face-attribute 'font-lock-comment-face
                                        :foreground))
    (setq show-trailing-whitespace t))

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
                                   newline-mark))))

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
