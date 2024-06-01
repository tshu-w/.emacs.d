;;; core-config.el --- -*- lexical-binding: t; -*-

;; Copyright (C) 2020-2023  Tianshu Wang

;; Author: Tianshu Wang <wang@tianshu.me>

;;; Commentary:

;;; Code:

(set-language-environment 'utf-8)
(set-default-coding-systems 'utf-8)

(setq user-full-name "Tianshu Wang"
      user-mail-address "wang@tianshu.me")

(defvar default-font "SauceCodePro NFM")
(defvar font-size 14)
(defvar unicode-font "Noto Sans CJK SC")
(defvar unicode-scale (/ 16.0 font-size))
(defvar emoji-font "Noto Color Emoji")
(defvar symbol-font "Noto Sans Symbols")

(when (eq system-type 'darwin)
  (setq ns-pop-up-frames nil
        frame-resize-pixelwise t)

  (setq unicode-font "PingFang SC"
        emoji-font "Apple Color Emoji"
        symbol-font "Apple Symbols"))

(defun setup-font (&rest args)
  (set-face-attribute 'default nil :font (font-spec :family default-font :size font-size))

  (when (fboundp 'set-fontset-font)
    (dolist (charset '(kana han cjk-misc bopomofo))
      (set-fontset-font t charset unicode-font))
    (add-to-list 'face-font-rescale-alist `(,unicode-font . ,unicode-scale))
    (set-fontset-font t 'emoji emoji-font nil 'prepend)
    (set-fontset-font t 'symbol symbol-font nil 'prepend)))

(add-hook 'after-init-hook #'setup-font)

(setq initial-scratch-message nil   ;; "make scratch buffer empty"
      inhibit-startup-message t)    ;; "disable splash screen"

;; use spaces instead of tabs
(setq-default indent-tabs-mode nil)

;; change `tab-width' and `fill-column'
(setq-default tab-width 4
              fill-column 80)

;; no beep and visual blinking
(setq ring-bell-function 'ignore
      visible-bell nil)

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

;; Highlight and allow to open http link at point in programming buffers
;; goto-address-prog-mode only highlights links in strings and comments
(add-hook 'prog-mode-hook #'goto-address-prog-mode)
;; Highlight and follow bug references in comments and strings
(add-hook 'prog-mode-hook #'bug-reference-prog-mode)
;; enable subword-mode in prog-mode
(add-hook 'prog-mode-hook #'subword-mode)

;; scroll compilation to first error or end
(setq compilation-scroll-output 'first-error)

;; Don't try to ping things that look like domain names
(setq ffap-machine-p-known 'reject)

;; Use system trash for file deletion.
(setq delete-by-moving-to-trash t)

;; autosave each change
(setq bookmark-save-flag 1)

;; don't set a fringe mark at bookmarked lines
(setq bookmark-set-fringe-mark nil)

;; keep focus while navigating help buffers
(setq help-window-select t)

;; When emacs asks for "yes" or "no", let "y" or "n" suffice
;; (fset 'yes-or-no-p 'y-or-n-p)
(setq use-short-answers t)

;; don't load outdated compiled files.
(setq load-prefer-newer t)

;; don't save duplicates in kill-ring
(setq kill-do-not-save-duplicates t)

;; break lines after more characters
(setq word-wrap-by-category t)

;; suppress annoying warnings
(setq warning-minimum-level :error)

(defun server-remove-kill-buffer-hook ()
  "Remove prompt if the file is opened in other clients."
  (remove-hook 'kill-buffer-query-functions 'server-kill-buffer-query-function))
(add-hook 'server-visit-hook #'server-remove-kill-buffer-hook)

;; Do not allow the cursor in the minibuffer prompt
(setq minibuffer-prompt-properties
      '(read-only t cursor-intangible t face minibuffer-prompt))
(add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)

(use-package autorevert
  :hook (after-init . global-auto-revert-mode)
  :config
  (setq global-auto-revert-non-file-buffers t
        auto-revert-verbose nil))

(use-package dabbrev
  :defer t
  :config
  (setq dabbrev-abbrev-char-regexp "[A-Za-z-_]"
        dabbrev-ignored-buffer-regexps '("\\.\\(?:pdf\\|jpe?g\\|png\\)\\'")))

(use-package desktop
  :commands restart-emacs-without-desktop
  :init (desktop-save-mode)
  :config
  ;; inhibit no-loaded prompt
  (setq desktop-file-modtime (file-attribute-modification-time
                              (file-attributes
                               (desktop-full-file-name)))
        desktop-lazy-verbose nil
        desktop-load-locked-desktop t
        desktop-restore-eager 1
        desktop-save t)

  (dolist (param '(foreground-color background-color background-mode font cursor-color mouse-color))
    (push `(,param . :never) frameset-filter-alist))

  (defun desktop-read@inhibit-message (fn)
    "Inhibit `desktop-read' message"
    (let ((inhibit-message t))
      (funcall fn)))
  (advice-add 'desktop-read :around #'desktop-read@inhibit-message))

(use-package dired
  :defer t
  :config
  (setq dired-auto-revert-buffer t
        dired-create-destination-dirs 'always
        dired-do-revert-buffer t
        dired-dwim-target t
        dired-listing-switches "-aBhl --group-directories-first"
        dired-vc-rename-file t))

(use-package display-line-numbers
  :hook ((text-mode prog-mode conf-mode) . display-line-numbers-mode)
  :config
  (setq-default display-line-numbers-type 'relative
                display-line-numbers-width-start t))

(use-package doc-view
  :defer t
  :config
  (setq doc-view-resolution 400))

(use-package ediff
  :defer t
  :config
  (setq-default ediff-window-setup-function 'ediff-setup-windows-plain
                ediff-split-window-function 'split-window-horizontally
                ediff-merge-split-window-function 'split-window-horizontally))

(use-package elec-pair
  :hook (after-init . electric-pair-mode))

(use-package epg
  :defer t
  :config
  ;; TODO: debug `epg-wait-for-process' hang
  (fset 'epg-wait-for-status 'ignore)
  (setq epg-gpg-home-directory (getenv "GNUPGHOME")))

(use-package files
  :hook ((before-save . delete-trailing-whitespace)
         (after-save . executable-make-buffer-file-executable-if-script-p))
  :init
  (setq find-file-visit-truename t
        make-backup-files nil        ;; don't create backup~ files
        revert-without-query '(".*") ;; disable revert query
        enable-remote-dir-locals t)
  :config
  ;; see document of `move-file-to-trash'
  (defun system-move-file-to-trash (filename)
    (process-file-shell-command
     (format "trash %S" (file-local-name filename))))

  (defun make-directory-maybe ()
    "Create parent directory if not exists while visiting file."
    (let ((dir (file-name-directory buffer-file-name)))
      (unless (file-exists-p dir)
        (if (y-or-n-p (format "Directory %s does not exist,do you want you create it? " dir))
            (make-directory dir t)
          (keyboard-quit)))))
  (add-to-list 'find-file-not-found-functions 'make-directory-maybe nil #'eq))

(use-package flymake
  :hook (prog-mode . flymake-mode)
  :config
  (setq flymake-no-changes-timeout nil))

(use-package newcomment
  :commands comment-or-uncomment
  :config
  (defun comment-or-uncomment (n)
    (interactive "*p")
    (if (or (region-active-p)
            (save-excursion
              (beginning-of-line)
              (looking-at "\\s-*$")))
        (call-interactively 'comment-dwim)
      (comment-or-uncomment-region
       (line-beginning-position) (line-end-position n)))))

(use-package project
  :defer t
  :config
  (setq project-vc-merge-submodules nil
        project-switch-commands '((project-switch-to-buffer "Find buffer")
                                  (project-find-file "Find file")
                                  (project-find-regexp "Find regexp")
                                  (project-find-dir "Find directory")))

  (defcustom project-root-files '(".project")
    "Files that indicate the root of a project."
    :group 'project
    :type '(repeat string))

  (defun project-try-root (dir)
    "Search up the `DIR' for `project-root-files'."
    (when-let ((root
                (seq-some
                 (lambda (n) (locate-dominating-file dir n))
                 project-root-files)))
      (cons 'transient (expand-file-name root))))

  (add-to-list 'project-find-functions 'project-try-root t))

(use-package recentf
  :hook (after-init . recentf-mode)
  :config
  (setq recentf-auto-cleanup 'never
        recentf-filename-handlers '(abbreviate-file-name)
        recentf-max-saved-items 100
        recentf-initialize-file-name-history nil)

  (add-to-list 'recentf-exclude `(recentf-expand-file-name ,(straight--emacs-dir "straight"))))

(use-package savehist
  :hook (after-init . savehist-mode)
  :config
  (setq enable-recursive-minibuffers t ; allow commands in minibuffers
        history-length 100
        savehist-autosave-interval nil
        savehist-additional-variables '(evil-jumps-history
                                        mark-ring global-mark-ring
                                        search-ring regexp-search-ring
                                        extended-command-history))

  (add-hook 'savehist-save-hook
            (defun savehist-unpropertize-variables-h ()
              "Remove text properties from `kill-ring' to reduce savehist cache size."
              (setq kill-ring
                    (mapcar #'substring-no-properties
                            (cl-remove-if-not #'stringp kill-ring))
                    register-alist
                    (cl-loop for (reg . item) in register-alist
                             if (stringp item)
                             collect (cons reg (substring-no-properties item))
                             else collect (cons reg item)))))

  (add-hook 'savehist-save-hook
            (defun savehist-remove-unprintable-registers-h ()
              "Remove unwriteable registers (e.g. containing window configurations).
Otherwise, `savehist' would discard `register-alist' entirely if we don't omit
the unwritable tidbits."
              ;; Save new value in the temp buffer savehist is running
              ;; `savehist-save-hook' in. We don't want to actually remove the
              ;; unserializable registers in the current session!
              (setq-local register-alist
                          (cl-remove-if-not #'savehist-printable register-alist)))))

(use-package saveplace
  :hook (after-init . save-place-mode))

(use-package server
  :commands server-running-p
  :hook (after-init . (lambda () (unless (server-running-p)
                              (server-start)))))

(use-package simple
  :config
  (setq column-number-mode t
        delete-trailing-lines nil
        eval-expression-print-length nil
        eval-expression-print-level nil
        next-error-message-highlight t
        ;; save clipboard contents into kill-ring before replace them
        save-interprogram-paste-before-kill t))

(use-package tab-bar
  :hook (after-init . tab-bar-mode)
  :config
  (setq tab-bar-show nil
        tab-bar-close-button-show nil
        tab-bar-tab-hints t
        tab-bar-new-tab-choice "*scratch*"
        tab-bar-select-tab-modifiers '(super)
        tab-prefix-map (make-sparse-keymap)
        tab-bar-format '(tab-bar-format-align-right tab-bar-format-tabs tab-bar-separator))

  (defun tab-bar-switch-to-tab@override (name)
    "Like `tab-bar-switch-to-tab', but allow for the creation of a new, named tab on the fly."
    (interactive
     (let* ((recent-tabs (mapcar (lambda (tab)
                                   (alist-get 'name tab))
                                 (tab-bar--tabs-recent))))
       (list (completing-read (format-prompt "Switch to tab by name"
                                             (car recent-tabs))
                              recent-tabs nil nil nil nil recent-tabs))))
    (if-let ((tab-number (tab-bar--tab-index-by-name name)))
        (tab-bar-select-tab (1+ tab-number))
      (tab-bar-new-tab)
      (tab-bar-rename-tab name)))
  (advice-add #'tab-bar-switch-to-tab :override #'tab-bar-switch-to-tab@override)

  (defun project-open-in-tab (project)
    (interactive (list (project-prompt-project-dir)))
    (if-let ((tab-number (tab-bar--tab-index-by-name
                          (file-name-nondirectory (directory-file-name project)))))
        (tab-bar-select-tab (1+ tab-number))
      (tab-bar-new-tab)
      (project-switch-project project)
      (tab-bar-rename-tab (file-name-nondirectory (directory-file-name project))))))

(use-package tramp
  :defer t
  :commands tramp-file-local-name
  :config
  (setq remote-file-name-inhibit-cache 60
        tramp-verbose 1
        vc-handled-backends '(SVN Git))

  (add-to-list 'tramp-remote-path 'tramp-own-remote-path)

  ;; https://github.com/magit/magit/issues/4720
  (when (eq system-type 'darwin)
    (defun tramp-send-command@filter-args (args)
      (cl-destructuring-bind (vec command &optional neveropen nooutput) args
        (let ((new-command
               (if (string= "stty -icrnl -icanon min 1 time 0" command)
                   "stty -icrnl"
                 command)))
          (list vec new-command neveropen nooutput))))
    (advice-add 'tramp-send-command :filter-args #'tramp-send-command@filter-args)))

(use-package xref
  :defer t
  :config
  (setq xref-prompt-for-identifier '(not xref-find-definitions
                                         xref-find-definitions-other-window
                                         xref-find-definitions-other-frame
                                         xref-find-references))

  (setq xref-search-program 'ripgrep
        xref-show-definitions-function #'xref-show-definitions-completing-read
        xref-show-xrefs-function       #'xref-show-definitions-completing-read))

(use-package whitespace
  :hook ((prog-mode . show-trailing-whitespace)
         (diff-mode . whitespace-mode))
  :config
  (defun show-trailing-whitespace ()
    (set-face-attribute 'trailing-whitespace nil :background
                        (face-attribute 'font-lock-comment-face
                                        :foreground))
    (setq show-trailing-whitespace t)))

(use-package winner
  :commands (winner-undo winner-redo)
  :hook (after-init . winner-mode)
  :init
  (setq winner-dont-bind-my-keys t)
  :config
  (setq winner-boring-buffers-regexp "\\*.*\\*"))


(setq-default custom-file (no-littering-expand-var-file-name "custom.el"))
(when (file-exists-p custom-file)
  (load custom-file))


(provide 'core-config)
;;; core-config.el ends here
