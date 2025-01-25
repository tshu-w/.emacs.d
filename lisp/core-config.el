;;; core-config.el --- -*- lexical-binding: t; -*-

;; Copyright (C) 2020-2025  Tianshu Wang

;; Author: Tianshu Wang <wang@tianshu.me>

;;; Commentary:

;;; Code:

(use-package emacs
  :hook ((after-init . setup-font)
         (after-init . global-hl-line-mode)
         (after-init . midnight-mode))
  :init
  (setq user-full-name "Tianshu Wang"
        user-mail-address "wang@tianshu.me"

        initial-scratch-message nil   ;; "make scratch buffer empty"
        inhibit-startup-message t     ;; "disable splash screen"
        ;; smooth scrolling
        scroll-conservatively 101
        scroll-margin 2
        ;; no beep and visual blinking
        ring-bell-function 'ignore
        visible-bell nil
        ;; incress undo limit
        undo-limit 67108864           ;; 64mb.
        undo-strong-limit 100663296   ;; 96mb.
        undo-outer-limit 1006632960   ;; 960mb.

        delete-by-moving-to-trash t
        ffap-machine-p-known 'reject
        load-prefer-newer t
        sentence-end-double-space nil
        use-short-answers t
        warning-minimum-level :error
        word-wrap-by-category t)

  (setq-default tab-width 4
                fill-column 80)
  :config
  (set-language-environment 'utf-8)
  (set-default-coding-systems 'utf-8)

  (defvar default-font "MonoLisa")
  (defvar font-size 14)
  (defvar unicode-font "Noto Sans CJK SC")
  (defvar unicode-scale (/ 18.0 font-size))
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
      (set-fontset-font t 'symbol symbol-font nil 'prepend))))

(use-package autorevert
  :hook (after-init . global-auto-revert-mode)
  :config
  (setq global-auto-revert-non-file-buffers t
        auto-revert-verbose nil))

(use-package bookmark
  :defer t
  :config
  (setq bookmark-save-flag 1
        bookmark-fringe-mark nil))

(use-package compile
  :defer t
  :config
  (setq compilation-ask-about-save nil
        compilation-scroll-output 'first-error)

  (defvar-local compilation-original-buffer nil
    "The buffer where compile was originally called")

  (defun compilation-start-save-buffer (&rest _)
    "Save the buffer where compile was originally called."
    (let ((buffer (current-buffer)))
      (with-current-buffer next-error-last-buffer
        (set (make-local-variable 'compilation-original-buffer) buffer))))
  (advice-add 'compilation-start :after #'compilation-start-save-buffer))

(use-package dabbrev
  :defer t
  :config
  (setq dabbrev-abbrev-char-regexp "[A-Za-z-_]"
        dabbrev-ignored-buffer-regexps '("\\.\\(?:pdf\\|jpe?g\\|png\\)\\'")))

(use-package desktop
  :init (desktop-save-mode)
  :config
  (setq desktop-lazy-verbose nil
        desktop-load-locked-desktop t
        desktop-restore-eager 1
        desktop-save t)

  (dolist (param '(foreground-color background-color background-mode font cursor-color mouse-color))
    (push `(,param . :never) frameset-filter-alist))

  (defun desktop-restore-file-buffer@before (file-name _buffer-name _misc)
    "Restore a file buffer specified in a desktop file."
    (let ((jka-compr-file-name-regexp
           (car jka-compr-file-name-handler-entry)))
      (when (string-match-p epa-file-name-regexp file-name)
        (auto-encryption-mode 1))
      (when (string-match-p jka-compr-file-name-regexp file-name)
        (auto-compression-mode 1))))

  (advice-add 'desktop-restore-file-buffer :before #'desktop-restore-file-buffer@before))

(use-package dired
  :defer t
  :config
  (setq dired-auto-revert-buffer t
        dired-create-destination-dirs 'always
        dired-do-revert-buffer t
        dired-dwim-target t
        dired-listing-switches "-aBhl --group-directories-first"
        dired-recursive-copies 'always
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

(use-package epa
  :defer t
  :config
  (setq epa-file-select-keys 'auto
        epa-file-encrypt-to user-mail-address))

(use-package epg
  :defer t
  :config
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
  (setq flymake-no-changes-timeout nil
        flymake-fringe-indicator-position 'right-fringe
        flymake-margin-indicator-position 'right-margin))

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

(use-package prog-mode
  :defer t
  :config
  (global-prettify-symbols-mode 1)

  (add-hook 'prog-mode-hook #'goto-address-prog-mode)
  (add-hook 'prog-mode-hook #'bug-reference-prog-mode)
  (add-hook 'prog-mode-hook #'subword-mode))

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
        recentf-max-saved-items 100
        recentf-initialize-file-name-history nil)

  (add-to-list 'recentf-exclude (recentf-expand-file-name (straight--emacs-dir "straight"))))

(use-package repeat
  :hook (after-init . repeat-mode)
  :config
  (setq repeat-keep-prefix t
        repeat-timeout 5
        repeat-exit-key (kbd "RET")))

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
                              (server-start))))
  :config
  (defun server-remove-kill-buffer-hook ()
    "Remove prompt if the file is opened in other clients."
    (remove-hook 'kill-buffer-query-functions 'server-kill-buffer-query-function))
  (add-hook 'server-visit-hook #'server-remove-kill-buffer-hook))

(use-package simple
  :config
  ;; use spaces instead of tabs
  (setq-default indent-tabs-mode nil)

  (setq column-number-mode t
        delete-trailing-lines nil
        eval-expression-print-length nil
        eval-expression-print-level nil
        kill-do-not-save-duplicates t
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
        vc-handled-backends '(SVN Git)
        tramp-otp-password-prompt-regexp
        (rx-to-string
            `(: bol (* nonl)
                 (group (| "Verification code" "OTP"))
                 (* nonl) (any . ,tramp-compat-password-colon-equivalents) (* blank))))

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


(setq custom-file (no-littering-expand-var-file-name "custom.el"))
(load custom-file :no-error-if-file-is-missing)


(provide 'core-config)
;;; core-config.el ends here
