;;; editor-vsc.el --- -*- lexical-binding: t; -*-

;; Copyright (C) 2020-2025  Tianshu Wang

;; Author: Tianshu Wang <wang@tianshu.me>

;;; Commentary:

;;; Code:

(use-package vc
  :defer t
  :config
  (setq vc-follow-symlinks t
        vc-handled-backends '(Git)))

(use-package magit
  :straight t
  :init
  (setq magit-define-global-key-bindings nil)
  (with-eval-after-load 'project
    (define-key project-prefix-map "m" #'magit-project-status)
    (add-to-list 'project-switch-commands '(magit-project-status "Magit") t))
  (with-eval-after-load 'recentf
    (add-to-list 'recentf-exclude "COMMIT_EDITMSG\\'" "/git-rebase-todo\\'"))
  :config
  (setq magit-diff-refine-hunk t
        magit-display-buffer-function 'magit-display-buffer-same-window-except-diff-v1
        magit-revision-show-gravatars '("^Author:     " . "^Commit:     ")
        magit-save-repository-buffers 'dontask

        transient-default-level 5)

  (add-hook 'magit-diff-mode-hook (lambda () (toggle-truncate-lines -1)))
  (add-hook 'magit-process-find-password-functions 'magit-process-password-auth-source)

  (defun org-reveal-advice (&rest _args)
    "Unfold the org headings for a target line.
    This can be used to advice functions that might open .org files.

    For example: To unfold from a magit diff buffer, evaluate the following:
    (advice-add 'magit-diff-visit-file :after #'org-reveal-advice)"
    (when (derived-mode-p 'org-mode) (org-reveal)))

  (advice-add 'magit-blame-addition           :after #'org-reveal-advice)
  (advice-add 'magit-diff-visit-file          :after #'org-reveal-advice)
  (advice-add 'magit-diff-visit-worktree-file :after #'org-reveal-advice)

  (defun magit-log-dangling ()
    (interactive)
    (magit-log-setup-buffer
     (-filter
      (lambda (x) (not (or (equal "" x) (s-match "error" x))))
      (s-lines
       (shell-command-to-string
        "git fsck --no-reflogs | awk '/dangling commit/ {print $3}'")))
     '("--no-walk" "--color" "--decorate" "--follow")' nil))

  (transient-append-suffix 'magit-log "s"
    '("d" "dangling" magit-log-dangling))
  :general
  (tyrant-def
    "g"   (cons "git" (make-sparse-keymap))
    "gb"  'magit-blame
    "gc"  'magit-clone
    "gd"  'magit-diff
    "gf"  'magit-file-dispatch
    "gi"  'magit-init
    "gl"  'magit-log-buffer-file
    "gm"  'magit-dispatch
    "gs"  'magit-status
    "gS"  'magit-stage-file
    "gU"  'magit-unstage-file))

(use-package forge
  :straight t
  :after magit
  :init
  (setq forge-add-default-bindings nil
        forge-database-connector 'sqlite-builtin))

(use-package transient
  :straight t
  :defer t
  :config
  (general-def transient-base-map   "q" 'transient-quit-one)
  (general-def transient-sticky-map "q" 'transient-quit-seq))

(use-package diff-hl
  :straight t
  :hook (after-init . global-diff-hl-mode)
  :config
  (setq diff-hl-side 'right)

  (with-eval-after-load 'magit
    (add-hook 'magit-pre-refresh-hook 'diff-hl-magit-pre-refresh)
    (add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh))

  (general-def 'normal
    "g[" '(diff-hl-previous-hunk :jump t)
    "g]" '(diff-hl-next-hunk :jump t)))

(use-package git-modes :straight t :defer t)

(use-package git-timemachine
  :straight (:host codeberg :repo "pidu/git-timemachine")
  :config
  (general-def git-timemachine-mode-map
    "gt" '(:ignore t :which-key "git-timemachine"))
  :general
  (tyrant-def "gt" 'git-timemachine))

(use-package git-link
  :straight t
  :config
  (setq git-link-open-in-browser t)

  (defun git-link@around (fun remote start end)
    (if (git-link--relative-filename)
        (funcall fun remote start end)
      (funcall 'git-link-homepage remote)))
  (advice-add #'git-link :around #'git-link@around)

  (defun git-link-copy-url-only ()
    "Only copy the generated link to the kill ring."
    (interactive)
    (let (git-link-open-in-browser)
      (call-interactively 'git-link)))

  (defun git-link-commit-copy-url-only ()
    "Only copy the generated link to the kill ring."
    (interactive)
    (let (git-link-open-in-browser)
      (call-interactively 'git-link-commit)))
  :general
  (tyrant-def
    "gL"  (cons "links" (make-sparse-keymap))
    "go"  'git-link
    "gLc" 'git-link-commit
    "gLC" 'git-link-commit-copy-url-only
    "gLl" 'git-link
    "gLL" 'git-link-copy-url-only))

(use-package gitignore-templates
  :straight t
  :config
  (setq gitignore-templates-api 'github)
  :general
  (tyrant-def
    "gI"  (cons "gitignore" (make-sparse-keymap))
    "gIn" 'gitignore-templates-new-file
    "gIi" 'gitignore-templates-insert))


(provide 'editor-vsc)
;;; editor-vsc.el ends here
