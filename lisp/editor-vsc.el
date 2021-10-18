;;; editor-vsc.el --- -*- lexical-binding: t; -*-

;; Copyright (C) 2020-2021  Tianshu Wang

;; Author: Tianshu Wang <wang@tianshu.me>

;;; Commentary:

;;; Code:

(use-package vc
  :defer t
  :config
  (setq vc-follow-symlinks t))

(use-package magit
  :ensure t
  :init
  (setq magit-define-global-key-bindings nil)
  :config
  (setq magit-diff-refine-hunk t
        magit-display-buffer-function 'magit-display-buffer-fullframe-status-v1
        magit-revision-show-gravatars '("^Author:     " . "^Commit:     ")
        magit-save-repository-buffers 'dontask)

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

  (use-package magit-wip
    :config
    (setq magit-wip-merge-branch t)
    (magit-wip-mode))

  (general-def 'normal magit-log-select-mode-map "q" 'magit-log-select-quit)
  :general
  (tyrant-def
    "g"   '(:ignore t :which-key "git")
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

(use-package magit-gitflow
  :disabled t
  :ensure t
  :hook (magit-mode . turn-on-magit-gitflow)
  :config (general-def magit-mode-map "%" 'magit-gitflow-popup))

(use-package forge
  :ensure t
  :defer t
  :init
  (defun evil-collection-magit-setup@override ()
    "Set up `evil' bindings for `magit'."

    (evil-collection-define-key 'normal 'magit-blame-mode-map
      "q" 'magit-blame-quit)
    (evil-collection-define-key 'normal 'magit-blame-read-only-mode-map
      "q" 'magit-blame-quit)

    (require 'forge)
    (evil-collection-magit-init))
  (advice-add 'evil-collection-magit-setup :override #'evil-collection-magit-setup@override))

(use-package transient
  :ensure t
  :defer t
  :config (transient-bind-q-to-quit))

(use-package browse-at-remote
  :ensure t
  :general
  (tyrant-def "go" 'browse-at-remote))

(use-package diff-hl
  :ensure t
  :hook (after-init . global-diff-hl-mode)
  :config
  (setq diff-hl-side 'right)

  (with-eval-after-load 'magit
    (add-hook 'magit-pre-refresh-hook 'diff-hl-magit-pre-refresh)
    (add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh))

  (with-eval-after-load 'evil
    (evil-set-command-property 'diff-hl-previous-hunk :jump t)
    (evil-set-command-property 'diff-hl-next-hunk :jump t))

  (general-def 'normal
    "[ h" 'diff-hl-previous-hunk
    "] h" 'diff-hl-next-hunk))

(use-package git-auto-commit-mode :ensure t :defer t)

(use-package git-timemachine
  :ensure t
  :config
  (general-def git-timemachine-mode-map
    "gt" '(:ignore t :which-key "git-timemachine"))
  :general
  (tyrant-def "gt" 'git-timemachine))

(use-package git-link
  :ensure t
  :config
  (setq git-link-open-in-browser t)

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
    "gL"  '(:ignore t :which-key "links")
    "gLc" 'git-link-commit
    "gLC" 'git-link-commit-copy-url-only
    "gLl" 'git-link
    "gLL" 'git-link-copy-url-only
    "gLh" 'git-link-homepage))

(use-package gitignore-templates
  :ensure t
  :config
  (despot-def gitignore-mode-map
    "i" 'gitignore-templates-insert)
  :general
  (tyrant-def
    "gI"  '(:ignore t :which-key "gitignore")
    "gIn" 'gitignore-templates-new-file
    "gIi" 'gitignore-templates-insert))

(use-package gitattributes-mode :ensure t :defer t)

(use-package gitconfig-mode :ensure t :defer t)

(use-package gitignore-mode :ensure t :defer t)

(use-package github-stars :ensure t :defer t)


(provide 'editor-vsc)
;;; editor-vsc.el ends here
