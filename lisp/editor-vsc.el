;;; editor-vsc.el --- -*- lexical-binding: t; -*-

;; Copyright (C) 2020-2021  Tianshu Wang

;; Author: Tianshu Wang <wang@tianshu.me>

;;; Commentary:

;;; Code:

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
    "gf"  'magit-file-dispatch
    "gF"  '(:ignore t :which-key "file")
    "gFf" 'magit-find-file
    "gFl" 'magit-log-buffer-file
    "gFd" 'magit-diff
    "gi"  'magit-init
    "gm"  'magit-dispatch
    "gs"  'magit-status
    "gS"  'magit-stage-file
    "gU"  'magit-unstage-file))

(use-package magit-gitflow
  :disabled t
  :ensure t
  :hook (magit-mode . turn-on-magit-gitflow)
  :config (general-def magit-mode-map "%" 'magit-gitflow-popup))

(use-package forge :ensure t :after magit)

(use-package transient
  :ensure t
  :after magit
  :config (transient-bind-q-to-quit))

(use-package browse-at-remote
  :ensure t
  :general (tyrant-def "go" 'browse-at-remote))

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

(use-package git-auto-commit-mode :ensure t)

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
    "gl"  '(:ignore t :which-key "links")
    "glc" 'git-link-commit
    "glC" 'git-link-commit-copy-url-only
    "gll" 'git-link
    "glL" 'git-link-copy-url-only
    "glh" 'git-link-homepage))

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

(use-package gist
  :ensure t
  :config
  (evil-set-initial-state 'gist-list-mode 'motion)
  (general-def 'motion 'gist-list-menu-mode-map
    "K"   'gist-kill-current
    "o"   'gist-browse-current-url
    "gr"  'gist-list-reload
    "RET" 'gist-fetch-current)
  :general
  (tyrant-def
    "gg"  '(:ignore t :which-key "github gist")
    "ggb" 'gist-buffer
    "ggB" 'gist-buffer-private
    "ggl" 'gist-list
    "ggr" 'gist-region
    "ggR" 'gist-region-private))

(use-package github-stars
  :ensure t
  :general
  (tyrant-def
    "gG"   '(:ignore t :which-key "github stars")
    "gGs" 'github-stars-browse-url
    "gGl" 'github-stars-list))


(provide 'editor-vsc)
;;; editor-vsc.el ends here
