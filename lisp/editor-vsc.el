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

  ;; https://github.com/LuciusChen/.emacs.d/blob/main/lib/lib-magit.el
  (defconst gptel-commit-prompt
    "The user provides the result of running `git diff --cached`. You suggest a conventional commit message. Don't add anything else to the response. The following describes conventional commits.

# Conventional Commits 1.0.0

## Summary

The Conventional Commits specification is a lightweight convention on top of commit messages.
It provides an easy set of rules for creating an explicit commit history;
which makes it easier to write automated tools on top of.
This convention dovetails with [SemVer](http://semver.org),
by describing the features, fixes, and breaking changes made in commit messages.

The commit message should be structured as follows:

---
```
<type>[optional scope]: <description>

[optional body]

[optional footer(s)]
```
---

<br />
The commit contains the following structural elements, to communicate intent to the
consumers of your library:

1. **fix:** a commit of the _type_ `fix` patches a bug in your codebase (this correlates with [`PATCH`](http://semver.org/#summary) in Semantic Versioning).
1. **feat:** a commit of the _type_ `feat` introduces a new feature to the codebase (this correlates with [`MINOR`](http://semver.org/#summary) in Semantic Versioning).
1. **BREAKING CHANGE:** a commit that has a footer `BREAKING CHANGE:`, or appends a `!` after the type/scope, introduces a breaking API change (correlating with [`MAJOR`](http://semver.org/#summary) in Semantic Versioning).
A BREAKING CHANGE can be part of commits of any _type_.
1. _types_ other than `fix:` and `feat:` are allowed, for example [@commitlint/config-conventional](https://github.com/conventional-changelog/commitlint/tree/master/%40commitlint/config-conventional) (based on the [Angular convention](https://github.com/angular/angular/blob/22b96b9/CONTRIBUTING.md#-commit-message-guidelines)) recommends `build:`, `chore:`,
  `ci:`, `docs:`, `style:`, `refactor:`, `perf:`, `test:`, and others.
1. _footers_ other than `BREAKING CHANGE: <description>` may be provided and follow a convention similar to
  [git trailer format](https://git-scm.com/docs/git-interpret-trailers).

Additional types are not mandated by the Conventional Commits specification, and have no implicit effect in Semantic Versioning (unless they include a BREAKING CHANGE).
<br /><br />
A scope may be provided to a commit's type, to provide additional contextual information and is contained within parenthesis, e.g., `feat(parser): add ability to parse arrays`.")

  (defun gptel-commit ()
    "Generate commit message with gptel and insert it into the buffer."
    (interactive)
    (require 'gptel)
    (let* ((lines (magit-git-lines "diff" "--cached"))
           (changes (string-join lines "\n")))
      (gptel-request changes :system gptel-commit-prompt)))
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
