;;; editor-vsc.el --- -*- lexical-binding: t; -*-

;; Copyright (C) 2020  Tianshu Wang

;; Author: Tianshu Wang <volekingsg@gmail.com>

;;; Commentary:

;;; Code:

(use-package magit
  :ensure t
  :config
  (setq magit-revision-show-gravatars '("^Author:     " . "^Commit:     ")
        magit-display-buffer-function 'magit-display-buffer-fullframe-status-v1
        magit-save-repository-buffers 'dontask)

  (defun org-reveal-advice (&rest _args)
    "Unfold the org headings for a target line.
    This can be used to advice functions that might open .org files.

    For example: To unfold from a magit diff buffer, evaluate the following:
    (advice-add 'magit-diff-visit-file :after #'org-reveal-advice)"
    (when (derived-mode-p 'org-mode)
      (org-reveal)))

  (advice-add 'magit-blame-addition           :after #'org-reveal-advice)
  (advice-add 'magit-diff-visit-file          :after #'org-reveal-advice)
  (advice-add 'magit-diff-visit-worktree-file :after #'org-reveal-advice)

  (general-def 'normal magit-blame-read-only-mode-map
    "RET"    'magit-show-commit)

  (despot-def with-editor-mode-map
    ","      'with-editor-finish
    "a"      'with-editor-cancel
    "c"      'with-editor-finish
    "k"      'with-editor-cancel)
  (despot-def magit-log-select-mode-map
    ","      'magit-log-select-pick
    "a"      'magit-log-select-quit
    "c"      'magit-log-select-pick
    "k"      'magit-log-select-quit)
  :general
  (tyrant-def
    "g"   '(:ignore t :which-key "git")
    "gs"  'magit-status
    "gf"  '(:ignore t :which-key "file")
    "gb"  'magit-blame
    "gc"  'magit-clone
    "gfF" 'magit-find-file
    "gfl" 'magit-log-buffer-file
    "gfd" 'magit-diff
    "gi"  'magit-init
    "gL"  'magit-list-repositories
    "gm"  'magit-dispatch
    "gs"  'magit-status
    "gS"  'magit-stage-file
    "gU"  'magit-unstage-file))

(use-package evil-magit
  :ensure t
  :hook (magit-mode . evil-magit-init))

(use-package magit-gitflow
  :disabled t
  :ensure t
  :hook (magit-mode . turn-on-magit-gitflow)
  :config (general-def magit-mode-map "%" 'magit-gitflow-popup))

(use-package magit-svn
  :disabled t
  :ensure t
  :hook (magit-mode . turn-on-magit-svn)
  :config (general-def magit-mode-map "~" 'magit-svn))

(use-package forge
  :ensure t
  :after magit
  :config
  (despot-def forge-topic-mode-map
    "c" 'forge-create-post
    "e" 'forge-edit-post)
  (despot-def forge-post-mode-map
    "," 'forge-post-submit
    "c" 'forge-post-submit
    "k" 'forge-post-cancel
    "a" 'forge-post-cancel))

(use-package transient
  :ensure t
  :config (transient-bind-q-to-quit))

(use-package browse-at-remote
  :ensure t
  :general (tyrant-def "go" 'browse-at-remote))

(use-package diff-hl
  :ensure t
  :init
  (setq diff-hl-side 'right)
  (global-diff-hl-mode)
  (add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh)
  :general
  (tyrant-def "g=" 'diff-hl-diff-goto-hunk)
  (general-def 'normal
    "[ h" 'diff-hl-previous-hunk
    "] h" 'diff-hl-next-hunk))

(use-package git-timemachine
  :ensure t
  :config
  (defhydra git-timemachine-menu (
                                  :pre (unless (bound-and-true-p git-timemachine-mode)
                                         (call-interactively 'git-timemachine))
                                  :post (when (bound-and-true-p git-timemachine-mode)
                                          (git-timemachine-quit))
                                  :hint nil)
    "
Git Timemachine Transient State
[_p_/_N_] previous [_n_] next [_c_] current [_g_] goto nth rev [_Y_] copy hash [_q_] quit"
    ("c" git-timemachine-show-current-revision)
    ("g" git-timemachine-show-nth-revision)
    ("p" git-timemachine-show-previous-revision)
    ("n" git-timemachine-show-next-revision)
    ("N" git-timemachine-show-previous-revision)
    ("Y" git-timemachine-kill-revision)
    ("q" nil))
  :general
  (tyrant-def "gt" 'git-timemachine-menu/body))

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

(use-package git-messenger
  :ensure t
  :general (tyrant-def "gM" 'git-messenger:popup-message))

(use-package gitignore-templates
  :ensure t
  :general
  (tyrant-def "gI" 'gitignore-templates-new-file)
  (despot-def gitignore-mode-map
    "i" 'gitignore-templates-insert))

(use-package gitattributes-mode :ensure t)

(use-package gitconfig-mode :ensure t)

(use-package gitignore-mode :ensure t)

(use-package gist
  :ensure t
  :config
  (evilified-state-evilify-map gist-list-menu-mode-map
                               :mode gist-list-mode
                               :bindings
                               "f" 'gist-fetch-current
                               "K" 'gist-kill-current
                               "o" 'gist-browse-current-url)
  (evilified-state-evilify-map gist-list-mode-map
                               :mode gist-list-mode
                               :bindings
                               (kbd "gr") 'gist-list-reload)
  :general
  (tyrant-def
    "gg"  '(:ignore t :which-key "github gist")
    "ggb" 'gist-buffer
    "ggB" 'gist-buffer-private
    "ggl" 'gist-list
    "ggr" 'gist-region
    "ggR" 'gist-region-private))

(use-package helm :ensure t)

(use-package helm-github-stars
  :ensure t
  :commands (helm-github-stars)
  :config
  (setq helm-github-stars-username "tshu-w"))


(provide 'editor-vsc)
;;; editor-vsc.el ends here
