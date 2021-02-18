;;; editor-projectile.el --- -*- lexical-binding: t; -*-

;; Copyright (C) 2020-2021  Tianshu Wang

;; Author: Tianshu Wang <wang@tianshu.me>

;;; Commentary:

;;; Code:

(use-package projectile
  :ensure t
  :hook (after-init . projectile-mode)
  :general
  (tyrant-def
    "p"  '(:ignore t :which-key "projects")
    "p!" 'projectile-run-shell-command-in-root
    "p&" 'projectile-run-async-shell-command-in-root
    "p%" 'projectile-replace-regexp
    "pa" 'projectile-toggle-between-implementation-and-test
    "pb" 'projectile-switch-to-buffer
    "pc" 'projectile-compile-project
    "pd" 'projectile-find-dir
    "pD" 'projectile-dired
    "pe" 'projectile-edit-dir-locals
    "pf" 'projectile-find-file
    "pg" 'projectile-find-tag
    "pG" 'projectile-regenerate-tags
    "pI" 'projectile-invalidate-cache
    "pk" 'projectile-kill-buffers
    "pp" 'projectile-switch-project
    "pr" 'projectile-recentf
    "pR" 'projectile-replace
    "pT" 'projectile-test-project
    "pv" 'projectile-vc))

(use-package counsel-projectile
  :ensure t
  :config
  (defun counsel-projectile-rg-region-or-symbol ()
    "Use `counsel-projectile-rg' to search for
    the selected region or the symbol around point in the current
    project ."
    (interactive)
    (let ((counsel-projectile-rg-initial-input
           '(if (region-active-p)
                (buffer-substring-no-properties
                 (region-beginning) (region-end))
              (thing-at-point 'symbol t))))
      (counsel-projectile-rg)))

  (ivy-add-actions 'counsel-projectile-find-file
                   '(("R" (lambda (arg)
                            (interactive)
                            (call-interactively
                             #'projectile-invalidate-cache)
                            (ivy-resume))
                      "refresh list")))
  :general
  (tyrant-def
    "/"     '(counsel-projectile-rg :which-key "search project")
    "*"     '(counsel-projectile-rg-region-or-symbol :which-key "search project w/input")
    "pb"    'counsel-projectile-switch-to-buffer
    "pd"    'counsel-projectile-find-dir
    "pp"    'counsel-projectile-switch-project
    "pf"    'counsel-projectile-find-file
    "p SPC" 'counsel-projectile
    "sp"    'counsel-projectile-rg
    "sP"    'counsel-projectile-rg-region-or-symbol))


(provide 'editor-projectile)
;;; editor-projectile.el ends here
