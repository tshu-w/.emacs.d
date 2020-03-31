;;; core-packages.el --- -*- lexical-binding: t; -*-

;; Copyright (C) 2020  Tianshu Wang

;; Author: Tianshu Wang <volekingsg@gmail.com>

;; initialize package

;;; Commentary:

;;; Code:

(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("org"   . "https://orgmode.org/elpa/")
                         ("gnu"   . "https://elpa.gnu.org/packages/")))

;; initialize packages
(unless (bound-and-true-p package--initialized) ; To avoid warnings in 27
  (setq package-enable-at-startup nil)          ; To prevent initializing twice
  (package-initialize))

;; setup `use-package'
(unless (package-installed-p 'use-package)
  (when (not package-archive-contents)
    (package-refresh-contents))
  (package-install 'use-package))

(eval-when-compile
  (setq use-package-always-defer  t
        use-package-expand-minimally t)
  (if init-file-debug
      (setq use-package-verbose t
            use-package-minimum-reported-time 0
            use-package-expand-minimally nil
            use-package-compute-statistics t
            use-package-inject-hooks t
            debug-on-error t))

  (require 'use-package))

;; required by `use-package'
;; (use-package diminish)
;; (use-package bind-key)

(use-package auto-package-update
  :ensure t
  :init
  (defalias 'package-upgrade #'auto-package-update-now)
  :config
  (setq auto-package-update-delete-old-versions t
        auto-package-update-hide-results t))

(use-package exec-path-from-shell
  :if (memq window-system '(mac ns))
  :ensure t
  :init
  (setq exec-path-from-shell-arguments '("-l")
        exec-path-from-shell-variables '("PATH" "MANPATH"))
  (exec-path-from-shell-initialize))

(use-package benchmark-init
  :disabled t
  :ensure t
  :init (benchmark-init/activate)
  ;; To disable collection of benchmark data after init is done.
  :hook (after-init . benchmark-init/deactivate))

(use-package no-littering
  :ensure t
  :init (require 'no-littering)
  :config
  (with-eval-after-load 'recentf
    (add-to-list 'recentf-exclude no-littering-var-directory)
    (add-to-list 'recentf-exclude no-littering-etc-directory)))

(use-package restart-emacs
  :ensure t
  :commands (restart-emacs restart-emacs-debug-init)
  :config
  (defun restart-emacs-debug-init (&optional args)
    "Restart emacs and enable debug-init."
    (interactive)
    (restart-emacs (cons "--debug-init" args))))

(use-package help-fns+
  :commands describe-keymap)


(provide 'core-packages)
;;; core-packages.el ends here
