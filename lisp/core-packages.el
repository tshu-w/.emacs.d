;;; core-packages.el --- -*- lexical-binding: t; -*-

;; Copyright (C) 2020-2021  Tianshu Wang

;; Author: Tianshu Wang <wang@tianshu.me>

;; initialize package

;;; Commentary:

;;; Code:

(setq package-archives '(("melpa"        . "https://melpa.org/packages/")
                         ("melpa-stable" . "https://stable.melpa.org/packages/")
                         ("org"          . "https://orgmode.org/elpa/")
                         ("gnu"          . "https://elpa.gnu.org/packages/"))
      package-user-dir (concat user-emacs-directory "elpa/"
                               (format "%d%s%d"
                                       emacs-major-version
                                       version-separator
                                       emacs-minor-version)))

;; initialize packages
(unless (bound-and-true-p package--initialized) ; To avoid warnings in 27
  (setq package-enable-at-startup nil)          ; To prevent initializing twice
  (setq package-check-signature nil)
  (package-initialize))

;; setup `use-package'
(unless (package-installed-p 'use-package)
  (when (not package-archive-contents)
    (package-refresh-contents))
  (package-install 'use-package))

(eval-when-compile
  (setq use-package-expand-minimally t)
  (if init-file-debug
      (setq use-package-verbose t
            use-package-minimum-reported-time 0
            use-package-expand-minimally nil
            use-package-compute-statistics t
            use-package-inject-hooks t
            debug-on-error t))

  (require 'use-package))

(use-package auto-package-update
  :ensure t
  :defer t
  :init
  (defalias 'package-upgrade #'auto-package-update-now
    "Update installed Emacs packages.")
  :config
  (setq auto-package-update-delete-old-versions t
        auto-package-update-hide-results t))

(use-package exec-path-from-shell
  :if (memq window-system '(mac ns))
  :ensure t
  :defer t
  :init
  (setq exec-path-from-shell-arguments '("-l")
        exec-path-from-shell-variables '("PATH" "MANPATH" "GNUPGHOME"))
  (exec-path-from-shell-initialize))

(use-package benchmark-init
  :ensure t
  :disabled t
  :init (benchmark-init/activate)
  ;; To disable collection of benchmark data after init is done.
  :hook (after-init . benchmark-init/deactivate))

(use-package no-littering :ensure t :defer t)

(use-package quelpa
  :ensure t
  :commands quelpa-read-cache
  :init
  (setq quelpa-upgrade-p nil
        quelpa-update-melpa-p nil))

(use-package quelpa-use-package
  :ensure t
  :init
  (setq quelpa-use-package-inhibit-loading-quelpa t))

(use-package restart-emacs
  :ensure t
  :commands (restart-emacs restart-emacs-debug-init)
  :config
  (defun restart-emacs-debug-init (&optional args)
    "Restart emacs and enable debug-init."
    (interactive)
    (restart-emacs (cons "--debug-init" args))))

(use-package help-fns+ :commands describe-keymap)


(provide 'core-packages)
;;; core-packages.el ends here
