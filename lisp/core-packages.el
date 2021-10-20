;;; core-packages.el --- -*- lexical-binding: t; -*-

;; Copyright (C) 2020-2021  Tianshu Wang

;; Author: Tianshu Wang <wang@tianshu.me>

;; initialize package

;;; Commentary:

;;; Code:

(setq package-archives '(("melpa"        . "https://melpa.org/packages/")
                         ("melpa-stable" . "https://stable.melpa.org/packages/")
                         ("gnu"          . "https://elpa.gnu.org/packages/")
                         ("nongnu"       . "https://elpa.nongnu.org/nongnu/")))

;; initialize packages
(setq package-quickstart t
      package-quickstart-file (concat user-emacs-directory "var/package-quickstart.el"))
(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

;; bootstrap `use-package'
(unless (package-installed-p 'use-package)
  (package-install 'use-package))
(eval-and-compile
  (if init-file-debug
      (setq use-package-verbose t
            use-package-minimum-reported-time 0
            use-package-compute-statistics t
            use-package-inject-hooks t
            debug-on-error t)))
(eval-when-compile
  (require 'use-package))


(use-package auto-package-update
  :ensure t
  :commands package-upgrade
  :config
  (setq auto-package-update-delete-old-versions t
        auto-package-update-hide-results t)

  (defun package-upgrade ()
    "Update installed Emacs packages."
    (interactive)
    (auto-package-update-now)
    (package-quickstart-refresh))

  (fset 'apu--write-current-day 'ignore))

(use-package exec-path-from-shell
  :if (memq window-system '(mac ns))
  :ensure t
  :defer t
  :init
  (setq exec-path-from-shell-arguments '("-l")
        exec-path-from-shell-variables '("PATH" "MANPATH"
                                         "GNUPGHOME"
                                         "WAKATIME_HOME"
                                         "SSH_AUTH_SOCK"))
  (exec-path-from-shell-initialize))

(use-package no-littering :ensure t :defer t)

(use-package esup :ensure t :defer t)

(use-package restart-emacs
  :ensure t
  :commands restart-emacs-debug-init
  :config
  (defun restart-emacs-debug-init (&optional args)
    "Restart emacs and enable debug-init."
    (interactive)
    (restart-emacs (cons "--debug-init" args))))

(provide 'core-packages)
;;; core-packages.el ends here
