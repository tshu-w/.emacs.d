;;; emacs -nw -Q -l ~/.emacs.d/test.el

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

(require 'use-package)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
