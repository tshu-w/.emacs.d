;;; emacs -nw -Q -l test.el

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
