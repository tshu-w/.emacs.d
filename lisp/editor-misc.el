;;; editor-misc.el --- -*- lexical-binding: t; -*-

;; Copyright (C) 2020-2025  Tianshu Wang

;; Author: Tianshu Wang <wang@tianshu.me>

;;; Commentary:

;;; Code:

(use-package alert
  :straight t
  :defer t
  :config
  (when (eq system-type 'darwin)
    (defun alert-notifier-notify (info)
      (if alert-notifier-command
          (let ((args
                 (list "-group" "Emacs"
                       "-sender"  "org.gnu.Emacs"
                       "-activate" "org.gnu.Emacs"
                       "-title"   (alert-encode-string (plist-get info :title))
                       "-message" (alert-encode-string (plist-get info :message)))))
            (apply #'call-process alert-notifier-command nil nil nil args)))
      (alert-message-notify info))

    (setq alert-default-style 'notifier)))

(use-package atomic-chrome
  :straight t
  :hook (after-init . atomic-chrome-start-server)
  :config
  (setq atomic-chrome-extension-type-list '(ghost-text)
        atomic-chrome-url-major-mode-alist
        '(("overleaf\\.com" . TeX-tex-mode)
          ("github\\.com" . markdown-mode))))

(use-package browser-hist
  :straight (:host github :repo "agzam/browser-hist.el")
  :commands browser-hist-search
  :init
  (setq browser-hist-default-browser 'safari))

(use-package dumb-jump
  :straight t
  :defer t
  :init
  (add-hook 'xref-backend-functions #'dumb-jump-xref-activate)
  (setq dumb-jump-selector 'completing-read))

(use-package editorconfig
  :straight t
  :config
  (setq editorconfig-lisp-use-default-indent t)
  (editorconfig-mode))

(use-package elfeed
  :straight t
  :init
  (setq browse-url-generic-program "open"
        browse-url-generic-args '("--background"))
  :config
  (with-eval-after-load 'visual-fill-column
    (add-to-list 'visual-fill-column-major-modes 'elfeed-search-mode)
    (add-to-list 'visual-fill-column-major-modes 'elfeed-show-mode))

  (defun reverse-arg (fun &optional arg)
    (interactive "P")
    (apply fun (if arg nil '(4))))
  (advice-add 'elfeed-search-browse-url :around #'reverse-arg)
  :general
  (tyrant-def "af" 'elfeed))

(use-package elfeed-org
  :straight t
  :after elfeed
  :config
  (elfeed-org)
  (setq rmh-elfeed-org-files `(,(no-littering-expand-etc-file-name "elfeed/elfeed.org"))))

(use-package elfeed-tube
  :straight t
  :after elfeed
  :config
  (elfeed-tube-setup))

(use-package fcitx
  :straight t
  :hook (after-init . fcitx-aggressive-setup)
  :config
  (add-hook 'org-capture-mode-hook #'fcitx--deactivate))

(use-package gptel
  :straight t
  :init
  (setq gptel-model 'claude-3-5-sonnet
        gptel-directives
        `((default . nil)
          (paraphraser . "You are a paraphraser. Paraphrase and polish the text in the same language without changing its original meaning.")
          (translator . "You are a professional translator. Translate the text into English if it's written in Chinese, otherwise, translate it into Chinese. Only output the translated text without quotes.")
          (rewriter . "You are a rewriter. Concisely rewrite the text in the corresponding language and style.")
          (summarizer . "You are a summarizer. Summarize the text in the corresponding language and style without redundant description.")
          (programmer . "You are a careful programmer. Provide code and only code as output without any additional text, prompt or note.")
          (code-explainer . "You are a professional code explainer. Explain the code and report any bugs or errors."))
        gptel--system-message nil)
  :config
  (setq gptel--known-backends nil)
  (defvar gptel--oneapi
    (gptel-make-openai "OneAPI"
      :host "one-api.ponte.top"
      :key 'gptel-api-key
      :stream t
      :models '(o1-preview o1-mini gpt-4o gpt-4o-mini gpt-4-turbo gpt-4
                (claude-3-5-sonnet
                   :description "Highest level of intelligence and capability"
                   :capabilities (media tool cache)
                   :mime-types ("image/jpeg" "image/png" "image/gif" "image/webp" "application/pdf")
                   :context-window 200
                   :input-cost 3
                   :output-cost 15
                   :cutoff-date "2024-04")
                (claude-3-5-haiku
                   :description "Intelligence at blazing speeds"
                   :capabilities (media tool)
                   :mime-types ("image/jpeg" "image/png" "image/gif" "image/webp")
                   :context-window 200
                   :input-cost 1.00
                   :output-cost 5.00
                   :cutoff-date "2024-07")
                (claude-3-opus
                   :description "Top-level performance, intelligence, fluency, and understanding"
                   :capabilities (media tool cache)
                   :mime-types ("image/jpeg" "image/png" "image/gif" "image/webp")
                   :context-window 200
                   :input-cost 15
                   :output-cost 75
                   :cutoff-date "2023-08"))))
  (setq-default gptel-backend gptel--oneapi)

  (add-to-list 'gptel-response-filter-functions
               (defun gptel--colorize-response (content buffer)
                 (put-text-property 0 (length content) 'face 'font-lock-warning-face content)
                 content))

  (defun gptel-colorize-response (begin end)
    (save-excursion
      (put-text-property begin end 'font-lock-face 'font-lock-string-face)))
  (add-hook 'gptel-post-response-functions #'gptel-colorize-response)

  (defun clear-text-properties (start end)
    "Clear text properties between START and END."
    (interactive "r")
    (let ((inhibit-read-only t))
      (set-text-properties start end nil)))

  (with-eval-after-load 'embark
    (define-key embark-region-map (kbd "C") #'clear-text-properties)
    (define-key embark-region-map (kbd "g") #'gptel-transient-send))
  :general
  (tyrant-def "ag" 'gptel-menu))

(use-package helpful
  :straight t
  :config
  (defun helpful-reuse-window (buffer-or-name)
    "Switch to helpful BUFFER-OR-NAME.

The logic is simple, if we are currently in the helpful buffer,
reuse it's window, otherwise create new one."
    (if (eq major-mode 'helpful-mode)
        (pop-to-buffer-same-window buffer-or-name)
      (pop-to-buffer buffer-or-name)))

  (setq helpful-max-buffers 3
        helpful-switch-buffer-function #'helpful-reuse-window)

  (with-eval-after-load 'ibuffer
    (add-to-list 'ibuffer-help-buffer-modes 'helpful-mode))
  :general
  ([remap describe-command]  'helpful-command
   [remap describe-function] 'helpful-callable
   [remap describe-key]      'helpful-key
   [remap describe-symbol]   'helpful-symbol
   [remap describe-variable] 'helpful-variable))

(use-package jinx
  :straight t
  :hook (text-mode . jinx-mode)
  :config
  (add-to-list 'jinx-exclude-regexps '(t "\\cc"))
  (with-eval-after-load 'vertico-multiform
    (add-to-list 'vertico-multiform-categories '(jinx grid (vertico-grid-annotate . 20))))
  (with-eval-after-load 'evil
    (evil-define-motion evil-prev-jinx-error (count)
      "Go to the COUNT'th spelling mistake preceding point."
      :jump t (jinx-previous (or count 1)))
    (evil-define-motion evil-next-jinx-error (count)
      "Go to the COUNT'th spelling mistake after point."
      :jump t (jinx-next (or count 1))))
  :general
  ([remap ispell-word] 'jinx-correct-word
   [remap evil-prev-flyspell-error] 'evil-prev-jinx-error
   [remap evil-next-flyspell-error] 'evil-next-jinx-error))

(use-package link-hint
  :straight t
  :config
  (setq link-hint-restore nil)
  :general
  (general-def
    :keymaps '(compilation-mode-map
               custom-mode-map
               eww-link-keymap
               eww-mode-map
               help-mode-map
               helpful-mode-map
               Info-mode-map
               mu4e-view-mode-map
               xref--xref-buffer-mode-map
               woman-mode-map)
    :states  'normal
    "o"      'link-hint-open-link)

  (tyrant-def
    "jl" 'link-hint-open-link
    "jL" 'link-hint-open-multiple-links
    "jy" 'link-hint-copy-link))

(use-package nov
  :straight t
  :commands (nov-org-link-follow nov-org-link-store)
  :mode ("\\.epub\\'" . nov-mode)
  :init
  (with-eval-after-load 'org
    (org-link-set-parameters "nov"
                             :follow 'nov-org-link-follow
                             :store 'nov-org-link-store)))

(use-package oj :straight t :defer t)

(use-package pandoc-mode
  :straight t
  :hook (pandoc-mode . pandoc-load-default-settings)
  :commands pandoc
  :config
  (defun pandoc ()
    "Start pandoc for the buffer and open the menu"
    (interactive)
    ;; only run pandoc-mode if not active, as it resets pandoc--local-settings
    (if (not (bound-and-true-p pandoc-mode)) (pandoc-mode))
    (pandoc-main-hydra/body)))

(use-package pangu-spacing
  :straight t
  :hook (org-mode . pangu-spacing-mode)
  :config
  (setq pangu-spacing-real-insert-separtor t))

(use-package reveal-in-osx-finder
  :if (eq system-type 'darwin)
  :straight t
  :general (tyrant-def "bf" 'reveal-in-osx-finder))

(use-package reformatter :straight t :defer t)

(use-package rime
  :straight t
  :defer t
  :init
  (setq default-input-method "rime")
  :config
  (setq rime-user-data-dir (no-littering-expand-etc-file-name "rime/")
        rime-emacs-module-header-root (expand-file-name "include" (locate-dominating-file invocation-directory "include"))
        rime-show-candidate 'posframe
        rime-show-preedit 'inline
        rime-posframe-properties (list :internal-border-width 2))

  (add-to-list 'rime-translate-keybindings "C-`")

  (add-hook 'kill-emacs-hook (lambda ()
                               (when (fboundp 'rime-lib-sync-user-data)
                                 (ignore-errors (rime-sync)))))

  (general-def rime-mode-map "C-`" 'rime-send-keybinding))

(use-package rime-regexp
  :straight (:host github :repo "colawithsauce/rime-regexp.el")
  :commands rime-regexp-build-regexp-string
  :init
  (with-eval-after-load 'orderless
    (defun orderless-pinyin-regexp (component)
      "Match COMPONENT as a pinyin regexp with `rime-regexp-build-regexp-string'."
      (rime-regexp-build-regexp-string (orderless-regexp component)))

    (defun pinyin-if-ampersand (pattern _index _total)
      (when (string-suffix-p "&" pattern)
        `(orderless-pinyin-regexp . ,(substring pattern 0 -1))))

    (add-to-list 'orderless-style-dispatchers 'pinyin-if-ampersand))

  (with-eval-after-load 'avy
    (defun avy--regex-candidates@around (fn regex &optional beg end pred group)
      (let ((regex (rime-regexp-build-regexp-string regex)))
        (funcall fn regex beg end pred group)))
    (advice-add 'avy--regex-candidates :around #'avy--regex-candidates@around))
  :config
  (rime-regexp-load-rime))

(use-package sideline
  :straight t
  :defer t
  :init
  (use-package sideline-flymake
    :straight t
    :hook (flymake-mode . sideline-mode)
    :init
    (setq sideline-backends-right '(sideline-flymake))
    (add-hook 'flymake-mode-hook
              (lambda () (remove-hook 'eldoc-documentation-functions 'flymake-eldoc-function t)))))

(use-package terminal-here
  :straight t
  :config
  (setq terminal-here-mac-terminal-command 'iterm2
        terminal-here-project-root-function (lambda () (project-root (project-current t))))
  :general
  (tyrant-def
    "\""   'terminal-here-launch
    "p \"" 'terminal-here-project-launch))

(use-package treesit-auto
  :when (and (fboundp 'treesit-available-p) (treesit-available-p))
  :straight t
  :hook (after-init . global-treesit-auto-mode)
  :config
  (treesit-auto-add-to-auto-mode-alist))

(use-package typst-ts-mode
  :straight (:host sourcehut :repo "meow_king/typst-ts-mode")
  :after treesit
  :init
  (add-to-list 'treesit-language-source-alist '(typst "https://github.com/uben0/tree-sitter-typst"))
  (setq typst-ts-mode-indent-offset 2)
  :config
  (with-eval-after-load 'eglot
    (add-to-list 'eglot-server-programs
                 `((typst-ts-mode) .
                   ,(eglot-alternatives `(,typst-ts-lsp-download-path
                                          "tinymist"
                                          "typst-lsp")))))

  (defun typst-ts-prevent-preview-on-compilation-failure (process-status exit-status msg)
      "Prevent Typst preview when compilation fails."
      (unless (zerop exit-status)
        (remove-hook 'compilation-finish-functions
                     (typst-ts-mode-compile-and-preview--compilation-finish-function
                      compilation-original-buffer))))
  (advice-add 'compilation-handle-exit :before #'typst-ts-prevent-preview-on-compilation-failure)
  :general
  (despot-def typst-ts-mode-map
    "a" 'typst-ts-compile-and-preview
    "v" 'typst-ts-preview
    "," 'typst-ts-compile
    "w" 'typst-ts-watch))

(use-package undo-fu-session
  :straight t
  :hook (window-setup . undo-fu-session-global-mode)
  :config
  (setq undo-fu-seesion-ignored-files '("/COMMIT_EDITMSG\\'" "/git-rebase-todo\\'"))
  (advice-add 'undo-fu-session--mode-turn-on :after #'undo-fu-session-recover))

(use-package xr :straight t :defer t)

(use-package winum
  :straight t
  :hook (after-init . winum-mode)
  :init
  (with-eval-after-load 'which-key
    (push '((nil . "winum-select-window-[1-9]") . t) which-key-replacement-alist)
    (push '((nil . "buffer-to-window-[1-9]") . t) which-key-replacement-alist))
  :config
  (setq winum-auto-assign-0-to-minibuffer t
        winum-auto-setup-mode-line t
        winum-scope 'frame-local)

  (defun move-buffer-to-window (windownum follow-focus-p)
    "Moves a buffer to a window. follow-focus-p controls
whether focus moves to new window (with buffer), or stays on current"
    (interactive)
    (let ((b (current-buffer))
          (w1 (selected-window))
          (w2 (winum-get-window-by-number windownum)))
      (unless (eq w1 w2)
        (set-window-buffer w2 b)
        (switch-to-prev-buffer)
        (unrecord-window-buffer w1 b)))
    (when follow-focus-p (select-window (winum-get-window-by-number windownum))))

  (defun swap-buffers-to-window (windownum follow-focus-p)
    "Swaps visible buffers between active window and selected window.
follow-focus-p controls whether focus moves to new window (with buffer), or
stays on current"
    (interactive)
    (let* ((b1 (current-buffer))
           (w1 (selected-window))
           (w2 (winum-get-window-by-number windownum))
           (b2 (window-buffer w2)))
      (unless (eq w1 w2)
        (set-window-buffer w1 b2)
        (set-window-buffer w2 b1)
        (unrecord-window-buffer w1 b1)
        (unrecord-window-buffer w2 b2)))
    (when follow-focus-p (winum-select-window-by-number windownum)))

  (dotimes (i 9)
    (let ((n (+ i 1)))
      (eval `(defun ,(intern (format "buffer-to-window-%s" n)) (&optional arg)
               ,(format "Move buffer to the window with number %i." n)
               (interactive "P")
               (if arg
                   (move-buffer-to-window ,n t)
                 (swap-buffers-to-window ,n t))))))
  :general
  (tyrant-def
    "0"  '("select window 0 or 10" . winum-select-window-0-or-10)
    "1"  '("select window 1..9" . winum-select-window-1)
    "2"  'winum-select-window-2
    "3"  'winum-select-window-3
    "4"  'winum-select-window-4
    "5"  'winum-select-window-5
    "6"  'winum-select-window-6
    "7"  'winum-select-window-7
    "8"  'winum-select-window-8
    "9"  'winum-select-window-9
    "b1" '("Move buffer to window 1..9" . buffer-to-window-1)
    "b2" 'buffer-to-window-2
    "b3" 'buffer-to-window-3
    "b4" 'buffer-to-window-4
    "b5" 'buffer-to-window-5
    "b6" 'buffer-to-window-6
    "b7" 'buffer-to-window-7
    "b8" 'buffer-to-window-8
    "b9" 'buffer-to-window-9))


(provide 'editor-misc)
;;; editor-misc.el ends here
