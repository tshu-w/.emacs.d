;;; editor-completion.el --- -*- lexical-binding: t; -*-

;; Copyright (C) 2020-2021  Tianshu Wang

;; Author: Tianshu Wang <wang@tianshu.me>

;;; Commentary:

;;; Code:

(use-package ivy
  :ensure t
  :hook (after-init . ivy-mode)
  :config
  (setq ivy-height 15
        ivy-initial-inputs-alist nil ;; it will change after counsel load
        ivy-use-selectable-prompt t
        ivy-use-virtual-buffers t

        ivy-re-builders-alist '((t . ivy--cregex-plus))
        ivy-preferred-re-builders '((ivy--cregex-plus . "ivy")
                                    (ivy--cregex-ignore-order . "order")
                                    (ivy--regex-fuzzy . "fuzzy")))
  (defun ivy-tab ()
    (interactive)
    (let ((dir ivy--directory))
      (ivy-partial-or-done)
      (when (string= dir ivy--directory)
        (ivy-insert-current)
        (when (and (eq (ivy-state-collection ivy-last) #'read-file-name-internal)
                   (setq dir (ivy-expand-file-if-directory (ivy-state-current ivy-last))))
          (ivy--cd dir)
          (setq this-command 'ivy-cd)))))

  (defun ivy-c-h ()
    (interactive)
    (if (eq (ivy-state-collection ivy-last) #'read-file-name-internal)
        (if (string-equal (ivy--input) "")
            (counsel-up-directory)
          (delete-minibuffer-contents))
      (ivy-backward-delete-char)))

  (use-package pyim
    :ensure t
    :commands pyim-cregexp-build
    :config
    (setq pyim-default-scheme 'xiaohe-shuangpin))

  (defun ivy--cregex-plus (str)
    (let ((x (ivy--regex-plus str))
          (case-fold-search nil))
      (if (listp x)
          (mapcar (lambda (y)
                    (cons (pyim-cregexp-build (car y))
                          (cdr y)))
                  x)
        (if (string= "" x) x (pyim-cregexp-build x)))))

  (defun ivy--cregex-ignore-order (str)
    (let ((x (ivy--regex-ignore-order str))
          (case-fold-search nil))
      (if (listp x)
          (mapcar (lambda (y)
                    (cons (pyim-cregexp-build (car y))
                          (cdr y)))
                  x)
        (if (string= "" x) x (pyim-cregexp-build x)))))

  (general-def '(ivy-minibuffer-map ivy-switch-buffer-map)
    "<tab>"    'ivy-tab
    "C-h"      'ivy-c-h
    "C-S-h"    help-map)
  :general
  (tyrant-def
    "bb" 'ivy-switch-buffer
    "rv" 'ivy-push-view
    "rV" 'ivy-pop-view
    "rl" 'ivy-resume))

(use-package ivy-hydra :ensure t :defer t)

(use-package ivy-rich
  :ensure t
  ;; if `counsel' loads after `ivy-rich', it overrides some of `ivy-rich''s
  ;; transformers
  :hook ((counsel-mode . ivy-rich-mode)
         (ivy-rich-mode . ivy-rich-project-root-cache-mode))
  :config
  (setq ivy-rich-path-style 'abbrev
        ivy-virtual-abbreviate 'full)

  ;; https://github.com/Yevgnen/ivy-rich/issues/87#issuecomment-689581896
  (progn
    (defvar ivy-rich-cache
      (make-hash-table :test 'equal))

    (defun ivy-rich-cache-lookup (delegate candidate)
      (let ((result (gethash candidate ivy-rich-cache)))
        (unless result
          (setq result (funcall delegate candidate))
          (puthash candidate result ivy-rich-cache))
        result))

    (defun ivy-rich-cache-reset ()
      (clrhash ivy-rich-cache))

    (defun ivy-rich-cache-rebuild ()
      (mapc (lambda (buffer)
              (ivy-rich--ivy-switch-buffer-transformer (buffer-name buffer)))
            (buffer-list)))

    (defun ivy-rich-cache-rebuild-trigger ()
      (ivy-rich-cache-reset)
      (run-with-idle-timer 1 nil 'ivy-rich-cache-rebuild))

    (advice-add 'ivy-rich--ivy-switch-buffer-transformer :around 'ivy-rich-cache-lookup)
    (advice-add 'ivy-switch-buffer :after 'ivy-rich-cache-rebuild-trigger)))

(use-package ivy-posframe
  :ensure t
  :hook (ivy-mode . ivy-posframe-mode)
  :custom-face (fringe ((t (:background nil))))
  :config
  (setq ivy-posframe-parameters '((left-fringe . 8)
                                  (right-fringe . 8))
        ivy-posframe-display-functions-alist
        '((complete-symbol . ivy-posframe-display-at-point)
          (swiper . ivy-display-function-fallback)
          (swiper-all . ivy-display-function-fallback)
          (t . ivy-posframe-display-at-frame-center))))

(use-package counsel
  :ensure t
  :hook (after-init . counsel-mode)
  :config
  (setq ivy-height-alist '((counsel-evil-registers . 20)))

  (defun counsel-rg-region-or-symbol ()
    "Use `counsel-rg' to search for
    the selected region or the symbol around point in the current
    directory."
    (interactive)
    (counsel-rg (if (region-active-p)
                    (buffer-substring-no-properties
                     (region-beginning) (region-end))
                  (thing-at-point 'symbol t))))
  :general
  (tyrant-def
    "fL" 'counsel-locate
    "rm" 'counsel-evil-marks
    "rr" 'counsel-evil-registers
    "sf" 'counsel-rg
    "sF" 'counsel-rg-region-or-symbol))

(use-package swiper
  :ensure t
  :config
  (defun counsel-current-region-or-symbol ()
    "Return contents of the region or symbol at point.

If region is active, mark will be deactivated in order to prevent region
expansion when jumping around the buffer with counsel. See `deactivate-mark'."
    (if (region-active-p)
        (prog1
            (buffer-substring-no-properties (region-beginning) (region-end))
          (deactivate-mark))
      (thing-at-point 'symbol t)))

  (defun swiper-region-or-symbol ()
    "Run `swiper' with the selected region or the symbol
around point as the initial input."
    (interactive)
    (let ((input (counsel-current-region-or-symbol)))
      (swiper input)))

  (defun swiper-all-region-or-symbol ()
    "Run `swiper-all' with the selected region or the symbol
around point as the initial input."
    (interactive)
    (let ((input (counsel-current-region-or-symbol)))
      (swiper-all input)))
  :general
  (tyrant-def
    "ss"  'swiper
    "sS"  'swiper-region-or-symbol
    "sb"  'swiper-all
    "sB"  'swiper-all-region-or-symbol))

(use-package counsel-tramp
  :ensure t
  :general
  (tyrant-def "ft" 'counsel-tramp))

(use-package ivy-prescient
  :ensure t
  :hook (ivy-mode . ivy-prescient-mode)
  :config
  (setq ivy-prescient-enable-filtering nil))

(use-package wgrep :ensure t :defer t)


(use-package company
  :ensure t
  :custom-face (company-tooltip-mouse ((t (:background nil))))
  :hook (after-init . global-company-mode)
  :init
  (setq company-idle-delay 0
        company-minimum-prefix-length 1
        company-require-match nil
        company-selection-wrap-around t
        company-show-numbers t
        company-tooltip-align-annotations t
        company-transformers '(company-sort-prefer-same-case-prefix)
        company-dabbrev-ignore-case nil
        company-dabbrev-downcase nil
        company-dabbrev-char-regexp "[A-Za-z-_\\.'/]"
        company-dabbrev-ignore-buffers "\\`[ *]\\|\\.pdf\\'"
        company-backends '(company-files
                           company-capf
                           (company-dabbrev-code company-keywords)
                           company-dabbrev
                           company-ispell)
        company-global-modes '(not erc-mode message-mode help-mode gud-mode eshell-mode shell-mode))
  :config
  ;; `yasnippet' integration
  (with-no-warnings
    (with-eval-after-load 'yasnippet
      (defun company-backend-with-yas (backend)
        "Add `yasnippet' to company backend."
        (if (and (listp backend) (member 'company-yasnippet backend))
            backend
          (append (if (consp backend) backend (list backend))
                  '(:with company-yasnippet))))

      (defun company-enable-yas (&rest _)
        "Enable `yasnippet' in `company'."
        (setq company-backends
              (mapcar #'company-backend-with-yas company-backends)))

      (company-enable-yas)

      (defun my-company-yasnippet-disable-inline (fun command &optional arg &rest _ignore)
        "Enable yasnippet but disable it inline."
        (if (eq command 'prefix)
            (when-let ((prefix (funcall fun 'prefix)))
              (unless (memq (char-before (- (point) (length prefix))) '(?. ?> ?\())
                prefix))
          (funcall fun command arg)))

      (advice-add #'company-yasnippet :around #'my-company-yasnippet-disable-inline))))

(use-package company-box
  :ensure t
  :hook (company-mode . company-box-mode)
  :config
  (setq company-box-backends-colors nil
        company-box-enable-icon nil
        company-box-scrollbar t))

(use-package company-prescient
  :ensure t
  :hook (company-mode . company-prescient-mode))

(use-package company-try-hard
  :ensure t
  :general
  (general-def "C-;" 'company-try-hard)
  (general-def company-active-map "C-;" 'company-try-hard))


(use-package lsp-mode
  :ensure t
  :hook ((prog-mode . (lambda ()
                        (unless (derived-mode-p 'emacs-lisp-mode 'lisp-mode)
                          (lsp))))
         (lsp-mode . lsp-enable-which-key-integration))
  :init
  (setq read-process-output-max (* 1024 1024))
  :config
  (setq lsp-auto-guess-root t
        lsp-completion-provider :none
        lsp-enable-dap-auto-configure nil
        lsp-enable-file-watchers nil
        lsp-enable-folding nil
        lsp-enable-indentation nil
        lsp-enable-links nil
        lsp-enable-on-type-formatting nil
        lsp-enable-symbol-highlighting nil
        lsp-semantic-tokens-enable nil
        lsp-enable-text-document-color t
        lsp-headerline-breadcrumb-enable nil
        lsp-keep-workspace-alive nil
        lsp-modeline-diagnostics-enable nil
        lsp-modeline-code-actions-enable nil
        lsp-signature-function 'lsp--eldoc-message
        lsp-signature-render-documentation nil
        lsp-keymap-prefix "SPC l")

  (defun lsp-tramp-connection (local-command &optional generate-error-file-fn)
    "Create LSP stdio connection named name.
LOCAL-COMMAND is either list of strings, string or function which
returns the command to execute."
    (defvar tramp-connection-properties)
    ;; Force a direct asynchronous process.
    (when (file-remote-p default-directory)
      (add-to-list 'tramp-connection-properties
                   (list (regexp-quote (file-remote-p default-directory))
                         "direct-async-process" t)))
    (list :connect (lambda (filter sentinel name environment-fn)
                     (let* ((final-command (lsp-resolve-final-function
                                            local-command))
                            (_stderr (or (when generate-error-file-fn
                                           (funcall generate-error-file-fn name))
                                         (format "/tmp/%s-%s-stderr" name
                                                 (cl-incf lsp--stderr-index))))
                            (process-name (generate-new-buffer-name name))
                            (process-environment
                             (lsp--compute-process-environment environment-fn))
                            (proc (make-process
                                   :name process-name
                                   :buffer (format "*%s*" process-name)
                                   :command final-command
                                   :connection-type 'pipe
                                   :coding 'no-conversion
                                   :noquery t
                                   :filter filter
                                   :sentinel sentinel
                                   :file-handler t)))
                       (cons proc proc)))
          :test? (lambda () (-> local-command lsp-resolve-final-function
                           lsp-server-present?))))

  (tyrant-def lsp-mode
    :definer 'minor-mode "l" lsp-command-map))

(use-package lsp-ivy
  :ensure t
  :commands lsp-ivy-workspace-symbol)


(use-package yasnippet
  :ensure t
  :hook (after-init . yas-global-mode)
  :config
  (setq yas-triggers-in-field t
        yas-wrap-around-region t)

  ;; disable yas minor mode map
  (setq yas-minor-mode-map (make-sparse-keymap)))

(use-package yasnippet-snippets
  :ensure t
  :after yasnippet)

(use-package yatemplate
  :ensure t
  :after yasnippet
  :config
  (setq yatemplate-dir (no-littering-expand-etc-file-name "templates/"))
  (yatemplate-fill-alist)
  (auto-insert-mode))


(provide 'editor-completion)
;;; editor-completion.el ends here
