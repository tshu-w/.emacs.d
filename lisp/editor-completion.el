;;; editor-completion.el --- -*- lexical-binding: t; -*-

;; Copyright (C) 2020-2021  Tianshu Wang

;; Author: Tianshu Wang <wang@tianshu.me>

;;; Commentary:

;;; Code:

(use-package vertico
  :ensure t
  :hook (after-init . vertico-mode)
  :config
  (setq vertico-cycle t)

  (use-package vertico-directory
    :hook (rfn-eshadow-update-overlay . vertico-directory-tidy)
    :general
    (vertico-map
     "RET"   'vertico-directory-enter
     "DEL"   'vertico-directory-delete-char
     "M-DEL" 'vertico-directory-delete-word))

  (use-package vertico-quick
    :general
    (vertico-map "C-q" 'vertico-quick-exit)))

(use-package marginalia
  :ensure t
  :hook (after-init . marginalia-mode))

(use-package orderless
  :ensure t
  :init
  (setq completion-styles '(orderless)
        completion-category-defaults nil)
  :config
  (setq orderless-style-dispatchers '(flex-if-twiddle
                                      without-if-bang))

  (defun flex-if-twiddle (pattern _index _total)
    (when (string-suffix-p "~" pattern)
      `(orderless-flex . ,(substring pattern 0 -1))))

  (defun without-if-bang (pattern _index _total)
    (cond
     ((equal "!" pattern)
      '(orderless-literal . ""))
     ((string-prefix-p "!" pattern)
      `(orderless-without-literal . ,(substring pattern 1)))))

  (use-package pyim
    :ensure t
    :commands pyim-cregexp-build
    :config
    (setq pyim-default-scheme 'xiaohe-shuangpin))

  (defun orderless-pinyin-regexp (func component)
    (let ((result (funcall func component)))
      (pyim-cregexp-build result)))

  (advice-add 'orderless-regexp :around #'orderless-pinyin-regexp)

  (advice-add 'company-capf
              :around
              (lambda (capf-fn &rest args)
                (let ((completion-styles '(basic partial-completion substring)))
                  (apply capf-fn args)))))

(use-package consult
  :ensure t
  :init
  (advice-add #'completing-read-multiple :override #'consult-completing-read-multiple)
  :config
  (setq consult-narrow-key "<"
        consult-project-root-function (lambda () (project-root (project-current t))))

  (consult-customize consult-theme
                     :preview-key '(:debounce 0.2 any)
                     consult-ripgrep consult-git-grep consult-grep
                     consult-bookmark consult-recent-file consult-xref
                     consult--source-file consult--source-project-file consult--source-bookmark
                     :preview-key (kbd "M-."))
  :general
  ([remap switch-to-buffer] 'consult-buffer
   [remap imenu] 'consult-imenu)
  (tyrant-def
    "sI" '(consult-imenu-multi :which-key "imenu-multi")
    "sf" '(consult-find :which-key "locate files")
    "ss" '(consult-line :which-key "search lines")
    "sS" '(consult-line-multi :which-key "search lines a/ buffers")
    "/"  '(consult-ripgrep :which-key "search w/ regex")
    "tt" 'consult-minor-mode-menu))

(use-package embark
  :ensure t
  :config
  (with-eval-after-load 'which-key
    (defun embark-which-key-indicator ()
      "An embark indicator that displays keymaps using which-key.
The which-key help message will show the type and value of the
current target followed by an ellipsis if there are further
targets."
      (lambda (&optional keymap targets prefix)
        (if (null keymap)
            (which-key--hide-popup-ignore-command)
          (which-key--show-keymap
           (if (eq (caar targets) 'embark-become)
               "Become"
             (format "Act on %s '%s'%s"
                     (plist-get (car targets) :type)
                     (embark--truncate-target (plist-get (car targets) :target))
                     (if (cdr targets) "…" "")))
           (if prefix
               (pcase (lookup-key keymap prefix 'accept-default)
                 ((and (pred keymapp) km) km)
                 (_ (key-binding prefix 'accept-default)))
             keymap)
           nil nil t (lambda (binding)
                       (not (string-suffix-p "-argument" (cdr binding))))))))

    (setq embark-indicators '(embark-which-key-indicator
                              embark-highlight-indicator
                              embark-isearch-highlight-indicator)))

  (with-eval-after-load 'vertico
    (defun embark-vertico-indicator ()
      (let ((fr face-remapping-alist))
        (lambda (&optional keymap _targets prefix)
          (when (bound-and-true-p vertico--input)
            (setq-local face-remapping-alist
                        (if keymap
                            (cons '(vertico-current . embark-target) fr)
                          fr))))))

    (add-to-list 'embark-indicators #'embark-vertico-indicator))
  :general
  ("C-." 'embark-act
   "M-." 'embark-dwim)
  (embark-file-map "s" 'sudo-edit))

(use-package company
  :ensure t
  :custom-face (company-tooltip-mouse ((t (:background nil))))
  :hook (after-init . global-company-mode)
  :init
  (setq company-idle-delay 0
        company-minimum-prefix-length 1
        company-require-match nil
        company-selection-wrap-around t
        company-show-quick-access t
        company-tooltip-align-annotations t
        company-transformers '(delete-dups company-sort-prefer-same-case-prefix)
        company-dabbrev-ignore-case nil
        company-dabbrev-downcase nil
        company-dabbrev-char-regexp "[A-Za-z-_\\'/]"
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
        company-box-scrollbar nil))

(use-package company-try-hard
  :ensure t
  :general
  (general-def "C-;" 'company-try-hard)
  (general-def company-active-map "C-;" 'company-try-hard))

(use-package company-tabnine
  :ensure t
  :defer t
  :init
  (setq company-backends '(company-files
                           (company-capf :with company-tabnine :separate)
                           (company-dabbrev-code company-keywords)
                           company-dabbrev
                           company-tabnine
                           company-ispell)))


(use-package prescient
  :ensure t
  :hook (after-init . prescient-persist-mode)
  :init
  (use-package company-prescient
    :ensure t
    :hook (company-mode . company-prescient-mode))
  :config
  (setq prescient-sort-full-matches-first t
        prescient-sort-length-enable nil))


(use-package lsp-mode
  :ensure t
  :hook ((prog-mode . (lambda ()
                        (unless (derived-mode-p 'emacs-lisp-mode 'lisp-mode)
                          (lsp))))
         (lsp-mode . lsp-enable-which-key-integration))
  :init
  (setq read-process-output-max (* 1024 1024))
  :config
  (setq lsp-completion-provider :none
        lsp-diagnostic-clean-after-change t
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

  ;; https://github.com/emacs-lsp/lsp-mode/issues/2932
  (defun lsp-restart ()
    (interactive)
    (lsp-disconnect)
    (setq lsp--session nil)
    (lsp))

  (tyrant-def lsp-mode :definer 'minor-mode
    "l"  '(:keymap lsp-command-map)
    "lR" '(lsp-restart :which-key "restart")))


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


(provide 'editor-completion)
;;; editor-completion.el ends here
