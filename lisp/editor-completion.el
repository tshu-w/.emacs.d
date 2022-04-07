;;; editor-completion.el --- -*- lexical-binding: t; -*-

;; Copyright (C) 2020-2022  Tianshu Wang

;; Author: Tianshu Wang <wang@tianshu.me>

;;; Commentary:

;;; Code:

(use-package vertico
  :straight (:files (:defaults "extensions/*.el"))
  :hook (after-init . vertico-mode)
  :config
  ;; Hide commands in M-x which do not work in the current mode.
  (setq read-extended-command-predicate
        #'command-completion-default-include-p)

  (setq vertico-cycle t)

  (use-package vertico-buffer
    :defer t
    :hook (vertico-mode . vertico-buffer-mode)
    :config
    (setq vertico-buffer-display-action `(display-buffer-in-side-window
                                          (window-height . ,(+ 3 vertico-count))
                                          (side . top))))

  (use-package vertico-directory
    :hook (rfn-eshadow-update-overlay . vertico-directory-tidy)
    :general
    (vertico-map "RET"   'vertico-directory-enter
                 "DEL"   'vertico-directory-delete-char
                 "M-DEL" 'vertico-directory-delete-word))

  (use-package vertico-quick
    :general
    (vertico-map "C-q" 'vertico-quick-exit)))

(use-package marginalia
  :straight t
  :hook (after-init . marginalia-mode))

(use-package orderless
  :straight t
  :defer t
  :init
  (setq completion-styles '(orderless basic)
        completion-category-defaults nil)
  :config
  (defun flex-if-twiddle (pattern _index _total)
    (when (string-suffix-p "~" pattern)
      `(orderless-flex . ,(substring pattern 0 -1))))

  (defun without-if-bang (pattern _index _total)
    (cond
     ((equal "!" pattern)
      '(orderless-literal . ""))
     ((string-prefix-p "!" pattern)
      `(orderless-without-literal . ,(substring pattern 1)))))

  (setq orderless-style-dispatchers '(flex-if-twiddle without-if-bang))

  (advice-add 'company-capf
              :around
              (lambda (capf-fn &rest args)
                (let ((completion-styles '(basic partial-completion substring)))
                  (apply capf-fn args)))))

(use-package consult
  :straight t
  :init
  (advice-add #'completing-read-multiple :override #'consult-completing-read-multiple)
  (advice-add #'project-find-regexp :override #'consult-ripgrep)
  :config
  (setq consult-narrow-key "?"
        consult-preview-key (kbd "M-.")
        consult-project-root-function (lambda () (project-root (project-current t)))

        consult-ripgrep-args "rg --hidden --glob \"!.git/\" --null --line-buffered --color=never --max-columns=1000 --path-separator / --smart-case --no-heading --line-number .")

  (consult-customize consult-theme :preview-key '(:debounce 0.2 any)
                     consult-goto-line consult-imenu consult-line
                     :preview-key 'any)
  :general
  ([remap switch-to-buffer]    'consult-buffer
   [remap goto-line]           'consult-goto-line
   [remap imenu]               'consult-imenu
   [remap apropos]             'consult-apropos)
  (tyrant-def
    "jI" '(consult-imenu-multi :which-key "imenu-multi")
    "fl" '(consult-find :which-key "locate-files")
    "jj" '(consult-line :which-key "search lines")
    "jJ" '(consult-line-multi :which-key "search lines a/ buffers")
    "Tt" 'consult-minor-mode-menu)
  (org-mode-map
   [remap consult-imenu]       'consult-org-heading
   [remap consult-imenu-multi] 'consult-org-agenda))

(use-package embark
  :straight t
  :init
  (with-eval-after-load 'avy
    (defun avy-action-embark (pt)
      (unwind-protect
          (save-excursion
            (goto-char pt)
            (embark-act))
        (select-window
         (cdr (ring-ref avy-ring 0))))
      t)
    (setf (alist-get ?. avy-dispatch-alist) 'avy-action-embark))
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
                              embark-isearch-highlight-indicator))

    (defun embark-hide-which-key-indicator (fn &rest args)
      "Hide the which-key indicator immediately when using the completing-read prompter."
      (when-let ((win (get-buffer-window which-key--buffer
                                         'visible)))
        (quit-window 'kill-buffer win)
        (let ((embark-indicators (delq #'embark-which-key-indicator embark-indicators)))
          (apply fn args))))

    (advice-add #'embark-completing-read-prompter
                :around #'embark-hide-which-key-indicator))

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
  (:keymaps '(global normal)
            "C-." 'embark-act
            "M-." 'embark-dwim))

(use-package embark-consult
  :straight t
  :demand t
  :after (consult embark)
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

(use-package wgrep :straight t :defer t)


(use-package company
  :straight t
  :custom-face (company-tooltip-mouse ((t (:background nil))))
  :hook ((after-init . global-company-mode)
         (after-init . company-tng-mode))
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
        company-dabbrev-char-regexp "[A-Za-z-_]"
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

      (defun company-yasnippet-disable-inline (fun command &optional arg &rest _ignore)
        "Enable yasnippet but disable it inline."
        (if (eq command 'prefix)
            (when-let ((prefix (funcall fun 'prefix)))
              (unless (memq (char-before (- (point) (length prefix))) '(?. ?> ?\())
                prefix))
          (funcall fun command arg)))

      (advice-add #'company-yasnippet :around #'company-yasnippet-disable-inline))))

(use-package company-box
  :straight t
  :hook (company-mode . company-box-mode)
  :config
  (setq company-box-backends-colors nil
        company-box-enable-icon nil
        company-box-scrollbar nil
        company-box-doc-frame-parameters '((internal-border-width . 3))))

(use-package company-try-hard
  :straight t
  :general
  ("C-;" 'company-try-hard))

(use-package company-tabnine
  :straight t
  :defer t
  :init
  (setq company-backends '(company-files
                           (company-capf :with company-tabnine :separate)
                           (company-dabbrev-code company-keywords)
                           company-dabbrev
                           company-tabnine
                           company-ispell)))


(use-package prescient
  :straight t
  :hook (after-init . prescient-persist-mode)
  :init
  (use-package company-prescient
    :straight t
    :hook (company-mode . company-prescient-mode))
  :config
  (setq prescient-sort-full-matches-first t
        prescient-sort-length-enable nil))

(use-package eglot
  :straight t
  :commands expand-absolute-name
  :init
  (setq read-process-output-max (* 1024 1024))
  :config
  (setq eglot-stay-out-of '(company))

  ;; https://github.com/company-mode/company-mode/discussions/1313
  (advice-remove #'eglot--snippet-expansion-fn #'ignore)

  (defun expand-absolute-name (name)
    (if (file-name-absolute-p name)
        (tramp-file-local-name
         (expand-file-name
          (concat (file-remote-p default-directory) name)))
      name))

  (tyrant-def "cE" 'eglot)

  (tyrant-def eglot--managed-mode :definer 'minor-mode
    "ce"  '(:ignore t :which-key "eglot")
    "cea" 'eglot-code-actions
    "ceb" 'eglot-events-buffer
    "cer" 'eglot-rename
    "ceR" 'eglot-reconnect
    "cex" 'eglot-shutdown
    "ceX" 'eglot-shutdown-all
    "ce=" 'eglot-format))

(use-package yasnippet
  :straight t
  :hook (after-init . yas-global-mode)
  :init
  (setq yas-minor-mode-map nil)
  :config
  (add-to-list 'hippie-expand-try-functions-list 'yas-hippie-try-expand t)

  (setq yas-triggers-in-field t
        yas-wrap-around-region t))


(provide 'editor-completion)
;;; editor-completion.el ends here
