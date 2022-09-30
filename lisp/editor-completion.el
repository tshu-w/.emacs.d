;;; editor-completion.el --- -*- lexical-binding: t; -*-

;; Copyright (C) 2020-2022  Tianshu Wang

;; Author: Tianshu Wang <wang@tianshu.me>

;;; Commentary:

;;; Code:

(use-package vertico
  :straight (:files (:defaults "extensions/*.el"))
  :hook (after-init . vertico-mode)
  :config
  (setq vertico-cycle t)

  ;; Hide commands in M-x which do not work in the current mode.
  (setq read-extended-command-predicate
        #'command-completion-default-include-p)

  (defun crm-indicator (args)
    (cons (format "[CRM%s] %s"
                  (if (string-match "\\*\\(.\\)" crm-separator)
                      (match-string 1 crm-separator)
                    "")
                  (car args))
          (cdr args)))
  (advice-add #'completing-read-multiple :filter-args #'crm-indicator)

  (use-package vertico-buffer
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
    (vertico-map "C-<return>" 'vertico-quick-exit)))

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
  (advice-add #'project-find-regexp :override #'consult-ripgrep)
  :config
  (setq consult-narrow-key "?"
        consult-preview-key (kbd "M-.")
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
    "jI" '("imenu-multi" . consult-imenu-multi)
    "fl" '("locate-files" . consult-find)
    "jj" '("search lines" . consult-line)
    "jJ" '("search lines a/ buffers" . consult-line-multi)
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

(use-package corfu
  :straight (:files (:defaults "extensions/*.el"))
  :hook (after-init . global-corfu-mode)
  :init
  (setq completion-cycle-threshold 3
        tab-always-indent 'complete
        tab-first-completion 'eol

        corfu-auto t
        corfu-auto-prefix 1
        corfu-bar-width 0.5
        corfu-cycle t
        corfu-on-exact-match nil
        corfu-preselect-first nil)
  :config
  (use-package corfu-history
    :hook (global-corfu-mode . corfu-history-mode))

  (defun corfu-enable-in-minibuffer ()
    "Enable Corfu in the minibuffer if `completion-at-point' is bound."
    (when (where-is-internal #'completion-at-point (list (current-local-map)))
      ;; (setq-local corfu-auto nil) Enable/disable auto completion
      (corfu-mode 1)))
  (add-hook 'minibuffer-setup-hook #'corfu-enable-in-minibuffer)

  (with-eval-after-load 'evil-collection
    (advice-add 'evil-collection-corfu-setup :after
                (defun resert-corfu-esc ()
                  (general-def 'insert corfu-map "<escape>" 'nil))))
  :general
  (general-def corfu-map
    "RET"    nil
    "M-RET"  'corfu-quick-insert
    "S-SPC"  'corfu-insert-separator))

(use-package cape
  :straight t
  :defer t
  :init
  (add-to-list 'completion-at-point-functions #'cape-file)
  (add-to-list 'completion-at-point-functions #'cape-dabbrev))

(use-package tabnine-capf
  :straight (:host github :repo "50ways2sayhard/tabnine-capf" :files ("*.el" "*.sh"))
  :hook (kill-emacs . tabnine-capf-kill-process)
  :init
  (add-to-list 'completion-at-point-functions #'tabnine-completion-at-point))

(use-package tempel
  :straight t
  :hook ((text-mode prog-mode) . tempel-setup-capf)
  :init
  (setq tempel-trigger-prefix "<"
        tempel-path (no-littering-expand-etc-file-name "templates"))
  :config
  (defun tempel-setup-capf ()
    (setq-local completion-at-point-functions
                (cons #'tempel-complete
                      completion-at-point-functions)))

  (defun tempel-hippie-try-expand (old)
    "Integrate with hippie expand.
Just put this function in `hippie-expand-try-functions-list'."
    (if (not old)
        (tempel-expand t)
      (undo 1)))
  (add-to-list 'hippie-expand-try-functions-list #'tempel-hippie-try-expand t))

(use-package eglot
  :straight t
  :commands expand-absolute-name
  :hook (eglot-managed-mode .  yas-minor-mode)
  :init
  (setq read-process-output-max (* 1024 1024))

  ;; HOLD: https://github.com/joaotavora/eglot/issues/884
  (use-package yasnippet
    :straight t
    :init
    (setq yas-minor-mode-map nil))
  :config
  (setq eglot-stay-out-of '(company)
        eglot-connect-timeout 10
        eglot-events-buffer-size 0
        eglot-ignored-server-capabilities nil)

  (defun expand-absolute-name (name)
    (if (file-name-absolute-p name)
        (tramp-file-local-name
         (expand-file-name
          (concat (file-remote-p default-directory) name)))
      name))

  (when (fboundp #'tabnine-completion-at-point)
    (add-hook 'eglot-managed-mode-hook
              (defun merge-eglot-with-tabnine ()
                (remove-hook 'completion-at-point-functions #'eglot-completion-at-point t)
                (add-hook 'completion-at-point-functions
                          (cape-super-capf
                           #'eglot-completion-at-point
                           #'tabnine-completion-at-point) nil t))))

  (tyrant-def "cE" 'eglot)

  (general-def eglot--managed-mode
    :states '(normal insert motion emacs)
    :keymaps 'override
    :prefix-map 'tyrant-eglot-map
    :definer 'minor-mode
    :prefix "SPC"
    :non-normal-prefix "S-SPC"
    "ce"  (cons "eglot" (make-sparse-keymap))
    "cea" 'eglot-code-actions
    "ceb" 'eglot-events-buffer
    "cer" 'eglot-rename
    "ceR" 'eglot-reconnect
    "cex" 'eglot-shutdown
    "ceX" 'eglot-shutdown-all
    "ce=" 'eglot-format))

(provide 'editor-completion)
;;; editor-completion.el ends here
