;;; editor-completion.el --- -*- lexical-binding: t; -*-

;; Copyright (C) 2020-2023  Tianshu Wang

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
  (advice-add #'project-switch-to-buffer :override #'consult-project-buffer)
  :config
  (setq consult-narrow-key "?"
        consult-preview-key "M-.")

  (defun consult-delete-default-contents()
    (remove-hook 'pre-command-hook 'consult-delete-default-contents)
    (cond ((member this-command '(self-insert-command))
           (delete-minibuffer-contents))
          (t (put-text-property (minibuffer-prompt-end) (point-max) 'face 'default))))

  (consult-customize consult-theme
                     :preview-key '(:debounce 0.2 any)
                     consult-goto-line consult-imenu consult-line
                     :preview-key 'any
                     consult-line
                     :initial (when-let ((string (thing-at-point 'word)))
                                (add-hook 'pre-command-hook 'consult-delete-default-contents)
                                (propertize string 'face 'shadow)))

  (defvar consult--source-project-file
    `(:name     "Project File"
      :narrow   ?f
      :category file
      :face     consult-file
      :history  file-name-history
      :state    ,#'consult--file-state
      :enabled  ,(lambda () consult-project-function)
      :items
      ,(lambda ()
         (when-let (project (project-current t))
           (let* ((all-files (project-files project))
                  (common-parent-directory
                   (let ((common-prefix (try-completion "" all-files)))
                     (if (> (length common-prefix) 0)
                         (file-name-directory common-prefix))))
                  (cpd-length (length common-parent-directory))
                  items)
             (print all-files)
             (dolist (file all-files items)
               (let ((part (substring file cpd-length)))
                 (when (equal part "") (setq part "./"))
                 (put-text-property 0 1 'multi-category `(file . ,file) part)
                 (push part items))))))
      "Project file candidate source for `consult-buffer'."))

  (defvar consult--source-project-file-hidden
    `(:hidden t :narrow (?f . "Project File") ,@consult--source-project-file)
    "Like `consult--source-project-file' but hidden by default.")

  (defvar consult--source-project-recent-file-override
    `(:name "Recent File" :narrow (?r . "Recent File") ,@consult--source-project-file)
    "Like `consult--source-recent-file' but overridden the narrow key.")

  (setq consult-project-buffer-sources
        '(consult--source-project-buffer
          consult--source-project-recent-file-override
          consult--source-project-file-hidden))
  :general
  ([remap switch-to-buffer]    'consult-buffer
   [remap goto-line]           'consult-goto-line
   [remap imenu]               'consult-imenu)
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
        corfu-preselect 'prompt)

  (use-package corfu-history
    :hook (global-corfu-mode . corfu-history-mode))
  (use-package corfu-popupinfo
    :hook (global-corfu-mode . corfu-popupinfo-mode)
    :config
    (set-face-attribute 'corfu-popupinfo nil :height 0.95))
  :config
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
  (corfu-map
   "RET"    nil
   "M-RET"  'corfu-quick-insert
   "S-SPC"  'corfu-insert-separator))

(use-package cape
  :straight t
  :defer t
  :init
  (add-to-list 'completion-at-point-functions #'cape-file)
  (add-to-list 'completion-at-point-functions #'cape-dabbrev))

(use-package prescient
  :straight t
  :hook (after-init . prescient-persist-mode)
  :init
  (use-package vertico-prescient
    :straight t
    :hook (vertico-mode . vertico-prescient-mode)
    :init
    (setq vertico-prescient-enable-filtering nil))
  (use-package corfu-prescient
    :straight t
    :hook (corfu-mode . corfu-prescient-mode)
    :init
    (setq corfu-prescient-enable-filtering nil))
  :config
  (setq prescient-sort-full-matches-first t
        prescient-sort-length-enable nil))

(use-package tabnine-capf
  :straight (:host github :repo "50ways2sayhard/tabnine-capf" :files ("*.el" "*.sh"))
  :hook (kill-emacs . tabnine-capf-kill-process)
  :init
  (add-to-list 'completion-at-point-functions #'tabnine-completion-at-point))

(use-package copilot
  :straight (:host github :repo "zerolfx/copilot.el" :files ("dist" "*.el"))
  :hook (prog-mode . copilot-mode)
  :general
  ('insert copilot-mode-map
           "C-f" 'copilot-accept-completion
           "M-f" 'copilot-accept-completion-by-word
           "C-e" 'copilot-accept-completion-by-line
           "M-p" 'copilot-previous-completion
           "M-n" 'copilot-next-completion))

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
        eglot-ignored-server-capabilities nil)

  (defun expand-absolute-name (name)
    (if (file-name-absolute-p name)
        (tramp-file-local-name
         (expand-file-name
          (concat (file-remote-p default-directory) name)))
      name))

  (when (fboundp #'tabnine-completion-at-point)
    (add-hook 'eglot-managed-mode-hook
              (defun eglot-capf ()
                (remove-hook 'completion-at-point-functions #'eglot-completion-at-point t)
                (add-hook 'completion-at-point-functions
                          (cape-super-capf
                           #'eglot-completion-at-point
                           #'tabnine-completion-at-point) nil t))))

  (advice-add 'eglot-completion-at-point :around #'cape-wrap-buster)

  ;; TODO:
  ;; https://github.com/joaotavora/eglot/discussions/876
  ;; https://github.com/microsoft/pyright/issues/3282
  (defun eglot--uri-to-path@around (fun url)
    (let* ((uri (if (equal url "")
                    (project-root (eglot--project (eglot-current-server))) url))
           (path (funcall fun uri)))
      (if (file-directory-p path)
          (file-name-as-directory path)
        path)))
  (advice-add #'eglot--uri-to-path :around #'eglot--uri-to-path@around)

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
    "ce=" 'eglot-format)
  :general
  (tyrant-def "cE" 'eglot))

(provide 'editor-completion)
;;; editor-completion.el ends here
