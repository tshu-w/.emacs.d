;;; editor-checker.el --- -*- lexical-binding: t; -*-
;;; Commentary:

;; Copyright (C) 2020-2021  Tianshu Wang

;; Author: Tianshu Wang <wang@tianshu.me>

;;; Commentary:

;;; Code:

(use-package flycheck
  :straight t
  :hook (prog-mode . flycheck-mode)
  :custom-face (flycheck-info ((t (:underline nil))))
  :config
  (setq flycheck-check-syntax-automatically '(save mode-enabled)
        flycheck-display-errors-delay 0.25
        flycheck-emacs-lisp-load-path 'inherit)

  ;; Custom fringe indicator
  (when (fboundp 'define-fringe-bitmap)
    (define-fringe-bitmap 'flycheck-fringe-indicator
      (vector #b00000000
              #b00000000
              #b00000000
              #b00000000
              #b00000000
              #b00000000
              #b00000000
              #b00011100
              #b00111110
              #b00111110
              #b00111110
              #b00011100
              #b00000000
              #b00000000
              #b00000000
              #b00000000
              #b00000000))
    (let ((bitmap 'flycheck-fringe-indicator))
      (flycheck-define-error-level 'error
        :severity 2
        :overlay-category 'flycheck-error-overlay
        :fringe-bitmap bitmap
        :error-list-face 'flycheck-error-list-error
        :fringe-face 'flycheck-fringe-error)
      (flycheck-define-error-level 'warning
        :severity 1
        :overlay-category 'flycheck-warning-overlay
        :fringe-bitmap bitmap
        :error-list-face 'flycheck-error-list-warning
        :fringe-face 'flycheck-fringe-warning)
      (flycheck-define-error-level 'info
        :severity 0
        :overlay-category 'flycheck-info-overlay
        :fringe-bitmap bitmap
        :error-list-face 'flycheck-error-list-info
        :fringe-face 'flycheck-fringe-info)))

  (defun toggle-flycheck-error-list ()
    "Toggle flycheck's error list window.
If the error list is visible, hide it.  Otherwise, show it."
    (interactive)
    (-if-let (window (flycheck-get-error-list-window))
        (quit-window nil window)
      (flycheck-list-errors)))

  (defun goto-flycheck-error-list ()
    "Open and go to the error list buffer."
    (interactive)
    (if (flycheck-get-error-list-window)
        (switch-to-buffer flycheck-error-list-buffer)
      (progn
        (flycheck-list-errors)
        (switch-to-buffer-other-window flycheck-error-list-buffer))))
  :general
  (tyrant-def
    "e"  '(:ignore t :which-key "errors")
    "eb" 'flycheck-buffer
    "ec" 'flycheck-clear
    "eh" 'flycheck-describe-checker
    "el" 'toggle-flycheck-error-list
    "eL" 'goto-flycheck-error-list
    "en" 'next-error
    "ep" 'previous-error
    "es" 'flycheck-select-checker
    "eS" 'flycheck-set-checker-executable
    "ev" 'flycheck-verify-setup
    "ey" 'flycheck-copy-errors-as-kill
    "ex" 'flycheck-explain-error-at-point
    "ts" 'flycheck-mode))

(use-package flycheck-posframe
  :straight t
  :if (display-graphic-p)
  :custom-face (flycheck-posframe-border-face ((t (:inherit default))))
  :hook (flycheck-mode . flycheck-posframe-mode)
  :config
  (setq flycheck-posframe-border-width 1
        flycheck-posframe-warning-prefix "⚠ "
        flycheck-posframe-info-prefix "··· "
        flycheck-posframe-error-prefix "✕ ")

  (with-eval-after-load 'company
    ;; Don't display popups if company is open
    (add-hook 'flycheck-posframe-inhibit-functions #'company--active-p))

  (with-eval-after-load 'evil
    ;; Don't display popups while in insert or replace mode, as it can affect
    ;; the cursor's position or cause disruptive input delays.
    (add-hook 'flycheck-posframe-inhibit-functions #'evil-insert-state-p)
    (add-hook 'flycheck-posframe-inhibit-functions #'evil-replace-state-p)))

(use-package flyspell
  :init
  (setq flyspell-issue-message-flag nil
        flyspell-issue-welcome-flag nil)
  :config
  (defun add-word-to-dict-buffer ()
    "Save word at point as correct in current buffer."
    (interactive)
    (add-word-to-dict 'buffer))

  (defun add-word-to-dict-global ()
    "Save word at point as a correct word globally."
    (interactive)
    (add-word-to-dict 'save))

  (defun add-word-to-dict-session ()
    "Save word at point as correct in current session."
    (interactive)
    (add-word-to-dict 'session))

  (defun add-word-to-dict (scope)
    "Save word at point as a correct word.
SCOPE can be:
`save' to save globally,
`session' to save in current session or
`buffer' for buffer local."
    (let ((current-location (point))
          (word (flyspell-get-word)))
      (when (consp word)
        (if (word-in-dict-p (car word))
            (error "%s is already in dictionary" (car word))
          (progn
            (flyspell-do-correct scope nil (car word) current-location
                                 (cadr word) (caddr word) current-location)
            (ispell-pdict-save t))))))

  (defun word-in-dict-p (word)
    "Check if WORD is defined in any of the active dictionaries."
    ;; use the correct dictionary
    (flyspell-accept-buffer-local-defs)
    (let (poss ispell-filter)
      ;; now check spelling of word.
      (ispell-send-string "%\n")	;put in verbose mode
      (ispell-send-string (concat "^" word "\n"))
      ;; wait until ispell has processed word
      (while (progn
               (accept-process-output ispell-process)
               (not (string= "" (car ispell-filter)))))
      ;; Remove leading empty element
      (setq ispell-filter (cdr ispell-filter))
      ;; ispell process should return something after word is sent.
      ;; Tag word as valid (i.e., skip) otherwise
      (or ispell-filter
          (setq ispell-filter '(*)))
      (if (consp ispell-filter)
          (setq poss (ispell-parse-output (car ispell-filter))))
      (or (eq poss t) (stringp poss))))
  :general
  (tyrant-def
    "S"   '(:ignore t :which-key "spelling")
    "Sa"  '(:ignore t :which-key "add word to dict")
    "Sab" 'add-word-to-dict-buffer
    "Sag" 'add-word-to-dict-global
    "Sas" 'add-word-to-dict-session
    "Sb"  'flyspell-buffer
    "Sn"  'flyspell-goto-next-error
    "Sr"  'flyspell-region
    "tS"  'flyspell-mode))

(use-package flyspell-correct
  :straight t
  :general
  (tyrant-def
    "Sc" 'flyspell-correct-wrapper
    "Ss" 'flyspell-correct-at-point))


(provide 'editor-checker)
;;; editor-checker.el ends here
