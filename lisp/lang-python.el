;;; lang-python.el --- -*- lexical-binding: t; -*-

;; Copyright (C) 2020  Tianshu Wang

;; Author: Tianshu Wang <volekingsg@gmail.com>

;;; Commentary:

;;; Code:

(use-package python
  :defer t
  :config
  (setq-default python-indent-guess-indent-offset nil)

  (add-hook 'python-mode-hook
            (lambda ()
              (set (make-local-variable 'comment-inline-offset) 2)))

  ;; inferior-python-mode needs these variables to be defined. The python
  ;; package declares them but does not initialize them.
  (defvar python-shell--interpreter nil)
  (defvar python-shell--interpreter-args nil)

  ;; REPL
  (defun +python-shell-send-buffer-switch ()
    "Send buffer content to shell and switch to it in insert mode."
    (interactive)
    (let ((python-mode-hook nil))
      (python-shell-send-buffer)
      (python-shell-switch-to-shell)))

  (defun +python-shell-send-buffer ()
    "Send buffer content to shell and switch to it in insert mode."
    (interactive)
    (let ((python-mode-hook nil))
      (python-shell-send-buffer)))

  (defun +python-shell-send-defun-switch ()
    "Send function content to shell and switch to it in insert mode."
    (interactive)
    (let ((python-mode-hook nil))
      (python-shell-send-defun nil)
      (python-shell-switch-to-shell)))

  (defun +python-shell-send-defun ()
    "Send function content to shell and switch to it in insert mode."
    (interactive)
    (let ((python-mode-hook nil))
      (python-shell-send-defun nil)))

  (defun +python-shell-send-region-switch (start end)
    "Send region content to shell and switch to it in insert mode."
    (interactive "r")
    (let ((python-mode-hook nil))
      (python-shell-send-region start end)
      (python-shell-switch-to-shell)))

  (defun +python-shell-send-region (start end)
    "Send region content to shell and switch to it in insert mode."
    (interactive "r")
    (let ((python-mode-hook nil))
      (python-shell-send-region start end)))

  (defun +python-start-or-switch-repl ()
    "Start and/or switch to the REPL."
    (interactive)
    (let ((shell-process
           (or (python-shell-get-process)
               ;; `run-python' has different return values and different
               ;; errors in different emacs versions. In 24.4, it throws an
               ;; error when the process didn't start, but in 25.1 it
               ;; doesn't throw an error, so we demote errors here and
               ;; check the process later
               (with-demoted-errors "Error: %S"
                 ;; in Emacs 24.5 and 24.4, `run-python' doesn't return the
                 ;; shell process
                 (call-interactively #'run-python)
                 (python-shell-get-process)))))
      (unless shell-process
        (error "Failed to start python shell properly"))
      (pop-to-buffer (process-buffer shell-process))))

  (defun +python-execute-file (arg)
    "Execute a python script in a shell."
    (interactive "P")
    (let ((compile-command
           (format "%s %s" python-shell-interpreter
                   (shell-quote-argument
                    (file-name-nondirectory buffer-file-name)))))
      (if arg
          (call-interactively 'compile)
        (compile compile-command t)
        (with-current-buffer (get-buffer "*compilation*")
          (inferior-python-mode)))))

  (defun +python-execute-file-focus (arg)
    "Execute a python script in a shell and switch to the shell buffer in
 `insert state'."
    (interactive "P")
    (+python-execute-file arg)
    (switch-to-buffer-other-window "*compilation*")
    (goto-char (point-max)))

  ;; from https://www.snip2code.com/Snippet/127022/Emacs-auto-remove-unused-import-statemen
  (defun +python-remove-unused-imports()
    "Use Autoflake to remove unused function."
    (interactive)
    (if (executable-find "autoflake")
        (progn
          (shell-command (format "autoflake --remove-all-unused-imports -i %s"
                                 (shell-quote-argument (buffer-file-name))))
          (revert-buffer t t t))
      (message "Error: Cannot find autoflake executable.")))

  (defun +python-toggle-breakpoint ()
    "Add a break point, highlight it."
    (interactive)
    (let ((trace (cond ((executable-find "python3.7") "breakpoint()")
                       ((executable-find "python3.8") "breakpoint()")
                       (t "import pdb; pdb.set_trace()")))
          (line (thing-at-point 'line)))
      (if (and line (string-match trace line))
          (kill-whole-line)
        (progn
          (back-to-indentation)
          (insert trace)
          (insert "\n")
          (python-indent-line)))))

  (despot-def python-mode-map
    "'"     '+python-start-or-switch-repl
    "c"     '(:ignore t :which-key "execute")
    "cc"    '+python-execute-file
    "cC"    '+python-execute-file-focus
    "d"     '(:ignore t :which-key "debug")
    "db"    '+python-toggle-breakpoint
    "r"     '(:ignore t :which-key "refactor")
    "ri"    '+python-remove-unused-imports
    "s"     '(:ignore t :which-key "REPL")
    "sB"    '+python-shell-send-buffer-switch
    "sb"    '+python-shell-send-buffer
    "sF"    '+python-shell-send-defun-switch
    "sf"    '+python-shell-send-defun
    "si"    '+python-start-or-switch-repl
    "sR"    '+python-shell-send-region-switch
    "sr"    '+python-shell-send-region
    "v"     '(:ignore t :which-key "virtualenv")))

(use-package blacken
  :ensure t
  :after python
  :config
  (setq blacken-fast-unsafe t)
  (despot-def python-mode-map "=" 'blacken-buffer))

(use-package cython-mode :ensure t :defer t)

(use-package pip-requirements :ensure t :defer t)

(use-package importmagic
  :ensure t
  :hook (python-mode . importmagic-mode)
  :config
  (despot-def python-mode-map "rf" 'importmagic-fix-symbol-at-point))

(use-package py-isort
  :ensure t
  :after python
  :config
  (despot-def python-mode-map "rI" 'py-isort-buffer))

(use-package conda
  :ensure t
  :after python
  :config
  (when (memq window-system '(mac ns))
    (setq conda-anaconda-home "/usr/local/anaconda3/"))

  (add-hook 'python-mode-hook (lambda ()
                                (unless
                                    (file-remote-p default-directory)
                                  conda-env-autoactivate-mode)))

  (despot-def python-mode-map
    "vl" 'conda-env-list
    "va" 'conda-env-activate
    "vd" 'conda-env-deactivate
    "vA" 'conda-env-autoactivate-mode
    "vb" 'conda-env-activate-for-buffer))

(use-package pytest
  :ensure t
  :after python
  :config
  (add-to-list 'pytest-project-root-files "setup.cfg")

  (despot-def python-mode-map
    "t" '(:ignore t :which-key "test")
    "ta" 'pytest-all
    "tA" 'pytest-pdb-all
    "td" 'pytest-directory
    "tf" 'pytest-last-failed
    "tF" 'pytest-pdb-last-failed
    "tl" 'pytest-again
    "tm" 'pytest-module
    "tM" 'pytest-pdb-module
    "to" 'pytest-one
    "tO" 'pytest-pdb-one))

(use-package lsp-pyright :ensure t :after python)


(provide 'lang-python)
;;; lang-python.el ends here
