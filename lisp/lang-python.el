;;; lang-python.el --- -*- lexical-binding: t; -*-

;; Copyright (C) 2020-2021  Tianshu Wang

;; Author: Tianshu Wang <wang@tianshu.me>

;;; Commentary:

;;; Code:

(use-package python
  :defer t
  :config
  (setq python-indent-guess-indent-offset-verbose nil)

  (add-hook 'python-mode-hook
            (defun init-python-mode ()
              "Stuff to do when opening `python-mode' files."
              (set (make-local-variable 'comment-inline-offset) 2)
              (setq fill-column 88
                    tab-width 4)))

  (reformatter-define python-isort
    :program "isort"
    :args '("--combine-as" "--combine-star" "--atomic" "--stdout" "-"))

  (reformatter-define python-blacken
    :program "black"
    :args '("--fast" "-"))

  (despot-def python-mode-map
    "i" 'python-isort
    "=" 'python-blacken))

(use-package lsp-pyright
  :straight t
  :after python
  :config
  (defun expand-absolute-name (name)
    (if (file-name-absolute-p name)
        (tramp-file-local-name
         (expand-file-name
          (concat (file-remote-p default-directory) name)))
      name))

  (lsp-register-custom-settings
   `(("python.analysis.stubPath" (lambda () (expand-absolute-name lsp-pyright-stub-path)))
     ("python.venvPath" (lambda () (if lsp-pyright-venv-path
                                  (expand-absolute-name lsp-pyright-venv-path) "")))))

  (lsp-register-client
   (make-lsp-client :new-connection (lsp-tramp-connection (lambda ()
                                                            (cons "pyright-langserver"
                                                                  lsp-pyright-langserver-command-args)))
                    :major-modes '(python-mode)
                    :remote? t
                    :server-id 'pyright-remote
                    :multi-root lsp-pyright-multi-root
                    :initialization-options (lambda () (ht-merge (lsp-configuration-section "pyright")
                                                            (lsp-configuration-section "python")))
                    :initialized-fn (lambda (workspace)
                                      (with-lsp-workspace workspace
                                        (lsp--set-configuration
                                         (make-hash-table :test 'equal))))
                    :download-server-fn (lambda (_client callback error-callback _update?)
                                          (lsp-package-ensure 'pyright callback error-callback))
                    :notification-handlers (lsp-ht ("pyright/beginProgress" 'lsp-pyright--begin-progress-callback)
                                                   ("pyright/reportProgress" 'lsp-pyright--report-progress-callback)
                                                   ("pyright/endProgress" 'lsp-pyright--end-progress-callback)))))


(provide 'lang-python)
;;; lang-python.el ends here
