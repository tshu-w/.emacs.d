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
                    tab-width 4))))

(use-package blacken
  :ensure t
  :after python
  :config
  (setq blacken-fast-unsafe t
        blacken-line-length 'fill)
  (despot-def python-mode-map "=" 'blacken-buffer))

;; TODO: PR for https://github.com/purcell/emacs-reformatter/issues/32
(use-package python-isort
  :ensure t
  :after python
  :config
  (setq python-isort-arguments '("--ca" "--cs" "--stdout" "-"))
  (despot-def python-mode-map "i" 'python-isort))

(use-package lsp-pyright
  :ensure t
  :after python
  :config
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
