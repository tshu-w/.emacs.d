;;; org-capture-autorefile.el --- -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2025  Tianshu Wang

;; Author: Tianshu Wang <wang@tianshu.me>

;; This file is not part of GNU Emacs

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;;; Code:

(defvar org-capture-web-link-key "w")
(defvar org-capture-auto-refile-rules
  `(("https?://arxiv\\.org" ,org-project-file "Daily Papers")
    ("https?://dl\\.acm\\.org" ,org-project-file "Daily Papers")
    ("https?://git\\(?:hub\\|lab\\)\\.com" ,org-inbox-file "Repos")))

(add-to-list 'org-capture-templates
             `(,org-capture-web-link-key
               "Web" plain (file+function org-inbox-file org-capture-goto-link)
               "%i\n" :empty-lines 1 :immediate-finish t))

(defun org-capture-goto-link ()
  (let ((file (nth 1 (org-capture-get :target)))
        (headline (plist-get org-store-link-plist :description))
        (link (plist-get org-store-link-plist :link)))
    (org-capture-put :target (list 'file+headline file headline))
    (widen)
    (goto-char (point-min))
    (let (case-fold-search)
      (if (re-search-forward
           (format org-complex-heading-regexp-format
                   (regexp-quote headline)) nil t)
          (org-end-of-subtree)
        (org-capture-put :flag t)
        (goto-char (point-max))
        (or (bolp) (insert "\n"))
        (insert "* TODO " headline "\n")
        (insert "[[" link "]]\n")
        (point)))))

(defun org-refile-to (file headline)
  "`org-refile' to exact HEADLINE in FILE.
Create at the end of the FILE if HEADLINE doesn't exist."
  (let* ((buffer (or (find-buffer-visiting file)
                     (find-file-noselect file)))
         (pos (save-excursion
                (or (org-find-exact-headline-in-buffer headline buffer t)
                    (with-current-buffer buffer
                      (goto-char (point-max))
                      (unless (bolp) (insert "\n"))
                      (insert "* " headline "\n")
                      (point))))))
    (org-refile nil nil (list headline file nil pos))))

(defun org-capture-auto-refile ()
  (when (and (string= (org-capture-get :key) org-capture-web-link-key)
             (org-capture-get :flag))
    (catch 'break
      (dolist (rule org-capture-auto-refile-rules)
        (let ((regexp   (nth 0 rule))
              (file     (nth 1 rule))
              (headline (nth 2 rule))
              (link     (plist-get org-store-link-plist :link)))
          (when (string-match-p regexp link)
            (let ((base (or (buffer-base-buffer) (current-buffer)))
                  (pos (make-marker)))
              (set-marker pos (save-excursion (org-back-to-heading t) (point)) base)
              (save-window-excursion
                (with-current-buffer base
                  (org-with-point-at pos
                    (org-refile-to file headline)))))
            (throw 'break t)))))))
