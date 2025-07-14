;;; lang-org.el --- -*- lexical-binding: t; -*-

;; Copyright (C) 2020-2025  Tianshu Wang

;; Author: Tianshu Wang <wang@tianshu.me>

;;; Commentary:

;;; Code:

(use-package org
  :init
  (setq org-directory "~/Documents/Org/"
        org-note-directory (concat org-directory "note/")
        org-inbox-file (concat org-directory "inbox.org")
        org-log-file (concat org-directory "log.org")
        org-project-file (concat org-directory "projects.org")
        org-default-notes-file org-inbox-file
        org-modules '(ol-docview ol-info org-id org-habit))

  (advice-add 'server-execute :before
              (defun enable-org-protocol (&rest r)
                (unless (featurep 'org-protocol) (require 'org-protocol))))

  (autoload 'org-super-agenda "org-agenda")
  :config
  (setq org-todo-keywords
        '((sequence "NEXT(n!)" "TODO(t)" "HOLD(h!)" "|" "DONE(d)" "CXLD(c)")))

  (setq org-columns-default-format "%40ITEM %1PRIORITY %20TAGS %6Effort(EFFORT){:} %8CLOCKSUM"
        org-cycle-open-archived-trees t
        org-fontify-quote-and-verse-blocks t
        org-fontify-whole-heading-line t
        org-global-properties '(("STYLE_ALL" . "habit")
                                ("Effort_ALL" . "0:10 0:15 0:30 0:45 1:00 2:00 3:00 5:00"))
        org-hide-emphasis-markers t
        org-hide-leading-stars t
        org-image-actual-width '(0.7)
        org-imenu-depth 3
        org-log-done 'time
        org-log-into-drawer t
        org-log-redeadline nil
        org-log-reschedule nil
        org-pretty-entities t
        org-read-date-prefer-future nil
        org-reverse-note-order t
        org-startup-folded t
        org-startup-indented t
        org-startup-with-inline-images t
        org-tags-column 0
        org-track-ordered-property-with-tag t
        org-use-property-inheritance t
        org-use-sub-superscripts '{}
        org-export-with-sub-superscripts '{}
        org-export-backends '(beamer html icalendar latex md)
        org-yank-adjusted-subtrees t)

  (add-hook 'org-mode-hook
            (defun init-org-mode ()
              "Stuff to do when opening `org-mode' files."
              (setq truncate-lines nil)
              ;; disable <> auto pairing in electric-pair-mode for org-mode
              (setq-local electric-pair-inhibit-predicate
                          `(lambda (c) (if (char-equal c ?<) t
                                    (,electric-pair-inhibit-predicate c))))

              (setq imenu-create-index-function #'org-imenu-get-tree)))

  (defun open-org-inbox-file ()
    "Open `org-inbox-file'"
    (interactive)
    (find-file org-inbox-file))

  (defalias 'open-org-default-notes-file #'open-org-inbox-file
    "Open `org-default-notes-file'")

  (defun open-org-project-file ()
    "Open `org-project-file'"
    (interactive)
    (find-file org-project-file))

  (unless (fboundp 'find-lisp-find-files)
    (autoload #'find-lisp-find-files "find-lisp"))

  (defun org-note-files ()
    "Get the list of `org-mode' file in `org-note-directory'."
    (find-lisp-find-files org-note-directory "\.org$"))

  (use-package org-agenda
    :defer t
    :init
    (setq org-agenda-files `(,org-directory))
    :config
    (setq org-agenda-bulk-custom-functions '((?R org-datetree-refile))
          org-agenda-clockreport-parameter-plist '(:maxlevel 5)
          org-agenda-columns-add-appointments-to-effort-sum t
          org-agenda-include-diary nil
          org-agenda-prefix-format '((agenda . " %i %-16:c%?-16t% s")
                                     (todo . " %i %-16:c")
                                     (tags . " %i %-16:c")
                                     (search . " %i %-16:c"))
          org-habit-graph-column 60
          org-agenda-persistent-filter t
          org-agenda-restore-windows-after-quit t
          org-agenda-skip-deadline-prewarning-if-scheduled t
          org-agenda-skip-scheduled-if-done t
          org-agenda-skip-deadline-if-done t
          org-agenda-skip-timestamp-if-done t
          org-agenda-skip-scheduled-if-deadline-is-shown t
          org-agenda-span 'day
          org-agenda-time-grid '((daily require-timed)
                                 (800 1000 1200 1400 1600 1800 2000)
                                 "......" "----------------")
          org-agenda-time-leading-zero t
          org-agenda-todo-ignore-scheduled 'all
          org-agenda-todo-ignore-deadlines 'near
          org-agenda-window-setup 'only-window
          org-enforce-todo-dependencies t
          org-enforce-todo-checkbox-dependencies nil
          org-stuck-projects '("PROJ+LEVEL=1/-DONE-CXLD" ("NEXT") nil "\\"))

    (setq org-agenda-custom-commands
          `(("o" "Super agenda"
             ((agenda "")
              (todo "NEXT" ((org-agenda-overriding-header "\nNext actions")))
              (todo "TODO" ((org-agenda-overriding-header "\nInbox")
                            (org-agenda-files `(,org-inbox-file))))
              (tags "CLOSED>=<today>"
                    ((org-agenda-overriding-header "\nCompleted today")
                     (org-agenda-archives-mode t))))
             ((org-agenda-block-separator nil)))

            ("r" "Agenda review"
             ((agenda "" ((org-agenda-span 7)))
              (stuck "")
              (todo "NEXT")
              (todo "TODO")
              (todo "HOLD"))
             ((org-agenda-archives-mode t)
              (org-agenda-compact-blocks nil)
              (org-agenda-show-all-dates nil)))))

    (defun org-super-agenda (&optional arg)
      (interactive "P")
      (org-agenda arg "o"))

    (advice-add 'org-agenda-exit :after #'org-save-all-org-buffers)

    (setq org-priority-get-priority-function
          (defun org-inherited-priority (s)
            (cond
             ;; Priority cookie in this heading
             ((string-match org-priority-regexp s)
              (* 1000 (- org-priority-lowest
                         (org-priority-to-value (match-string 2 s)))))
             ;; No priority cookie, but already at highest level
             ((not (org-up-heading-safe))
              (* 1000 (- org-priority-lowest org-priority-default)))
             ;; Look for the parent's priority
             (t (org-inherited-priority (org-get-heading)))))))

  (use-package org-attach
    :defer t
    :commands (org-attach-follow org-attach-complete-link)
    :init
    (org-link-set-parameters "attachment"
                             :follow #'org-attach-follow
                             :complete #'org-attach-complete-link)
    :config
    (setq org-attach-archive-delete 'query
          org-attach-id-dir (concat org-directory "attach/")
          org-attach-method 'mv
          org-attach-store-link-p 'file))

  (use-package ob
    :defer t
    :init
    (setq org-babel-load-languages nil
          org-confirm-babel-evaluate nil
          org-edit-src-content-indentation 0
          org-src-fontify-natively t
          org-src-preserve-indentation t
          org-src-tab-acts-natively nil)
    :config
    (defun ob-fix-inline-images ()
      "Fix redisplay of inline images after a code block evaluation."
      (when org-inline-image-overlays
        (org-redisplay-inline-images)))
    (add-hook 'org-babel-after-execute-hook #'ob-fix-inline-images)

    (defun org-babel-execute-src-block@before (&optional _arg info _params)
      "Load language if needed"
      (let* ((lang (nth 0 info))
             (sym (if (member (downcase lang) '("c" "cpp" "c++")) 'C (intern lang)))
             (backup-languages org-babel-load-languages))
        ;; - (LANG . nil) forbidden languages that are not loaded.
        ;; - (LANG . t) The loaded language is not repeated.
        (unless (assoc sym backup-languages)
          (condition-case err
              (progn
                (org-babel-do-load-languages 'org-babel-load-languages (list (cons sym t)))
                (setq-default org-babel-load-languages (append (list (cons sym t)) backup-languages)))
            (file-missing
             (setq-default org-babel-load-languages backup-languages)
             err)))))

    (advice-add 'org-babel-execute-src-block :before #'org-babel-execute-src-block@before))

  (use-package oc
    :defer t
    :config
    (setq org-cite-export-processors '((beamer natbib)
                                       (latex biblatex)
                                       (t csl))
          org-cite-global-bibliography '("~/Documents/Bibliography/references.bib")))

  (use-package org-capture
    :defer t
    :init
    (setq org-capture-templates
          `(("i" "Item" entry (file org-inbox-file)
             "* %?\n%i\n" :empty-lines 1 :prepend ,org-reverse-note-order)
            ("I" "Item w/ Link" entry (file org-inbox-file)
             "* %?\n%a\n%i\n" :empty-lines 1 :prepend ,org-reverse-note-order)
            ("t" "Todo" entry (file org-inbox-file)
             "* TODO %?\n%i\n" :empty-lines 1 :prepend ,org-reverse-note-order)
            ("T" "Todo w/ Link" entry (file org-inbox-file)
             "* TODO %?\n%a\n%i\n" :empty-lines 1 :prepend ,org-reverse-note-order)
            ("l" "Log" entry (file+function org-log-file
                                            org-reverse-datetree-goto-date-in-file)
             "* %?\n" :clock-in t :clock-keep t :empty-lines 1)

            ("r"  "Review")
            ("ry" "Yesterday" plain
             (file+function org-log-file
                            (lambda () (org-reverse-datetree-goto-date-in-file
                                        (time-add (current-time) (days-to-time -1)))))
             "%?\n%i\n" :immediate-finish t :jump-to-captured t)
            ("rt" "Today" plain
             (file+function org-log-file
                            (lambda () (org-reverse-datetree-goto-date-in-file)))
             "%?\n%i\n" :immediate-finish t :jump-to-captured t)
            ("rl" "Last Week" plain
             (file+function org-log-file
                            (lambda () (let ((org-reverse-datetree-level-formats
                                         (butlast org-reverse-datetree-level-formats)))
                                    (org-reverse-datetree-goto-date-in-file
                                     (time-add (current-time) (days-to-time -7))))))
             "%?\n%i\n" :immediate-finish t :jump-to-captured t)
            ("rw" "This Week" plain
             (file+function org-log-file
                            (lambda () (let ((org-reverse-datetree-level-formats
                                         (butlast org-reverse-datetree-level-formats)))
                                    (org-reverse-datetree-goto-date-in-file))))
             "%?\n%i\n" :immediate-finish t :jump-to-captured t)
            ("rD" "Select a Date" plain
             (file+function org-log-file
                            org-reverse-datetree-goto-read-date-in-file)
             "%?\n%i\n" :immediate-finish t :jump-to-captured t)
            ("rW" "Select a Week" plain
             (file+function org-log-file
                            (lambda () (let ((org-reverse-datetree-level-formats
                                         (butlast org-reverse-datetree-level-formats)))
                                    (org-reverse-datetree-goto-read-date-in-file))))
             "%?\n%i\n" :immediate-finish t :jump-to-captured t)
            ("rM" "Select a Month" plain
             (file+function org-log-file
                            (lambda () (let ((org-reverse-datetree-level-formats
                                         (butlast org-reverse-datetree-level-formats 2)))
                                    (org-reverse-datetree-goto-read-date-in-file))))
             "%?\n%i\n" :immediate-finish t :jump-to-captured t)
            ("rY" "Select a Year" plain
             (file+function org-log-file
                            (lambda () (let ((org-reverse-datetree-level-formats
                                         (butlast org-reverse-datetree-level-formats 3)))
                                    (org-reverse-datetree-goto-read-date-in-file))))
             "%?\n%i\n" :immediate-finish t :jump-to-captured t)

            ("p"  "Protocol")
            ("pw" "Web" plain (file+function org-inbox-file org-capture-goto-link)
             "%i\n" :empty-lines 1 :immediate-finish t :prepend nil)))
    :config
    (defun org-capture-goto-link ()
      (let ((file (nth 1 (org-capture-get :target)))
            (headline (plist-get org-store-link-plist :description))
            (link (plist-get org-store-link-plist :link)))
        (widen)
        (goto-char (point-min))
        (let (case-fold-search)
          (if (re-search-forward
               (format org-complex-heading-regexp-format
                       (regexp-quote headline)) nil t)
              (org-end-of-subtree)
            (org-capture-put :type `entry
                             :template "* TODO %:description\n%l\n%i\n")))))

    (when (eq system-type 'darwin)
      (defun org-capture-after-finalize ()
        (when (string-prefix-p "p" (org-capture-get :key))
          (run-at-time 0.25 nil #'ns-switch-back-to-previous-application)))

      (add-hook 'org-capture-after-finalize-hook #'org-capture-after-finalize)))

  (use-package org-clock
    :defer t
    :init
    (org-clock-persistence-insinuate)
    (org-clock-auto-clockout-insinuate)
    :config
    (setq org-clock-auto-clock-resolution 'when-no-clock-is-running
          org-clock-history-length 10
          org-clock-idle-time 10
          org-clock-in-resume t
          org-clock-persist t
          org-clock-persist-query-resume nil
          org-clock-out-remove-zero-time-clocks t
          org-clock-out-when-done t
          org-clock-report-include-clocking-task t

          org-clock-reminder-timer (run-with-timer
                                    t (* org-clock-idle-time 60)
                                    (lambda ()
                                      (unless (org-clocking-p)
                                        (alert "Do you forget to clock-in?"
                                               :title "Org Clock")))))

    (plist-put org-clocktable-defaults :maxlevel 5)
    (plist-put org-clocktable-defaults :link t)
    (plist-put org-clocktable-defaults :formula '%)
    (plist-put org-clocktable-defaults :fileskip0 t)
    (plist-put org-clocktable-defaults :tcolumns 1)
    (plist-put org-clocktable-defaults :properties '("Effort")))

  (use-package ox-latex
    :defer t
    :init
    (setq org-latex-compiler "xelatex"
          org-latex-pdf-process '("latexmk -f -pdf -%latex -interaction=nonstopmode -output-directory=%o %f"
                                  "latexmk -c -bibtex")
          org-latex-prefer-user-labels t
          org-preview-latex-default-process 'dvisvgm
          org-preview-latex-image-directory (no-littering-expand-var-file-name "ltximg/")
          org-preview-latex-process-alist
          '((dvisvgm :programs ("xelatex" "dvisvgm")
                     :description "xdv > svg"
                     :message "you need to install the programs: xelatex and dvisvgm."
                     :image-input-type "xdv" :image-output-type "svg" :image-size-adjust (1.7 . 1.5)
                     :latex-compiler ("xelatex -no-pdf -interaction nonstopmode -output-directory %o %f")
                     :image-converter ("dvisvgm %f --no-fonts --exact-bbox --scale=%S --output=%O"))))

    (plist-put org-format-latex-options :scale 1.5))

  (use-package org-refile
    :defer t
    :config
    (setq org-outline-path-complete-in-steps nil
          org-refile-allow-creating-parent-nodes 'confirm
          org-refile-use-outline-path 'file
          org-refile-targets '((nil :maxlevel . 4)
                               (org-agenda-files :maxlevel . 3))))

  (setq org-fast-tag-selection-single-key t
        org-tags-match-list-sublevels 'intented
        org-tag-alist '((:startgroup)
                        ("@office"  . ?o)
                        ("@home"    . ?h)
                        (:endgroup)
                        (:startgroup)
                        ("@computer")
                        ("@phone")
                        ("@pad")
                        (:endgroup)
                        ("PROJ"     . ?p)
                        ("NOTE"     . ?n)
                        (:newline)
                        ("trivia"   . ?t)
                        ("errand"   . ?e)
                        ("action"   . ?a)
                        ("focused"  . ?f)
                        ("interest" . ?i)))

  (defun org-link-make-string@replace-vertical-bar-in-description (fn link &optional description)
    "Replace vertical-bar with hypen in org link `DESCRIPTION'."
    (apply fn link (list
                    (when description
                      (replace-regexp-in-string "|" "-" description)))))
  (advice-add 'org-link-make-string :around #'org-link-make-string@replace-vertical-bar-in-description)

  (progn
    (defmacro +org-emphasize (fname char)
      "Make function for setting the emphasis in org mode"
      `(defun ,fname () (interactive)
              (org-emphasize ,char)))

    (+org-emphasize org-bold ?*)
    (+org-emphasize org-code ?~)
    (+org-emphasize org-italic ?/)
    (+org-emphasize org-clear ? )
    (+org-emphasize org-strike-through ?+)
    (+org-emphasize org-underline ?_)
    (+org-emphasize org-verbatim ?=))

  ;; from org-checklist.el
  (defun org-reset-checkbox-state-maybe ()
    "Reset all checkboxes in an entry if the `RESET_CHECK_BOXES' property is set"
    (interactive "*")
    (if (org-entry-get (point) "RESET_CHECK_BOXES")
        (org-reset-checkbox-state-subtree)))

  (defun org-checklist ()
    (when (member org-state org-done-keywords) ;; org-state dynamically bound in org.el/org-todo
      (org-reset-checkbox-state-maybe)))

  (add-hook 'org-after-todo-state-change-hook 'org-checklist)

  (despot-def org-mode-map
    "'"     'org-edit-special
    ","     'org-ctrl-c-ctrl-c
    "*"     'org-ctrl-c-star
    "-"     'org-ctrl-c-minus
    "#"     'org-update-statistics-cookies
    "RET"   'org-ctrl-c-ret
    "M-RET" 'org-meta-return
    "a"     'org-agenda
    "A"     'org-attach
    "b"     (cons "babel" (make-sparse-keymap))
    "ba"    'org-babel-sha1-hash
    "bb"    'org-babel-execute-buffer
    "bc"    'org-babel-check-src-block
    "bd"    'org-babel-demarcate-block
    "be"    'org-babel-execute-maybe
    "bf"    'org-babel-tangle-file
    "bg"    'org-babel-goto-named-src-block
    "bi"    'org-babel-lob-ingest
    "bI"    'org-babel-view-src-block-info
    "bj"    'org-babel-insert-header-arg
    "bl"    'org-babel-load-in-session
    "bn"    'org-babel-next-src-block
    "bo"    'org-babel-open-src-block-result
    "bp"    'org-babel-previous-src-block
    "br"    'org-babel-goto-named-result
    "bs"    'org-babel-execute-subtree
    "bt"    'org-babel-tangle
    "bu"    'org-babel-goto-src-block-head
    "bv"    'org-babel-expand-src-block
    "bx"    'org-babel-do-key-sequence-in-edit-buffer
    "bz"    'org-babel-switch-to-session
    "bZ"    'org-babel-switch-to-session-with-code
    "c"     'org-capture
    "C"     (cons "clocks" (make-sparse-keymap))
    "Cc"    'org-clock-cancel
    "Cd"    'org-clock-display
    "Ce"    'org-evaluate-time-range
    "Cg"    'org-clock-goto
    "Ci"    'org-clock-in
    "CI"    'org-clock-in-last
    "Cj"    'org-clock-jump-to-current-clock
    "Co"    'org-clock-out
    "CR"    'org-clock-report
    "Cr"    'org-resolve-clocks
    "d"     (cons "dates" (make-sparse-keymap))
    "dd"    'org-deadline
    "ds"    'org-schedule
    "dt"    'org-time-stamp
    "dT"    'org-time-stamp-inactive
    "e"     'org-export-dispatch
    "i"     (cons "insert" (make-sparse-keymap))
    "ia"    'org-attach
    "ib"    'org-insert-structure-template
    "ic"    'org-cite-insert
    "id"    'org-insert-drawer
    "ie"    'org-set-effort
    "if"    'org-footnote-new
    "ig"    'org-mac-link-get-link
    "ih"    'org-insert-heading
    "iH"    'org-insert-heading-after-current
    "ii"    'org-id-get-create
    "iI"    'org-insert-item
    "il"    'org-insert-link
    "in"    'org-add-note
    "ip"    'org-set-property
    "is"    'org-insert-subheading
    "it"    'org-set-tags-command
    "p"     'org-priority
    "s"     (cons "trees/subtrees" (make-sparse-keymap))
    "sa"    'org-toggle-archive-tag
    "sA"    'org-archive-to-archive-sibling
    "s$"    'org-archive-subtree-default
    "sb"    'org-tree-to-indirect-buffer
    "sd"    'org-cut-subtree
    "sh"    'org-promote-subtree
    "sj"    'org-move-subtree-down
    "sk"    'org-move-subtree-up
    "sl"    'org-demote-subtree
    "sn"    'org-narrow-to-subtree
    "sr"    'org-refile
    "sR"    'org-datetree-refile
    "ss"    'org-sparse-tree
    "sS"    'org-sort
    "sw"    'widen
    "t"     (cons "tables" (make-sparse-keymap))
    "ta"    'org-table-align
    "tb"    'org-table-blank-field
    "tc"    'org-table-convert
    "td"    (cons "delete" (make-sparse-keymap))
    "tdc"   'org-table-delete-column
    "tdr"   'org-table-kill-row
    "te"    'org-table-eval-formula
    "tE"    'org-table-export
    "tf"    'org-table-field-info
    "th"    'org-table-previous-field
    "tH"    'org-table-move-column-left
    "ti"    (cons "insert" (make-sparse-keymap))
    "tic"   'org-table-insert-column
    "tih"   'org-table-insert-hline
    "tiH"   'org-table-hline-and-move
    "tir"   'org-table-insert-row
    "tI"    'org-table-import
    "tj"    'org-table-next-row
    "tJ"    'org-table-move-row-down
    "tK"    'org-table-move-row-up
    "tl"    'org-table-next-field
    "tL"    'org-table-move-column-right
    "tn"    'org-table-create
    "tN"    'org-table-create-with-table.el
    "tp"    'org-plot/gnuplot
    "tr"    'org-table-recalculate
    "ts"    'org-table-sort-lines
    "tt"    (cons "toggles" (make-sparse-keymap))
    "ttf"   'org-table-toggle-formula-debugger
    "tto"   'org-table-toggle-coordinate-overlays
    "tw"    'org-table-wrap-region
    "T"     (cons "toggles" (make-sparse-keymap))
    "Tc"    'org-toggle-checkbox
    "Te"    'org-toggle-pretty-entities
    "Ti"    'org-toggle-inline-images
    "Tl"    'org-toggle-link-display
    "Tt"    'org-show-todo-tree
    "TV"    'space-doc-mode
    "Tx"    'org-latex-preview
    "x"     (cons "text" (make-sparse-keymap))
    "xb"    'org-bold
    "xc"    'org-code
    "xi"    'org-italic
    "xo"    'org-open-at-point
    "xr"    'org-clear
    "xs"    'org-strike-through
    "xu"    'org-underline
    "xv"    'org-verbatim)

  (general-def 'normal org-mode-map "RET" 'org-open-at-point)
  :general
  (tyrant-def
    "o"      (cons "org" (make-sparse-keymap))
    "o/"     'org-occur-in-agenda-files
    "oa"     'org-agenda
    "oc"     'org-capture
    "oC"     (cons "clock" (make-sparse-keymap))
    "oCc"    'org-clock-cancel
    "oCg"    'org-clock-goto
    "oCi"    'org-clock-in-last
    "oCj"    'org-clock-jump-to-current-clock
    "oCo"    'org-clock-out
    "oCr"    'org-resolve-clocks
    "od"     'open-org-default-notes-file
    "ol"     'org-store-link
    "oo"     'org-super-agenda
    "op"     'open-org-project-file))

(use-package org-mac-link
  :straight t
  :defer t
  :config
  (setq org-mac-link-devonthink-app-p nil
        org-mac-link-acrobat-app-p nil
        org-mac-link-brave-app-p nil
        org-mac-link-chrome-app-p nil
        org-mac-link-evernote-app-p nil
        org-mac-link-mail-app-p nil
        org-mac-link-outlook-app-p nil
        org-mac-link-qutebrowser-app-p nil))

(use-package evil-org
  :straight t
  :after evil
  :hook (org-mode . evil-org-mode)
  :init
  (setq evil-org-key-theme '(navigation insert textobjects additional todo heading))

  (with-eval-after-load 'org-agenda
    (autoload #'evil-org-agenda-set-keys "evil-org-agenda" nil t)
    (evil-org-agenda-set-keys))

  (with-eval-after-load 'org-capture
    (add-hook 'org-capture-mode-hook #'evil-insert-state)
    (add-hook 'org-capture-after-finalize-hook #'evil-normal-state)
    (add-hook 'org-capture-after-finalize-hook #'evil-refresh-cursor)))

(use-package orgonomic
  :straight (:host github :repo "aaronjensen/emacs-orgonomic")
  :hook (org-mode . orgonomic-mode))

(use-package org-appear
  :straight t
  :hook (org-mode . org-appear-mode)
  :config
  (setq org-appear-autolinks t
        org-appear-autosubmarkers t
        org-appear-autoentities t))

(use-package org-modern
  :straight t
  :after org
  :hook (org-mode . org-modern-mode)
  :config
  (setq org-modern-star '("◉" "○" "◎" "▣" "▢" "◈" "◇")
        org-modern-hide-stars nil
        org-modern-todo nil
        org-modern-priority nil
        org-modern-statistics nil
        org-modern-tag nil
        org-modern-table-vertical 1
        org-modern-timestamp nil))

(use-package org-project
  :straight (:host github :repo "delehef/org-project")
  :init
  (setq org-project-link-heading nil
        org-project-prompt-for-project t
        org-project-todos-file org-project-file)
  :config
  (defun org-project-open ()
    "Open the project file and jump to the project heading only if exsits."
    (interactive)
    (if (and (project-current)
             (let* ((projectpath (org-project--current-project))
                    (file (org-project--get-capture-file projectpath))
                    (headline (org-project--build-heading projectpath)))
               (with-current-buffer (org-capture-target-buffer file)
                 (widen)
	             (goto-char (point-min))
                 (re-search-forward
                  (format org-complex-heading-regexp-format
					      (regexp-quote headline)) nil t))))
        (org-project-open-todos)
      (open-org-project-file)))
  :general
  (tyrant-def "op" 'org-project-open)
  (general-def project-prefix-map
    "o" 'org-project-quick-capture
    "O" 'org-project-capture))

(use-package org-reverse-datetree
  :straight t
  :commands org-datetree-refile
  :init
  (setq-default org-reverse-datetree-level-formats '("%Y" "%Y-%m %B" "%Y W%W" "%Y-%m-%d %A"))
  :config
  (defun org-datetree-refile ()
    (interactive)
    (org-reverse-datetree-refile-to-file org-log-file)))

(use-package org-node
  :straight t
  :init
  (with-eval-after-load 'org (require 'org-node))
  :config
  (setq org-mem-do-sync-with-org-id t
        org-mem-watch-dirs `(,org-note-directory)
        org-node-file-directory-ask 'org-mem-watch-dirs
        org-node-affixation-fn 'org-node-prepend-olp-append-tags
        org-node-alter-candidates t
        org-node-blank-input-hint nil)

  (org-mem-updater-mode)
  (org-node-cache-mode)

  (org-node-backlink-mode)
  (org-node-complete-at-point-mode)
  (add-hook 'org-open-at-point-functions #'org-node-try-visit-ref-node)
  (remove-hook 'org-node-creation-hook   #'org-node-ensure-crtime-property)

  (defun org-node-add-date-after-title ()
    "Add a #+date: line after #+title: line if it exists and no #+date: already present."
    (save-excursion
      (goto-char (point-min))
      (when (re-search-forward "^#\\+title:" nil t)
        (let ((title-end (line-end-position)))
          (unless (re-search-forward "^#\\+date:" nil t)
            (goto-char title-end)
            (insert "\n#+date: " (format-time-string (org-time-stamp-format))))))))
  (add-hook 'org-node-creation-hook #'org-node-add-date-after-title)

  (defun quote-initial ()
    (let ((initial (plist-get org-store-link-plist :initial)))
      (if (equal initial "") ""
        (format "#+begin_quote\n%s\n#+end_quote" initial))))

  (defun org-node-ref-capture ()
    "Capture a reference node."
    (org-node-cache-ensure)
    (let* ((title (plist-get org-store-link-plist :description))
           (link (plist-get org-store-link-plist :link))
           (node (gethash title org-node--candidate<>entry))
           (org-node-file-directory-ask (concat org-note-directory "refs/")))
      (if node
          (progn (org-node--goto node t)
                 (org-end-of-subtree))
        (org-node-create title (org-id-new))
        (unless (org-entry-get nil "ROAM_REFS")
          (org-entry-put nil "ROAM_REFS" link))
        (end-of-buffer))))

  (with-eval-after-load 'org-capture
    (add-to-list 'org-capture-templates
                 '("pr" "Node Ref" plain (function org-node-ref-capture)
                   "%?\n%(quote-initial)" :unnarrowed t) t))

  (defun org-node-open-refs ()
    "Open REFs of the node at point."
    (interactive)
    (save-excursion
      (if-let ((node (org-node-at-point)))
          (goto-char (org-mem-entry-pos node)))
      (when-let* ((p (org-entry-get (point) "ROAM_REFS"))
                  (refs (when p (split-string-and-unquote p)))
                  (refs (if (length> refs 1)
                            (completing-read-multiple "Open: " refs)
                          refs))
                  (user-error "No ROAM_REFS found"))

        (when-let ((oc-cites (seq-map
                              (lambda (ref) (substring ref 1))
                              (seq-filter (apply-partially #'string-prefix-p "@") refs))))
          (citar-run-default-action oc-cites))

        (dolist (ref refs)
          (unless (string-prefix-p "@" ref)
            (browse-url ref))))))

  (despot-def org-mode-map
    "n"  (cons "note" (make-sparse-keymap))
    "nc" 'org-node-insert-transclusion
    "nC" 'org-node-insert-transclusion-as-subtree
    "ne" 'org-node-extract-subtree
    "nh" 'org-node-insert-heading
    "ni" 'org-node-insert-link
    "no" 'org-node-open-refs
    "nr" 'org-node-visit-random
    "nR" 'org-node-refile
    "nt" 'org-node-add-tags)
  :general
  (tyrant-def "on" 'org-node-find))


(provide 'lang-org)
;;; lang-org.el ends here
