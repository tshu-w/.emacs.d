;;; lang-org.el --- -*- lexical-binding: t; -*-

;; Copyright (C) 2020-2021  Tianshu Wang

;; Author: Tianshu Wang <wang@tianshu.me>

;;; Commentary:

;;; Code:

(use-package org
  :ensure org-plus-contrib
  :defer t
  :init
  (setq org-directory "~/Documents/Org/"
        org-note-directory (concat org-directory "notes/")
        org-journal-directory (concat org-directory "journals/")
        org-inbox-file (concat org-directory "inbox.org")
        org-project-file (concat org-directory "projects.org")
        org-default-notes-file org-inbox-file)

  (defadvice server-execute (before enable-org-protocol activate)
    (unless (featurep 'org-protocol) (require 'org-protocol)))
  :config
  (add-to-list 'org-modules 'org-tempo t)
  (add-to-list 'org-modules 'org-protocol t)

  (setq org-todo-keywords
        '((sequence "TODO(t)" "|" "DONE(d)")
          (sequence "WAITING(w@)" "SOMEDAY(s)" "|" "CANCELED(c)"))
        org-todo-keyword-faces
        '(("CANCELED" . org-done)
          ("WAITING" . (:foreground "light coral" :weight bold))
          ("SOMEDAY" . (:foreground "plum" :weight bold))))

  (setq org-columns-default-format "%40ITEM %1PRIORITY %20TAGS %6Effort(EFFORT){:} %8CLOCKSUM"
        org-fontify-quote-and-verse-blocks t
        org-fontify-whole-heading-line t
        org-global-properties '(("STYLE_ALL" . "habit")
                                ("Effort_ALL" . "0:10 0:15 0:30 0:45 1:00 2:00 3:00 5:00"))
        org-hide-emphasis-markers t
        org-highlight-latex-and-related '(native script entities)
        org-image-actual-width '(500)
        org-imenu-depth 3
        org-log-done 'time
        org-log-into-drawer t
        org-preview-latex-image-directory (no-littering-expand-var-file-name "ltximg/")
        org-startup-folded t
        org-startup-indented t
        org-startup-with-inline-images t
        org-track-ordered-property-with-tag t
        org-use-property-inheritance t
        org-use-sub-superscripts "{}"
        org-yank-adjusted-subtrees t)

  (add-hook 'org-mode-hook
            (defun init-org-mode ()
              "Stuff to do when opening `org-mode' files."
              (setq truncate-lines nil)
              ;; disable <> auto pairing in electric-pair-mode for org-mode
              (setq-local electric-pair-inhibit-predicate
                          `(lambda (c) (if (char-equal c ?<) t
                                    (,electric-pair-inhibit-predicate c))))

              ;; auto update modified time-stamp when saving
              (setq-local time-stamp-pattern "^#\\+last_modified:[ \t]%%$"
                          time-stamp-format "[%Y-%m-%d %a]")
              (add-hook 'before-save-hook 'time-stamp nil t)))

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

  (defun org-journal-files ()
    "Get the list of `org-mode' file in `org-journal-directory'."
    (find-lisp-find-files org-journal-directory "\.org$"))

  (use-package org-agenda
    :defer t
    :init
    (setq org-agenda-files `(,org-directory ,org-journal-directory))
    :config
    (add-to-list 'org-modules 'org-habit t)

    (setq org-agenda-clockreport-parameter-plist '(:maxlevel 5 :scope agenda-with-archives)
          org-agenda-columns-add-appointments-to-effort-sum t
          org-agenda-compact-blocks t
          org-agenda-dim-blocked-tasks t
          org-agenda-persistent-filter t
          org-agenda-restore-windows-after-quit t
          org-agenda-skip-additional-timestamps-same-entry t
          org-agenda-skip-deadline-prewarning-if-scheduled t
          org-agenda-skip-scheduled-if-done t
          org-agenda-skip-deadline-if-done t
          org-agenda-skip-timestamp-if-done t
          org-agenda-skip-scheduled-if-deadline-is-shown t
          org-agenda-sorting-strategy '((agenda time-up priority-down)
                                        (todo todo-state-up priority-down category-keep)
                                        (tags todo-state-up priority-down category-keep)
                                        (search category-keep))
          org-agenda-span 'day
          org-agenda-start-on-weekday nil
          org-agenda-time-grid nil
          org-agenda-time-leading-zero t
          org-agenda-todo-ignore-scheduled 'all
          org-agenda-todo-ignore-deadlines 'near
          org-deadline-warning-days 10
          org-enforce-todo-dependencies t
          org-enforce-todo-checkbox-dependencies nil
          org-habit-graph-column 75
          org-stuck-projects '("+PROJ/-WAITING-SOMEDAY-DONE-CANCELED" ("TODO") nil ""))

    (setq org-agenda-custom-commands
          '(("r" . "Review")
            ("ry" "Yesterday"
             ((agenda "" ((org-agenda-span 1))))
             ((org-agenda-start-day "-1d")
              (org-agenda-start-with-log-mode '(closed clock state))
              (org-agenda-start-with-clockreport-mode t)
              (org-agenda-archives-mode t)))
            ("rt" "Today"
             ((agenda "" ((org-agenda-span 1))))
             ((org-agenda-start-with-log-mode '(closed clock state))
              (org-agenda-start-with-clockreport-mode t)
              (org-agenda-archives-mode t)))
            ("rw" "Last Week"
             ((agenda "" ((org-agenda-span 7)
                          (org-agenda-start-on-weekday 1)))
              (todo "TODO" ((org-agenda-overriding-header "All TODO items without scheduled or deadline")
                            (org-agenda-skip-function '(or (org-agenda-skip-entry-if 'timestamp)
                                                           (org-agenda-skip-subtree-if 'regexp "habit")))))
              (stuck "")
              (todo "WAITING")
              (todo "SOMEDAY"))
             ((org-agenda-start-day "-1w")
              (org-agenda-start-with-clockreport-mode t)
              (org-agenda-archives-mode t)))
            ("rW" "This Week"
             ((agenda "" ((org-agenda-span 7)
                          (org-agenda-start-on-weekday 1)))
              (todo "TODO" ((org-agenda-overriding-header "All TODO items without scheduled or deadline")
                            (org-agenda-skip-function '(or (org-agenda-skip-entry-if 'timestamp)
                                                           (org-agenda-skip-subtree-if 'regexp "habit")))))
              (stuck "")
              (todo "WAITING")
              (todo "SOMEDAY"))
             ((org-agenda-start-with-clockreport-mode t)
              (org-agenda-archives-mode t)))
            ("d" "Upcoming deadlines" agenda ""
             ((org-agenda-entry-types '(:deadline))
              (org-agenda-span 1)
              (org-deadline-warning-days 30)))))

    (advice-add 'org-agenda-quit :before #'org-save-all-org-buffers)

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
             (t (org-inherited-priority (org-get-heading))))))

    (general-def 'motion org-agenda-mode-map
      "sd" 'org-agenda-filter-remove-all)
    (despot-def org-agenda-mode-map
      "d"  '(:ignore t :which-key "dates")
      "dd" 'org-agenda-deadline
      "ds" 'org-agenda-schedule))

  (use-package org-attach
    :defer t
    :config
    (setq org-attach-archive-delete 'query
          org-attach-id-dir (concat org-directory "attach/")
          org-attach-method 'mv
          org-attach-store-link-p 'file))

  (use-package ob
    :defer t
    :init
    (setq org-confirm-babel-evaluate nil
          org-edit-src-content-indentation 0
          org-src-fontify-natively t
          org-src-preserve-indentation t
          org-src-tab-acts-natively nil)

    (use-package ob-python
      :commands (org-babel-execute:python))
    (use-package ob-emacs-lisp
      :commands (org-babel-execute:elisp
                 org-babel-expand-body:elisp
                 org-babel-execute:emacs-lisp
                 org-babel-expand-body:emacs_lisp))
    (use-package ob-latex
      :commands (org-babel-execute:latex
                 org-babel-expand-body:latex
                 org-babel-prep-session:latex))
    (use-package ob-shell
      :ensure nil
      :commands (org-babel-execute:sh
                 org-babel-expand-body:sh
                 org-babel-execute:shell
                 org-babel-expand-body:shell))
    (use-package ob-C
      :commands (org-babel-execute:C
                 org-babel-expand-body:C
                 org-babel-execute:C++
                 org-babel-expand-body:C++)
      :config
      (setq org-babel-C-compiler "gcc -std=c++17"
            org-babel-C++-compiler "g++ -std=c++17"))

    (use-package verb
      :ensure t
      :defer t
      :init (use-package ob-verb
              :commands (org-babel-execute:verb)))
    (use-package ob-mermaid
      :ensure t
      :commands (org-babel-execute:mermaid))
    :config
    (defun ob-fix-inline-images ()
      "Fix redisplay of inline images after a code block evaluation."
      (when org-inline-image-overlays
        (org-redisplay-inline-images)))
    (add-hook 'org-babel-after-execute-hook #'ob-fix-inline-images))

  (use-package org-capture
    :defer t
    :init
    (setq org-capture-templates
          '(("i" "Inbox" entry (file org-inbox-file) "* %?\n%i\n")
            ("t" "Todo" entry (file org-inbox-file) "* TODO %?\n%i\n")
            ("j" "Journal" entry (function org-datetree-goto-location)
             "* %<%H:%M> %?\n" :clock-in t :clock-keep t)
            ("l" "Link")
            ("ll" "Web link" plain (file+function org-inbox-file org-capture-goto-link)
             "%i\n" :empty-lines 1 :immediate-finish t)
            ("li" "Inbox with link" entry (file org-inbox-file)
             "* %?\n%a\n%i\n")
            ("lt" "Todo with link" entry (file org-inbox-file)
             "* TODO %?\n%a\n%i\n")
            ("r" "Review")
            ("ry" "Yesterday" entry (function
                                     (lambda ()
                                       (org-datetree-goto-location
                                        (time-add (current-time) (days-to-time -1)))))
             "* Daily Review\n\n%(org-capture-insert-clock-report)" :immediate-finish t :jump-to-captured t)
            ("rt" "Today" entry (function org-datetree-goto-location)
             "* Daily Review\n\n%(org-capture-insert-clock-report)" :immediate-finish t :jump-to-captured t)
            ("rd" "Select a date" entry (function org-datetree-goto-read-date-location)
             "* Daily Review\n\n%(org-capture-insert-clock-report)" :immediate-finish t :jump-to-captured t)
            ("rw" "Last Week" entry (function
                                     (lambda ()
                                       (let ((org-reverse-datetree-level-formats
                                              (butlast org-reverse-datetree-level-formats)))
                                         (org-datetree-goto-location
                                          (time-add (current-time) (days-to-time -7))))))
             "* Weekly Review\n\n%(org-capture-insert-clock-report)" :immediate-finish t :jump-to-captured t)
            ("rW" "This Week" entry (function
                                     (lambda ()
                                       (let ((org-reverse-datetree-level-formats
                                              (butlast org-reverse-datetree-level-formats)))
                                         (org-datetree-goto-location))))
             "* Weekly Review\n\n%(org-capture-insert-clock-report)" :immediate-finish t :jump-to-captured t)))
    :config
    (defun org-capture-insert-clock-report ()
      (concat "#+BEGIN: clocktable :scope agenda-with-archives :block \n"
              "#+END:\n"))

    (progn ;; web link
      (setq org-capture-web-link-key "ll"
            org-capture-auto-refile-rules
            `(("https?://arxiv\\.org" ,org-inbox-file "arXiv")
              ("https?://git\\(?:hub\\|lab\\)\\.com" ,org-inbox-file "Repos")))

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
              (insert "* " headline "\n")
              (insert "[[" link "]]\n")
              (point)))))

      (defun org-refile-to (file headline)
        "`org-refile' to exact HEADLINE in FILE.
Create at last if HEADLINE doesn't exist."
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

      (add-hook 'org-capture-before-finalize-hook #'org-capture-auto-refile)

      (when (memq window-system '(mac ns))
        (defun org-capture-after-finalize ()
          (when (string= (org-capture-get :key) org-capture-web-link-key)
            (run-at-time 0.25 nil #'macos-switch-back-to-previous-application)))

        (add-hook 'org-capture-after-finalize-hook #'org-capture-after-finalize))))

  (use-package org-clock
    :defer 3
    :config
    (org-clock-persistence-insinuate)
    (org-clock-auto-clockout-insinuate)

    (setq org-clock-auto-clockout-timer 3600
          org-clock-auto-clock-resolution 'when-no-clock-is-running
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
    :config
    (setq org-latex-compiler "xelatex"
          org-latex-packages-alist '(("" "mathspec" t))
          org-latex-pdf-process '("latexmk -xelatex -quiet -shell-escape -bibtex -f %f"
                                  "rm -fr %b.out %b.log %b.tex auto")
          org-latex-prefer-user-labels t
          org-preview-latex-default-process 'dvisvgm
          org-preview-latex-process-alist
          '((dvisvgm :programs ("xelatex" "dvisvgm")
                     :description "xdv > svg"
                     :message "you need to install the programs: xelatex and dvisvgm."
                     :image-input-type "xdv" :image-output-type "svg" :image-size-adjust (1.7 . 1.5)
                     :latex-compiler ("xelatex -no-pdf -interaction nonstopmode -output-directory %o %f")
                     :image-converter ("dvisvgm %f -n -b min -c %S -o %O"))
            (imagemagick :programs ("xelatex" "convert")
                         :description "pdf > png"
                         :message "you need to install the programs: xelatex and imagemagick."
                         :image-input-type "pdf" :image-output-type "png" :image-size-adjust (1.0 . 1.0)
                         :latex-compiler ("xelatex -interaction nonstopmode -output-directory %o %f")
                         :image-converter ("convert -density %D -trim -antialias %f -quality 100 %O"))))

    (plist-put org-format-latex-options :scale 1.5))

  (use-package org-mac-link
    :defer t
    :config
    (setq org-mac-grab-devonthink-app-p nil
          org-mac-grab-Acrobat-app-p nil
          org-mac-grab-Brave-app-p nil
          org-mac-grab-Evernote-app-p nil
          org-mac-grab-Firefox-app-p nil
          org-mac-grab-Mail-app-p nil
          org-mac-grab-Outlook-app-p nil))

  (use-package org-refile
    :defer t
    :config
    (setq org-outline-path-complete-in-steps nil
          org-refile-allow-creating-parent-nodes 'confirm
          org-refile-use-outline-path 'file
          org-refile-targets '((nil :maxlevel . 4)
                               (org-agenda-files :maxlevel . 3)
                               (org-note-files :maxlevel . 2))))

  (setq org-fast-tag-selection-single-key t
        org-tags-column -80
        org-tags-match-list-sublevels 'intented
        org-tags-exclude-from-inheritance '("PROJ")
        org-tag-alist '((:startgroup)
                        ("@office"  . ?o)
                        ("@home"    . ?h)
                        (:endgroup)
                        (:startgroup)
                        ("@computer")
                        ("@phone")
                        ("@pad")
                        (:endgroup)
                        (:startgroup)
                        ("daily"    . ?d)
                        ("weekly"   . ?w)
                        ("monthly"  . ?m)
                        ("annually" . ?A)
                        (:endgroup)
                        ("PROJ"     . ?p)
                        ("NOTE"     . ?n)
                        (:newline)
                        ("trivia"   . ?t)
                        ("errand"   . ?e)
                        ("action"   . ?a)
                        ("focused"  . ?f)
                        ("interest" . ?i)))

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

  (progn
    (defun org-review (key)
      "Org review with org capture and agenda."
      (org-capture nil key)
      (org-agenda nil key)
      (other-window 1))

    (defhydra org-review (:hint nil :exit t)
      "
Org Review Transient state
[_y_] yesterday    [_w_] last week
[_t_] today        [_W_] this week"
      ("y" (lambda () (interactive) (org-review "ry")))
      ("t" (lambda () (interactive) (org-review "rt")))
      ("W" (lambda () (interactive) (org-review "rW")))
      ("w" (lambda () (interactive) (org-review "rw")))))

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
    "b"     '(:ignore t :which-key "babel")
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
    "C"     '(:ignore t :which-key "clocks")
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
    "d"     '(:ignore t :which-key "dates")
    "dd"    'org-deadline
    "ds"    'org-schedule
    "dt"    'org-time-stamp
    "dT"    'org-time-stamp-inactive
    "e"     '(:ignore t :which-key "export")
    "ee"    'org-export-dispatch
    "ea"    'org-export-apple-note
    "f"     '(:ignore t :which-key "feeds")
    "fi"    'org-feed-goto-inbox
    "fu"    'org-feed-update-all
    "i"     '(:ignore t :which-key "insert")
    "ia"    'org-attach
    "ib"    'org-insert-structure-template
    "id"    'org-insert-drawer
    "ie"    'org-set-effort
    "if"    'org-footnote-new
    "ig"    'org-mac-grab-link
    "ih"    'org-insert-heading
    "iH"    'org-insert-heading-after-current
    "ii"    'org-insert-item
    "il"    'org-insert-link
    "in"    'org-add-note
    "ip"    'org-set-property
    "is"    'org-insert-subheading
    "it"    'org-set-tags-command
    "m"     '(:ignore t :which-key "more")
    "p"     'org-priority
    "s"     '(:ignore t :which-key "trees/subtrees")
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
    "sN"    'widen
    "sr"    'org-refile
    "sR"    'org-datetree-refile
    "ss"    'org-sparse-tree
    "sS"    'org-sort
    "t"     '(:ignore t :which-key "tables")
    "ta"    'org-table-align
    "tb"    'org-table-blank-field
    "tc"    'org-table-convert
    "td"    '(:ignore t :which-key "delete")
    "tdc"   'org-table-delete-column
    "tdr"   'org-table-kill-row
    "te"    'org-table-eval-formula
    "tE"    'org-table-export
    "tf"    'org-table-field-info
    "th"    'org-table-previous-field
    "tH"    'org-table-move-column-left
    "ti"    '(:ignore t :which-key "insert")
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
    "tt"    '(:ignore t :which-key "toggle")
    "ttf"   'org-table-toggle-formula-debugger
    "tto"   'org-table-toggle-coordinate-overlays
    "tw"    'org-table-wrap-region
    "T"     '(:ignore t :which-key "toggles")
    "Tc"    'org-toggle-checkbox
    "Te"    'org-toggle-pretty-entities
    "Ti"    'org-toggle-inline-images
    "Tl"    'org-toggle-link-display
    "Tt"    'org-show-todo-tree
    "TT"    'org-todo
    "TV"    'space-doc-mode
    "Tx"    'org-latex-preview
    "x"     '(:ignore t :which-key "text")
    "xb"    'org-bold
    "xc"    'org-code
    "xi"    'org-italic
    "xo"    'org-open-at-point
    "xr"    'org-clear
    "xs"    'org-strike-through
    "xu"    'org-underline
    "xv"    'org-verbatim
    "L"     'org-shiftright
    "H"     'org-shiftleft
    "J"     'org-shiftdown
    "K"     'org-shiftup
    "C-S-l" 'org-shiftcontrolright
    "C-S-h" 'org-shiftcontrolleft
    "C-S-j" 'org-shiftcontroldown
    "C-S-k" 'org-shiftcontrolup)

  (general-def 'normal org-mode-map
    "RET"      'org-open-at-point)
  :general
  (tyrant-def
    "o"      '(:ignore t :which-key "org")
    "o/"     'org-occur-in-agenda-files
    "oa"     'org-agenda-list
    "oc"     'org-capture
    "oC"     '(:ignore t :which-key "clock")
    "oCc"    'org-clock-cancel
    "oCg"    'org-clock-goto
    "oCi"    'org-clock-in-last
    "oCj"    'org-clock-jump-to-current-clock
    "oCo"    'org-clock-out
    "oCr"    'org-resolve-clocks
    "od"     'open-org-default-notes-file
    "ol"     'org-store-link
    "oo"     'org-agenda
    "op"     'open-org-project-file
    "ov"     'org-review/body))

(use-package evil-org
  :ensure t
  :hook (org-mode . evil-org-mode)
  :init
  (with-eval-after-load 'org-agenda
    (autoload #'evil-org-agenda-set-keys "evil-org-agenda" nil t)
    (evil-org-agenda-set-keys))
  :config
  (add-hook 'evil-org-mode-hook
            (lambda ()
              (evil-org-set-key-theme
               '(navigation insert textobjects additional shift todo heading))))

  (defun surround-drawer ()
    (let ((dname (read-from-minibuffer "" "")))
      (cons (format ":%s:" (upcase (or dname ""))) ":END:")))
  (defun surround-code ()
    (let ((dname (read-from-minibuffer "" "")))
      (cons (format "#+BEGIN_SRC %s" (or dname "")) "#+END_SRC")))

  (with-eval-after-load 'evil-surround
    (add-to-list 'evil-surround-pairs-alist '(?: . surround-drawer))
    (add-to-list 'evil-surround-pairs-alist '(?# . surround-code))))

(use-package appt
  :hook (after-init . appt-activate)
  :config
  (fset 'diary-list-entries 'ignore)

  (defun appt-disp-alert (min-to-appt current-time appt-msg)
    (alert (format "Appointment in %s minutes" min-to-appt)
           :title (format "%s" appt-msg)))

  (defun org-agenda-to-appt-refresh ()
    (org-agenda-to-appt t))

  (setq appt-display-diary nil
        appt-display-interval 5
        appt-message-warning-time 15
        appt-disp-window-function 'appt-disp-alert)

  (with-eval-after-load 'org-capture
    (add-hook 'org-capture-after-finalize-hook #'org-agenda-to-appt-refresh))

  (with-eval-after-load 'org-agenda
    (run-at-time nil 6000 #'org-agenda-to-appt-refresh)))

(use-package org-download
  :ensure t
  :hook ((org-mode dired-mode) . org-download-enable)
  :config
  (defun +org-download-method (link)
    (org-download--fullname (org-link-unescape link)))
  (setq org-download-method '+org-download-method)

  (setq org-download-annotate-function (lambda (_link) "")
        org-download-method 'attach
        org-download-screenshot-method "screencapture -i %s"))

(use-package org-edit-latex
  :ensure t
  :hook (org-mode . org-edit-latex-mode)
  :init (setq org-edit-latex-create-master nil))

(use-package org-mru-clock
  :ensure t
  :config
  (setq org-mru-clock-files #'org-agenda-files)
  :general
  (tyrant-def "oCi"    'org-mru-clock-in))

(use-package org-projectile
  :ensure t
  :init
  (setq org-link-elisp-confirm-function nil)
  (with-eval-after-load 'org-capture (require 'org-projectile))
  :config
  (setq org-projectile-projects-file org-project-file)

  (add-to-list 'org-capture-templates
               (org-projectile-project-todo-entry) t)
  (add-to-list 'org-capture-templates
               (org-projectile-project-todo-entry
                :capture-character "lp"
                :capture-heading "Project Todo with link"
                :capture-template "* TODO %?\n%a\n") t)

  (defun org-projectile-goto-todos ()
    (interactive)
    (if (projectile-project-p)
        (org-projectile-goto-location-for-project (projectile-project-name))
      (find-file org-projectile-projects-file)))
  :general
  (tyrant-def "po" 'org-projectile-goto-todos))

(use-package org-randomnote
  :ensure t
  :config
  (setq org-randomnote-candidates (org-note-files))
  :general
  (despot-def org-mode-map
    "R"  '(:ignore t :which-key "random")
    "Rn" 'org-randomnote))

(use-package org-random-todo
  :ensure t
  :general
  (despot-def org-mode-map
    "Rt" 'org-random-todo-goto-new))

(use-package org-reverse-datetree
  :ensure t
  :commands (org-datetree-goto-location
             org-datetree-goto-read-date-location
             org-datetree-refile)
  :init
  (setq-default org-reverse-datetree-level-formats '("%Y" "%Y-%m %B" "%Y W%W" "%Y-%m-%d %A"))
  :config
  (setq org-datetree-file-format (concat org-journal-directory "%Y.org"))

  (defun org-datetree-goto-location (&optional time)
    "wrapper for `org-reverse-datetree-goto-date-in-file'.
go to `org-datetree-file-format' file based on TIME."
    (let* ((time (or time (current-time)))
           (file (format-time-string org-datetree-file-format time)))
      (set-buffer (or (org-find-base-buffer-visiting file)
                      (find-file-noselect file)))
      (org-reverse-datetree-goto-date-in-file time)))

  (defun org-datetree-goto-read-date-location (&optional time)
    "wrapper for `org-reverse-datetree-goto-read-date-in-file'.
go to `org-journal-file-format' file based on TIME."
    (interactive)
    (let* ((time (or time (org-read-date nil t nil)))
           (file (format-time-string org-datetree-file-format time)))
      (set-buffer (or (org-find-base-buffer-visiting file)
                      (find-file-noselect file)))
      (org-reverse-datetree-goto-date-in-file time)))

  (defun org-datetree-refile (ask-always &optional time prefer)
    "wrapper for `org-reverse-datetree-refile-to-file'.
go to `org-journal-file-format' file based on TIME."
    (interactive "P")
    (let* ((prefer (or prefer '("CLOSED")))
           (time (or time (org-reverse-datetree--get-entry-time
                           :ask-always ask-always
                           :prefer prefer)))
           (file (format-time-string org-datetree-file-format time)))
      (org-reverse-datetree-refile-to-file file time :ask-always ask-always :prefer prefer))))

(use-package org-roam
  :ensure t
  :defer 5
  :init
  (setq org-roam-capture-templates
        '(("d" "default" plain (function org-roam-capture--get-point)
           "%?"
           :file-name "${slug}"
           :head "#+title: ${title}\n#+created: %u\n#+last_modified: %u\n\n"
           :unnarrowed t))
        org-roam-db-gc-threshold most-positive-fixnum
        org-roam-db-location (no-littering-expand-var-file-name "org-roam.db")
        org-roam-directory org-note-directory
        org-roam-tag-sources '(prop all-directories))
  :config
  (org-roam-mode)

  (add-hook 'org-roam-buffer-prepare-hook
            (defun init-org-roam-buffer ()
              "Stuff to do when prefering org-roam buffers."
              (hide-mode-line-mode)
              (writeroom-mode -1)))

  (despot-def org-mode-map
    "ir"    'org-roam-insert
    "r"     '(:ignore t :which-key "roam")
    "r SPC" 'org-roam
    "rf"    'org-roam-find-file
    "rr"    'org-roam-find-ref
    "ri"    'org-roam-jump-to-index
    "rt"    'org-roam-tag-add
    "rT"    'org-roam-tag-delete))

(use-package org-roam-protocol :after org-protocol)

(use-package org-roam-server
  :ensure t
  :config
  (defun open-org-roam-server()
    "Enable `org-roam-server-mode' and open link"
    (interactive)
    (unless (bound-and-true-p org-roam-server-mode)
      (org-roam-server-mode))
    (browse-url
     (format "http://%s:%d" org-roam-server-host org-roam-server-port)))
  :general
  (despot-def org-mode-map "rs" 'open-org-roam-server))

(use-package org-ref
  :ensure t
  :after (:any org bibtex)
  :init
  (setq org-ref-completion-library 'org-ref-ivy-cite)
  :config
  (setq reftex-default-bibliography '("~/Documents/Zotero/references.bib"
                                      "~/Documents/Zotero/refs.bib")
        org-ref-default-bibliography reftex-default-bibliography
        org-ref-get-pdf-filename-function 'org-ref-get-pdf-filename-helm-bibtex
        org-ref-notes-function 'org-ref-notes-function-many-files
        org-ref-notes-directory (concat org-directory "notes/papers/")
        orhc-bibtex-cache-file (no-littering-expand-var-file-name ".orhc-bibtex-cache"))

  (use-package bibtex-completion
    :commands (bibtex-completion-edit-notes bibtex-completion-find-pdf)
    :init
    (setq bibtex-autokey-year-length 4
          bibtex-completion-additional-search-fields '(keywords)
          bibtex-completion-bibliography reftex-default-bibliography
          bibtex-completion-notes-path org-ref-notes-directory
          bibtex-completion-notes-template-multiple-files
          "#+title: ${author-or-editor} (${year}): ${title}\n#+roam_key: cite:${=key=}\n#+created: %u\n#+last_modified: %u\n\n"
          bibtex-completion-pdf-field "file"))

  (defun org-ref-open-zotero-at-point ()
    "Open the Zotero item for bibtex key under point."
    (interactive)
    (let ((thekey (org-ref-get-bibtex-key-under-cursor)))
      (open-file-in-external-app (format "zotero://select/items/@%s" thekey))))

  (defun org-ref-open-in-zotero ()
    "Open the Zotero item for a bibtex entry, if it exists."
    (interactive)
    (save-excursion
      (bibtex-beginning-of-entry)
      (let* ((bibtex-expand-strings t)
             (entry (bibtex-parse-entry t))
             (thekey (reftex-get-bib-field "=key=" entry)))
        (open-file-in-external-app (format "zotero://select/items/@%s" thekey)))))

  (general-def org-ref-cite-keymap
    "<tab>" nil
    "H-z"  'org-ref-open-zotero-at-point)

  (defhydra org-ref-cite-hydra (:color blue :hint nil)
    "
_p_: Open pdf     _w_: WOS          _g_: Google Scholar _K_: Copy citation to clipboard
_u_: Open url     _r_: WOS related  _P_: Pubmed         _k_: Copy key to clipboard
_n_: Open notes   _c_: WOS citing   _C_: Crossref       _f_: Copy formatted entry
_o_: Open entry   _e_: Email entry  ^ ^                 _q_: quit
_z_: Open zotero  _i_: Insert cite  _h_: change type
"
    ("o" org-ref-open-citation-at-point nil)
    ("p" (funcall org-ref-open-pdf-function) nil)
    ("n" org-ref-open-notes-at-point nil)
    ("u" org-ref-open-url-at-point nil)
    ("z" org-ref-open-zotero-at-point nil)
    ("w" org-ref-wos-at-point nil)
    ("r" org-ref-wos-related-at-point nil)
    ("c" org-ref-wos-citing-at-point nil)
    ("g" org-ref-google-scholar-at-point nil)
    ("P" org-ref-pubmed-at-point nil)
    ("C" org-ref-crossref-at-point nil)
    ("K" org-ref-copy-entry-as-summary nil)
    ("k" (progn
           (kill-new
            (car (org-ref-get-bibtex-key-and-file))))
     nil)
    ("f" (kill-new
          (org-ref-format-entry (org-ref-get-bibtex-key-under-cursor)))
     nil)

    ("e" (kill-new (save-excursion
                     (org-ref-open-citation-at-point)
                     (org-ref-email-bibtex-entry)))
     nil)
    ("i" (funcall org-ref-insert-cite-function))
    ("h" org-ref-change-cite-type)
    ("q" nil))

  (defhydra org-ref-bibtex-hydra (:color blue :hint nil)
    "
_p_: Open pdf     _y_: Copy key               _N_: New entry            _w_: WOS
_b_: Open url     _f_: Copy formatted entry   _o_: Copy entry           _c_: WOS citing
_r_: Refile entry _k_: Add keywords           _d_: delete entry         _a_: WOS related
_e_: Email entry  _K_: Edit keywords          _L_: clean entry          _P_: Pubmed
_U_: Update entry _N_: New entry              _R_: Crossref             _g_: Google Scholar
_s_: Sort entry   _a_: Remove nonascii        _h_: helm-bibtex          _q_: quit
_u_: Update field _F_: file funcs             _A_: Assoc pdf with entry
_n_: Open notes   ^ ^                         _T_: Title case
_z_: Open Zotero  ^ ^                         _S_: Sentence case
"
    ("p" org-ref-open-bibtex-pdf)
    ("P" org-ref-bibtex-pubmed)
    ("w" org-ref-bibtex-wos)
    ("c" org-ref-bibtex-wos-citing)
    ("a" org-ref-bibtex-wos-related)
    ("R" org-ref-bibtex-crossref)
    ("g" org-ref-bibtex-google-scholar)
    ("N" org-ref-bibtex-new-entry/body)
    ("n" org-ref-open-bibtex-notes)
    ("z" org-ref-open-in-zotero)
    ("o" (lambda ()
           (interactive)
           (bibtex-copy-entry-as-kill)
           (message "Use %s to paste the entry"
                    (substitute-command-keys (format "\\[bibtex-yank]")))))
    ("d" bibtex-kill-entry)
    ("L" org-ref-clean-bibtex-entry)
    ("y" (save-excursion
           (bibtex-beginning-of-entry)
           (when (looking-at bibtex-entry-maybe-empty-head)
             (kill-new (bibtex-key-in-head)))))
    ("f" (progn
           (bibtex-beginning-of-entry)
           (kill-new
            (org-ref-format-entry
             (cdr (assoc "=key=" (bibtex-parse-entry t)))))))
    ("k" helm-tag-bibtex-entry)
    ("K" (lambda ()
           (interactive)
           (org-ref-set-bibtex-keywords
            (read-string "Keywords: "
                         (bibtex-autokey-get-field "keywords"))
            t)))
    ("b" org-ref-open-in-browser)
    ("r" (lambda ()
           (interactive)
           (bibtex-beginning-of-entry)
           (bibtex-kill-entry)
           (find-file (completing-read
                       "Bibtex file: "
                       (f-entries "." (lambda (f) (f-ext? f "bib")))))
           (goto-char (point-max))
           (bibtex-yank)
           (save-buffer)
           (kill-buffer)))
    ("e" org-ref-email-bibtex-entry)
    ("U" (doi-utils-update-bibtex-entry-from-doi (org-ref-bibtex-entry-doi)))
    ("u" doi-utils-update-field)
    ("F" org-ref-bibtex-file/body)
    ("h" helm-bibtex)
    ("A" org-ref-bibtex-assoc-pdf-with-entry)
    ("a" org-ref-replace-nonascii)
    ("s" org-ref-sort-bibtex-entry)
    ("T" org-ref-title-case-article)
    ("S" org-ref-sentence-case-article)
    ("q" nil))

  (despot-def org-mode-map "ic" 'org-ref-insert-link)

  (general-def 'normal bibtex-mode-map
    "RET"      'org-ref-bibtex-hydra/body
    "C-j"      'org-ref-bibtex-next-entry
    "C-k"      'org-ref-bibtex-previous-entry
    "gj"       'org-ref-bibtex-next-entry
    "gk"       'org-ref-bibtex-previous-entry)

  (despot-def bibtex-mode-map
    "," 'org-ref-bibtex-file/body
    "a" 'arxiv-add-bibtex-entry
    "c" 'crossref-add-bibtex-entry
    "d" 'doi-add-bibtex-entry
    "i" 'isbn-to-bibtex))

(use-package org-superstar
  :ensure t
  :hook (org-mode . org-superstar-mode)
  :config
  (org-superstar-configure-like-org-bullets))

(use-package deft
  :ensure t
  :config
  (setq deft-auto-save-interval 0
        deft-default-extension "org"
        deft-directory org-directory
        deft-recursive t
        deft-use-filter-string-for-filename t
        deft-file-naming-rules '((noslash . "-") (nospace . "_") (case-fn . downcase))
        deft-strip-summary-regexp (concat "\\("
                                          "[\n\t]" ;; blank
                                          "\\|^#\\+[[:upper:]_]+:.*$" ;; org-mode metadata
                                          "\\|^#\\+[[:alnum:]_]+:.*$" ;; org-mode metadata
                                          "\\)"))

  (general-def 'normal deft-mode-map
    "A"  'deft-archive-file
    "D"  'deft-delete-file
    "F"  'deft-find-file
    "gr" 'deft-refresh
    "q"  'quit-window
    "R"  'deft-rename-file
    "S"  'deft-filter-clear
    "T"  'deft-toggle-incremental-search
    "/"  'deft-filter)
  :general
  (tyrant-def "ad" 'deft))

(use-package toc-org
  :ensure t
  :hook ((org-mode markdown-mode) . toc-org-mode))


(provide 'lang-org)
;;; lang-org.el ends here
