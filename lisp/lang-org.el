;;; lang-org.el --- -*- lexical-binding: t; -*-

;; Copyright (C) 2020  Tianshu Wang

;; Author: Tianshu Wang <volekingsg@gmail.com>

;;; Commentary:

;;; Code:

(use-package org
  :ensure org-plus-contrib
  :defer t
  :init
  (setq org-directory "~/Documents/Org/"
        org-note-directory (concat org-directory "notes/")
        org-inbox-file (concat org-directory "inbox.org")
        org-project-file (concat org-directory "projects.org")
        org-default-notes-file org-inbox-file)

  (use-package org-protocol
    :defer t
    :init
    (defadvice server-execute (before enable-org-protocol activate)
      (unless (featurep 'org-protocol) (require 'org-protocol))))
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
        org-highlight-latex-and-related '(native)
        org-image-actual-width '(500)
        org-imenu-depth 4
        org-log-done 'time
        org-log-into-drawer t
        org-startup-folded t
        org-startup-indented t
        org-startup-with-inline-images t
        org-track-ordered-property-with-tag t
        org-use-property-inheritance t
        org-use-sub-superscripts "{}"
        org-yank-adjusted-subtrees t)

  (defun init-org-mode ()
    "Stuff to do when opening `org-mode' files."
    (setq truncate-lines nil)
    ;; disable <> auto pairing in electric-pair-mode for org-mode
    (setq-local electric-pair-inhibit-predicate
                `(lambda (c) (if (char-equal c ?<) t
                          (,electric-pair-inhibit-predicate c))))
    (org-align-tags t))

  (add-hook 'org-mode-hook #'init-org-mode)

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
    (autoload #'find-lisp-find-files "find-lisp" nil t))
  (defun org-note-files ()
    "Get the list of note files."
    (find-lisp-find-files org-note-directory "\.org$"))

  (use-package org-agenda
    :defer t
    :init
    (setq org-agenda-files `(,org-directory))
    :config
    (add-to-list 'org-modules 'org-habit t)

    (setq org-agenda-clockreport-parameter-plist '(:maxlevel 3 :scope agenda-with-archives)
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
                                        (todo priority-down category-keep)
                                        (tags priority-down category-keep)
                                        (search category-keep))
          org-agenda-span 'day
          org-agenda-start-on-weekday nil
          org-agenda-time-grid nil
          org-agenda-time-leading-zero t
          org-agenda-todo-ignore-scheduled 'all
          org-agenda-todo-ignore-deadlines 'far
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
    (advice-add 'org-refile      :after  #'org-save-all-org-buffers)

    (defhydra org-agenda (
                          :foreign-keys run
                          :hint nil
                          :pre (setq which-key-inhibit t)
                          :post (setq which-key-inhibit nil))
      "
Org-agenda transient state
Headline^^          Visit entry^^               Filter^^                  Date^^                 Toggle mode^^       View^^           Clock^^       Other^^
--------^^--------- -----------^^-------------- ------^^----------------- ----^^---------------- -----------^^------ ----^^---------- -----^^------ -----^^-----------
[_ht_] set status   [_SPC_] in other window     [_ft_] by tag             [_ds_] schedule        [_tf_] follow       [_vd_] day       [_cI_] in     [_gr_] reload
[_hk_] kill         [_TAB_] & go to location    [_fr_] refine by tag      [_dS_] un-schedule     [_tl_] log          [_vw_] week      [_cO_] out    [_._]  go to today
[_hr_] refile       [_RET_] & del other windows [_fc_] by category        [_dd_] set deadline    [_ta_] archive      [_vt_] fortnight [_cq_] cancel [_gd_] go to date
[_hA_] archive      [_o_]   link                [_fh_] by top headline    [_dD_] remove deadline [_tr_] clock report [_vm_] month     [_cj_] jump   ^^
[_h:_] set tags     ^^                          [_fx_] by regexp          [_dt_] timestamp       [_ti_] clock issues [_vy_] year      ^^            ^^
[_hp_] set priority ^^                          [_fd_] delete all filters [_+_]  do later        [_td_] diaries      [_vn_] next span ^^            ^^
^^                  ^^                          ^^                        [_-_]  do earlier      ^^                  [_vp_] prev span ^^            ^^
^^                  ^^                          ^^                        ^^                     ^^                  [_vr_] reset     ^^            ^^
[_q_] quit
"
      ;; Entry
      ("h:" org-agenda-set-tags)
      ("hA" org-agenda-archive-default)
      ("hk" org-agenda-kill)
      ("hp" org-agenda-priority)
      ("hr" org-agenda-refile)
      ("ht" org-agenda-todo)
      ;; Visit entry
      ("SPC"   org-agenda-show-and-scroll-up)
      ("<tab>" org-agenda-goto :exit t)
      ("TAB"   org-agenda-goto :exit t)
      ("RET"   org-agenda-switch-to :exit t)
      ("o"     link-hint-open-link :exit t)
      ;; Date
      ("+" org-agenda-do-date-later)
      ("-" org-agenda-do-date-earlier)
      ("dd" org-agenda-deadline)
      ("dD" (lambda () (interactive)
              (let ((current-prefix-arg '(4)))
                (call-interactively 'org-agenda-deadline))))
      ("ds" org-agenda-schedule)
      ("dS" (lambda () (interactive)
              (let ((current-prefix-arg '(4)))
                (call-interactively 'org-agenda-schedule))))
      ("dt" org-agenda-date-prompt)
      ;; View
      ("vd" org-agenda-day-view)
      ("vm" org-agenda-month-view)
      ("vn" org-agenda-later)
      ("vp" org-agenda-earlier)
      ("vr" org-agenda-reset-view)
      ("vt" org-agenda-fortnight-view)
      ("vw" org-agenda-week-view)
      ("vy" org-agenda-year-view)
      ;; Toggle mode
      ("ta" org-agenda-archives-mode)
      ("td" org-agenda-toggle-diary)
      ("tf" org-agenda-follow-mode)
      ("ti" org-agenda-show-clocking-issues)
      ("tl" org-agenda-log-mode)
      ("tr" org-agenda-clockreport-mode)
      ;; Filter
      ("fc" org-agenda-filter-by-category)
      ("fd" org-agenda-filter-remove-all)
      ("fh" org-agenda-filter-by-top-headline)
      ("fr" org-agenda-filter-by-tag-refine)
      ("ft" org-agenda-filter-by-tag)
      ("fx" org-agenda-filter-by-regexp)
      ;; Clock
      ("cI" org-agenda-clock-in :exit t)
      ("cj" org-agenda-clock-goto :exit t)
      ("cO" org-agenda-clock-out)
      ("cq" org-agenda-clock-cancel)
      ;; Other
      ("q" nil :exit t)
      ("gr" org-agenda-redo)
      ("." org-agenda-goto-today)
      ("gd" org-agenda-goto-date))

    (despot-def org-agenda-mode-map
      "a"  'org-agenda
      "c"  'org-agenda-columns
      "d"  '(:ignore t :which-key "dates")
      "dd" 'org-agenda-deadline
      "ds" 'org-agenda-schedule
      "p"  'org-agenda-priority)

    (general-def 'motion org-agenda-mode-map
      "M-SPC"    'org-agenda/body
      "s-M-SPC"  'org-agenda/body))

  (use-package appt
    :defer 5
    :config
    (defun appt-disp-alert (min-to-appt current-time appt-msg)
      (alert (format "Appointment in %s minutes" min-to-appt)
             :title (format "%s" appt-msg)))

    (defun org-agenda-to-appt-refresh ()
      (org-agenda-to-appt t))

    (setq appt-display-interval 5
          appt-message-warning-time 15
          appt-disp-window-function 'appt-disp-alert)

    (add-hook 'org-capture-after-finalize-hook #'org-agenda-to-appt-refresh)
    (run-at-time nil 6000 #'org-agenda-to-appt-refresh)

    (appt-activate t))

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
    (add-hook 'org-babel-after-execute-hook 'ob-fix-inline-images))

  (use-package org-capture
    :defer t
    :init
    (setq org-capture-templates
          '(("i" "Inbox" entry (file org-inbox-file) "* %?\n%i\n")
            ("t" "Todo" entry (file org-inbox-file) "* TODO %?\n%i\n")
            ("j" "Journal" plain (function org-journal-find-location)
             "** %(format-time-string org-journal-time-format)%?"
             :jump-to-captured t :immediate-finish t)
            ("l" "Link")
            ("ll" "Link" plain (file+function org-inbox-file org-capture-goto-link)
             "%i\n" :empty-lines 1 :immediate-finish t)
            ("li" "Inbox with link" entry (file org-inbox-file)
             "* %?\n%a\n%i\n")
            ("lt" "Todo with link" entry (file org-inbox-file)
             "* TODO %?\n%a\n%i\n")
            ("r" "Review")
            ("ry" "Yesterday" plain
             (function (lambda () (org-journal-find-location
                              (time-add (current-time) (days-to-time -1)))))
             "** Daily Review\n%?\n" :jump-to-captured t :immediate-finish t)
            ("rt" "Today" plain (function org-journal-find-location)
             "** Daily Review\n%?\n" :jump-to-captured t :immediate-finish t)
            ("rw" "Last Week" plain
             (function (lambda ()
                         (org-journal-find-location
                          (let* ((time (decode-time (current-time)))
                                 (year (decoded-time-year time))
                                 (week (car (calendar-iso-from-absolute
                                             (calendar-absolute-from-gregorian
                                              (-select-by-indices
                                               '(4 3 5) time))))))
                            (iso-week-to-time year (- week 1) 7)))))
             "* Week %(format-time-string \"%_V\" (time-add (current-time) (days-to-time -7))) Review\n%?\n"
             :jump-to-captured t :immediate-finish t)
            ("rW" "This Week" plain
             (function (lambda ()
                         (org-journal-find-location
                          (let* ((time (decode-time (current-time)))
                                 (year (decoded-time-year time))
                                 (week (car (calendar-iso-from-absolute
                                             (calendar-absolute-from-gregorian
                                              (-select-by-indices
                                               '(4 3 5) time))))))
                            (iso-week-to-time year week 7)))))
             "* Week %(format-time-string \"%_V\") Review\n%?\n"
             :jump-to-captured t :immediate-finish t)))
    :config
    (defun org-capture-goto-link ()
      (org-capture-put :target
                       (list 'file+headline
                             (nth 1 (org-capture-get :target))
                             (plist-get org-store-link-plist :description)))
      (org-capture-put-target-region-and-position)
      (widen)
      (let ((hd (plist-get org-store-link-plist :description)))
        (goto-char (point-min))
        (if (re-search-forward
             (format org-complex-heading-regexp-format (regexp-quote hd)) nil t)
            (org-end-of-subtree)
          (goto-char (point-max))
          (or (bolp) (insert "\n"))
          (insert "* " hd "\n"
                  "[[" (plist-get org-store-link-plist :link) "]]" "\n")))))

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
                         :image-converter ("convert -density %D -trim -antialias %f -quality 100 %O")))))

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
      "Org review with org-journal capture and agenda."
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
    "o#"     'org-agenda-list-stuck-projects
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
    "og"     'org-mac-grab-link
    "ol"     'org-store-link
    "om"     'org-tags-view
    "oo"     'org-agenda
    "op"     'open-org-project-file
    "os"     'org-search-view
    "ot"     'org-todo-list
    "ov"     'org-review/body))

(use-package evil-org
  :ensure t
  :hook (org-mode . evil-org-mode)
  :init
  (with-eval-after-load 'org-agenda
    (require 'evil-org-agenda)
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

(use-package org-download
  :ensure t
  :hook ((org-mode dired-mode) . org-download-enable)
  :config
  (defun +org-download-method (link)
    (org-download--fullname (org-link-unescape link)))
  (setq org-download-method '+org-download-method)

  (setq org-download-method 'attach
        org-download-screenshot-method "screencapture -i %s"
        org-download-image-attr-list '("#+ATTR_ORG:  :width 500px"
                                       "#+ATTR_HTML: :width 80% :align center"))

  (despot-def org-mode-map
    "iD" '(:ignore nil :which-key "download")
    "iDy" 'org-download-yank
    "iDs" 'org-download-screenshot))

(use-package org-edit-latex
  :ensure t
  :hook (org-mode . org-edit-latex-mode)
  :init (setq org-edit-latex-create-master nil))

(use-package org-fragtog
  :ensure t
  :general
  (despot-def org-mode-map "Tf" 'org-fragtog-mode))

(use-package org-journal
  :ensure t
  :after calendar
  :commands (org-journal-find-location)
  :init
  (setq org-journal-dir          "~/Documents/Org/journals/"
        org-journal-date-format  "%A, %B %d %Y"
        org-journal-file-header #'org-journal-file-header-func
        org-journal-file-type    'monthly)
  :config
  (defun org-journal-find-location (&optional time)
    (org-journal-new-entry t time)
    (org-end-of-subtree))

  (defun org-journal-file-header-func (time)
    "Custom function to create journal header."
    (concat
     (pcase org-journal-file-type
       (`daily "#+title: Daily Journal\n#+STARTUP: showeverything\n\n")
       (`weekly "#+title: Weekly Journal\n#+STARTUP: folded\n\n")
       (`monthly "#+title: Monthly Journal\n#+STARTUP: folded\n\n")
       (`yearly "#+title: Yearly Journal\n#+STARTUP: folded\n\n"))))

  (despot-def org-journal-mode-map
    "j"   'org-journal-new-entry
    "n"   'org-journal-next-entry
    "p"   'org-journal-previous-entry)

  (despot-def calendar-mode-map
    "r"   'org-journal-read-entry
    "i"   'org-journal-new-date-entry
    "n"   'org-journal-next-entry
    "p"   'org-journal-previous-entry
    "s"   'org-journal-search-forever
    "w"   'org-journal-search-calendar-week
    "m"   'org-journal-search-calendar-month
    "y"   'org-journal-search-calendar-year)
  :general
  (tyrant-def
    "oj"  '(:ignore t :which-key "org-journal")
    "ojj" 'org-journal-new-entry
    "ojs" 'org-journal-search-forever
    "ojt" 'org-journal-new-scheduled-entry
    "ojv" 'org-journal-schedule-view))

(use-package org-mru-clock
  :ensure t
  :config
  (setq org-mru-clock-files #'org-agenda-files)
  :general
  (tyrant-def
    "oCi"    'org-mru-clock-in
    "oC SPC" 'org-mru-clock-select-recent-task))

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
  (tyrant-def
    "oR"  '(:ignore t :which-key "random")
    "oRn" 'org-randomnote))

(use-package org-random-todo
  :ensure t
  :general (tyrant-def "oRt" 'org-random-todo-goto-new))

(use-package org-roam
  :ensure t
  :defer 5
  :init
  (setq org-roam-capture-templates
        '(("d" "default" plain (function org-roam-capture--get-point)
           "%?"
           :file-name "${slug}"
           :head "#+title: ${title}\n#+created: %<%Y%m%d>\n"
           :unnarrowed t))
        org-roam-db-gc-threshold most-positive-fixnum
        org-roam-db-location (no-littering-expand-var-file-name "org-roam.db")
        org-roam-directory org-note-directory
        org-roam-tag-sources '(prop all-directories))
  :config
  (org-roam-mode)

  (add-hook 'org-roam-buffer-prepare-hook #'hide-mode-line-mode)

  (despot-def org-mode-map
    "ir" 'org-roam-insert-immediate
    "iR" 'org-roam-insert)
  :general
  (tyrant-def
    "or"     '(:ignore t :which-key "roam")
    "orf"    'org-roam-find-file
    "ori"    'org-roam-jump-to-index
    "orr"    'org-roam-find-ref
    "or SPC" 'org-roam
    "ort"    '(:ignore t :which-key "tags")
    "orta"   'org-roam-tag-add
    "ortd"   'org-roam-tag-delete))

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
  (tyrant-def "ors" 'open-org-roam-server))

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
        orhc-bibtex-cache-file (no-littering-expand-var-file-name ".orhc-bibtex-cache"))

  (use-package bibtex-completion
    :commands (bibtex-completion-edit-notes bibtex-completion-find-pdf)
    :init
    (setq bibtex-autokey-year-length 4
          bibtex-completion-additional-search-fields '(keywords)
          bibtex-completion-bibliography reftex-default-bibliography
          bibtex-completion-library-path '("~/Documents/Zotero/storage/")
          bibtex-completion-notes-path (concat org-directory "notes/papers/")
          bibtex-completion-notes-template-multiple-files
          "#+title: ${author-or-editor} (${year}): ${title}\n#+roam_key: cite:${=key=}\n\n"
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

  (defhydra org-ref-cite-hydra (:color blue)
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

  (despot-def org-mode-map "ic" 'org-ref-insert-link)

  (general-def 'normal bibtex-mode-map
    "C-j"      'org-ref-bibtex-next-entry
    "C-k"      'org-ref-bibtex-previous-entry
    "gj"       'org-ref-bibtex-next-entry
    "gk"       'org-ref-bibtex-previous-entry)

  (despot-def bibtex-mode-map
    ;; Open
    "b"  'org-ref-open-in-browser
    "n"  'org-ref-open-bibtex-notes
    "p"  'org-ref-open-bibtex-pdf
    "z"  'org-ref-open-in-zotero
    ;; Misc
    "h"  'org-ref-bibtex-hydra/body
    "i"  'org-ref-bibtex-hydra/org-ref-bibtex-new-entry/body-and-exit
    "s"  'bibtex-sort-buffer
    ;; Lookup utilities
    "l"  '(:ignore t :which-key "lookup")
    "la" 'arxiv-add-bibtex-entry
    "lc" 'crossref-add-bibtex-entry
    "ld" 'doi-add-bibtex-entry
    "li" 'isbn-to-bibtex
    "lp" 'pubmed-insert-bibtex-from-pmid))

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
  :hook ((org-mode markdown-mode) . toc-org-mode)
  :config (setq toc-org-max-depth 3))


(provide 'lang-org)
;;; lang-org.el ends here
