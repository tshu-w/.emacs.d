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
        org-default-notes-file (expand-file-name "inbox.org" org-directory))
  :config
  (add-to-list 'org-modules 'org-tempo t)
  (add-to-list 'org-modules 'org-protocol t)

  (setq org-todo-keywords
        '((sequence "TODO(t)" "|" "DONE(d)")
          (sequence "WAITING(w@/!)" "SOMEDAY(s)" "|" "CANCELED(c)"))
        org-todo-keyword-faces
        '(("CANCELED" . org-done)
          ("WAITING" . (:foreground "light coral" :weight bold))
          ("SOMEDAY" . (:foreground "plum" :weight bold))))

  (setq org-columns-default-format "%40ITEM %1PRIORITY %20TAGS %6Effort(EFFORT){:} %8CLOCKSUM"
        org-clocktable-defaults '(:maxlevel 2 :lang "en" :scope file :block nil :wstart 1 :mstart 1 :tstart nil :tend nil :step nil :stepskip0 nil :fileskip0 t :tags nil :match nil :emphasize nil :link t :narrow 40! :indent t :hidefiles nil :formula % :timestamp nil :level nil :tcolumns 1 :formatter nil :properties ("Effort"))
        org-image-actual-width 500
        org-imenu-depth 5
        org-global-properties '(("STYLE_ALL" . "habit")
                                ("Effort_ALL" . "0:10 0:15 0:30 0:45 1:00 2:00 3:00 5:00"))
        org-hide-emphasis-markers t
        org-log-done 'time
        org-log-into-drawer t
        org-startup-indented t
        org-startup-with-inline-images t
        org-track-ordered-property-with-tag t)

  (add-hook 'org-mode-hook (lambda () (setq truncate-lines nil)))
  ;; disable <> auto pairing in electric-pair-mode for org-mode
  (add-hook 'org-mode-hook
            (lambda ()
              (setq-local electric-pair-inhibit-predicate
                          `(lambda (c) (if (char-equal c ?<) t
                                    (,electric-pair-inhibit-predicate c))))))

  (defun find-org-default-notes-file ()
    "Edit the `org-default-notes-file', in the current window."
    (interactive)
    (find-file org-default-notes-file))

  ;; Org Agenda
  (use-package org-agenda
    :defer t
    :init
    (setq org-agenda-files '("~/Documents/Org"))
    :config
    (add-to-list 'org-modules 'org-habit t)

    (setq org-agenda-clockreport-parameter-plist
          '(:maxlevel 3 :scope agenda-with-archives)
          org-agenda-columns-add-appointments-to-effort-sum t
          org-agenda-compact-blocks t
          org-agenda-dim-blocked-tasks t
          org-agenda-persistent-filter t
          org-agenda-restore-windows-after-quit t
          org-agenda-skip-additional-timestamps-same-entry t
          org-agenda-skip-deadline-prewarning-if-scheduled 'pre-scheduled
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
          org-agenda-todo-ignore-scheduled 'all
          org-agenda-todo-ignore-deadlines 'far
          org-agenda-time-grid nil
          org-deadline-warning-days 10
          org-enforce-todo-dependencies t
          org-enforce-todo-checkbox-dependencies nil
          org-habit-graph-column 75
          org-stuck-projects
          '("+PROJ/-WAITING-SOMEDAY-DONE-CANCELED" ("TODO") nil ""))

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

    (defhydra org-agenda (
                          :hint nil
                          :pre (setq which-key-inhibit t)
                          :post (setq which-key-inhibit nil)
                          :foreign-keys run)
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
      "i"  '(:ignore t :which-key "insert")
      "ie" 'org-agenda-set-effort
      "ip" 'org-agenda-set-property
      "it" 'org-agenda-set-tags
      "p"  'org-agenda-priority
      "s"  '(:ignore t :which-key "trees/subtrees")
      "sr" 'org-agenda-refile)

    (general-def 'motion org-agenda-mode-map
      "C-h"      nil
      "M-SPC"    'org-agenda/body
      "s-M-SPC"  'org-agenda/body))

  ;; Org Alert
  (use-package appt
    :defer 10
    :config
    (setq appt-display-interval 5
          appt-message-warning-time 15
          appt-disp-window-function 'appt-disp-window-and-notification)

    (defun appt-disp-window-and-notification (min-to-appt current-time appt-msg)
      (when (memq window-system '(mac ns))
        (macos-notify
         (format "Appointment in %s minutes" min-to-appt)
         (format "%s" appt-msg)))
      (appt-disp-window min-to-appt current-time appt-msg))

    (add-hook 'org-capture-after-finalize-hook 'org-agenda-to-appt)
    (run-at-time nil 600 'org-agenda-to-appt)

    (appt-activate 1))

  ;; Org Attach
  (use-package org-attach
    :defer t
    :config
    (setq org-attach-archive-delete 'query
          org-attach-id-dir (concat org-directory "attach/")
          org-attach-method 'mv))

  ;; Org Babel
  (use-package ob
    :commands org-babel/body
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
                 org-babel-execute:bash
                 org-babel-expand-body:bash))
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
    (add-hook 'org-babel-after-execute-hook 'ob-fix-inline-images)

    (defhydra org-babel (:hint nil)
      "
  Org Babel Transient state
  [_j_/_k_] navigate src blocks         [_e_] execute src block
  [_g_]^^   goto named block            [_'_] edit src block
  [_z_]^^   recenter screen             [_q_] quit"
      ("q" nil :exit t)
      ("j" org-babel-next-src-block)
      ("k" org-babel-previous-src-block)
      ("g" org-babel-goto-named-src-block)
      ("z" recenter-top-bottom)
      ("e" org-babel-execute-maybe)
      ("'" org-edit-special :exit t)))

  ;; Org Capture
  (use-package org-capture
    :defer t
    :init
    (setq org-inbox-file org-default-notes-file
          org-capture-templates
          '(("i" "Inbox" entry (file org-inbox-file)
             "* %?\n%i\n")
            ("t" "Todo" entry (file org-inbox-file)
             "* TODO %?\n%i\n")
            ("j" "Journal" plain (function org-journal-find-location)
             "** %(format-time-string org-journal-time-format)%?")
            ("l" "Link")
            ("ll" "Link" plain (file+function org-inbox-file org-capture-goto-link)
             "%i\n" :empty-lines 1 :immediate-finish t)
            ("li" "Inbox with link" entry (file org-inbox-file)
             "* %?\n%a\n%i\n")
            ("lt" "Todo with link" entry (file org-inbox-file)
             "* TODO %?\n%a\n%i\n")
            ("r" "Review")
            ("ry" "Yesterday" plain (function (lambda () (org-journal-find-location -1)))
             "** Daily Review\n%?\n")
            ("rt" "Today" plain (function org-journal-find-location)
             "** Daily Review\n%?\n")
            ("rw" "Last Week" plain (function (lambda () (org-journal-find-location -7)))
             "* Week %(format-time-string \"%_V\" (time-add (current-time) (days-to-time -7))) Review\n%?\n")
            ("rW" "This Week" plain (function org-journal-find-location)
             "* Week %(format-time-string \"%_V\") Review\n%?\n")))
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
                  "[[" (plist-get org-store-link-plist :link) "]]" "\n"))))

    (despot-def org-capture-mode-map
      "," 'org-capture-finalize
      "a" 'org-capture-kill
      "c" 'org-capture-finalize
      "k" 'org-capture-kill
      "r" 'org-capture-refile))

  ;; Org Clock
  (use-package org-clock
    :defer 2
    :config
    (org-clock-persistence-insinuate)
    (setq org-clock-auto-clock-resolution 'when-no-clock-is-running
          org-clock-history-length 10
          org-clock-idle-time 10
          org-clock-in-resume t
          org-clock-persist t
          org-clock-persist-query-resume nil
          org-clock-out-remove-zero-time-clocks t
          org-clock-out-when-done t
          org-clock-report-include-clocking-task t))

  ;; Org Latex
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

  ;; Org mac link
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

  ;; Org Refile
  (setq org-note-files (directory-files-recursively
                        "~/Documents/Org/notes" "^.*\\.org$")
        org-outline-path-complete-in-steps nil
        org-refile-allow-creating-parent-nodes 'confirm
        org-refile-use-outline-path 'file
        org-refile-targets '((nil :maxlevel . 5)
                             (org-note-files :maxlevel . 4)
                             (org-agenda-files :maxlevel . 4)))

  ;; Org Tag
  (setq org-fast-tag-selection-single-key t
        org-tags-column 0
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

  ;; System interaction function
  (defun supress-frame-splitting (&rest r)
    (let ((frame-name (frame-parameter nil 'name)))
      (when (or (equal "capture" frame-name)
                (equal "agenda" frame-name))
        (delete-other-windows))))

  (defun org-capture-finalize@after (&rest r)
    (when (equal "l" (plist-get org-capture-plist :key))
      (run-at-time 0 nil #'osx-switch-back-to-previous-application))
    (when (equal "capture" (frame-parameter nil 'name))
      (delete-frame)))

  (defun org-agenda-finalize@after (&rest r)
    (when (equal "agenda" (frame-parameter nil 'name))
      (delete-frame)))

  (defun org-capture-select-template@around (org-capture-select-template &optional keys)
    (let ((res (ignore-errors (funcall org-capture-select-template keys))))
      (unless res (setq res "q"))
      (when (and (equal "capture" (frame-parameter nil 'name))
                 (equal "q" res))
        (delete-frame))
      res))

  (defun org-agenda-get-restriction-and-command@around (org-agenda-get-restriction-and-command prefix-descriptions)
    (let ((res (ignore-errors (funcall org-agenda-get-restriction-and-command prefix-descriptions))))
      (when (and (not res)
                 (equal "agenda" (frame-parameter nil 'name)))
        (delete-frame))
      res))

  (advice-add 'org-agenda-quit                        :before #'org-save-all-org-buffers)
  (advice-add 'org-agenda-quit                        :after  #'org-agenda-finalize@after)
  (advice-add 'org-agenda-exit                        :after  #'org-agenda-finalize@after)
  (advice-add 'org-agenda-get-restriction-and-command :around #'org-agenda-get-restriction-and-command@around)
  (advice-add 'org-capture-finalize                   :after  #'org-capture-finalize@after)
  (advice-add 'org-capture-select-template            :around #'org-capture-select-template@around)
  (advice-add 'org-refile                             :after  #'org-save-all-org-buffers)
  (advice-add 'org-switch-to-buffer-other-window      :after  #'supress-frame-splitting)


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
  (+org-emphasize org-verbatim ?=)

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
    ("y" (lambda () (interactive)
           (org-review "ry")))
    ("t" (lambda () (interactive)
           (org-review "rt")))
    ("W" (lambda () (interactive)
           (org-review "rW")))
    ("w" (lambda () (interactive)
           (org-review "rw"))))

  ;; Export Org to Apple Note
  ;; https://emacs-china.org/t/org-apple-note/10706
  ;; https://vxlabs.com/2018/10/29/importing-orgmode-notes-into-apple-notes/
  (defun string-utils-escape-double-quotes (str-val)
    "Return STR-VAL with every double-quote escaped with backslash."
    (save-match-data
      (replace-regexp-in-string "\"" "\\\\\"" str-val)))
  (defun string-utils-escape-backslash (str-val)
    "Return STR-VAL with every backslash escaped with an additional backslash."
    (save-match-data
      (replace-regexp-in-string "\\\\" "\\\\\\\\" str-val)))

  (defun org-export-apple-note ()
    (interactive)
    (let ((title (file-name-base (buffer-file-name)))
          (as-tmpl "set TITLE to \"%s\"
  set NBODY to \"%s\"
  tell application \"Notes\"
          tell folder \"Org-mode\"
                  if not (note named TITLE exists) then
                          make new note with properties {name:TITLE}
                  end if
                  set body of note TITLE to NBODY
          end tell
  end tell"))
      (with-current-buffer (org-export-to-buffer 'html "*orgmode-to-apple-notes*")
        (let ((body (string-utils-escape-double-quotes
                     (string-utils-escape-backslash (buffer-string)))))
          ;; install title + body into template above and send to notes
          (do-applescript (format as-tmpl title body))
          ;; get rid of temp orgmode-to-apple-notes buffer
          (kill-buffer)
          (delete-window)
          (message "export successfully")))))

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
    "b."    'org-babel/body
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
    "od"     '(find-org-default-notes-file :which-key "default-org-file")
    "of"     '(:ignore t :which-key "feeds")
    "ofi"    'org-feed-goto-inbox
    "ofu"    'org-feed-update-all
    "og"     'org-mac-grab-link
    "ol"     'org-store-link
    "om"     'org-tags-view
    "oo"     'org-agenda
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
        org-download-image-attr-list '("#+ATTR_HTML: :width 80% :align center"))

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
  (despot-def org-mode-map
    "Tf" 'org-fragtog-mode))

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
  (defun org-journal-find-location (&optional days)
    (let ((date (time-add (current-time) (days-to-time (or days 0)))))
      (org-journal-new-entry t date)
      (goto-char (point-max))))

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

(use-package org-mime
  :disabled t
  :ensure t
  :general
  (despot-def message-mode-map
    "e"  'org-mime-htmlize)
  (despot-def org-mode-map
    "em" 'org-mime-org-buffer-htmlize
    "es" 'org-mime-org-subtree-htmlize))

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
  (setq org-projectile-projects-file "~/Documents/Org/projects.org")

  (add-to-list 'org-capture-templates
               (org-projectile-project-todo-entry) t)
  (add-to-list 'org-capture-templates
               (org-projectile-project-todo-entry
                :capture-character "lp"
                :capture-heading "Project Todo with link"
                :capture-template "* TODO %?\n%a\n") t)

  (defun org-projectile-capture (&optional arg)
    (interactive "P")
    (if arg
        (org-projectile-project-todo-completing-read :empty-lines 1)
      (org-projectile-capture-for-current-project :empty-lines 1)))

  (defun org-projectile-goto-todos ()
    (interactive)
    (if (projectile-project-p)
        (org-projectile-goto-location-for-project (projectile-project-name))
      (find-file org-projectile-projects-file)))
  :general
  (tyrant-def
    "op" 'org-projectile-capture
    "po" 'org-projectile-goto-todos))

(use-package org-randomnote
  :ensure t
  :general (tyrant-def
             "oR"  '(:ignore t :which-key "random")
             "oRn" 'org-randomnote))

(use-package org-random-todo
  :ensure t
  :general (tyrant-def "oRt" 'org-random-todo-goto-new))

(use-package org-roam
  :ensure t
  :defer 5
  :config
  (setq org-roam-capture-templates
        '(("d" "default" plain (function org-roam-capture--get-point)
           "%?"
           :file-name "notes/${slug}"
           :head "#+title: ${title}\n#+created: %<%Y%m%d>\n\n"
           :unnarrowed t))
        org-roam-capture-ref-templates
        '(("r" "ref" plain (function org-roam-capture--get-point)
           "%?"
           :file-name "notes/${slug}"
           :head "#+title: ${title}\n#+roam_key: ${ref}\n\n"
           :unnarrowed t)
          ("a" "annotation" plain (function org-roam-capture--get-point)
           "${body}\n"
           :file-name "notes/${slug}"
           :head "#+title: ${title}\n#+roam_key: ${ref}\n\n"
           :immediate-finish t
           :unnarrowed t
           :empty-lines 1))
        org-roam-db-gc-threshold most-positive-fixnum
        org-roam-directory (file-truename org-directory)
        org-roam-tag-sources '(prop all-directories))

  (org-roam-mode)

  (despot-def org-mode-map
    "ir" 'org-roam-insert-immediate
    "iR" 'org-roam-insert)
  :general
  (tyrant-def
    "or"  '(:ignore t :which-key "roam")
    "orb" 'org-roam-buffer-toggle-display
    "orf" 'org-roam-find-file
    "ori" 'org-roam-find-index
    "orr" 'org-roam-find-ref))

(use-package org-roam-protocol
  :after org-protocol)

(use-package org-roam-server
  :ensure t
  :general
  (tyrant-def "ors" 'org-roam-server-mode))

(use-package org-ref
  :ensure t
  :after (:any org autex bibtex)
  :init
  (setq org-ref-completion-library 'org-ref-ivy-cite)
  :config
  (setq reftex-default-bibliography '("~/Documents/Zotero/references.bib")

        org-ref-default-bibliography '("~/Documents/Zotero/references.bib")
        org-ref-bibliography-notes "~/Documents/Org/notes/papers/")

  (use-package bibtex-completion
    :commands (bibtex-completion-edit-notes bibtex-completion-find-pdf)
    :init
    (setq bibtex-autokey-year-length 4
          bibtex-completion-additional-search-fields '(keywords)
          bibtex-completion-bibliography '("~/Documents/Zotero/references.bib")
          bibtex-completion-library-path '("~/Documents/Zotero/storage/")
          bibtex-completion-notes-path "~/Documents/Org/notes/papers/"
          bibtex-completion-notes-template-multiple-files
          "#+title: ${author-or-editor} (${year}): ${title}\n#+roam_key: cite:${=key=}\n\n"
          bibtex-completion-pdf-field "file"
          bibtex-dialect 'biblatex))

  ;; Tell org-ref to let bibtex-completion find notes for it
  (setq org-ref-notes-function
        (lambda (citekey)
	        (let ((bibtex-completion-bibliography (org-ref-find-bibliography)))
	          (bibtex-completion-edit-notes
	           (list (car (org-ref-get-bibtex-key-and-file citekey)))))))

  (defun +org-ref-open-pdf-at-point ()
    "Open the pdf for bibtex key under point if it exists."
    (interactive)
    (let* ((results (org-ref-get-bibtex-key-and-file))
           (key (car results))
           (pdf-file (car (bibtex-completion-find-pdf key))))
      (if (file-exists-p pdf-file)
          (org-open-file pdf-file)
        (message "No PDF found for %s" key))))

  (setq org-ref-open-pdf-function '+org-ref-open-pdf-at-point)

  (general-def org-ref-cite-keymap "<tab>" nil)

  (despot-def :keymaps '(LaTeX-mode-map org-mode-map)
    "ic"       'org-ref-insert-link)

  (general-def 'normal bibtex-mode-map
    "C-j"      'org-ref-bibtex-next-entry
    "C-k"      'org-ref-bibtex-previous-entry
    "gj"       'org-ref-bibtex-next-entry
    "gk"       'org-ref-bibtex-previous-entry)

  (despot-def bibtex-mode-map
    ;; Navigation
    "j"  'org-ref-bibtex-next-entry
    "k"  'org-ref-bibtex-previous-entry
    ;; Open
    "b"  'org-ref-open-in-browser
    "n"  'org-ref-open-bibtex-notes
    "p"  'org-ref-open-bibtex-pdf
    ;; Misc
    "h"  'org-ref-bibtex-hydra/body
    "i"  'org-ref-bibtex-hydra/org-ref-bibtex-new-entry/body-and-exit
    "s"  'org-ref-sort-bibtex-entry
    ;; Lookup utilities
    "l"  '(:ignore t :which-key "lookup")
    "la" 'arxiv-add-bibtex-entry
    "lA" 'arxiv-get-pdf-add-bibtex-entry
    "ld" 'doi-utils-add-bibtex-entry-from-doi
    "li" 'isbn-to-bibtex
    "lp" 'pubmed-insert-bibtex-from-pmid))

(use-package org-superstar
  :ensure t
  :hook (org-mode . org-superstar-mode)
  :config
  (org-superstar-configure-like-org-bullets))

(use-package toc-org
  :disabled t
  :ensure t
  :hook (org-mode . toc-org-enable)
  :config (setq toc-org-max-depth 10))


(provide 'lang-org)
;;; lang-org.el ends here
