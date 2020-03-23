;;; lang-org.el -*- lexical-binding: t; -*-

;; Copyright (C) 2020  Tianshu Wang

;; Author: Tianshu Wang <volekingsg@gmail.com>

(use-package org
  :ensure org-plus-contrib
  :mode ("\\.org\\'" . org-mode)
  :config
  (add-to-list 'org-modules 'org-tempo t)

  (setq org-todo-keywords
        '((sequence "TODO(t)" "|" "DONE(d)")
          (sequence "PROJ(p)" "WAITING(w@/!)" "|" "SOMEDAY(s)" "CANCELED(c)"))
        org-todo-keyword-faces
        '(("CANCELED" . org-upcoming-distant-deadline)
          ("PROJ" . (:foreground "RosyBrown4" :weight bold))
          ("WAITING" . (:foreground "light coral" :weight bold))
          ("SOMEDAY" . (:foreground "plum" :weight bold))))

  (setq org-columns-default-format "%50ITEM %2PRIORITY %10Effort(Effort){:} %10CLOCKSUM"
        org-directory "~/Documents/Org/"
        org-default-notes-file (expand-file-name "gtd.org" org-directory)
        org-id-locations-file (concat cache-dir ".org-id-locations")
        org-image-actual-width 500
        org-imenu-depth 8
        org-global-properties '(("STYLE_ALL" . "habit"))
        org-hide-emphasis-markers t
        org-latex-prefer-user-labels t
        org-log-done t
        org-log-into-drawer t
        org-publish-timestamp-directory (concat cache-dir ".org-timestamps/")
        org-src-fontify-natively t
        org-src-tab-acts-natively t
        org-startup-indented t
        org-startup-with-inline-images t
        org-tags-match-list-sublevels 'intented
        org-track-ordered-property-with-tag t)

  (add-hook 'org-mode-hook '(lambda () (setq truncate-lines nil)))
  ;; disable <> auto pairing in electric-pair-mode for org-mode
  (add-hook 'org-mode-hook '(lambda () (setq-local electric-pair-inhibit-predicate
                                              `(lambda (c)
                                                 (if (char-equal c ?<) t (,electric-pair-inhibit-predicate c))))))

  (defun find-org-default-notes-file ()
    "Edit the `org-default-notes-file', in the current window."
    (interactive)
    (find-file org-default-notes-file))

  ;; Org Agenda
  (use-package org-agenda
    :ensure nil
    :init
    (setq org-agenda-files '("~/Documents/Org"))
    (add-to-list 'org-modules 'org-habit t)
    :config
    (setq org-agenda-clockreport-parameter-plist
          '(:maxlevel 3 :scope agenda-with-archives :fileskip0 t :stepskip0 t
                      :emphasize t :link t :narrow 80! :tcolumns 1 :formula %)
          org-agenda-columns-add-appointments-to-effort-sum t
          org-agenda-dim-blocked-tasks t
          org-agenda-persistent-filter t
          org-agenda-restore-windows-after-quit t
          org-agenda-skip-additional-timestamps-same-entry t
          org-agenda-skip-deadline-prewarning-if-scheduled 'pre-scheduled
          org-agenda-skip-scheduled-if-done t
          org-agenda-skip-deadline-if-done t
          org-agenda-skip-timestamp-if-done t
          org-agenda-skip-scheduled-if-deadline-is-shown t
          org-agenda-span 'day
          org-agenda-start-on-weekday nil
          org-agenda-todo-ignore-scheduled 'all
          org-agenda-todo-ignore-deadlines 'far
          org-agenda-time-grid nil
          org-deadline-warning-days 7
          org-enforce-todo-dependencies t
          org-enforce-todo-checkbox-dependencies nil
          org-habit-graph-column 75
          org-stuck-projects '("/PROJ" ("TODO") nil ""))

    (setq org-agenda-custom-commands
          '(;; ("c" "Custom agenda view"
            ;;  ((agenda "" ((org-agenda-overriding-header "Today's tasks:")
            ;;               (org-agenda-span 1)))
            ;;   (todo "TODO" ((org-agenda-overriding-header "All TODO items without scheduled or deadline")
            ;;                 (org-agenda-skip-function '(or (org-agenda-skip-entry-if 'timestamp)
            ;;                                                (org-agenda-skip-subtree-if 'regexp "habit"))))))
            ;;  ((org-agenda-compact-blocks t)
            ;;   (org-agenda-dim-blocked-tasks 'invisible)))
            ("O" . "Overview")
            ("Od" "Daily Review"
             ((agenda "" ((org-agenda-span 1))))
             ((org-agenda-compact-blocks t)
              (org-agenda-start-with-log-mode '(closed clock state))
              (org-agenda-start-with-clockreport-mode t)
              (org-agenda-archives-mode t)))
            ("Ow" "Weekly Review"
             ((agenda "" ((org-agenda-span 7)
                          (org-agenda-start-on-weekday 1)))
              (stuck "")
              (todo "PROJ")
              (todo "WAITING")
              (todo "SOMEDAY"))
             ((org-agenda-compact-blocks t)
              (org-agenda-start-with-clockreport-mode t)
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
      "C"  '(:ignore t :which-key "clocks")
      "Cc" 'org-agenda-clock-cancel
      "Ci" 'org-agenda-clock-in
      "Co" 'org-agenda-clock-out
      "d"  '(:ignore t :which-key "dates")
      "dd" 'org-agenda-deadline
      "ds" 'org-agenda-schedule
      "i"  '(:ignore t :which-key "insert")
      "ie" 'org-agenda-set-effort
      "ip" 'org-agenda-set-property
      "iP" 'org-agenda-priority
      "it" 'org-agenda-set-tags
      "s"  '(:ignore t :which-key "trees/subtrees")
      "sr" 'org-agenda-refile)

    (general-def 'motion org-agenda-mode-map
      "C-h"      nil
      "M-SPC"    'org-agenda/body
      "s-M-SPC"  'org-agenda/body))

  ;; Org Attach
  (setq org-attach-auto-tag "ATTACH"
        org-attach-archive-delete 'query
        org-attach-directory "attach/"
        org-attach-method 'mv)

  ;; Org Babel
  (use-package ob
    :ensure nil
    :config
    (defun ob-fix-inline-images ()
      "Fix redisplay of inline images after a code block evaluation."
      (when org-inline-image-overlays
        (org-redisplay-inline-images)))
    (add-hook 'org-babel-after-execute-hook 'ob-fix-inline-images)

    (setq org-confirm-babel-evaluate nil
          org-edit-src-content-indentation 0
          org-src-fontify-natively t
          org-src-preserve-indentation t
          org-src-tab-acts-natively t)

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
      ("'" org-edit-special :exit t))

    (use-package ob-python
      :ensure nil
      :commands (org-babel-execute:python))
    (use-package ob-emacs-lisp
      :ensure nil
      :commands (org-babel-execute:elisp
                 org-babel-expand-body:elisp
                 org-babel-execute:emacs-lisp
                 org-babel-expand-body:emacs_lisp))
    (use-package ob-latex
      :ensure nil
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
      :ensure nil
      :commands (org-babel-execute:C
                 org-babel-expand-body:C
                 org-babel-execute:C++
                 org-babel-expand-body:C++)
      :config
      (setq org-babel-C-compiler "gcc -std=c++17"
            org-babel-C++-compiler "g++ -std=c++17")))

  ;; Org Capture
  (use-package org-capture
    :ensure nil
    :config
    (defun org-capture-goto-link ()
      (org-capture-put :target (list 'file+headline
                                     (nth 1 (org-capture-get :target))
                                     (org-capture-get :annotation)))
      (org-capture-put-target-region-and-position)
      (widen)
      (let ((hd (org-capture-get :annotation)))
        (goto-char (point-min))
        (if (re-search-forward
             (format org-complex-heading-regexp-format (regexp-quote hd)) nil t)
            (org-end-of-subtree)
          (goto-char (point-max))
          (or (bolp) (insert "\n"))
          (insert "* " (nth 2 (org-capture-get :target)) "\n"))))

    (setq org-gtd-file (expand-file-name "gtd.org" org-directory))

    (setq org-capture-templates
          '(("i" "Inbox" entry (file org-gtd-file)
             "* %?\n  %i\n")
            ("j" "Journal" plain (function org-journal-find-location)
             "** %(format-time-string org-journal-time-format)%?")
            ("t" "Todo" entry (file org-gtd-file)
             "* TODO %?\n  %i\n")
            ("l" "Link" plain (file+function org-gtd-file org-capture-goto-link)
             "%i\n" :empty-lines 1 :immediate-finish t)
            ("r" "Record" entry (file org-gtd-file)
             "* %?\n  %i\n" :clock-in t :clock-keep t)
            ("R" "Review")
            ("Ry" "Yesterday" plain (function (lambda () (org-journal-find-location -1)))
             "** Daily Review\n%?\n%i")
            ("Rd" "Daily Review" plain (function org-journal-find-location)
             "** Daily Review\n%?\n%i")
            ("Rw" "Weekly Review" plain (function org-journal-find-location)
             "* Weekly Review\n%?\n%i")))

    (despot-def org-capture-mode-map
      "," 'org-capture-finalize
      "a" 'org-capture-kill
      "c" 'org-capture-finalize
      "k" 'org-capture-kill
      "r" 'org-capture-refile))

  ;; Org Clock
  (org-clock-persistence-insinuate)
  (setq org-clock-auto-clock-resolution 'when-no-clock-is-running
        org-clock-history-length 10
        org-clock-idle-time 10
        org-clock-in-resume t
        org-clock-persist t
        org-clock-persist-file (concat cache-dir "org-clock-save.el")
        org-clock-persist-query-resume nil
        org-clock-out-remove-zero-time-clocks t
        org-clock-out-when-done t
        org-clock-report-include-clocking-task t)

  ;; Org Latex
  (setq org-pandoc-options-for-latex-pdf '((pdf-engine . "xelatex"))
        org-pandoc-options-for-beamer-pdf '((pdf-engine . "xelatex")))

  (setq org-latex-compiler "xelatex"
        org-latex-packages-alist '(("" "mathspec" t)
                                   ("fontset=macnew,UTF8" "ctex" t))
        org-latex-pdf-process '("latexmk -xelatex -quiet -shell-escape -f %f"
                                "rm -fr %b.out %b.log %b.tex auto")
        org-preview-latex-default-process 'dvisvgm
        org-preview-latex-process-alist
        '((dvisvgm :programs ("xelatex" "dvisvgm")
                   :description "xdv > svg" :use-xcolor t
                   :message "you need to install the programs: xelatex and dvisvgm."
                   :image-input-type "xdv" :image-output-type "svg" :image-size-adjust (1.7 . 1.5)
                   :latex-compiler ("xelatex -no-pdf -interaction nonstopmode -output-directory %o %f")
                   :image-converter ("dvisvgm %f -n -b min -c %S -o %O"))
          (imagemagick :programs ("xelatex" "convert")
                       :description "pdf > png" :use-xcolor t
                       :message "you need to install the programs: xelatex and imagemagick."
                       :image-input-type "pdf" :image-output-type "png" :image-size-adjust (1.0 . 1.0)
                       :latex-compiler ("xelatex -interaction nonstopmode -output-directory %o %f")
                       :image-converter ("convert -density %D -trim -antialias %f -quality 100 %O"))))

  ;; Org mac grab
  (setq org-mac-grab-Outlook-app-p nil
        org-mac-grab-Firefox-app-p nil
        org-mac-grab-Evernote-app-p nil
        org-mac-grab-Brave-app-p nil
        org-mac-grab-Acrobat-app-p nil)

  ;; Org Refile
  (setq org-note-files (directory-files-recursively "~/Documents/Org/Notes" "^.*\\.org$")
        org-outline-path-complete-in-steps nil
        org-refile-allow-creating-parent-nodes 'confirm
        org-refile-use-outline-path 'file
        org-refile-targets '((nil :maxlevel . 5)
                             (org-note-files :maxlevel . 4)
                             (org-agenda-files :maxlevel . 4)))

  ;; Org Tag
  (setq org-fast-tag-selection-single-key 'expert
        org-tags-column 0
        org-tag-alist (quote ((:startgroup)
                              ("@office" . ?o)
                              ("@home" . ?h)
                              ("@computer" .?c)
                              ("@phone" . ?p)
                              (:endgroup)
                              ("PERSONAL" . ?p)
                              ("WORK" . ?w)
                              ("NOTE" . ?n)
                              ("errants" . ?e)
                              ("Action" . ?a)
                              ("Focused" . ?f)
                              ("Dessert" . ?d))))

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

  (advice-add #'org-agenda-quit                        :before #'org-save-all-org-buffers)
  (advice-add #'org-agenda-quit                        :after  #'org-agenda-finalize@after)
  (advice-add #'org-agenda-exit                        :after  #'org-agenda-finalize@after)
  (advice-add #'org-agenda-get-restriction-and-command :around #'org-agenda-get-restriction-and-command@around)
  (advice-add #'org-capture-finalize                   :after  #'org-capture-finalize@after)
  (advice-add #'org-capture-select-template            :around #'org-capture-select-template@around)
  (advice-add #'org-refile                             :after  #'org-save-all-org-buffers)
  (advice-add #'org-switch-to-buffer-other-window      :after  #'supress-frame-splitting)


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

  (defun org-clock-recents ()
    (interactive)
    (org-clock-in '(4)))

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
  :general
  (general-def 'normal org-mode-map
    "RET"      'org-open-at-point)

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
    "sA"    'org-archive-subtree
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
    "oC SPC" 'org-clock-recents
    "oCr"    'org-resolve-clocks
    "od"     '(find-org-default-notes-file :which-key "default-org-file")
    "oe"     'org-store-agenda-views
    "of"     '(:ignore t :which-key "feeds")
    "ofi"    'org-feed-goto-inbox
    "ofu"    'org-feed-update-all
    "og"     'org-mac-grab-link
    "ol"     'org-store-link
    "om"     'org-tags-view
    "oo"     'org-agenda
    "os"     'org-search-view
    "ot"     'org-todo-list))

(use-package evil-org
  :after (evil org)
  :hook (org-mode . evil-org-mode)
  :config
  (add-hook 'evil-org-mode-hook
            (lambda ()
              (evil-org-set-key-theme
               '(textobjects insert navigation additional shift todo heading))))
  (require 'evil-org-agenda)
  (evil-org-agenda-set-keys)

  (defun surround-drawer ()
    (let ((dname (read-from-minibuffer "" "")))
      (cons (format ":%s:" (upcase (or dname ""))) ":END:")))
  (defun surround-code ()
    (let ((dname (read-from-minibuffer "" "")))
      (cons (format "#+BEGIN_SRC %s" (or dname "")) "#+END_SRC")))
  (with-eval-after-load 'evil-surround
    (add-to-list 'evil-surround-pairs-alist '(?: . surround-drawer))
    (add-to-list 'evil-surround-pairs-alist '(?# . surround-code))))

(use-package org-bullets
  :hook (org-mode . org-bullets-mode))

(use-package org-download
  :hook ((org-mode dired-mode) . org-download-enable)
  :config
  (defun my-org-download-method (link)
    (org-download--fullname (org-link-unescape link)))
  (setq org-download-method 'my-org-download-method)

  (setq org-download-method 'attach
        org-download-screenshot-method "screencapture -i %s"
        org-download-image-attr-list '("#+ATTR_HTML: :width 80% :align center"))
  :general
  (despot-def org-mode-map
    "iD" '(:ignore nil :which-key "download")
    "iDy" 'org-download-yank
    "iDs" 'org-download-screenshot))

(use-package org-edit-latex
  :hook (org-mode . org-edit-latex-mode))

(use-package org-journal
  :init
  (defun org-journal-find-location (&optional days)
    (let ((date (time-add (current-time) (days-to-time (or days 0)))))
      (org-journal-new-entry t date)
      (goto-char (point-max))))

  (defun org-journal-date-format-func (time)
    "Custom function to insert journal date header,
and some custom text on a newly created journal file."
    (when (= (buffer-size) 0)
      (insert
       (pcase org-journal-file-type
         (`daily   "#+TITLE: Daily Journal\n\n")
         (`weekly  "#+TITLE: Weekly Journal\n\n")
         (`monthly "#+TITLE: Monthly Journal\n\n")
         (`yearly  "#+TITLE: Yearly Journal\n\n"))))
    (concat org-journal-date-prefix (format-time-string "%A, %B %d %Y" time)))

  (setq org-journal-dir          "~/Documents/Org/Journals/"
        org-journal-cache-file   (concat cache-dir "org-journal.cache")
        org-journal-date-format #'org-journal-date-format-func
        org-journal-file-type    'weekly)
  :general
  (tyrant-def
    "oj"  '(:ignore t :which-key "org-journal")
    "ojj" 'org-journal-new-entry
    "ojs" 'org-journal-search-forever
    "ojt" 'org-journal-new-scheduled-entry
    "ojv" 'org-journal-schedule-view)
  (despot-def org-journal-mode-map
    "j"   'org-journal-new-entry
    "n"   'org-journal-open-next-entry
    "p"   'org-journal-open-previous-entry)
  (despot-def calendar-mode-map
    "r"   'org-journal-read-entry
    "i"   'org-journal-new-date-entry
    "n"   'org-journal-next-entry
    "p"   'org-journal-previous-entry
    "s"   'org-journal-search-forever
    "w"   'org-journal-search-calendar-week
    "m"   'org-journal-search-calendar-month
    "y"   'org-journal-search-calendar-year))

(use-package org-mime
  :general
  (despot-def message-mode-map
    "e"  '(:ignore t :which-key "export")
    "em" 'org-mime-htmlize)
  (despot-def org-mode-map
    "em" 'org-mime-org-buffer-htmlize
    "es" 'org-mime-org-subtree-htmlize))

(use-package org-projectile
  :after (projectile)
  :commands (org-projectile-todo-files)
  :init
  (org-projectile-per-project)
  (setq org-projectile-per-project-filepath "TODOs.org")

  (defun append-org-agenda-files (file)
    "append to org-agenda-files if file exists"
    (when (file-exists-p file)
      (push file org-agenda-files)))

  (mapcar 'append-org-agenda-files (org-projectile-todo-files))
  :config
  (defun org-projectile-capture (&optional arg)
    (interactive "P")
    (if arg
        (org-projectile-project-todo-completing-read :empty-lines 1)
      (org-projectile-capture-for-current-project :empty-lines 1)))

  (defun org-projectile-goto-todos ()
    (interactive)
    (org-projectile-goto-location-for-project (projectile-project-name)))
  :general
  (tyrant-def
    "op" 'org-projectile-capture
    "po" 'org-projectile-goto-todos))

(use-package org-ref
  :config
  (setq reftex-default-bibliography '("~/Documents/Zotero/references.bib")

        org-ref-default-bibliography '("~/Documents/Zotero/references.bib")
        org-ref-bibliography-notes "~/Documents/Zotero/notes.org"
        org-ref-pdf-directory "~/Documents/Zotero/storage/"
        org-ref-get-pdf-filename-function 'org-ref-get-pdf-filename-helm-bibtex

        bibtex-completion-bibliography '("~/Documents/Zotero/references.bib")
        bibtex-completion-notes-path "~/Documents/Zotero/notes.org"
        bibtex-completion-library-path '("~/Documents/Zotero/storage/")
        bibtex-completion-find-additional-pdfs t
        bibtex-completion-pdf-field "file")

  (defun +org-ref-open-pdf-at-point ()
    "Open the pdf for bibtex key under point if it exists."
    (interactive)
    (message "Empty Function")
    (let* ((results (org-ref-get-bibtex-key-and-file))
           (key (car results))
           (pdf-file (car (bibtex-completion-find-pdf key))))
      (print pdf-file)
      (if (file-exists-p pdf-file)
          (org-open-file pdf-file)
        (message "No PDF found for %s" key))))

  (setq org-ref-open-pdf-function '+org-ref-open-pdf-at-point)

  (general-def org-ref-cite-keymap "<tab>" nil)
  :general
  (despot-def :keymaps '(LaTeX-mode-map org-mode-map)
    "ic"       'org-ref-insert-link)

  (general-def 'normal bibtex-mode-map
    "C-j"      'org-ref-bibtex-next-entry
    "C-k"      'org-ref-bibtex-previous-entry
    "gj"       'org-ref-bibtex-next-entry
    "gk"       'org-ref-bibtex-previous-entry)

  (despot-def bibtex-mode-map
    ;; Navigation
    "j" 'org-ref-bibtex-next-entry
    "k" 'org-ref-bibtex-previous-entry
    ;; Open
    "b" 'org-ref-open-in-browser
    "n" 'org-ref-open-bibtex-notes
    "p" 'org-ref-open-bibtex-pdf
    ;; Misc
    "h" 'org-ref-bibtex-hydra/body
    "i" 'org-ref-bibtex-hydra/org-ref-bibtex-new-entry/body-and-exit
    "s" 'org-ref-sort-bibtex-entry
    ;; Lookup utilities
    "l"  '(:ignore t :which-key "lookup")
    "la" 'arxiv-add-bibtex-entry
    "lA" 'arxiv-get-pdf-add-bibtex-entry
    "ld" 'doi-utils-add-bibtex-entry-from-doi
    "li" 'isbn-to-bibtex
    "lp" 'pubmed-insert-bibtex-from-pmid))

(use-package toc-org
  :hook (org-mode . toc-org-enable)
  :config (setq toc-org-max-depth 10))


(provide 'lang-org)
