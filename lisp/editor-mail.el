;;; editor-mail.el --- -*- lexical-binding: t; -*-

;; Copyright (C) 2020  Tianshu Wang

;; Author: Tianshu Wang <volekingsg@gmail.com>

;;; Commentary:

;;; Code:

(add-to-list 'load-path "/usr/local/Cellar/mu/")

(use-package mu4e
  :commands (mu4e-update-mail-and-index)
  :init
  (setq mu4e-maildir "~/.mail"
        mu4e-attachment-dir "~/Downloads"
        mu4e-get-mail-command "mbsync -a"
        mu4e-main-buffer-hide-personal-addresses t
        mu4e-update-interval (* 6 60 60)
        mu4e-update-timer (run-at-time
                           t mu4e-update-interval
                           (lambda () (mu4e-update-mail-and-index
                                  mu4e-index-update-in-background))))
  :config
  (setq mail-user-agent 'mu4e-user-agent
        message-citation-line-format "On %a, %b %d %Y, %f wrote:\n"
        message-kill-buffer-on-exit t
        message-sendmail-envelope-from 'header
        smtpmail-smtp-service 587

        mu4e-change-filenames-when-moving t
        mu4e-completing-read-function 'completing-read
        mu4e-compose-dont-reply-to-self t
        mu4e-compose-format-flowed t
        mu4e-confirm-quit nil
        mu4e-headers-fields '((:account . 10)
                              ;; (:recipnum . 10)
                              (:human-date . 12)
                              (:flags . 6)
                              (:from . 22)
                              (:subject))
        mu4e-hide-index-messages t
        mu4e-use-fancy-chars t
        mu4e-view-prefer-html t
        mu4e-view-show-addresses t
        mu4e-view-show-images t
        mu4e-view-image-max-width 800)

  ;; Add a column to display what email account the email belongs to.
  (add-to-list 'mu4e-header-info-custom
               '(:account
                 :name "Account"
                 :shortname "Account"
                 :help "Which account this email belongs to"
                 :function
                 (lambda (msg)
                   (let ((maildir (mu4e-message-field msg :maildir)))
                     (format "%s" (substring maildir 1 (string-match-p "/" maildir 1)))))))

  (setq mu4e-bookmarks
        '(("flag:unread AND NOT flag:trashed" "Unread messages"   ?u)
          ("maildir:/inbox/" "Inbox messages"                     ?i)
          ("maildir:/sent/" "Sent messages"                       ?s)
          ("maildir:/junk/" "Junk messages"                       ?j)
          ("maildir:/trash/" "Trash messages"                     ?T)
          ("maildir:/archive/" "Archive messages"                 ?a)
          ("maildir:/drafts/"  "Drafts messages"                  ?d)
          ("flag:flagged AND NOT flag:trashed" "Flagged messages" ?f)
          ("date:today..now" "Today's messages"                   ?t)
          ("date:7d..now" "Last 7 days"                           ?w)
          ("mime:image/*" "Messages with images"                  ?p)))

  ;; mu4e contexts setting,
  ;; offical example: https://www.djcbsoftware.nl/code/mu/mu4e/Contexts-example.html
  (load "mu4e-contexts.el.gpg")

  (require 'smtpmail-async)
  (setq send-mail-function         'async-smtpmail-send-it
        message-send-mail-function 'async-smtpmail-send-it)

  (when (fboundp 'imagemagick-register-types)
    (imagemagick-register-types))

  (add-to-list 'mu4e-view-actions
               '("View in browser" . mu4e-action-view-in-browser) t)

  (add-hook 'mu4e-compose-mode-hook
            (lambda () (use-hard-newlines t 'guess)))

  (add-hook 'message-send-hook
            (lambda () (unless (yes-or-no-p "Sure you want to send this?")
                    (signal 'quit nil))))

  ;; from http://www.djcbsoftware.nl/code/mu/mu4e/Attaching-files-with-dired.html
  (require 'gnus-dired)
  ;; make the `gnus-dired-mail-buffers' function also work on
  ;; message-mode derived modes, such as mu4e-compose-mode
  (defun gnus-dired-mail-buffers ()
    "Return a list of active message buffers."
    (let (buffers)
      (save-current-buffer
        (dolist (buffer (buffer-list t))
          (set-buffer buffer)
          (when (and (derived-mode-p 'message-mode)
                     (null message-sent-message-via))
            (push (buffer-name buffer) buffers))))
      (nreverse buffers)))
  (setq gnus-dired-mail-mode 'mu4e-user-agent)
  (add-hook 'dired-mode-hook 'turn-on-gnus-dired-mode)

  (setenv "XAPIAN_CJK_NGRAM" "yes")
  ;; Xapian, the search engine of mu has a poor support of CJK characters,
  ;; which causes only query contains no more than 2 CJK characters works.
  ;;
  ;; https://researchmap.jp/?page_id=457
  ;;
  ;; This workaroud breaks any CJK words longer than 2 characters into
  ;; combines of bi-grams. Example: 我爱你 -> (我爱 爱你)
  ;; from https://github.com/panjie/mu4e-goodies/blob/master/mu4e-goodies-hacks.el
  (defun mu4e-goodies~break-cjk-word (word)
    "Break CJK word into list of bi-grams like: 我爱你 -> 我爱 爱你"
    (if (or (<= (length word) 2)
            (equal (length word) (string-bytes word))) ; only ascii chars
        word
      (let ((pos nil)
            (char-list nil)
            (br-word nil))
        (if (setq pos (string-match ":" word))     ; like: "s:abc"
            (concat (substring word 0 (+ 1 pos))
                    (mu4e-goodies~break-cjk-word (substring word (+ 1 pos))))
          (if (memq 'ascii (find-charset-string word)) ; ascii mixed with others like: abc你好
              word
            (progn
              (setq char-list (split-string word "" t))
              (while (cdr char-list)
                (setq br-word (concat br-word (concat (car char-list) (cadr char-list)) " "))
                (setq char-list (cdr char-list)))
              br-word))))))

  (defun mu4e-goodies~break-cjk-query (expr)
    "Break CJK strings into bi-grams in query."
    (let ((word-list (split-string expr " " t))
          (new ""))
      (dolist (word word-list new)
        (setq new (concat new (mu4e-goodies~break-cjk-word word) " ")))))

  (setq mu4e-query-rewrite-function 'mu4e-goodies~break-cjk-query)

  (setf (alist-get 'trash mu4e-marks)
        (list :char '("d" . "▼")
              :prompt "dtrash"
              :dyn-target (lambda (target msg)
                            (mu4e-get-trash-folder msg))
              :action (lambda (docid msg target)
                        ;; Here's the main difference to the regular trash mark,
                        ;; no +T before -N so the message is not marked as
                        ;; IMAP-deleted:
                        (mu4e~proc-move docid (mu4e~mark-check-target target) "-N"))))

  (despot-def mu4e-compose-mode-map
    "," 'message-send-and-exit
    "c" 'message-send-and-exit
    "k" 'message-kill-buffer
    "a" 'message-kill-buffer
    "s" 'message-dont-send         ; saves as draft
    "f" 'mml-attach-file)
  :general
  (tyrant-def
    "a m" 'mu4e
    "a M" 'mu4e-compose-new))

(use-package mu4e-org
  ;; :hook (mu4e-compose-mode . org-mu4e-compose-org-mode)
  :config
  (evil-set-initial-state 'mu4e-compose-mode 'normal)
  (setq mu4e-org-link-query-in-headers-mode nil
        org-mu4e-convert-to-html t))

(use-package mu4e-alert
  :ensure t
  :defer 5
  :config (mu4e-alert-enable-mode-line-display))


(provide 'editor-mail)
;;; editor-mail.el ends here
