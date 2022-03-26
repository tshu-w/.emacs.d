;;; editor-mail.el --- -*- lexical-binding: t; -*-

;; Copyright (C) 2020-2022  Tianshu Wang

;; Author: Tianshu Wang <wang@tianshu.me>

;;; Commentary:

;;; Code:

(use-package mu4e
  :load-path "/usr/local/share/emacs/site-lisp/mu/mu4e"
  :commands (mu4e-org-open
             mu4e-org-store-link)
  :init
  (setq mu4e-maildir "$XDG_DATA_HOME/mail"
        mu4e-attachment-dir "~/Downloads"
        mu4e-get-mail-command "mbsync -c $XDG_CONFIG_HOME/isync/mbsyncrc -a"
        mu4e-main-buffer-hide-personal-addresses t)

  (with-eval-after-load 'org
    (org-link-set-parameters "mu4e"
                             :follow #'mu4e-org-open
                             :store  #'mu4e-org-store-link))
  :config
  (setq mail-user-agent 'mu4e-user-agent
        message-citation-line-format "On %a, %b %d %Y, %f wrote:\n"
        message-kill-buffer-on-exit t
        message-sendmail-envelope-from 'header
        smtpmail-smtp-service 587

        mu4e-change-filenames-when-moving t
        mu4e-completing-read-function 'completing-read
        mu4e-compose-complete-only-personal t
        mu4e-compose-dont-reply-to-self t
        mu4e-compose-reply-ignore-address '("not?\\(?:_-\\)?reply")
        mu4e-confirm-quit nil
        mu4e-headers-fields '((:account . 10)
                              (:human-date . 12)
                              (:flags . 6)
                              (:from . 22)
                              (:subject))
        mu4e-hide-index-messages t
        mu4e-use-fancy-chars nil
        mu4e-view-prefer-html nil
        mu4e-view-show-addresses t
        mu4e-view-show-images t
        mu4e-view-image-max-width 700)

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
        '(("maildir:/Inbox/" "Inbox messages"                     ?i)
          ("maildir:/Sent/" "Sent messages"                       ?s)
          ("maildir:/Spam/" "Spam messages"                       ?S)
          ("maildir:/Trash/" "Trash messages"                     ?T)
          ("maildir:/Archive/" "Archive messages"                 ?a)
          ("maildir:/Drafts/"  "Drafts messages"                  ?d)
          ("flag:flagged AND NOT flag:trashed" "Flagged messages" ?f)
          ("date:today..now" "Today's messages"                   ?t)
          ("date:7d..now" "Last 7 days"                           ?w)
          ("mime:image/*" "Messages with images"                  ?p)))

  ;; mu4e contexts setting,
  ;; offical example: https://www.djcbsoftware.nl/code/mu/mu4e/Contexts-example.html
  (setq mu4e-context-policy 'pick-first
        mu4e-contexts
        `(,(make-mu4e-context
            :name "fastmail"
            :match-func (lambda (msg)
                          (when msg
                            (string-prefix-p "/fastmail" (mu4e-message-field msg :maildir))))
            :enter-func (lambda () (mu4e-message "Switch to the fastmail context"))
            :leave-func (lambda () (mu4e-clear-caches))
            :vars '((user-mail-address . "wang@tianshu.me")
                    (mu4e-compose-signature . "Tianshu Wang\n")
                    (mu4e-sent-folder . "/fastmail/Sent")
                    (mu4e-drafts-folder . "/fastmail/Drafts")
                    (mu4e-trash-folder . "/fastmail/Trash")
                    (mu4e-refile-folder . "/fastmail/Archive")
                    (smtpmail-smtp-server . "smtp.fastmail.com")
                    (smtpmail-stream-type . ssl)
                    (smtpmail-smtp-service . 465)
                    (smtpmail-auth-credentials . (expand-file-name "~/.authinfo.gpg"))))
          ,(make-mu4e-context
            :name "iscas"
            :match-func (lambda (msg)
                          (when msg
                            (string-prefix-p "/iscas" (mu4e-message-field msg :maildir))))
            :enter-func (lambda () (mu4e-message "Switch to the iscas context"))
            :leave-func (lambda () (mu4e-clear-caches))
            :vars '((user-mail-address . "tianshu2020@iscas.ac.cn")
                    (mu4e-compose-signature . "Tianshu Wang\n")
                    (mu4e-sent-folder . "/iscas/Sent")
                    (mu4e-drafts-folder . "/iscas/Drafts")
                    (mu4e-trash-folder . "/iscas/Trash")
                    (mu4e-refile-folder . "/iscas/Archive")
                    (smtpmail-smtp-server . "mail.cstnet.cn")
                    (smtpmail-stream-type . ssl)
                    (smtpmail-smtp-service . 994)
                    (smtpmail-auth-credentials . (expand-file-name "~/.authinfo.gpg"))))))

  (when (require 'smtpmail-async nil t)
    (setq send-mail-function         'async-smtpmail-send-it
          message-send-mail-function 'async-smtpmail-send-it))

  (when (fboundp 'imagemagick-register-types)
    (imagemagick-register-types))

  (add-to-list 'mu4e-view-actions
               '("View in browser" . mu4e-action-view-in-browser) t)

  (add-hook 'mu4e-compose-mode-hook
            (lambda () (use-hard-newlines t 'guess)))

  (add-hook 'message-send-hook
            (lambda () (unless (yes-or-no-p "Sure you want to send this?")
                    (signal 'quit nil))))

  (progn
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

    (setq mu4e-query-rewrite-function 'mu4e-goodies~break-cjk-query))

  (setf (alist-get 'trash mu4e-marks)
        (list :char '("d" . "▼")
              :prompt "dtrash"
              :dyn-target (lambda (target msg)
                            (mu4e-get-trash-folder msg))
              :action (lambda (docid msg target)
                        ;; Here's the main difference to the regular trash mark,
                        ;; no +T before -N so the message is not marked as
                        ;; IMAP-deleted:
                        (mu4e~proc-move docid (mu4e~mark-check-target target) "+S-u-N"))))

  ;; TEMP: https://github.com/djcb/mu/issues/2193
  (defun mu4e~proc-start@around (fun)
    (let ((default-directory temporary-file-directory))
      (funcall fun)))
  (advice-add 'mu4e~proc-start :around #'mu4e~proc-start@around)
  :general
  (tyrant-def "am" 'mu4e))


(provide 'editor-mail)
;;; editor-mail.el ends here
