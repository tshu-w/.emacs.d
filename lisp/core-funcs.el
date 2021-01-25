;;; core-funcs.el --- -*- lexical-binding: t; -*-

;; Copyright (C) 2020  Tianshu Wang

;; Author: Tianshu Wang <volekingsg@gmail.com>

;;; Commentary:

;;; Code:

;; ---------------------------------------------------------------------------
;; File
;; ---------------------------------------------------------------------------

;; from http://emacsredux.com/blog/2013/05/18/instant-access-to-init-dot-el/
(defun find-user-init-file ()
  "Edit the `user-init-file', in the current window."
  (interactive)
  (find-file (concat user-emacs-directory "init.el")))

(defun load-user-init-file ()
  "Load the `user-init-file', in same window."
  (interactive)
  (load-file (concat user-emacs-directory "init.el")))

(defun rename-current-buffer-file (&optional arg)
  "Rename the current buffer and the file it is visiting.
If the buffer isn't visiting a file, ask if it should
be saved to a file, or just renamed.

If called without a prefix argument, the prompt is
initialized with the current directory instead of filename."
  (interactive "P")
  (let* ((old-short-name (buffer-name))
         (old-filename (buffer-file-name)))
    (if (and old-filename (file-exists-p old-filename))
        ;; the buffer is visiting a file
        (let* ((old-dir (file-name-directory old-filename))
               (new-name (read-file-name "New name: " (if arg old-dir old-filename)))
               (new-dir (file-name-directory new-name))
               (new-short-name (file-name-nondirectory new-name))
               (file-moved-p (not (string-equal new-dir old-dir)))
               (file-renamed-p (not (string-equal new-short-name old-short-name))))
          (cond ((get-buffer new-name)
                 (error "A buffer named '%s' already exists!" new-name))
                ((string-equal new-name old-filename)
                 (error "Rename failed! Same new and old name")
                 (rename-current-buffer-file))
                (t
                 (let ((old-directory (file-name-directory new-name)))
                   (when (and (not (file-exists-p old-directory))
                              (yes-or-no-p
                               (format "Create directory '%s'?" old-directory)))
                     (make-directory old-directory t)))
                 (rename-file old-filename new-name 1)
                 (rename-buffer new-name)
                 (set-visited-file-name new-name)
                 (set-buffer-modified-p nil)
                 (when (fboundp 'recentf-add-file)
                   (recentf-add-file new-name)
                   (recentf-remove-if-non-kept old-filename))
                 (when (and (require 'projectile nil 'noerror)
                            (projectile-project-p))
                   (call-interactively #'projectile-invalidate-cache))
                 (message (cond ((and file-moved-p file-renamed-p)
                                 (concat "File Moved & Renamed\n"
                                         "From: " old-filename "\n"
                                         "To:   " new-name))
                                (file-moved-p
                                 (concat "File Moved\n"
                                         "From: " old-filename "\n"
                                         "To:   " new-name))
                                (file-renamed-p
                                 (concat "File Renamed\n"
                                         "From: " old-short-name "\n"
                                         "To:   " new-short-name)))))))
      ;; the buffer is not visiting a file
      (let ((key))
        (while (not (memq key '(?s ?r)))
          (setq key (read-key (propertize
                               (format
                                (concat "Buffer '%s' is not visiting a file: "
                                        "[s]ave to file or [r]ename buffer?")
                                old-short-name)
                               'face 'minibuffer-prompt)))
          (cond ((eq key ?s)            ; save to file
                 ;; this allows for saving a new empty (unmodified) buffer
                 (unless (buffer-modified-p) (set-buffer-modified-p t))
                 (save-buffer))
                ((eq key ?r)            ; rename buffer
                 (let ((new-buffer-name (read-string "New buffer name: ")))
                   (while (get-buffer new-buffer-name)
                     ;; ask to rename again, if the new buffer name exists
                     (if (yes-or-no-p
                          (format (concat "A buffer named '%s' already exists: "
                                          "Rename again?")
                                  new-buffer-name))
                         (setq new-buffer-name (read-string "New buffer name: "))
                       (keyboard-quit)))
                   (rename-buffer new-buffer-name)
                   (message (concat "Buffer Renamed\n"
                                    "From: " old-short-name "\n"
                                    "To:   " new-buffer-name))))
                ;; ?\a = C-g, ?\e = Esc and C-[
                ((memq key '(?\a ?\e)) (keyboard-quit))))))))

(defun delete-current-buffer-file ()
  "Remove file connected to current buffer and kill buffer."
  (interactive)
  (let ((filename (buffer-file-name))
        (buffer (current-buffer))
        (name (buffer-name)))
    (if (not (and filename (file-exists-p filename)))
        (ido-kill-buffer)
      (if (yes-or-no-p
           (format "Are you sure you want to delete this file: '%s'?" name))
          (progn
            (delete-file filename t)
            (kill-buffer buffer)
            (when (and (require 'projectile nil 'noerror)
                       (projectile-project-p))
              (call-interactively #'projectile-invalidate-cache))
            (message "File deleted: '%s'" filename))
        (message "Canceled: File deletion")))))

(defun sudo-edit (&optional arg)
  "Edit file with administrator privileges."
  (interactive "P")
  (require 'tramp)
  (let ((fname (if (or arg (not buffer-file-name))
                   (read-file-name "File: ")
                 buffer-file-name)))
    (find-file
     (if (not (tramp-tramp-file-p fname))
         (concat "/sudo:root@localhost:" fname)
       (with-parsed-tramp-file-name fname parsed
         (when (equal parsed-user "root")
           (error "Already root!"))
         (let* ((new-hop (tramp-make-tramp-file-name
                          ;; Try to retrieve a tramp method suitable for
                          ;; multi-hopping
                          (cond ((tramp-get-method-parameter
                                  parsed 'tramp-login-program))
                                ((tramp-get-method-parameter
                                  parsed 'tramp-copy-program))
                                (t parsed-method))
                          parsed-user
                          parsed-domain
                          parsed-host
                          parsed-port
                          nil
                          parsed-hop))
                (new-hop (substring new-hop 1 -1))
                (new-hop (concat new-hop "|"))
                (new-fname (tramp-make-tramp-file-name
                            "sudo"
                            parsed-user
                            parsed-domain
                            parsed-host
                            parsed-port
                            parsed-localname
                            new-hop)))
           new-fname))))))

(defun open-file-in-external-app (file-path)
  "Open FILE-PATH in external application."
  (cond
   ((eq system-type 'windows-nt)
    (w32-shell-execute "open" (replace-regexp-in-string "/" "\\\\" file-path)))
   ((eq system-type 'darwin) (shell-command (format "open \"%s\"" file-path)))
   ((eq system-type 'gnu/linux) (let ((process-connection-type nil))
                                  (start-process "" nil "xdg-open" file-path)))))

(defun open-file-or-directory-in-external-app (arg)
  "Open current file in external application.
If the universal prefix argument is used then open the folder
containing the current file by the default explorer."
  (interactive "P")
  (if arg
      (open-file-in-external-app (expand-file-name default-directory))
    (let ((file-path (if (derived-mode-p 'dired-mode)
                         (dired-get-file-for-visit)
                       buffer-file-name)))
      (if file-path
          (open-file-in-external-app file-path)
        (message "No file associated to this buffer.")))))

;; find file functions in split
(defun display-in-split (buffer alist)
  "Split selected window and display BUFFER in the new window.
BUFFER and ALIST have the same form as in `display-buffer'. If ALIST contains
a split-side entry, its value must be usable as the SIDE argument for
`split-window'."
  (let ((window (split-window nil nil (cdr (assq 'split-side alist)))))
    (window--display-buffer buffer window 'window alist)
    window))

(defun find-file-vsplit (file)
  "Find FILE in vertical split."
  (interactive "FFind file (vsplit): ")
  (let ((buffer (find-file-noselect file)))
    (pop-to-buffer buffer '(display-in-split (split-side . right)))))

(defun find-file-split (file)
  "Find FILE in horizontal split."
  (interactive "FFind file (split): ")
  (let ((buffer (find-file-noselect file)))
    (pop-to-buffer buffer '(display-in-split (split-side . below)))))

(defun -delete-file (filename &optional ask-user)
  "Remove specified file or directory.

Also kills associated buffer (if any exists) and invalidates
projectile cache when it's possible.

When ASK-USER is non-nil, user will be asked to confirm file
removal."
  (interactive "f")
  (when (and filename (file-exists-p filename))
    (let ((buffer (find-buffer-visiting filename)))
      (when buffer
        (kill-buffer buffer)))
    (when (or (not ask-user)
              (yes-or-no-p "Are you sure you want to delete this file? "))
      (message filename)
      (delete-file filename)
      (when (and (require 'projectile nil 'noerror)
                 (projectile-project-p))
        (call-interactively #'projectile-invalidate-cache)))))

(defun delete-file-confirm (filename)
  "Remove specified file or directory after users approval.

FILENAME is deleted using `-delete-file' function.."
  (interactive "f")
  (funcall-interactively #'-delete-file filename t))


;; ---------------------------------------------------------------------------
;; Buffer
;; ---------------------------------------------------------------------------

;; from https://gist.github.com/3402786
(defun toggle-maximize-buffer ()
  "Maximize buffer."
  (interactive)
  (save-excursion
    (if (and (= 1 (length (window-list)))
             (assoc ?_ register-alist))
        (jump-to-register ?_)
      (progn
        (window-configuration-to-register ?_)
        (delete-other-windows)))))

(defun indent-region-or-buffer ()
  "Indent a region if selected, otherwise the whole buffer."
  (interactive)
  (save-excursion
    (if (region-active-p)
        (progn
          (indent-region (region-beginning) (region-end))
          (message "Indented selected region."))
      (progn
        (evil-indent (point-min) (point-max))
        (message "Indented buffer.")))
    (whitespace-cleanup)))

;; from http://emacsblog.org/2007/01/17/indent-whole-buffer/
(defun iwb-region-or-buffer ()
  "IWBs a region if selected, otherwise the whole buffer."
  (interactive)
  (save-excursion
    (if (region-active-p)
        (progn
          (untabify (region-beginning) (region-end))
          (indent-region (region-beginning) (region-end)))
      (progn
        (set-buffer-file-coding-system default-file-name-coding-system)
        ;; (set-buffer-file-coding-system 'utf-8-unix)
        (untabify (point-min) (point-max))
        (indent-region (point-min) (point-max))
        (whitespace-cleanup)))))

(defun switch-to-help-buffer ()
  "Open or select the `*Help*' buffer, if it exists."
  (interactive)
  (if (get-buffer "*Help*")
      (switch-to-buffer (help-buffer))
    (message "No previous Help buffer found")))

(defun switch-to-scratch-buffer (&optional arg)
  "Switch to the `*scratch*' buffer, creating it first if needed.
if prefix argument ARG is given, switch to it in an other, possibly new window."
  (interactive "P")
  (if arg
      (switch-to-buffer-other-window (get-buffer-create "*scratch*"))
    (switch-to-buffer (get-buffer-create "*scratch*"))))

(defun switch-to-messages-buffer (&optional arg)
  "Switch to the `*Messages*' buffer.
if prefix argument ARG is given, switch to it in an other, possibly new window."
  (interactive "P")
  (with-current-buffer (messages-buffer)
    (goto-char (point-max))
    (if arg
        (switch-to-buffer-other-window (current-buffer))
      (switch-to-buffer (current-buffer)))))

(defun switch-to-minibuffer-window ()
  "Switch to minibuffer window (if active)."
  (interactive)
  (when (active-minibuffer-window)
    (select-window (active-minibuffer-window))))

(defun switch-to-compilation-buffer ()
  "Go to last compilation buffer."
  (interactive)
  (if compilation-last-buffer
      (pop-to-buffer compilation-last-buffer)
    (user-error "There is no compilation buffer?")))

(defun toggle-compilation-window ()
  "Show/Hide the window containing the '*compilation*' buffer."
  (interactive)
  (when-let ((buffer compilation-last-buffer))
    (if (get-buffer-window buffer 'visible)
        (delete-windows-on buffer)
      (display-buffer buffer))))

(defvar killed-buffer-list nil
  "List of recently killed buffers.")

(defun add-buffer-to-killed-list ()
  "If buffer is associated with a file name, add that file
to the `killed-buffer-list' when killing the buffer."
  (when buffer-file-name
    (push buffer-file-name killed-buffer-list)))
(add-hook 'kill-buffer-hook #'add-buffer-to-killed-list)

(defun reopen-killed-buffer ()
  "Reopen the most recently killed file buffer, if one exists."
  (interactive)
  (when killed-buffer-list
    (find-file (pop killed-buffer-list))))

;; our own implementation of kill-this-buffer from menu-bar.el
(defun kill-current-buffer (&optional arg)
  "Kill the current buffer.
If the universal prefix argument is used then kill also the window."
  (interactive "P")
  (if (window-minibuffer-p)
      (abort-recursive-edit)
    (if (equal '(4) arg)
        (kill-buffer-and-window)
      (kill-buffer))))

;; from http://emacswiki.org/emacs/KillingBuffers
(defun kill-other-buffers (&optional arg)
  "Kill all other buffers.
If the universal prefix argument is used then kill the windows too."
  (interactive "P")
  (when (yes-or-no-p (format "Killing all buffers except \"%s\"? "
                             (buffer-name)))
    (let* ((buffers-to-kill (if (bound-and-true-p persp-mode)
                                (persp-buffer-list)
                              (buffer-list))))
      (mapc 'kill-buffer (delq (current-buffer) buffers-to-kill)))
    (when (equal '(4) arg) (delete-other-windows))
    (message "Buffers deleted!")))

;; from https://github.com/gempesaw/dotemacs/blob/emacs/dg-elisp/dg-defun.el
(defun rudekill-matching-buffers (regexp &optional internal-too)
  "Kill - WITHOUT ASKING - buffers whose name matches the specified REGEXP. See
the `kill-matching-buffers` for grateful killing. The optional 2nd argument
indicates whether to kill internal buffers too.

Returns the count of killed buffers."
  (let* ((buffers (remove-if-not
                   (lambda (buffer)
                     (let ((name (buffer-name buffer)))
                       (and name (not (string-equal name ""))
                            (or internal-too (/= (aref name 0) ?\s))
                            (string-match regexp name))))
                   (buffer-list))))
    (mapc 'kill-buffer buffers)
    (length buffers)))

(defun kill-matching-buffers-rudely (regexp &optional internal-too)
  "Kill - WITHOUT ASKING - buffers whose name matches the specified REGEXP. See
the `kill-matching-buffers` for grateful killing. The optional 2nd argument
indicates whether to kill internal buffers too.

Returns a message with the count of killed buffers."
  (interactive "sKill buffers matching this regular expression: \nP")
  (message
   (format "%d buffer(s) killed."
           (rudekill-matching-buffers regexp internal-too))))

(defun alternate-buffer (&optional window)
  "Switch back and forth between current and last buffer in the current WINDOW."
  (interactive)
  (cl-destructuring-bind (buf start pos)
      (or (cl-find (window-buffer window) (window-prev-buffers)
                   :key #'car :test-not #'eq)
          (list (other-buffer) nil nil))
    (if (not buf)
        (message "Last buffer not found.")
      (set-window-buffer-start-and-point window buf start pos))))


;; ---------------------------------------------------------------------------
;; Window
;; ---------------------------------------------------------------------------

;; from @bmag
(defun window-layout-toggle ()
  "Toggle between horizontal and vertical layout of two windows."
  (interactive)
  (if (= (count-windows) 2)
      (let* ((window-tree (car (window-tree)))
             (current-split-vertical-p (car window-tree))
             (first-window (nth 2 window-tree))
             (second-window (nth 3 window-tree))
             (second-window-state (window-state-get second-window))
             (splitter (if current-split-vertical-p
                           #'split-window-horizontally
                         #'split-window-vertically)))
        (delete-other-windows first-window)
        ;; `window-state-put' also re-selects the window if needed, so we don't
        ;; need to call `select-window'
        (window-state-put second-window-state (funcall splitter)))
    (error "Can't toggle window layout when the number of windows isn't two")))

;; originally from magnars and modified by ffevotte for dedicated windows
;; support, it has quite diverged by now
(defun rotate-windows-forward (count)
  "Rotate each window forwards.
A negative prefix argument rotates each window backwards.
Dedicated (locked) windows are left untouched."
  (interactive "p")
  (let* ((non-dedicated-windows (cl-remove-if 'window-dedicated-p (window-list)))
         (states (mapcar #'window-state-get non-dedicated-windows))
         (num-windows (length non-dedicated-windows))
         (step (+ num-windows count)))
    (if (< num-windows 2)
        (error "You can't rotate a single window!")
      (dotimes (i num-windows)
        (window-state-put
         (elt states i)
         (elt non-dedicated-windows (% (+ step i) num-windows)))))))

(defun rotate-windows-backward (count)
  "Rotate each window backwards.
Dedicated (locked) windows are left untouched."
  (interactive "p")
  (rotate-windows-forward (* -1 count)))

;; from http://dfan.org/blog/2009/02/19/emacs-dedicated-windows/
(defun toggle-current-window-dedication ()
  "Toggle dedication state of a window. Commands that change the buffer that a
window is displaying will not typically change the buffer displayed by
a dedicated window."
  (interactive)
  (let* ((window    (selected-window))
         (dedicated (window-dedicated-p window)))
    (set-window-dedicated-p window (not dedicated))
    (message "Window %sdedicated to %s"
             (if dedicated "no longer " "")
             (buffer-name))))

;; from https://gist.github.com/timcharper/493269
(defun split-window-vertically-and-focus ()
  "Split the window vertically and focus the new window."
  (interactive)
  (split-window-vertically)
  (windmove-down)
  (when (and (boundp 'golden-ratio-mode)
             (symbol-value golden-ratio-mode))
    (golden-ratio)))

(defun split-window-horizontally-and-focus ()
  "Split the window horizontally and focus the new window."
  (interactive)
  (split-window-horizontally)
  (windmove-right)
  (when (and (boundp 'golden-ratio-mode)
             (symbol-value golden-ratio-mode))
    (golden-ratio)))

(defun alternate-window ()
  "Switch back and forth between current and last window in the current frame."
  (interactive)
  (let (;; switch to first window previously shown in this frame
        (prev-window (get-mru-window nil t t)))
    ;; Check window was not found successfully
    (unless prev-window (user-error "Last window not found"))
    (select-window prev-window)))


;; ---------------------------------------------------------------------------
;; Frame
;; ---------------------------------------------------------------------------

(defun kill-frame ()
  "Kill server buffer and hide the main Emacs window."
  (interactive)
  (condition-case nil
      (delete-frame nil 1)
    (error
     (iconify-frame))))


;; ---------------------------------------------------------------------------
;; Jump Handlers
;; ---------------------------------------------------------------------------

(defvar default-jump-handlers '(xref-find-definitions)
  "List of jump handlers available in every mode.")

(defvar-local jump-handlers '()
  "List of jump handlers local to this buffer.")

(defvar default-reference-handlers '(xref-find-references)
  "List of reference handlers available in every mode.")

(defvar-local reference-handlers '()
  "List of reference handlers local to this buffer.")

(defmacro define-jump-handlers (mode &rest handlers)
  "Defines jump HANDLERS for the given MODE.
This defines a variable `jump-handlers-MODE' to which
handlers can be added, and a function added to MODE-hook which
sets `jump-handlers' in buffers of that mode."
  (let ((mode-hook (intern (format "%S-hook" mode)))
        (func (intern (format "init-jump-handlers-%S" mode)))
        (handlers-list (intern (format "jump-handlers-%S" mode))))
    `(progn
       (defvar ,handlers-list ',handlers
         ,(format (concat "List of mode-specific jump handlers for %S. "
                          "These take priority over those in "
                          "`default-jump-handlers'.")
                  mode))
       (defun ,func ()
         (setq jump-handlers
               (append ,handlers-list
                       default-jump-handlers)))
       (add-hook ',mode-hook ',func))))

(defmacro define-reference-handlers (mode &rest handlers)
  "Defines reference HANDLERS for the given MODE.
This defines a variable `reference-handlers-MODE' to which
handlers can be added, and a function added to MODE-hook which
sets `reference-handlers' in buffers of that mode."
  (let ((mode-hook (intern (format "%S-hook" mode)))
        (func (intern (format "init-reference-handlers-%S" mode)))
        (handlers-list (intern (format "reference-handlers-%S" mode))))
    `(progn
       (defvar ,handlers-list ',handlers
         ,(format (concat "List of mode-specific reference handlers for %S. "
                          "These take priority over those in "
                          "`default-reference-handlers'.")
                  mode))
       (defun ,func ()
         (setq reference-handlers
               (append ,handlers-list
                       default-reference-handlers)))
       (add-hook ',mode-hook ',func))))

(defun jump-to-definition ()
  "Jump to definition around point using the best tool for this action."
  (interactive)
  (catch 'done
    (let ((old-buffer (current-buffer))
          (old-point (point)))
      (dolist (-handler (or jump-handlers default-jump-handlers))
        (let ((handler (if (listp -handler) (car -handler) -handler))
              (async (when (listp -handler)
                       (plist-get (cdr -handler) :async))))
          (ignore-errors
            (call-interactively handler))
          (when (or (eq async t)
                    (and (fboundp async) (funcall async))
                    (not (eq old-point (point)))
                    (not (equal old-buffer (current-buffer))))
            (throw 'done t)))))
    (message "No jump handler was able to find this symbol.")))

(defun jump-to-definition-other-window ()
  "Jump to definition around point in other window."
  (interactive)
  (let ((pos (point)))
    ;; since `jump-to-definition' can be asynchronous we cannot use
    ;; `save-excursion' here, so we have to bear with the jumpy behavior.
    (switch-to-buffer-other-window (current-buffer))
    (goto-char pos)
    (jump-to-definition)))

(defun jump-to-reference ()
  "Jump to reference around point using the best tool for this action."
  (interactive)
  (catch 'done
    (let ((old-window (selected-window))
          (old-buffer (current-buffer))
          (old-point (point)))
      (dolist (-handler (or reference-handlers default-reference-handlers))
        (let ((handler (if (listp -handler) (car -handler) -handler))
              (async (when (listp -handler)
                       (plist-get (cdr -handler) :async))))
          (ignore-errors
            (call-interactively handler))
          (when (or (eq async t)
                    (and (fboundp async) (funcall async))
                    (not (eq old-point (point)))
                    (not (equal old-buffer (window-buffer old-window))))
            (throw 'done t)))))
    (message "No reference handler was able to find this symbol.")))

(defun jump-to-reference-other-window ()
  "Jump to reference around point in other window."
  (interactive)
  (let ((pos (point)))
    ;; since `jump-to-reference' can be asynchronous we cannot use
    ;; `save-excursion' here, so we have to bear with the jumpy behavior.
    (switch-to-buffer-other-window (current-buffer))
    (goto-char pos)
    (jump-to-reference)))


;; ---------------------------------------------------------------------------
;; Misc
;; ---------------------------------------------------------------------------

(defun echo (msg &rest args)
  "Display MSG in echo-area without logging it in *Messages* buffer."
  (interactive)
  (let ((message-log-max nil))
    (apply 'message msg args)))

(defun macos-switch-back-to-previous-application ()
  "Switch back to previous application on macOS."
  (interactive)
  (do-applescript
   (mapconcat
    #'identity
    '("tell application \"System Events\""
      "  tell process \"Finder\""
      "    activate"
      "    keystroke tab using {command down}"
      "  end tell"
      "end tell")
    "\n")))

(defun set-file-executable ()
  "Add executable permissions on current file."
  (interactive)
  (when (buffer-file-name)
    (set-file-modes buffer-file-name
                    (logior (file-modes buffer-file-name) #o100))
    (message (concat "Made " buffer-file-name " executable"))))

(defun iso-week-to-time (year week day)
  "Convert ISO year, week, day to elisp time value."
  (apply #'encode-time
         (append '(0 0 0)
                 (-select-by-indices
                  '(1 0 2)
                  (calendar-gregorian-from-absolute (calendar-iso-to-absolute
                                                     (list week day year)))))))


(provide 'core-funcs)
;;; core-funcs.el ends here
