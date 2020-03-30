;;; editor-layouts.el -*- lexical-binding: t; -*-

;; Copyright (C) 2020  Tianshu Wang

;; Author: Tianshu Wang <volekingsg@gmail.com>

(defconst default-persp-name "Default")

(use-package persp-mode
  :ensure t
  :init
  (setq layouts-restrict-spc-tab t)

  (setq persp-add-buffer-on-after-change-major-mode 'free
        persp-auto-resume-time -1
        persp-is-ibc-as-f-supported nil
        persp-nil-name default-persp-name
        persp-set-last-persp-for-new-frames nil
        persp-save-dir (concat cache-dir "layouts/"))

  (when (member "--persp-r" command-line-args)
    (setq persp-auto-resume-time 1)
    (delete "--persp-r" command-line-args))

  (when (member "--persp-q" command-line-args)
    (setq persp-auto-resume-time -1
          persp-auto-save-opt 0)
    (delete "--persp-q" command-line-args))

  (persp-mode)
  :config
  ;; fix persp and ivy-posframe conflit
  (defun persp-load-state-from-file@after (&optional _ _ _)
    (posframe-delete-all))
  (advice-add #'persp-load-state-from-file :after #'persp-load-state-from-file@after)

  (defun restart-emacs-resume-layouts (&optional args)
    "Restart emacs and resume layouts."
    (interactive)
    (restart-emacs (cons "--persp-r" args)))

  (defun current-persp-name ()
    "Get name of the current perspective."
    (safe-persp-name (get-frame-persp)))

  (defvar last-selected-persp default-persp-name
    "Previously selected layout.")
  (defun save-last-selected-perspective (_ &optional _ _)
    (setq last-selected-persp persp-last-persp-name))

  (defun alternate-persp ()
    "Open the previously selected layout, if it exists."
    (interactive)
    (unless (eq 'non-existent
                (gethash last-selected-persp
                         *persp-hash* 'non-existent))
      (persp-switch last-selected-persp)))

  (defun kill-current-persp ()
    "Kill current perspective"
    (interactive)
    (persp-kill (current-persp-name)))

  (defun format-persp-name (name pos)
    "Format the perspective name given by NAME for display in mode-line."
    (let* ((persp-name (if (file-directory-p name)
                           (file-name-nondirectory (directory-file-name name))
                         name))
           (string-name (format "%s" persp-name))
           (current (equal name (current-persp-name)))
           (caption (concat (number-to-string (if (eq 9 pos) 0 (+ 1 pos)))
                            ":" string-name)))
      (if current
          (propertize (concat "[" caption "]") 'face 'warning)
        caption)))

  (defun format-persp-states ()
    "Return an one liner string containing all the perspective names."
    (let ((persp-list (or (persp-names-current-frame-fast-ordered)
                          (list persp-nil-name))))
      (concat " "
              (mapconcat (lambda (persp)
                           (format-persp-name
                            persp (position persp persp-list)))
                         persp-list " | "))))

  (defun show-persp-hint ()
    "Show persp hint."
    (interactive)
    (echo (format-persp-states)))

  (defun persp-switch@after (_ &optional _ _)
    (show-persp-hint))

  (defun switch-persp-by-pos (pos)
    "Switch to perspective of position POS.
  If POS has no layout, ask the user if a new layout should be created."
    (let ((persp-to-switch
           (nth pos (persp-names-current-frame-fast-ordered))))
      (if persp-to-switch
          (persp-switch persp-to-switch)
        (persp-switch nil))))

  ;; Define all `switch-to-persp-X' functions
  (dolist (i (number-sequence 9 0 -1))
    (eval `(defun ,(intern (format "switch-to-persp-%s" i)) nil
             ,(format "Switch to perspective %s.\n%s"
                      i "See `switch-persp-by-pos' for details.")
             (interactive)
             (switch-persp-by-pos ,(if (eq 0 i) 9 (- i 1))))))

  (defun goto-default-persp ()
    "Go to `default-persp-name' perspective"
    (interactive)
    (when default-persp-name
      (persp-switch default-persp-name)))

  (defun move-element-left (element list)
    "Move ELEMENT one step to the left in LIST."
    (let (value)
      (dolist (name list value)
        (if (and (equal name element) (car value))
            (setq value (cons (car value) (cons name (cdr value))))
          (setq value (cons name value))))
      (nreverse value)))

  (defun move-element-right (element list)
    "Move ELEMENT one step to the right in LIST."
    (nreverse (move-element-left element (reverse list))))

  (defun move-current-persp-right ()
    "Moves the current perspective one step to the right."
    (interactive)
    (setq persp-names-cache (move-element-right
                             (current-persp-name)
                             persp-names-cache))
    (show-persp-hint))

  (defun move-current-persp-left ()
    "Moves the current perspective one step to the left."
    (interactive)
    (setq persp-names-cache (move-element-left
                             (current-persp-name)
                             persp-names-cache))
    (show-persp-hint))

  ;; Persp and Projectile integration
  (defun persp-switch-project (name &optional body)
    "Switch to project persp with adding project buffers and execute BODY."
    (let ((persp-reset-windows-on-nil-window-conf nil)
          (persp-already-exists (persp-with-name-exists-p name)))
      (persp-switch name)
      (condition-case nil
          (eval body)
        (quit (persp-kill-without-buffers name)))
      (unless persp-already-exists
        (let ((persp (persp-get-by-name name)))
          (when (persp-p persp)
            (persp-add-buffer (projectile-project-buffers
                               (expand-file-name name))
                              persp nil nil))))))

  ;; Persp and Ivy integration
  (defun persp-not-contains-buffer-p (buffer)
    "Return non-nil if current perspective doesn't contain BUFFER."
    (not (persp-contain-buffer-p buffer)))

  (defun ivy-switch-buffers-not-restricted ()
    (interactive)
    (let ((ivy-ignore-buffers
           (remove #'persp-not-contains-buffer-p ivy-ignore-buffers)))
      (ivy-switch-buffer)))

  (defun ivy-persp-switch-project-action (project)
    "Default action for `ivy-persp-switch-project'."
    (persp-switch-project
     project `(counsel-projectile-switch-project-action ,project)))

  (defun ivy-persp-switch-project-open-dired (project)
    (interactive)
    (persp-switch-project
     project `(dired ,project)))

  (defun ivy-persp-switch-project (arg)
    "Select a project layout using Ivy."
    (interactive "P")
    (require 'counsel-projectile)
    (ivy-read "Switch to Project Perspective: "
              (if (projectile-project-p)
                  (cons (abbreviate-file-name (projectile-project-root))
                        (projectile-relevant-known-projects))
                projectile-known-projects)
              :action #'ivy-persp-switch-project-action
              :caller #'ivy-persp-switch-project))

  (with-eval-after-load "ivy"
    (add-hook 'ivy-ignore-buffers #'persp-not-contains-buffer-p)
    (setq ivy-sort-functions-alist
          (append ivy-sort-functions-alist
                  '((persp-kill-buffer   . nil)
                    (persp-remove-buffer . nil)
                    (persp-add-buffer    . nil)
                    (persp-switch        . nil)
                    (persp-window-switch . nil)
                    (persp-frame-switch  . nil)))))

  (with-eval-after-load 'counsel-projectile
    (ivy-set-actions 'ivy-persp-switch-project
                     '(("d" ivy-persp-switch-project-open-dired "dired"))))

  (advice-add #'persp-activate :before #'save-last-selected-perspective)
  (advice-add #'persp-switch   :after  #'persp-switch@after)

  (with-eval-after-load 'which-key
    ;; rename the switch-to-persp-1 entry, to 1..10
    (push '(("\\(.*\\)1" . "switch-to-persp-1") .
            ("\\11..0" . "switch-to-persp 1..10"))
          which-key-replacement-alist)

    ;; hide the "[0,2-9] -> switch-to-persp-[0,2-9]" entries
    (push '((nil . "switch-to-persp-[0,2-9]") . t)
          which-key-replacement-alist))
  :general
  (tyrant-def
    "bB"    'ivy-switch-buffers-not-restricted
    "l"     '(:ignore t :which-key "layouts")
    "l0"    'switch-to-persp-0
    "l1"    'switch-to-persp-1
    "l2"    'switch-to-persp-2
    "l3"    'switch-to-persp-3
    "l4"    'switch-to-persp-4
    "l5"    'switch-to-persp-5
    "l6"    'switch-to-persp-6
    "l7"    'switch-to-persp-7
    "l8"    'switch-to-persp-8
    "l9"    'switch-to-persp-9
    "l SPC" 'persp-switch
    "l TAB" 'alternate-persp
    "l["    'move-current-persp-left
    "l]"    'move-current-persp-right
    "la"    'persp-add-buffer
    "lA"    'persp-import-buffers
    "ld"    'goto-default-persp
    "lh"    'show-persp-hint
    "lx"    'kill-current-persp
    "lX"    'persp-kill
    "ll"    'persp-load-state-from-file
    "ln"    'persp-next
    "lN"    'persp-add-new
    "lp"    'persp-prev
    "lr"    'persp-remove-buffer
    "pl"    'ivy-persp-switch-project
    "qR"    'restart-emacs-resume-layouts))

;; use eyebrowse to save window config
(use-package eyebrowse
  :ensure t
  :after persp-mode
  :init
  (when (bound-and-true-p persp-mode)
    (eyebrowse-mode)

    (defun get-persp-workspace (&optional persp frame)
      "Get the correct workspace parameters for perspective.
PERSP is the perspective, and defaults to the current perspective.
FRAME is the frame where the parameters are expected to be used, and
defaults to the current frame."
      (let ((param-names (if (display-graphic-p frame)
                             '(gui-eyebrowse-window-configs
                               gui-eyebrowse-current-slot
                               gui-eyebrowse-last-slot)
                           '(term-eyebrowse-window-configs
                             term-eyebrowse-current-slot
                             term-eyebrowse-last-slot))))
        (--map (persp-parameter it persp) param-names)))

    (defun set-persp-workspace (workspace-params &optional persp frame)
      "Set workspace parameters for perspective.
WORKSPACE-PARAMS should be a list containing 3 elements in this order:
- window-configs, as returned by (eyebrowse--get 'window-configs)
- current-slot, as returned by (eyebrowse--get 'current-slot)
- last-slot, as returned by (eyebrowse--get 'last-slot)
PERSP is the perspective, and defaults to the current perspective.
FRAME is the frame where the parameters came from, and defaults to the
current frame.

Each perspective has two sets of workspace parameters: one set for
graphical frames, and one set for terminal frames."
      (let ((param-names (if (display-graphic-p frame)
                             '(gui-eyebrowse-window-configs
                               gui-eyebrowse-current-slot
                               gui-eyebrowse-last-slot)
                           '(term-eyebrowse-window-configs
                             term-eyebrowse-current-slot
                             term-eyebrowse-last-slot))))
        (--zip-with (set-persp-parameter it other persp)
                    param-names workspace-params)))

    (defun save-eyebrowse-for-perspective (&optional frame)
      "Save FRAME's eyebrowse workspace to FRAME's perspective.
FRAME defaults to the current frame."
      (set-persp-workspace (list (eyebrowse--get 'window-configs frame)
                                 (eyebrowse--get 'current-slot frame)
                                 (eyebrowse--get 'last-slot frame))
                           (get-frame-persp frame)
                           frame))

    (defun update-eyebrowse-for-perspective (&rest _args)
      "Update and save current frame's eyebrowse workspace to its perspective."
      (let* ((current-slot (eyebrowse--get 'current-slot))
             (current-tag (nth 2 (assoc current-slot (eyebrowse--get 'window-configs)))))
        (eyebrowse--update-window-config-element
         (eyebrowse--current-window-config current-slot current-tag)))
      (save-eyebrowse-for-perspective))

    (defun load-eyebrowse-for-perspective (type &optional frame)
      "Load an eyebrowse workspace according to a perspective's parameters.
 FRAME's perspective is the perspective that is considered, defaulting to
 the current frame's perspective.
 If the perspective doesn't have a workspace, create one."
      (when (eq type 'frame)
        (let* ((workspace-params (get-persp-workspace (get-frame-persp frame) frame))
               (window-configs (nth 0 workspace-params))
               (current-slot (nth 1 workspace-params))
               (last-slot (nth 2 workspace-params)))
          (if window-configs
              (progn
                (eyebrowse--set 'window-configs window-configs frame)
                (eyebrowse--set 'current-slot current-slot frame)
                (eyebrowse--set 'last-slot last-slot frame)
                (eyebrowse--load-window-config current-slot))
            (eyebrowse--set 'window-configs nil frame)
            (eyebrowse-init frame)
            (save-eyebrowse-for-perspective frame)))))

    (defun load-eyebrowse-after-loading-layout (_state-file _phash persp-names)
      "Bridge between `persp-after-load-state-functions' and
`load-eyebrowse-for-perspective'.

_PHASH is the hash were the loaded perspectives were placed, and
PERSP-NAMES are the names of these perspectives."
      (let ((cur-persp (get-current-persp)))
        ;; load eyebrowse for current perspective only if it was one of the loaded
        ;; perspectives
        (when (member (or (and cur-persp (persp-name cur-persp))
                          persp-nil-name)
                      persp-names)
          (load-eyebrowse-for-perspective 'frame))))

    ;; hooks
    (add-hook 'persp-before-switch-functions
              #'update-eyebrowse-for-perspective)
    (add-hook 'eyebrowse-post-window-switch-hook
              #'save-eyebrowse-for-perspective)
    (add-hook 'persp-activated-functions
              #'load-eyebrowse-for-perspective)
    (add-hook 'persp-before-save-state-to-file-functions
              #'update-eyebrowse-for-perspective)
    (add-hook 'persp-after-load-state-functions
              #'load-eyebrowse-after-loading-layout)

    ;; eyebrowse's advice for rename-buffer only updates workspace window
    ;; configurations that are stored in frame properties, but Spacemacs's
    ;; persp-mode integration saves workspace window configurations in
    ;; perspective parameters.  We need to replace eyebrowse's advice with
    ;; perspective-aware advice in order to ensure that window
    ;; configurations for inactive perspectives get updated.
    (defun fixup-window-configs (orig-fn newname &optional unique)
      "Update the buffer's name in the eyebrowse window-configs of any perspectives
containing the buffer."
      (let* ((old (buffer-name))
             (new (funcall orig-fn newname unique)))
        (dolist (persp (persp--buffer-in-persps (current-buffer)))
          (dolist (window-config
                   (append (persp-parameter 'gui-eyebrowse-window-configs persp)
                           (persp-parameter 'term-eyebrowse-window-configs persp)))
            (eyebrowse--rename-window-config-buffers window-config old new)))))

    (when (ad-find-advice 'rename-buffer 'around 'eyebrowse-fixup-window-configs)
      (ad-disable-advice 'rename-buffer 'around 'eyebrowse-fixup-window-configs)
      (ad-activate 'rename-buffer))
    (advice-add 'rename-buffer :around #'fixup-window-configs)))


(provide 'editor-layouts)
