;;; ide-config.el --- Modern IDE features -*- lexical-binding: t; -*-

;;; Commentary:
;; Enhanced IDE-like features including better tabs and file browser

;;; Code:

;; Centaur Tabs for a modern tab bar experience
(use-package centaur-tabs
  :demand
  :custom
  (centaur-tabs-style "rounded")
  (centaur-tabs-height 32)
  (centaur-tabs-set-icons t)
  (centaur-tabs-set-modified-marker t)
  (centaur-tabs-modified-marker "•")
  (centaur-tabs-set-bar 'under)
  (centaur-tabs-set-close-button nil)
  (centaur-tabs-gray-out-icons 'buffer)
  (centaur-tabs-cycle-scope 'tabs)
  :config
  (centaur-tabs-mode t)
  (centaur-tabs-headline-match)
  (centaur-tabs-group-by-projectile-project)
  
  ;; Improved buffer filtering to exclude system buffers
  (defun centaur-tabs-hide-tab (x)
    "Do not show buffers in tabs that match any of these conditions."
    (let ((name (format "%s" x)))
      (or
       ;; Hide special buffers with leading *
       (string-prefix-p "*" name)
       ;; Hide dashboard buffer
       (string= name "*dashboard*")
       ;; Hide specific buffers
       (string= name "*scratch*")
       (string= name "*Messages*")
       (string= name "*Warnings*")
       ;; Hide dired buffers
       (derived-mode-p 'dired-mode)
       (derived-mode-p 'dashboard-mode))))
  
  ;; Tab groups configuration
  (defun centaur-tabs-buffer-groups ()
    "Group tabs by project and mode."
    (list
     (cond
      ;; Programming modes
      ((derived-mode-p 'prog-mode)
       "Code")
      ;; Org files
      ((string-equal major-mode "org-mode")
       "Org")
      ;; Markdown files
      ((string-equal major-mode "markdown-mode")
       "Markdown")
      ;; Web files
      ((or (string-equal major-mode "html-mode")
           (string-equal major-mode "css-mode")
           (string-equal major-mode "js-mode")
           (string-equal major-mode "web-mode"))
       "Web")
      ;; Default group
      (t "Main"))))
  
  ;; Mouse support for tabs
  (define-key centaur-tabs-mode-map (kbd "<C-mouse-1>") 'centaur-tabs-buffer-select-mouse)
  (define-key centaur-tabs-mode-map (kbd "<C-mouse-3>") 'centaur-tabs-mouse-close-tab))

;; Enable mouse support in all contexts
(xterm-mouse-mode 1)
(global-set-key [mouse-4] 'scroll-down-line)
(global-set-key [mouse-5] 'scroll-up-line)

;; Configure treemacs but DONT auto-open it
(use-package treemacs
  :defer t
  :custom
  (treemacs-position 'left)
  (treemacs-width 35)
  (treemacs-indentation 2)
  (treemacs-indentation-string " ")
  (treemacs-show-hidden-files t)
  (treemacs-collapse-dirs 3)
  (treemacs-silent-refresh t)
  (treemacs-silent-filewatch t)
  (treemacs-file-event-delay 500)
  (treemacs-file-follow-delay 0.1)
  ;; Explicitly disable auto-open behaviors
  (treemacs-follow-after-init nil)
  (treemacs-expand-after-init nil)
  (treemacs-find-workspace-method 'find-for-file-or-pick-first)
  (treemacs-sorting 'alphabetic-asc)
  (treemacs-persist-file (expand-file-name "treemacs-persist" user-emacs-directory))
  (treemacs-recenter-after-file-follow 'on-distance)
  (treemacs-recenter-distance 0.1)
  :config
  ;; Only enable these when explicitly requested
  (treemacs-filewatch-mode t)
  (treemacs-fringe-indicator-mode 'always)

  ;; Define function to reveal current file without auto-opening
  (defun treemacs-reveal-current-file ()
    "Reveal and focus the current file in the treemacs window."
    (interactive)
    (let ((path (buffer-file-name)))
      (when path
        (save-selected-window
          (if (treemacs-get-local-window)
              (progn
                (treemacs-select-window)
                (treemacs-find-file-node path))
            (treemacs)
            (treemacs-select-window)
            (treemacs-find-file-node path))))))
  
  ;; Make sure the treemacs window doesn't get closed easily
  (setq treemacs-is-never-other-window t))

;; Treemacs icons and visual enhancements
(use-package treemacs-all-the-icons
  :after (treemacs all-the-icons)
  :config
  (treemacs-load-theme "all-the-icons"))

;; Projectile integration for Treemacs
(use-package treemacs-projectile
  :after (treemacs projectile))

;; Magit integration for Treemacs
(use-package treemacs-magit
  :after (treemacs magit))

;; Evil mode integration for Treemacs
(use-package treemacs-evil
  :after (treemacs evil))

;; Better mouse support for Treemacs
(with-eval-after-load 'treemacs
  (define-key treemacs-mode-map [mouse-1] #'treemacs-single-click-expand-action)
  
  ;; Right-click menu for treemacs
  (defun treemacs-context-menu (event)
    "Open a context menu for node at EVENT."
    (interactive "e")
    (let ((menu (make-sparse-keymap "Treemacs")))
      (define-key menu [open] '("Open" . treemacs-visit-node-default))
      (define-key menu [open-external] '("Open Externally" . treemacs-visit-node-in-external-application))
      (define-key menu [separator1] '("--"))
      (define-key menu [create-file] '("Create File" . treemacs-create-file))
      (define-key menu [create-dir] '("Create Directory" . treemacs-create-dir))
      (define-key menu [rename] '("Rename" . treemacs-rename))
      (define-key menu [delete] '("Delete" . treemacs-delete))
      (define-key menu [separator2] '("--"))
      (define-key menu [copy-path] '("Copy Path" . treemacs-copy-path))
      (define-key menu [copy-root] '("Copy Project Root" . treemacs-copy-project-root))
      (popup-menu menu event))))
  
  (define-key treemacs-mode-map [mouse-3] #'treemacs-context-menu))

;; Always show Line numbers in treemacs
(add-hook 'treemacs-mode-hook 
          (lambda () (display-line-numbers-mode -1)))

;; Show visual file icons in dired
(use-package all-the-icons-dired
  :hook (dired-mode . all-the-icons-dired-mode))

;; Force single window at startup
(defun ensure-single-window ()
  "Ensure we only have a single window after startup."
  (delete-other-windows)
  ;; Clear echo area
  (message nil))

(add-hook 'emacs-startup-hook #'ensure-single-window)
(add-hook 'window-setup-hook #'ensure-single-window)

;; Add keybindings with my-leader-keys
(with-eval-after-load 'general
  (my-leader-keys
   "t" '(:ignore t :which-key "tabs/tree")
   "tt" '(treemacs :which-key "toggle treemacs")
   "tf" '(treemacs-reveal-current-file :which-key "find current file")
   "tp" '(treemacs-add-project-to-workspace :which-key "add project")
   "tc" '(treemacs-collapse-all-projects :which-key "collapse all")
   "tr" '(treemacs-refresh :which-key "refresh treemacs")
   
   "tb" '(:ignore t :which-key "tabs")
   "tbn" '(centaur-tabs-forward :which-key "next tab")
   "tbp" '(centaur-tabs-backward :which-key "previous tab")
   "tbg" '(centaur-tabs-counsel-switch-group :which-key "switch tab group")
   "tbs" '(centaur-tabs-switch-group :which-key "switch group")
   "tbk" '(centaur-tabs-kill-other-buffers-in-current-group :which-key "kill other tabs")
   "tba" '(centaur-tabs-select-end-tab :which-key "last tab")
   "tbi" '(centaur-tabs-select-beg-tab :which-key "first tab")))

(provide 'ide-config)
;;; ide-config.el ends here
