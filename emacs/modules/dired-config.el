;;; dired-config.el --- Enhanced directory editing -*- lexical-binding: t; -*-

;;; Commentary:
;; Comprehensive Dired enhancements for a better file management experience

;;; Code:

;; Basic built-in Dired improvements
(use-package dired
  :ensure nil  ;; dired is built-in
  :commands (dired dired-jump)
  :bind (("C-x C-j" . dired-jump))
  :custom
  ;; Always delete and copy recursively
  (dired-recursive-deletes 'always)
  (dired-recursive-copies 'always)
  ;; Auto refresh dired when file changes
  (dired-auto-revert-buffer t)
  ;; Allow dired to delete or copy dirs
  (dired-allow-to-change-permissions t)
  :config
  ;; Use the same buffer for viewing directory
  (put 'dired-find-alternate-file 'disabled nil)
  
  ;; Use different switches based on OS
  (setq dired-listing-switches
        (if (eq system-type 'darwin)
            "-lahgG"
          "-lahgo --group-directories-first"))
  
  ;; Move files between split panes
  (setq dired-dwim-target t))

;; Colorful dired
(use-package diredfl
  :hook (dired-mode . diredfl-mode)
  :config
  ;; Use more subdued colors for better readability
  (set-face-attribute 'diredfl-dir-name nil :bold t))

;; Dired subtree - view subdirectories in-line
(use-package dired-subtree
  :after dired
  :bind (:map dired-mode-map
         ("<tab>" . dired-subtree-toggle)
         ("<backtab>" . dired-subtree-cycle))
  :custom
  (dired-subtree-use-backgrounds nil)
  (dired-subtree-line-prefix "  "))

;; Extra file operations
(use-package dired-aux
  :ensure nil
  :config
  ;; Define extra file handlers for better compression/archive support
  (setq dired-compress-files-alist
        '(("\\.tar\\.gz\\'" . "tar -cf - %i | gzip -c9 > %o")
          ("\\.tar\\.bz2\\'" . "tar -cf - %i | bzip2 -c9 > %o")
          ("\\.tar\\.xz\\'" . "tar -cf - %i | xz -c9 > %o")
          ("\\.zip\\'" . "zip -r9 %o %i"))))

;; Dired-x - additional Dired functionality
(use-package dired-x
  :ensure nil
  :hook (dired-mode . dired-omit-mode)
  :config
  ;; Omit dot files and backup/auto-save files
  (setq dired-omit-files
        (concat dired-omit-files "\\|^\\..+$\\|\\.bak$\\|\\.~$"))
  ;; Don't omit directories, even if they match omit rules
  (setq dired-omit-extensions
        (nconc dired-omit-extensions
               '(".pyc" ".elc" ".o" ".hi"))))

;; All-the-icons support for dired (if available)
(use-package all-the-icons-dired
  :if (and (display-graphic-p)
           (package-installed-p 'all-the-icons))
  :hook (dired-mode . all-the-icons-dired-mode))

;; Editable dired mode
(use-package wdired
  :ensure nil
  :after dired
  :bind (:map dired-mode-map
         ("e" . wdired-change-to-wdired-mode))
  :custom
  (wdired-allow-to-change-permissions t)
  (wdired-create-parent-directories t))

;; Functions to create/remove files and directories
(defun dired-create-empty-file (file)
  "Create an empty FILE."
  (interactive 
   (list (read-file-name "Create empty file: ")))
  (with-temp-buffer
    (write-file file))
  (revert-buffer))

(defun dired-do-touch ()
  "Touch marked files in dired."
  (interactive)
  (dolist (file (dired-get-marked-files))
    (shell-command (concat "touch " (shell-quote-argument file))))
  (revert-buffer))

;; Add keybindings with my-leader-keys
(with-eval-after-load 'general
  (my-leader-keys
   "d" '(:ignore t :which-key "dired")
   "dd" '(dired :which-key "open dired")
   "dj" '(dired-jump :which-key "dired jump to current")
   "df" '(find-name-dired :which-key "find name")
   "dg" '(find-grep-dired :which-key "find grep")
   
   ;; Mark operations
   "dm" '(:ignore t :which-key "mark")
   "dma" '(dired-mark-files-regexp :which-key "mark by regexp")
   "dme" '(dired-mark-extension :which-key "mark by extension")
   "dmd" '(dired-mark-directories :which-key "mark directories")
   "dmu" '(dired-unmark-all-marks :which-key "unmark all")
   
   ;; File operations
   "do" '(:ignore t :which-key "operations")
   "doc" '(dired-do-copy :which-key "copy")
   "dom" '(dired-do-rename :which-key "move/rename")
   "dod" '(dired-do-delete :which-key "delete")
   "doz" '(dired-do-compress :which-key "compress")
   "dos" '(dired-do-symlink :which-key "symlink")
   "doh" '(dired-do-hardlink :which-key "hardlink")
   "dot" '(dired-do-touch :which-key "touch")
   "don" '(dired-create-empty-file :which-key "create empty file")
   
   ;; Toggles and view options
   "dt" '(:ignore t :which-key "toggle")
   "dth" '(dired-hide-details-mode :which-key "hide details")
   "dti" '(all-the-icons-dired-mode :which-key "toggle icons")
   "dto" '(dired-omit-mode :which-key "toggle omit mode")
   "dte" '(wdired-change-to-wdired-mode :which-key "edit mode (wdired)")))

;; Configure dired to be more intuitive
(with-eval-after-load 'dired
  ;; Delete the original buffer when renaming file
  (define-key dired-mode-map (kbd "RET") 'dired-find-alternate-file)
  
  ;; Additional key bindings
  (define-key dired-mode-map (kbd "C-+") 'dired-create-empty-file)
  (define-key dired-mode-map (kbd "M-+") 'dired-create-directory)
  (define-key dired-mode-map (kbd "I") 'dired-info-file)
  (define-key dired-mode-map (kbd "C-t") 'dired-do-touch))

(provide 'dired-config)
;;; dired-config.el ends here
