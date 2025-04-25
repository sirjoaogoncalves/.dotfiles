;;; tramp-config.el --- TRAMP remote access configuration -*- lexical-binding: t; -*-

;;; Commentary:
;; Enhanced remote editing capabilities through TRAMP

;;; Code:

;; Basic TRAMP configuration
(use-package tramp
  :ensure nil  ;; TRAMP is built-in
  :defer t
  :config
  ;; Faster connections by disabling version control
  (setq tramp-verbose 1)  ;; Reduce verbosity (default is 7)
  
  ;; Default connection method
  (setq tramp-default-method "ssh")
  
  ;; Keep remote files out of recentf
  (setq recentf-exclude
        (append recentf-exclude
                '("\\`/\\(?:ssh\\|su\\|sudo\\|scp\\|rsync\\|krb5\\|sshx\\|sftp\\):")))
  
  ;; Don't ask for password for these hosts
  (setq password-cache t)
  (setq password-cache-expiry 3600)  ;; Cache for 1 hour
  
  ;; Speed optimizations for remote connections
  (setq tramp-completion-reread-directory-timeout nil)
  
  ;; Enable remote directory tracking with shell, zsh, and bash
  (add-to-list 'tramp-remote-path 'tramp-own-remote-path)
  
  ;; Enable editing via sudo and doas
  (add-to-list 'tramp-methods
               '("sudo"
                 (tramp-login-program "sudo")
                 (tramp-login-args (("-u" "%u") ("-s") ("-H") ("-p" "Password:") ("%c")))
                 (tramp-remote-shell "/bin/sh")
                 (tramp-remote-shell-args ("-c"))))
  
  (add-to-list 'tramp-methods
               '("doas"
                 (tramp-login-program "doas")
                 (tramp-login-args (("-u" "%u") ("-s")))
                 (tramp-remote-shell "/bin/sh")
                 (tramp-remote-shell-args ("-c"))))
  
  ;; Use control master for connections
  (setq tramp-use-ssh-controlmaster-options t)
  
  ;; Setup backup behavior - don't create backup~ files remotely
  (add-to-list 'backup-directory-alist
               (cons tramp-file-name-regexp nil)))

;; Performance optimizations for TRAMP remote editing
(defun my/tramp-mode-hook ()
  "Settings optimized for TRAMP remote editing."
  (when (file-remote-p default-directory)
    ;; Disable version control on remote files
    (setq-local vc-ignore-dir-regexp
                (format "\\(%s\\)\\|\\(%s\\)"
                        vc-ignore-dir-regexp
                        tramp-file-name-regexp))
    
    ;; Disable backup
    (setq-local backup-inhibited t)
    
    ;; Disable auto-save
    (setq-local auto-save-default nil)
    
    ;; Disable lock files
    (setq-local create-lockfiles nil)
    
    ;; Disable expensive features
    (when (bound-and-true-p lsp-mode)
      (lsp-disconnect))
    (when (bound-and-true-p flycheck-mode)
      (flycheck-mode -1))
    (when (bound-and-true-p flymake-mode)
      (flymake-mode -1))
    
    ;; Use faster completion
    (setq-local company-idle-delay 0.5)
    
    ;; Remote file notification is SLOW - disable it
    (when (fboundp 'file-notify-rm-all-watches)
      (file-notify-rm-all-watches))))

(add-hook 'find-file-hook #'my/tramp-mode-hook)

;; Avoid delays when using sudo with file editing
(defadvice tramp-handle-expand-file-name
    (around tramp-expand-file-name-fix)
  "Fix expanding //sudo: to be /sudo:root@localhost:."
  (if (string-match (rx bos "//" (literal "sudo") ":")
                    (ad-get-arg 0))
      (let ((fixed-name (replace-regexp-in-string
                         "//sudo:"
                         "/sudo:root@localhost:"
                         (ad-get-arg 0))))
        (ad-set-arg 0 fixed-name))
  ad-do-it))

(ad-activate 'tramp-handle-expand-file-name)

;; Allow opening files as root easily
(defun find-file-as-root (file)
  "Find FILE as root."
  (interactive "FFind file as root: ")
  (let ((tramp-file-name (concat "/sudo::" (expand-file-name file))))
    (find-file tramp-file-name)))

(defun find-current-file-as-root ()
  "Find the current file as root."
  (interactive)
  (if buffer-file-name
      (let ((tramp-file-name (concat "/sudo::" buffer-file-name)))
        (find-file tramp-file-name))
    (message "Current buffer is not visiting a file")))

;; Remote directory bookmarks
(defvar my-remote-bookmarks
  '(("My Server" . "/ssh:username@myserver.com:/home/username/")
    ("Web Server" . "/ssh:user@webserver:/var/www/")
    ("Database Server" . "/ssh:dbuser@dbserver:/home/dbuser/"))
  "Alist of remote server bookmarks.")

(defun my-connect-to-remote-bookmark ()
  "Connect to a predefined remote server using bookmarks."
  (interactive)
  (let* ((servers (mapcar 'car my-remote-bookmarks))
         (choice (completing-read "Connect to: " servers))
         (path (cdr (assoc choice my-remote-bookmarks))))
    (find-file path)))

;; Quick functions to edit specific files
(defun edit-remote-config (file)
  "Edit FILE on remote server."
  (interactive
   (list (read-file-name "Edit remote file: "
                       (expand-file-name "/ssh:"))))
  (find-file file))

;; Add keybindings with my-leader-keys
(with-eval-after-load 'general
  (my-leader-keys
   "r" '(:ignore t :which-key "remote")
   "rr" '(my-connect-to-remote-bookmark :which-key "connect to remote bookmark")
   "rf" '(edit-remote-config :which-key "edit remote file")
   "rs" '(find-file-as-root :which-key "find file as root")
   "rc" '(find-current-file-as-root :which-key "open current file as root")))

;; Automatically follow symlinks to version controlled files
;; (helps with sudo-edited files)
(setq vc-follow-symlinks t)

;; Automatically kill processes when closing remote buffers
(setq tramp-default-proxies-alist nil)
(setq tramp-auto-save-directory (expand-file-name "tramp-autosave" user-emacs-directory))

;; Create necessary directories
(unless (file-exists-p tramp-auto-save-directory)
  (make-directory tramp-auto-save-directory t))

;; Improve multi-hop performance
(setq tramp-ssh-controlmaster-options
      "-o ControlMaster=auto -o ControlPath='tramp.%%C' -o ControlPersist=no")

(provide 'tramp-config)
;;; tramp-config.el ends here
