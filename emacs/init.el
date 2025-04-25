;;; init.el --- Main configuration loader -*- lexical-binding: t; -*-

;; This file loads all the separate configuration files

;;; Commentary:

;;; Code:

;; Increase garbage collection threshold for faster startup
(setq gc-cons-threshold 50000000)
(setq large-file-warning-threshold 100000000)

;; Add core and modules directories to load path
(add-to-list 'load-path (expand-file-name "core" user-emacs-directory))
(add-to-list 'load-path (expand-file-name "modules" user-emacs-directory))

;; Load core configuration
(require 'packages)   ;; Package management
(require 'defaults)   ;; Better default settings
(require 'ui)         ;; UI configuration
(require 'keybindings);; Global keybindings

;; Load feature modules
(require 'evil-config)     ;; Evil mode (optional)
(require 'completion)      ;; Completion frameworks
(require 'dashboard-config);; Dashboard
(require 'development)     ;; Development tools
(require 'git)             ;; Git integration
(require 'ai)              ;; AI tools including Minuet
(require 'terminal)        ;; Terminal integration

;; Load custom file
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load custom-file))

;; Reset GC threshold to reasonable value
(add-hook 'emacs-startup-hook
          (lambda ()
            (setq gc-cons-threshold 16777216) ;; 16mb
            (message "Emacs loaded in %s with %d garbage collections."
                     (format "%.2f seconds"
                             (float-time
                              (time-subtract after-init-time before-init-time)))
                     gcs-done)))

(provide 'init)
;;; init.el ends here
