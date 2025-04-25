;;; packages.el --- Package management -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:
;; Setup package repositories
(require 'package)
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("org" . "https://orgmode.org/elpa/")
                         ("elpa" . "https://elpa.gnu.org/packages/")))
(package-initialize)

;; Setup use-package
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t) ;; Always ensure packages are installed

;; Core packages
(use-package exec-path-from-shell
  :config
  (when (memq window-system '(mac ns x))
    (exec-path-from-shell-initialize))
  (exec-path-from-shell-copy-env "GEMINI_API_KEY"))

(use-package general)   ;; Keybinding framework
(use-package which-key) ;; Key binding hints

;; Visual packages
(use-package all-the-icons
  :if (display-graphic-p))

;; Enable diminish to hide minor modes in modeline
(use-package diminish)

;;vterm
(use-package vterm)

(provide 'packages)
;;; packages.el ends here

