;;; ui.el --- UI configuration -*- lexical-binding: t; -*-

;; Disable visible scrollbar, toolbar, tooltips
(scroll-bar-mode -1)        ;; Disable visible scrollbar
(tool-bar-mode -1)          ;; Disable the toolbar
(tooltip-mode -1)           ;; Disable tooltips
(menu-bar-mode -1)          ;; Disable the menu bar

;; Enable which-key
(which-key-mode)
(setq which-key-idle-delay 0.3)
(diminish 'which-key-mode)

;; Font configuration - uncomment and modify as needed
;; (set-face-attribute 'default nil :font "Fira Code" :height 120)
;; (set-face-attribute 'fixed-pitch nil :font "Fira Code" :height 120)
;; (set-face-attribute 'variable-pitch nil :font "Cantarell" :height 120 :weight 'regular)

;; Theme
(use-package doom-themes
  :config
  (load-theme 'doom-tokyo-night t)
  (doom-themes-visual-bell-config))

;; Modeline
(use-package doom-modeline
  :init (doom-modeline-mode 1)
  :custom
  (doom-modeline-height 25)
  (doom-modeline-bar-width 3)
  (doom-modeline-lsp t)
  (doom-modeline-github t)
  (doom-modeline-minor-modes nil)
  (doom-modeline-buffer-file-name-style 'truncate-except-project))

;; Rainbow delimiters for better bracket visibility
(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

;; Highlight matching parentheses
(use-package paren
  :config
  (set-face-attribute 'show-paren-match nil :weight 'bold)
  (setq show-paren-style 'parenthesis)
  (setq show-paren-delay 0)
  (show-paren-mode 1))

;; Display line and column numbers in mode-line
(column-number-mode 1)

;; Visual line mode (word wrap) for text modes
(add-hook 'text-mode-hook 'visual-line-mode)

;; Highlight TODO keywords
(use-package hl-todo
  :hook (prog-mode . hl-todo-mode)
  :config
  (setq hl-todo-keyword-faces
        '(("TODO"   . "#FF0000")
          ("FIXME"  . "#FF0000")
          ("DEBUG"  . "#A020F0")
          ("GOTCHA" . "#FF4500")
          ("NOTE"   . "#1E90FF"))))

(provide 'ui)
;;; ui.el ends here
