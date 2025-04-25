;;; development.el --- Development tools -*- lexical-binding: t; -*-

;; Projectile for project management
(use-package projectile
  :diminish projectile-mode
  :config
  (projectile-mode +1)
  :custom
  (projectile-completion-system 'default)
  :bind-keymap
  ("C-c p" . projectile-command-map))

;; Treemacs file explorer
(use-package treemacs
  :defer t
  :config
  (treemacs-follow-mode t)
  (treemacs-filewatch-mode t))

;; Integrate treemacs with projectile
(use-package treemacs-projectile
  :after (treemacs projectile))

;; Flycheck for syntax checking
(use-package flycheck
  :init (global-flycheck-mode)
  :diminish flycheck-mode)

;; LSP mode for language server support
(use-package lsp-mode
  :commands (lsp lsp-deferred)
  :hook ((python-mode . lsp-deferred)
         (js-mode . lsp-deferred)
         (php-mode . lsp-deferred))
  :init
  (setq lsp-keymap-prefix "C-c l")
  :config
  (lsp-enable-which-key-integration t))

;; LSP UI enhancements
(use-package lsp-ui
  :hook (lsp-mode . lsp-ui-mode)
  :custom
  (lsp-ui-doc-position 'bottom))


;; Yasnippet for code templates
(use-package yasnippet
  :diminish yas-minor-mode
  :config
  (yas-global-mode 1))

;; Collection of snippets
(use-package yasnippet-snippets
  :after yasnippet)

;; Format all for automatic code formatting
(use-package format-all
  :commands format-all-buffer
  :hook (prog-mode . format-all-mode)
  :config
  (setq format-all-show-errors 'warnings))

;; Language specific setup
;; Python
(use-package python
  :mode ("\\.py\\'" . python-mode)
  :custom
  (python-indent-offset 4))

;; JavaScript/TypeScript
(use-package js2-mode
  :mode "\\.js\\'"
  :custom
  (js2-basic-offset 2))

;; PHP
(use-package php-mode
  :mode "\\.php\\'")

;; Web development
(use-package web-mode
  :mode ("\\.html\\'" "\\.css\\'" "\\.jsx\\'" "\\.tsx\\'")
  :custom
  (web-mode-markup-indent-offset 2)
  (web-mode-css-indent-offset 2)
  (web-mode-code-indent-offset 2))

;; Add keybindings with my-leader-keys
(with-eval-after-load 'general
  (my-leader-keys
    "p" '(:ignore t :which-key "project")
    "pp" '(projectile-switch-project :which-key "switch project")
    "pf" '(projectile-find-file :which-key "find file in project")
    "pb" '(projectile-switch-to-buffer :which-key "switch buffer in project")
    "pk" '(projectile-kill-buffers :which-key "kill project buffers")
    "pc" '(projectile-compile-project :which-key "compile project")
    "pa" '(projectile-add-known-project :which-key "add project") 
    "pt" '(treemacs :which-key "toggle treemacs")
    
    "l" '(:ignore t :which-key "lsp")
    "la" '(lsp-execute-code-action :which-key "code action")
    "lr" '(lsp-rename :which-key "rename")
    "ld" '(lsp-find-definition :which-key "find definition")
    "lD" '(lsp-find-declaration :which-key "find declaration")
    "lr" '(lsp-find-references :which-key "find references")
    "lf" '(lsp-format-buffer :which-key "format buffer")))

(provide 'development)
;;; development.el ends here
