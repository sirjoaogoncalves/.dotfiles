;;; terminal.el --- Terminal support -*- lexical-binding: t; -*-

;; Terminal integration with vterm
(use-package vterm
  :commands (vterm)
  :config
  ;; Use system's default shell
  (setq vterm-shell (getenv "SHELL"))
  ;; Set directory tracking
  (setq vterm-kill-buffer-on-exit t)
  ;; Set maximum scrollback
  (setq vterm-max-scrollback 10000))

;; Function to open terminal in current directory
(defun open-terminal-here ()
  "Open a terminal in the current directory using system's default shell."
  (interactive)
  (let ((default-directory (or 
                            ;; Use the directory of the current buffer if it has one
                            (when buffer-file-name
                              (file-name-directory buffer-file-name))
                            ;; Or use the current directory of dired
                            (when (eq major-mode 'dired-mode)
                              default-directory)
                            ;; Fall back to default-directory
                            default-directory)))
    (vterm)))

(provide 'terminal)
;;; terminal.el ends here
