;;; dashboard-config.el --- Ghost Emacs Dashboard -*- lexical-binding: t; -*-

;; Create a custom Ghost Emacs banner image
(defun dashboard-ghost-emacs-banner ()
  "Return the Ghost Emacs ASCII art banner."
  (let ((ghost-art "
⠀⠀⠀⠀⠀⢀⣴⣿⣿⣿⣦⠀
⠀⠀⠀⠀⣰⣿⡟⢻⣿⡟⢻⣧
⠀⠀⠀⣰⣿⣿⣇⣸⣿⣇⣸⣿
⠀⠀⣴⣿⣿⣿⣿⠟⢻⣿⣿⣿
⣠⣾⣿⣿⣿⣿⣿⣤⣼⣿⣿⠇
⢿⡿⢿⣿⣿⣿⣿⣿⣿⣿⡿⠀
⠀⠀⠈⠿⠿⠋⠙⢿⣿⡿⠁⠀
"))
    (propertize ghost-art 'face '(:foreground "#bd93f9" :height 0.9))))

;; Create a temporary file for the banner
(defun dashboard-create-ghost-banner ()
  "Create a temporary file with Ghost Emacs banner."
  (let* ((temporary-file-directory
          (expand-file-name "dashboard-banners/" user-emacs-directory))
         (banner-file (expand-file-name "ghost-emacs-banner.txt" temporary-file-directory)))
    ;; Ensure directory exists
    (unless (file-exists-p temporary-file-directory)
      (make-directory temporary-file-directory t))
    
    ;; Write banner to file
    (with-temp-file banner-file
      (insert (dashboard-ghost-emacs-banner)))
    
    ;; Return the file path
    banner-file))

;; Setup Dashboard
(use-package dashboard
  :config
  (dashboard-setup-startup-hook)
  
  ;; Dashboard configuration
  (setq dashboard-banner-logo-title "Ghost Emacs"
        dashboard-startup-banner (dashboard-create-ghost-banner)
        dashboard-image-banner-max-height 250
        dashboard-image-banner-max-width 250
        dashboard-center-content t
        dashboard-set-heading-icons nil
        dashboard-set-file-icons nil
        dashboard-set-navigator nil
        dashboard-set-init-info nil
        dashboard-set-footer t)

  ;; Clear dashboard items
  (setq dashboard-items '())

  ;; Custom footer with system information
  (setq dashboard-footer-messages 
        (list (format "Emacs v%s • %d packages • %.2fs startup" 
                      emacs-version 
                      (length package-activated-list)
                      (float-time (time-subtract after-init-time before-init-time)))))
  
  ;; Customize footer appearance
  (setq dashboard-footer-icon ""))

;; Hook dashboard refresh into startup
(add-hook 'after-init-hook 'dashboard-refresh-buffer)

;; Define keybinding with my-leader-keys
(with-eval-after-load 'general
  (my-leader-keys
    "d" '(:ignore t :which-key "dashboard")
    "dd" '(dashboard-refresh-buffer :which-key "open dashboard")))

(provide 'dashboard-config)
;;; dashboard-config.el ends here
