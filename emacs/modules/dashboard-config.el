;;; dashboard-config.el --- Elegant dashboard configuration -*- lexical-binding: t; -*-

;; Setup Dashboard
(use-package dashboard
  :config
  (dashboard-setup-startup-hook)
  
  ;; Dashboard content
  (setq dashboard-items '((recents  . 8)
                          (bookmarks . 5)
                          (projects . 5)))
  
  ;; Disable logo
  (setq dashboard-startup-banner nil)
  (setq dashboard-set-heading-icons t)
  (setq dashboard-set-file-icons t)
  
  ;; Center content
  (setq dashboard-center-content t)
  
  ;; Remove welcome title
  (setq dashboard-banner-logo-title "")
  (setq dashboard-init-info "")
  
  ;; Custom header with date on the right
  (defun dashboard-insert-custom-header ()
    "Insert custom header with date on the right."
    (let* ((date-string (format-time-string "%A, %B %d, %Y"))
           (date-width (length date-string))
           (window-width (window-width))
           (padding (- window-width date-width 2)))
      (insert (make-string 2 ?\n))
      (insert (make-string padding ?\s))
      (insert (propertize date-string 'face '(:inherit dashboard-banner-logo-title :height 1.1)))
      (insert (make-string 3 ?\n))))
  
  (advice-add #'dashboard-insert-banner :override #'dashboard-insert-custom-header)
  
  ;; Custom footer with centered system info
  (defun dashboard-insert-custom-footer ()
    "Insert custom footer with centered system information."
    (let* ((footer-text (concat 
                         (propertize (format "Emacs v%s" emacs-version) 'face 'font-lock-comment-face)
                         " • "
                         (propertize (format "%d packages loaded" (length package-activated-list)) 'face 'font-lock-comment-face)
                         " • "
                         (propertize (format "%.2fs startup time" (float-time (time-subtract after-init-time before-init-time))) 'face 'font-lock-comment-face)))
           (footer-width (length (substring-no-properties footer-text)))
           (window-width (window-width))
           (padding (max 0 (/ (- window-width footer-width) 2))))
      
      ;; Add space before footer
      (insert (make-string 3 ?\n))
      
      ;; Center the footer
      (insert (make-string padding ?\s))
      (insert footer-text)))
  
  (setq dashboard-footer-messages nil)
  (setq dashboard-footer-icon "")
  (advice-add #'dashboard-insert-footer :override #'dashboard-insert-custom-footer)
  
  ;; Make sure Projects and Bookmarks sections display properly
  (setq dashboard-set-navigator nil)
  (setq dashboard-set-init-info nil)
  (setq dashboard-projects-backend 'projectile)
  (setq dashboard-path-style 'truncate-beginning)
  (setq dashboard-path-max-length 60)
  (setq dashboard-show-shortcuts nil)
  
  ;; Make sure bookmarks are loaded
  (require 'bookmark)
  
  ;; Ensure recent files are loaded
  (recentf-mode 1)
  
  ;; Ensure projectile is initialized
  (with-eval-after-load 'projectile
    (setq projectile-known-projects-file 
          (expand-file-name "projectile-bookmarks.eld" user-emacs-directory))))

;; Define keybinding with my-leader-keys
(with-eval-after-load 'general
  (my-leader-keys
    "d" '(:ignore t :which-key "dashboard")
    "dd" '(dashboard-refresh-buffer :which-key "open dashboard")))

(provide 'dashboard-config)
;;; dashboard-config.el ends here
