;;; completion.el --- Completion frameworks -*- lexical-binding: t; -*-

;; Vertico for vertical completion UI
(use-package vertico
  :init
  (vertico-mode)
  :custom
  (vertico-cycle t))

;; Marginalia for annotations in the minibuffer
(use-package marginalia
  :init
  (marginalia-mode))

;; Consult for enhanced search and navigation
(use-package consult
  :bind
  (("C-s" . consult-line)           ;; Search current buffer
   ("C-x b" . consult-buffer)       ;; Switch buffer
   ("C-c f" . consult-find)         ;; Find file
   ("C-c g" . consult-grep)))       ;; Grep

;; Orderless for flexible matching
(use-package orderless
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion)))))

;; Corfu for completion at point
(use-package corfu
  :custom
  (corfu-cycle t)                ;; Enable cycling through candidates
  (corfu-auto t)                 ;; Enable auto completion
  (corfu-auto-prefix 2)          ;; Complete with 2 letters
  (corfu-auto-delay 0.0)         ;; No delay for auto completion
  (corfu-separator ?\s)          ;; Use space as separator
  (corfu-quit-at-boundary 'separator) ;; Don't quit at completion boundary
  :init
  (global-corfu-mode))

;; Cape for completion at point extensions
(use-package cape
  :init
  (add-to-list 'completion-at-point-functions #'cape-file)
  (add-to-list 'completion-at-point-functions #'cape-dabbrev))

;; Company mode for older packages
(use-package company
  :hook (prog-mode . company-mode)
  :config
  (setq company-idle-delay 0)
  (setq company-minimum-prefix-length 1)
  :diminish)

;; Use my-leader-keys to add completion-related keybindings
(with-eval-after-load 'general
  (my-leader-keys
    "c" '(:ignore t :which-key "completion")
    "cc" '(completion-at-point :which-key "complete at point")
    "ci" '(consult-imenu :which-key "imenu")
    "cl" '(consult-line :which-key "search lines")))

(provide 'completion)
;;; completion.el ends here
