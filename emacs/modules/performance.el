;;; performance.el --- Performance optimizations -*- lexical-binding: t; -*-

;;; Commentary:
;; Optimizations for better performance and faster startup

;;; Code:

;;; Garbage Collection Strategy

;; Use higher gc threshold during startup
;; The init.el file already sets it for startup, this makes it permanent
(defvar my-gc-cons-threshold (* 64 1024 1024))
(add-hook 'emacs-startup-hook
          (lambda ()
            (setq gc-cons-threshold my-gc-cons-threshold
                  gc-cons-percentage 0.3)))

;; Adopt a smarter GC strategy for better responsiveness
(use-package gcmh
  :diminish gcmh-mode
  :init
  (setq gcmh-idle-delay 5
        gcmh-high-cons-threshold (* 32 1024 1024)  ;; 32MB
        gcmh-low-cons-threshold (* 16 1024 1024)   ;; 16MB
        gcmh-verbose nil)
  :config
  (gcmh-mode 1))

;; Temporarily disable GC during minibuffer use for better responsiveness
(defun my/minibuffer-setup-hook ()
  (setq gc-cons-threshold most-positive-fixnum))

(defun my/minibuffer-exit-hook ()
  (setq gc-cons-threshold my-gc-cons-threshold))

(add-hook 'minibuffer-setup-hook #'my/minibuffer-setup-hook)
(add-hook 'minibuffer-exit-hook #'my/minibuffer-exit-hook)

;;; Native Compilation Settings
(when (featurep 'native-compile)
  ;; Configure native compilation for better performance
  (setq native-comp-async-report-warnings-errors nil
        native-comp-deferred-compilation t
        native-comp-jit-compilation t
        native-comp-speed 2))  ;; Compilation level (0-3, higher = more optimization)

;; Configure a fixed place for native compilation cache
(when (fboundp 'startup-redirect-eln-cache)
  (startup-redirect-eln-cache
   (convert-standard-filename
    (expand-file-name "var/eln-cache/" user-emacs-directory))))

;;; IO & File Related Optimizations

;; Increase read chunks for faster loading of large files
(setq read-process-output-max (* 4 1024 1024)) ;; 4MB (from 1MB set in defaults.el)

;; Disable bidirectional text rendering for performance
(setq-default bidi-display-reordering nil)
(setq bidi-inhibit-bpa t)  ;; Potentially risky but major speed boost
(setq-default bidi-paragraph-direction 'left-to-right)

;; Disable expensive font operations for non-text modes
(setq inhibit-compacting-font-caches t)

;; Optimize process communications
(setq process-adaptive-read-buffering nil)  ;; Reduce lag for process interaction

;; Cache more recent files for faster access
(setq recentf-max-saved-items 200)
(setq recentf-exclude '("\\.git/" "\\.emacs.d/elpa/"))

;;; Editor Performance Optimizations

;; Reduce rendering work
(setq auto-window-vscroll nil)           ;; Don't adjust window-vscroll to view tall lines
(setq fast-but-imprecise-scrolling t)    ;; Faster scrolling over unfontified regions
(setq scroll-conservatively 101)         ;; More conservative scrolling
(setq scroll-margin 1)                   ;; Lowered from defaults.el for performance

;; Improve long-line performance
(setq-default bidi-display-reordering nil)
(setq redisplay-skip-fontification-on-input t)

;; Disable expensive minor modes when files are too large
(defun my/check-large-file ()
  "Check if file is large - if so, disable certain modes."
  (when (> (buffer-size) (* 1024 1024))
    (when (bound-and-true-p lsp-mode)
      (lsp-disconnect))
    (when (bound-and-true-p flycheck-mode)
      (flycheck-mode -1))
    (when (bound-and-true-p flymake-mode)
      (flymake-mode -1))
    (when (bound-and-true-p eldoc-mode)
      (eldoc-mode -1))
    (font-lock-mode -1)
    (setq-local line-number-mode nil)
    (setq-local global-hl-line-mode nil)
    (display-line-numbers-mode -1)
    (message "Large file detected. Disabled some features to improve performance.")))

(add-hook 'find-file-hook #'my/check-large-file)

;; Better byte compilation for packages
(setq byte-compile-warnings '(not free-vars unresolved noruntime lexical make-local))

;;; Lazy Loading Features

;; Defer loading of some features until they're needed
(with-eval-after-load 'hydra
  (setq hydra-is-helpful t))  ;; Only compute helpful hints when hydra is loaded

;; Don't load heavy lisp packages until used
(setq elisp-flymake-byte-compile-load-path load-path)

;;; LSP Optimizations
(with-eval-after-load 'lsp-mode
  ;; Optimize LSP for better performance
  (setq lsp-enable-file-watchers nil
        lsp-enable-folding nil
        lsp-enable-text-document-color nil
        lsp-headerline-breadcrumb-enable nil
        lsp-modeline-code-actions-enable nil
        lsp-modeline-diagnostics-enable nil
        lsp-keep-workspace-alive nil
        lsp-signature-auto-activate nil
        lsp-completion-provider :none   ;; Use Emacs' native completion
        lsp-idle-delay 0.2))

;; Improve company-mode performance
(with-eval-after-load 'company
  (setq company-minimum-prefix-length 3
        company-idle-delay 0.2
        company-tooltip-limit 10))

;;; Minibuffer Optimizations

;; Smarter resizing of minibuffer
(setq resize-mini-windows 'grow-only)

;; Optimize minibuffer loading
(setq enable-recursive-minibuffers t)
(minibuffer-depth-indicate-mode 1)

;;; Improve Emacs Frame Performance

;; Reduce frame operations that cause redisplay
(setq frame-resize-pixelwise nil)
(setq frame-inhibit-implied-resize t)

;; Keybindings for performance diagnostics
(with-eval-after-load 'general
  (my-leader-keys
   "P" '(:ignore t :which-key "performance")
   "Pp" '(profiler-start :which-key "start profiler")
   "Ps" '(profiler-stop :which-key "stop profiler")
   "Pr" '(profiler-report :which-key "profiler report")
   "Pg" '(garbage-collect :which-key "garbage collect")
   "Pd" '(memory-report :which-key "memory usage report")))

(defun memory-report ()
  "Report memory usage."
  (interactive)
  (message "Memory used: %d bytes" (memory-use-counts)))

(provide 'performance)
;;; performance.el ends here
