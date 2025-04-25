;;; git.el --- Git integration -*- lexical-binding: t; -*-

;; Magit - Git client for Emacs
(use-package magit
  :commands magit-status
  :custom
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))

;; Git time machine to navigate through file history
(use-package git-timemachine
  :defer t)

;; Display git changes in the gutter
(use-package git-gutter
  :diminish git-gutter-mode
  :hook (prog-mode . git-gutter-mode)
  :config
  (setq git-gutter:update-interval 0.02))

;; Git blame information
(use-package blamer
  :defer t
  :custom
  (blamer-idle-time 0.3)
  (blamer-min-offset 70)
  :custom-face
  (blamer-face ((t :foreground "#7a88cf"
                    :background nil
                    :height 140
                    :italic t))))

;; Add keybindings with my-leader-keys
(with-eval-after-load 'general
  (my-leader-keys
    "g" '(:ignore t :which-key "git")
    "gs" '(magit-status :which-key "git status")
    "gb" '(magit-blame :which-key "git blame")
    "gl" '(magit-log-current :which-key "git log")
    "gc" '(magit-commit :which-key "git commit")
    "gp" '(magit-push :which-key "git push")
    "gP" '(magit-pull :which-key "git pull")
    "gd" '(magit-diff :which-key "git diff")
    "gt" '(git-timemachine :which-key "git time machine")
    "gB" '(blamer-mode :which-key "toggle git blame")
    "gS" '(magit-stage-file :which-key "git stage file")
    "gU" '(magit-unstage-file :which-key "git unstage file")))

(provide 'git)
;;; git.el ends here
