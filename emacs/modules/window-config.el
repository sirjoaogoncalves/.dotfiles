;;; window-config.el --- Window and frame management -*- lexical-binding: t; -*-

;;; Commentary:
;; Enhanced window, tab, and frame management configuration

;;; Code:

;; Enable winner-mode for window configuration undo/redo
(use-package winner
  :ensure nil ; built-in package
  :init
  (winner-mode 1)
  :config
  ;; Increase history size
  (setq winner-ring-size 50))

;; Tab bar mode for workspace management
(use-package tab-bar
  :ensure nil ; built-in since Emacs 27
  :custom
  (tab-bar-show 1)                     ; Show tab bar
  (tab-bar-close-button-show nil)      ; Hide close button
  (tab-bar-new-button-show nil)        ; Hide new button
  (tab-bar-format '(tab-bar-format-tabs tab-bar-separator))
  (tab-bar-tab-hints t)                ; Show tab numbers
  (tab-bar-select-tab-modifiers '(meta))  ; M-<number> to select tab
  :config
  (tab-bar-mode 1)
  
  ;; Make tab bar look cleaner
  (custom-set-faces
   '(tab-bar ((t (:inherit mode-line))))
   '(tab-bar-tab ((t (:inherit mode-line :foreground "black" :background "light gray" :box (:line-width 1 :style released-button)))))
   '(tab-bar-tab-inactive ((t (:inherit mode-line-inactive))))))

;; Window management with ace-window
(use-package ace-window
  :bind (("M-o" . ace-window))
  :custom
  (aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))
  (aw-scope 'frame)
  :config
  (custom-set-faces
   '(aw-leading-char-face
     ((t (:inherit ace-jump-face-foreground :height 2.0))))))

;; Buffer expose (similar to MacOS Expose for windows)
(use-package buffer-expose
  :bind (("C-c e" . buffer-expose)
         ("C-c E" . buffer-expose-no-stars)
         ("C-c C-e" . buffer-expose-current-mode))
  :custom
  (buffer-expose-show-headers nil)
  (buffer-expose-auto-select-buffer 1.0))

;; Eyebrowse for window layouts
(use-package eyebrowse
  :custom
  (eyebrowse-keymap-prefix (kbd "C-c w"))
  (eyebrowse-new-workspace t)
  (eyebrowse-mode-line-separator " | ")
  (eyebrowse-mode-line-style 'smart)
  :config
  (eyebrowse-mode t))

;; Popper for managing popup windows
(use-package popper
  :bind (("C-`" . popper-toggle)
         ("M-`" . popper-cycle)
         ("C-M-`" . popper-toggle-type))
  :custom
  (popper-reference-buffers
   '("\\*Messages\\*"
     "\\*Warnings\\*"
     "\\*Compile-Log\\*"
     "\\*compilation\\*"
     "\\*Async Shell Command\\*"
     "Output\\*$"
     "\\*Completions\\*"
     "\\*Backtrace\\*"
     "\\*Calendar\\*"
     help-mode
     compilation-mode))
  (popper-display-control t)
  (popper-mode-line t)
  :init
  (popper-mode 1)
  (popper-echo-mode 1))

;; Functions for window manipulation

(defun toggle-window-split ()
  "Toggle between horizontal and vertical split with two windows."
  (interactive)
  (if (= (count-windows) 2)
      (let* ((this-win-buffer (window-buffer))
             (next-win-buffer (window-buffer (next-window)))
             (this-win-edges (window-edges (selected-window)))
             (next-win-edges (window-edges (next-window)))
             (this-win-2nd (not (and (<= (car this-win-edges)
                                         (car next-win-edges))
                                     (<= (cadr this-win-edges)
                                         (cadr next-win-edges)))))
             (splitter
              (if (= (car this-win-edges)
                     (car (window-edges (next-window))))
                  'split-window-horizontally
                'split-window-vertically)))
        (delete-other-windows)
        (let ((first-win (selected-window)))
          (funcall splitter)
          (if this-win-2nd (other-window 1))
          (set-window-buffer (selected-window) this-win-buffer)
          (set-window-buffer (next-window) next-win-buffer)
          (select-window first-win)
          (if this-win-2nd (other-window 1))))
    (message "Not exactly two windows")))

(defun rotate-windows ()
  "Rotate windows in a clockwise direction."
  (interactive)
  (if (<= (count-windows) 1)
      (message "Only one window")
    (let ((buffers (mapcar #'window-buffer (window-list))))
      (when buffers
        (let* ((first-buffer (car buffers))
               (rotated (append (cdr buffers) (list first-buffer))))
          (dotimes (i (length rotated))
            (set-window-buffer (nth i (window-list)) (nth i rotated)))
          (select-window (get-buffer-window (car buffers))))))))

(defun toggle-window-maximize ()
  "Toggle between maximizing the current window and restoring the previous configuration."
  (interactive)
  (if (and (= (count-windows) 1)
           (assq 'toggle-window-maximize-state (window-parameters)))
      (progn
        (set-window-parameter nil 'toggle-window-maximize-state nil)
        (winner-undo))
    (progn
      (set-window-parameter nil 'toggle-window-maximize-state t)
      (winner-save-unconditionally)
      (delete-other-windows))))

(defun split-window-below-and-focus ()
  "Split window horizontally and focus the new window."
  (interactive)
  (split-window-below)
  (other-window 1))

(defun split-window-right-and-focus ()
  "Split window vertically and focus the new window."
  (interactive)
  (split-window-right)
  (other-window 1))

;; Create hydra for window management
(use-package hydra
  :config
  (defhydra hydra-window (:color pink :hint nil)
    "
                                                                      ╭────────────┐
    ^_k_^                Move         Size          Other             │ Windows     │
  _h_   _l_             _a_ce-window  _[_: shrink   _u_ndo: _w_inapp  ╰────────────╯
    ^_j_^              _o_ther        _]_: enlarge  _r_edo: _W_inapp
  _0_ Kill^            _s_wap         _=_: balance  _t_oggle split
  _1_ Del Others       _b_uffer       ^ ^           _m_aximize toggle
  _2_ Split Horiz^     _f_ind file    ^ ^           _d_elete all
  _3_ Split Vert       _F_ind other   ^ ^           _p_opup toggle
  _q_uit
"
    ;; Navigation
    ("h" windmove-left)
    ("j" windmove-down)
    ("k" windmove-up)
    ("l" windmove-right)
    ("a" ace-window)
    ("o" other-window)
    
    ;; Resize
    ("[" shrink-window-horizontally)
    ("]" enlarge-window-horizontally)
    ("{" shrink-window)
    ("}" enlarge-window)
    ("=" balance-windows)
    
    ;; Split/window controls
    ("s" ace-swap-window)
    ("t" toggle-window-split)
    ("m" toggle-window-maximize)
    ("0" delete-window)
    ("1" delete-other-windows)
    ("2" split-window-below-and-focus)
    ("3" split-window-right-and-focus)
    ("d" kill-buffer-and-window)
    ("r" rotate-windows)
    
    ;; Window history
    ("u" winner-undo)
    ("r" winner-redo)
    ("w" winner-undo)
    ("W" winner-redo)
    
    ;; Files/buffers
    ("b" consult-buffer)
    ("f" find-file)
    ("F" find-file-other-window)
    
    ;; Popups
    ("p" popper-toggle)
    
    ;; exit
    ("q" nil "quit")))

;; Add keybindings with my-leader-keys
(with-eval-after-load 'general
  (my-leader-keys
   "w" '(:ignore t :which-key "window")
   "ww" '(hydra-window/body :which-key "window hydra")
   "wj" '(windmove-down :which-key "window down")
   "wk" '(windmove-up :which-key "window up")
   "wh" '(windmove-left :which-key "window left")
   "wl" '(windmove-right :which-key "window right")
   "ws" '(split-window-below-and-focus :which-key "split below")
   "wv" '(split-window-right-and-focus :which-key "split right")
   "wd" '(delete-window :which-key "delete window")
   "wD" '(kill-buffer-and-window :which-key "kill buffer and window")
   "wm" '(toggle-window-maximize :which-key "maximize toggle")
   "w=" '(balance-windows :which-key "balance windows")
   "wr" '(rotate-windows :which-key "rotate")
   "wu" '(winner-undo :which-key "undo window config")
   "wR" '(winner-redo :which-key "redo window config")
   "wt" '(toggle-window-split :which-key "toggle split direction")
   "wa" '(ace-window :which-key "ace window")
   
   "TAB" '(:ignore t :which-key "tabs")
   "TAB TAB" '(tab-bar-switch-to-tab :which-key "switch tab")
   "TAB n" '(tab-bar-new-tab :which-key "new tab")
   "TAB c" '(tab-bar-close-tab :which-key "close tab")
   "TAB r" '(tab-bar-rename-tab :which-key "rename tab")
   "TAB ]" '(tab-bar-switch-to-next-tab :which-key "next tab")
   "TAB [" '(tab-bar-switch-to-prev-tab :which-key "prev tab")
   "TAB d" '(tab-bar-close-other-tabs :which-key "close other tabs")
   "TAB b" '(tab-bar-switch-to-recent-tab :which-key "recent tab")
   
   "l" '(:ignore t :which-key "layouts")
   "lc" '(eyebrowse-create-window-config :which-key "create layout")
   "ln" '(eyebrowse-next-window-config :which-key "next layout")
   "lp" '(eyebrowse-prev-window-config :which-key "prev layout")
   "l0" '(eyebrowse-switch-to-window-config-0 :which-key "layout 0")
   "l1" '(eyebrowse-switch-to-window-config-1 :which-key "layout 1")
   "l2" '(eyebrowse-switch-to-window-config-2 :which-key "layout 2")
   "l3" '(eyebrowse-switch-to-window-config-3 :which-key "layout 3")
   "l4" '(eyebrowse-switch-to-window-config-4 :which-key "layout 4")
   "l5" '(eyebrowse-switch-to-window-config-5 :which-key "layout 5")
   "l6" '(eyebrowse-switch-to-window-config-6 :which-key "layout 6")
   "l7" '(eyebrowse-switch-to-window-config-7 :which-key "layout 7")
   "l8" '(eyebrowse-switch-to-window-config-8 :which-key "layout 8")
   "l9" '(eyebrowse-switch-to-window-config-9 :which-key "layout 9")
   "lr" '(eyebrowse-rename-window-config :which-key "rename layout")
   "ll" '(eyebrowse-last-window-config :which-key "last layout")
   "lx" '(eyebrowse-close-window-config :which-key "close layout")))

(provide 'window-config)
;;; window-config.el ends here
