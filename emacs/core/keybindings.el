;;; keybindings.el --- Global keybindings -*- lexical-binding: t; -*-

;; Use General for better keybinding definitions
(require 'general)

;; Create leader key (similar to Doom's SPC)
(general-create-definer my-leader-keys
  :keymaps '(normal insert visual emacs) ; Apply leader in these common evil states + emacs state
  :prefix "SPC"
  :global-prefix "C-SPC") ; Use C-SPC as the global prefix if needed elsewhere

;; Define some basic keybindings
(my-leader-keys
 ;; File operations
 "f" '(:ignore t :which-key "file") ; Prefix key definition - CORRECT
 "ff" #'find-file :which-key "find file" ; CORRECTED SYNTAX: command symbol, then plist
 "fs" #'save-buffer :which-key "save file" ; CORRECTED SYNTAX
 "fR" #'rename-file-and-buffer :which-key "rename file" ; CORRECTED SYNTAX
 "fd" #'dired :which-key "dired" ; CORRECTED SYNTAX

 ;; File and folder operations
 "fn" '(:ignore t :which-key "new") ; CORRECT
 "fnf" #'create-file-in-current-dir :which-key "new file" ; CORRECTED SYNTAX
 "fnd" #'create-directory-in-current-dir :which-key "new directory" ; CORRECTED SYNTAX

 "fD" '(:ignore t :which-key "delete") ; CORRECT
 "fDf" #'delete-current-file :which-key "delete file" ; CORRECTED SYNTAX
 "fDd" #'delete-directory-prompt :which-key "delete directory" ; CORRECTED SYNTAX

 "fc" '(:ignore t :which-key "copy") ; CORRECT
 "fcf" #'copy-file-current :which-key "copy file" ; CORRECTED SYNTAX
 "fcd" #'copy-directory-prompt :which-key "copy directory" ; CORRECTED SYNTAX

 ;; Buffer operations
 "b" '(:ignore t :which-key "buffer") ; CORRECT
 "bb" #'switch-to-buffer :which-key "switch buffer" ; CORRECTED SYNTAX
 "bd" #'kill-current-buffer :which-key "kill buffer" ; CORRECTED SYNTAX
 "bs" #'save-buffer :which-key "save buffer" ; CORRECTED SYNTAX (same as fs)
 "bR" #'rename-buffer :which-key "rename buffer" ; CORRECTED SYNTAX
 "br" #'revert-buffer :which-key "revert buffer" ; CORRECTED SYNTAX

 ;; Window operations
 "w" '(:ignore t :which-key "window") ; CORRECT
 "wv" #'split-window-right :which-key "split vertical" ; CORRECTED SYNTAX
 "ws" #'split-window-below :which-key "split horizontal" ; CORRECTED SYNTAX
 "wh" #'windmove-left :which-key "window left" ; CORRECTED SYNTAX
 "wj" #'windmove-down :which-key "window down" ; CORRECTED SYNTAX
 "wk" #'windmove-up :which-key "window up" ; CORRECTED SYNTAX
 "wl" #'windmove-right :which-key "window right" ; CORRECTED SYNTAX
 "wq" #'delete-window :which-key "close window" ; CORRECTED SYNTAX
 "wo" #'delete-other-windows :which-key "close other windows" ; CORRECTED SYNTAX

 ;; Help/documentation
 "h" '(:ignore t :which-key "help") ; CORRECT
 "hf" #'describe-function :which-key "describe function" ; CORRECTED SYNTAX
 "hv" #'describe-variable :which-key "describe variable" ; CORRECTED SYNTAX
 "hk" #'describe-key :which-key "describe key" ; CORRECTED SYNTAX
 "hm" #'describe-mode :which-key "describe mode" ; CORRECTED SYNTAX

 ;; Search
 "s" '(:ignore t :which-key "search") ; CORRECT
 "ss" #'isearch-forward :which-key "search forward" ; CORRECTED SYNTAX
 "sr" #'isearch-backward :which-key "search backward" ; CORRECTED SYNTAX

 ;; Toggle operations
 "t" '(:ignore t :which-key "toggle") ; CORRECT
 "tt" #'load-theme :which-key "choose theme" ; CORRECTED SYNTAX
 "tl" #'display-line-numbers-mode :which-key "line numbers" ; CORRECTED SYNTAX
 "tm" #'menu-bar-mode :which-key "menu bar" ; CORRECTED SYNTAX

 ;; Open configuration
 "o" '(:ignore t :which-key "open") ; CORRECT
 ;; Lambda definition IS correct because the whole lambda is the command
 "oc" #'(lambda () (interactive) (find-file "~/.emacs.d/init.el")) :which-key "open init.el"
 "ot" '(open-terminal-here :which-key "open terminal")
 ;; Closing parenthesis for my-leader-keys definition block
 ) ;; This should now be read correctly

;; --- Rest of the functions remain the same as the previous corrected version ---

;; Function to rename file and buffer
(defun rename-file-and-buffer ()
  "Rename the current buffer and the file it's visiting."
  (interactive)
  (let ((filename (buffer-file-name)))
    (if (not filename)
        (message "Buffer is not visiting a file!")
      (let ((new-name (read-file-name "New name: " filename)))
        ;; Check if the new name is different before proceeding
        (when (and new-name (not (string-equal filename new-name)))
          (cond
           ((vc-backend filename) (vc-rename-file filename new-name))
           (t
            (rename-file filename new-name t) ; t means ok-if-already-exists is true (usually preferred)
            (set-visited-file-name new-name t t)))))))) ; t t means dont change buffer modification status and dont redisplay

;; Function to create a new file in the current directory
(defun create-file-in-current-dir ()
  "Create a new file in the current directory."
  (interactive)
  (let* ((base-dir (or
                    ;; Use the directory of the current buffer if it has one
                    (when buffer-file-name
                      (file-name-directory buffer-file-name))
                    ;; Or use the current directory of dired
                    (when (derived-mode-p 'dired-mode) ; Use derived-mode-p for robustness
                      default-directory)
                    ;; Fall back to default-directory
                    default-directory))
         ;; Ensure base-dir ends with a slash for read-string prompt clarity
         (prompt-dir (if (string-suffix-p "/" base-dir) base-dir (concat base-dir "/")))
         (filename (read-string (format "Create file in %s: " prompt-dir) prompt-dir)))
    ;; Basic validation: ensure filename is not empty or just the directory path
    (when (and filename (not (string-equal filename prompt-dir)))
      (find-file filename))))

;; Function to create a new directory
(defun create-directory-in-current-dir ()
  "Create a new directory in the current directory."
  (interactive)
  (let* ((base-dir (or
                    (when buffer-file-name
                      (file-name-directory buffer-file-name))
                    (when (derived-mode-p 'dired-mode)
                      default-directory)
                    default-directory))
         (prompt-dir (if (string-suffix-p "/" base-dir) base-dir (concat base-dir "/")))
         (dir-name (read-directory-name "Create directory: " prompt-dir)))
    (when (and dir-name (not (string-equal dir-name prompt-dir)))
      (make-directory dir-name t) ; t means create parent directories if needed
      (message "Directory created: %s" dir-name)
      (when (derived-mode-p 'dired-mode) ; Check if we are in dired
        (revert-buffer))))) ; Refresh dired view

;; Function to delete the current file
(defun delete-current-file ()
  "Delete the current file and kill its buffer."
  (interactive)
  (let ((filename (buffer-file-name))
        (buffer (current-buffer))) ; Store current buffer
    (if (not filename)
        (message "Buffer is not visiting a file!")
      (when (yes-or-no-p (format "Really delete %s? " filename))
        (delete-file filename t) ; t means trash if possible
        (message "File %s deleted." filename)
        ;; Kill the buffer *after* confirming deletion
        (kill-buffer buffer)))))

;; Function to delete a directory
(defun delete-directory-prompt ()
  "Prompt for a directory to delete recursively."
  (interactive)
  (let* ((base-dir (or
                    (when buffer-file-name
                      (file-name-directory buffer-file-name))
                    (when (derived-mode-p 'dired-mode)
                      default-directory)
                    default-directory))
         (prompt-dir (if (string-suffix-p "/" base-dir) base-dir (concat base-dir "/")))
         (dir-name (read-directory-name "Delete directory (recursively): " prompt-dir)))
    (when (and dir-name (file-directory-p dir-name) (not (string-equal dir-name prompt-dir)))
      (when (yes-or-no-p (format "Really delete directory %s and all its contents? " dir-name))
        (delete-directory dir-name t) ; t means recursive
        (message "Directory deleted: %s" dir-name)
        (when (derived-mode-p 'dired-mode)
          (revert-buffer))))))

;; Function to copy the current file
(defun copy-file-current ()
  "Copy the current file to a new location and visit the copy."
  (interactive)
  (let ((filename (buffer-file-name)))
    (if (not filename)
        (message "Buffer is not visiting a file!")
      (let* ((new-name (read-file-name (format "Copy %s to: " filename) (file-name-directory filename))))
        (when (and new-name (not (string-equal filename new-name)))
          (copy-file filename new-name t t t) ; ok-if-exists=t, keep-time=t, preserve-permissions=t
          (message "File copied to %s" new-name)
          (find-file new-name))))))

;; Function to copy a directory
(defun copy-directory-prompt ()
  "Prompt for a source and target directory to copy."
  (interactive)
  (let* ((base-dir (or
                    (when buffer-file-name
                      (file-name-directory buffer-file-name))
                    (when (derived-mode-p 'dired-mode)
                      default-directory)
                    default-directory))
         (prompt-dir (if (string-suffix-p "/" base-dir) base-dir (concat base-dir "/")))
         (source-dir (read-directory-name "Copy directory: " prompt-dir))
         (target-dir (read-directory-name (format "Copy %s to: " source-dir) prompt-dir)))
    (when (and source-dir (file-directory-p source-dir)
               target-dir (not (string-equal source-dir target-dir)))
      ;; copy-directory doesn't have a confirmation, maybe add one if desired
      (copy-directory source-dir target-dir nil t t) ; request confirmation = nil, keep time = t, copy contents = t
      (message "Directory %s copied to %s" source-dir target-dir)
      (when (derived-mode-p 'dired-mode)
        (revert-buffer)))))


(provide 'keybindings)
;;; keybindings.el ends here
