;;; terminal.el --- Terminal packages and configuration.

;;; Commentary:

;;; Code:

;; TODO: This is trash. Need to find a better way.
(defun memacs/ssh ()
  "Ssh into a remote shell.
NOTE: `default-directory' must be a remote file."
  (let ((user (file-remote-p default-directory 'user))
        (host (file-remote-p default-directory 'host))
        (path (file-remote-p default-directory 'localname)))
    (vterm-send-string (concat "ssh -t " user "@" host " \"cd " path "; \\$SHELL -l\"; \n \C-l"))))

(defun memacs/shell-pop ()
  "Open a shell, trying to respect default directory."
  (interactive)
  ;; (let ((default-directory (or (projectile-project-root) default-directory)))
  (if (file-remote-p default-directory)
      (progn
        (vterm)
        (memacs/ssh))
    (vterm)))

(defun memacs/shell-pop-in-project-root ()
  "Open a shell in the project root, if it exists."
  (interactive)
  (let ((default-directory (or (projectile-project-root) default-directory)))
    (shell-pop nil)))

(defun memacs/clear-vterm ()
  "Clears the vterm buffer."
  (interactive)
  (term-send-raw-string "\C-l")
  (vterm-clear-scrollback))

(defun memacs/add-vterm-keybindings ()
  "Add vterm keybindings."
  (when (eq system-type 'darwin)
    (general-define-key
    :keymaps 'vterm-mode-map
    :states 'emacs
    "s-k" #'memacs/clear-vterm)))

(use-package vterm)
(use-package shell-pop
  :after vterm
  :init
  (setq shell-pop-universal-key "M-'"
        shell-pop-full-span t
        shell-pop-shell-type '("vterm" "*vterm*" #'memacs/shell-pop))
  (add-hook 'shell-pop-in-hook (lambda () (evil-set-initial-state 'vterm-mode 'emacs)))
  (add-hook 'vterm-mode-hook #'memacs/add-vterm-keybindings)
  (memacs/project-prefix
    "t" '(memacs/shell-pop-in-project-root :which-key "Terminal")))

;;; terminal ends here
