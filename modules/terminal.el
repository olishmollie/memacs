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

;; TODO: This function requires starting a new shell in order to open at the
;; project root. I'd like to not have to do that.
(defun memacs/shell-pop()
  "Open a shell in the project root if it exists.
If `default-directory' is a remote file, ssh to the
remote server and cd to the correct directory."
  (interactive)
  (let ((default-directory (or (projectile-project-root) default-directory)))
    (if (file-remote-p default-directory)
        (progn
          (vterm)
          (memacs/ssh))
      (vterm))))

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
        shell-pop-autocd-to-working-dir nil
        shell-pop-shell-type '("vterm" "*vterm*" #'memacs/shell-pop))
  (add-hook 'shell-pop-in-hook (lambda () (evil-set-initial-state 'vterm-mode 'emacs)))
  (add-hook 'shell-pop-out-hook (lambda () (evil-set-initial-state 'vterm-mode 'insert)))
  (add-hook 'vterm-mode-hook #'memacs/add-vterm-keybindings))

;;; terminal ends here
