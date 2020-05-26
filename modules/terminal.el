;;; terminal.el --- Terminal packages and configuration.

;;; Commentary:

;;; Code:

(use-package vterm)
(use-package vterm-toggle)

(require 'advice)
(require 'evil)
(require 'vterm)

(setq vterm-always-compile-module t)
(setq vterm-toggle-fullscreen-p nil)

(add-to-list 'display-buffer-alist
             '((lambda (bufname _)
                 (with-current-buffer bufname
                   (equal major-mode 'vterm-mode)))
               (display-buffer-reuse-window display-buffer-at-bottom)
               (reusable-frames . visible)
                (window-height . 0.35)))

(defun memacs-vterm-clear ()
  "Clear vterm."
  (interactive)
  (vterm-clear)
  (vterm-clear-scrollback))

(defun memacs-add-terminal-keybindings ()
  "Add terminal keybindings."
  (when (eq system-type 'darwin)
    (define-key vterm-mode-map (kbd "s-k") #'memacs-vterm-clear)))

(defun memacs-init-terminal ()
  "Initialize vterm-mode."
  (memacs-add-terminal-keybindings)
  (evil-set-initial-state 'vterm-mode 'emacs))

(defun memacs-toggle-terminal ()
  "Show a terminal window if it exists, or create one."
  (interactive)
  (vterm-toggle))

(defun memacs-find-file-from-vterm (path)
  "Open PATH from a vterm window.  It is not meant to be called explicitly."
  (interactive)
  (if-let* ((buf (find-file-noselect path))
            (window (display-buffer
                     buf
                     `((display-buffer-reuse-window display-buffer-use-some-window)))))
      (progn
        (select-window window)
        (delete-other-windows))
    (message "Failed to open file: %s" path)))

;; Allow emacs to execute a 'find-file-from-vterm' message.
(add-to-list 'vterm-eval-cmds (list "find-file-from-vterm" #'memacs-find-file-from-vterm))

(add-hook 'vterm-mode-hook #'memacs-init-terminal)
(global-set-key (kbd "M-'") #'memacs-toggle-terminal)

;;; terminal ends here
