;;; terminal.el --- Terminal packages and configuration.

;;; Commentary:

;;; Code:

(use-package vterm)
(use-package vterm-toggle)

(require 'evil)
(require 'vterm)

(defvar memacs-terminal 'vterm)
(defvar memacs-terminal-buffer-name "Terminal")

(setq vterm-toggle-fullscreen-p nil)

(add-to-list 'display-buffer-alist
             '((lambda (bufname _)
                 (with-current-buffer bufname
                   (equal major-mode 'vterm-mode)))
               (display-buffer-reuse-window display-buffer-at-bottom)
               (reusable-frames . visible)
               (window-height . 0.30)))

(defun memacs/vterm-clear ()
  "Clear vterm."
  (interactive)
  (vterm-clear)
  (vterm-clear-scrollback))

(defun memacs/add-terminal-keybindings ()
  "Add terminal keybindings."
  (when (eq system-type 'darwin)
    (define-key vterm-mode-map (kbd "s-k") #'memacs/vterm-clear)))

(defun memacs/init-terminal ()
  "Initialize vterm-mode."
  (memacs/add-terminal-keybindings)
  (evil-set-initial-state 'vterm-mode 'emacs))

(defun memacs/toggle-terminal ()
  "Show a terminal window if it exists, or create one."
  (interactive)
  (vterm-toggle))

(add-hook 'vterm-mode-hook #'memacs/init-terminal)
(global-set-key (kbd "M-'") #'memacs/toggle-terminal)

;;; terminal ends here
