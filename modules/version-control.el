;;; version-control.el --- Version control package installation and configuration.

;;; Commentary:

;;; Code:
(use-package magit
  :defer t
  :init (setq magit-bury-buffer-function #'memacs/magit-kill-buffers))
(use-package evil-magit
  :after magit)
(use-package diff-hl
  :init (setq diff-hl-side 'right)
  :config
  (global-diff-hl-mode t)
  (diff-hl-margin-mode t))

(defun memacs/magit-kill-buffers (param)
  (magit-restore-window-configuration)
  (dolist (buffer (magit-mode-get-buffers))
    (kill-buffer buffer)))

;; Keybindings
(defvar memacs-magit-prefix-key "g"
  "Magit prefix-key.  Defaults to 'g'.")
(defvar memacs-magit-prefix-bindings '(("s" . ("status" . magit-status)))
  "Default magit prefix-bindings.")
(memacs/make-prefix-map 'memacs-magit-prefix-map 'memacs-prefix-map "Magit")
(memacs/bind-prefix-map 'memacs-magit-prefix-map memacs-magit-prefix-bindings)

(add-hook 'magit-post-refresh-hook #'diff-hl-magit-post-refresh)

;;; version-control ends here
