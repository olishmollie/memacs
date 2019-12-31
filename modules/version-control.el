;;; version-control.el --- Version control package installation and configuration.

;;; Commentary:

;;; Code:
(use-package magit
  :defer t
  :init
  (setq magit-bury-buffer-function #'memacs/magit-kill-buffers))
(use-package evil-magit
  :after magit)
(use-package diff-hl
  :defer t
  :hook (magit-post-refresh . diff-hl-magit-post-refresh)
  :init (setq diff-hl-side 'right)
  :config
  (global-diff-hl-mode t)
  (diff-hl-margin-mode t))

(defun memacs/magit-kill-buffers(param)
  (magit-restore-window-configuration)
  (dolist (buffer (magit-mode-get-buffers))
    (kill-buffer buffer)))

;;; version-control ends here
