;;; version-control.el --- Version control package installation and configuration.

;;; Commentary:

;;; Code:
(use-package magit
  :defer t
  :init (setq magit-bury-buffer-function #'memacs-magit-kill-buffers))
(use-package diff-hl
  :init (setq diff-hl-side 'right)
  :config
  (global-diff-hl-mode t)
  (diff-hl-margin-mode t))

(defun memacs-magit-kill-buffers (param)
  (magit-restore-window-configuration)
  (dolist (buffer (magit-mode-get-buffers))
    (kill-buffer buffer)))

(which-key-declare-prefixes
  "SPC g" "Version Control")

(general-create-definer memacs-version-control-prefix
  :states '(normal insert emacs)
  :prefix "SPC g"
  :non-normal-prefix "M-SPC g")

(memacs-version-control-prefix
  "d" '(magit-dispatch :which-key "Dispatch")
  "g" '(diff-hl-mode :which-key "Gutter")
  "s" '(magit-status :which-key "Status"))

(add-hook 'magit-post-refresh-hook #'diff-hl-magit-post-refresh)

(memacs-veemacs-state magit-mode
  (kbd "SPC")     #'memacs-global-prefix-map
  (kbd "gg")      #'beginning-of-buffer
  (kbd "G")       #'end-of-buffer
  (kbd "n")       #'magit-section-forward
  (kbd "}")       #'evil-forward-paragraph
  (kbd "{")       #'evil-backward-paragraph
  (kbd "C-k")     #'magit-discard)

;;; version-control ends here
