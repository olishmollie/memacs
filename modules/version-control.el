;;; version-control.el --- Version control package installation and configuration.

;;; Commentary:

;;; Code:
(use-package magit
  :defer t
  :init (setq magit-bury-buffer-function #'memacs/magit-kill-buffers))
(use-package diff-hl
  :init (setq diff-hl-side 'right)
  :config
  (global-diff-hl-mode t)
  (diff-hl-margin-mode t))

(defun memacs/magit-kill-buffers (param)
  (magit-restore-window-configuration)
  (dolist (buffer (magit-mode-get-buffers))
    (kill-buffer buffer)))

(which-key-declare-prefixes
  "SPC g" "Magit")

(general-create-definer memacs/magit-prefix
  :states '(normal insert emacs)
  :prefix "SPC g"
  :non-normal-prefix "M-SPC g")

(memacs/magit-prefix
  "s" '(magit-status :which-key "Status"))

(add-hook 'magit-post-refresh-hook #'diff-hl-magit-post-refresh)

(evil-set-initial-state 'magit-mode 'emacs)
(evil-add-hjkl-bindings magit-mode-map 'emacs
  (kbd "SPC")     #'memacs-global-prefix-map
  (kbd "w")       #'evil-forward-word-begin
  (kbd "b")       #'evil-backward-word-begin
  (kbd "gg")      #'beginning-of-buffer
  (kbd "G")       #'end-of-buffer
  (kbd "C-d")     #'evil-scroll-down
  (kbd "C-u")     #'evil-scroll-up
  (kbd "}")       #'evil-forward-paragraph
  (kbd "{")       #'evil-backward-paragraph)
;;; version-control ends here
