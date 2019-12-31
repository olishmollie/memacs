;;; mode-line.el --- Mode line packages and configuration.

;;; Commentary:

;;; Code:

(use-package spaceline
  :init (setq spaceline-highlight-face-func #'spaceline-highlight-face-evil-state)
  :config
  (spaceline-spacemacs-theme)
  (spaceline-toggle-evil-state-on)
  (spaceline-toggle-minor-modes-off)
  (spaceline-toggle-flycheck-error-on)
  (spaceline-toggle-flycheck-warning-on)
  (spaceline-toggle-flycheck-info-on)
  (spaceline-toggle-projectile-root-on)
  (setq powerline-height 30)
  )

;;; mode-line ends here
