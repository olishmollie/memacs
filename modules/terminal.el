;;; terminal.el --- Terminal packages and configuration.

;;; Commentary:


;;; Code:

;; TODO: This function requires starting a new shell in order to open at the
;; project root. I'd like to not have to do that.
(defun memacs/shell-pop()
  "Open a shell in the project root if it exists."
  (interactive)
  (let ((default-directory (or (projectile-project-root) default-directory)))
    (vterm)))

(use-package vterm)
(use-package shell-pop
  :after vterm
  :init (setq shell-pop-universal-key "M-'"
              shell-pop-full-span t
              shell-pop-autocd-to-working-dir nil
	      shell-pop-shell-type '("vterm" "*vterm*" #'memacs/shell-pop)))

(evil-set-initial-state 'vterm-mode 'emacs)

;;; terminal ends here
