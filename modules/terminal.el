;;; terminal.el --- Terminal packages and configuration.

;;; Commentary:


;;; Code:

(use-package vterm)
(use-package shell-pop
  :after vterm
  :init (setq shell-pop-universal-key "M-'"
              shell-pop-full-span t
	      shell-pop-shell-type '("vterm" "*vterm*" (lambda nil (vterm)))))

(evil-set-initial-state 'vterm-mode 'emacs)

;;; terminal ends here
