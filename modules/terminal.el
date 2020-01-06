;;; terminal.el --- Terminal packages and configuration.

;;; Commentary:


;;; Code:

(use-package vterm)
(use-package shell-pop
  :after vterm
  :init (setq shell-pop-full-span t
	      shell-pop-shell-type '("vterm" "*vterm*" (lambda nil (vterm)))))

(memacs/bind-prefix-map 'memacs-prefix-map
			'(("t" . ("terminal" . shell-pop))))

;;; terminal ends here
