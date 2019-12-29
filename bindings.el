;;; bindings.el --- Emacs bindings.

;;; Commentary:
;;; A lightweight Spacemacs-like configuration.

;;; Code:
(require 'core "~/.emacs.d/core.el")

(memacs/make-prefix-map 'memacs-prefix-map memacs-prefix-key
			evil-motion-state-map "MeMacs")

;; Magit
(memacs/make-prefix-map 'memacs-magit-prefix-map
			memacs-magit-prefix-key
			'memacs-prefix-map "Magit")

(memacs/bind-prefix-map 'memacs-magit-prefix-map '(("s" . magit-status)))

;; Buffer Management
(memacs/make-prefix-map 'memacs-buffer-prefix-map
			memacs-buffer-prefix-key
			'memacs-prefix-map "Buffers")

(memacs/bind-prefix-map 'memacs-buffer-prefix-map '(("b" . list-buffers)
						    ("p" . previous-buffer)
						    ("n" . next-buffer)
						    ("d" . kill-this-buffer)))

;; For some reason, Buffer-menu-mode-map is listed in evil-overriding-maps.
;; This clobbers the SPC prefix, so we need to override it here.
(define-key Buffer-menu-mode-map (kbd "SPC") 'memacs-prefix-map)
 
;; Window Management
(memacs/make-prefix-map 'memacs-window-prefix-map
			memacs-window-prefix-key
			'memacs-prefix-map "Windows")

(memacs/bind-prefix-map 'memacs-window-prefix-map
			'(("/" . ("vertical-split" . evil-window-vsplit))
			  ("-" . ("horizontal-split" . evil-window-split))
			  ("h" . ("window-left" . evil-window-left))
			  ("l" . ("window-right" . evil-window-right))
			  ("k" . ("window-up" . evil-window-up))
			  ("j" . ("window-down" . evil-window-down))
			  ("d" . ("window-delete" . evil-window-delete))
			  ("o" . delete-other-windows)))

;; File Management
(memacs/make-prefix-map 'memacs-file-prefix-map
			memacs-file-prefix-key
			'memacs-prefix-map "Files")

(memacs/bind-prefix-map 'memacs-file-prefix-map '(("f" . find-file)))

;; Help
(memacs/make-prefix-map 'memacs-help-prefix-map
			memacs-help-prefix-key
			'memacs-prefix-map "Help")

(memacs/bind-prefix-map 'memacs-help-prefix-map '(("v" . describe-variable)
						  ("f" . describe-function)
						  ("m" . describe-mode)
						  ("k" . describe-key)
						  ("?" . help-for-help)
						  ("." . display-local-help)
						  ("P" . describe-package)
						  ("a" . apropos-command)
						  ("s" . describe-syntax)))
  
;; Terminal Management
(global-set-key (kbd "C-'") 'shell-pop)


;;; bindings.el ends here
