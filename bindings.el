;;; bindings.el --- Emacs bindings.

;;; Commentary:
;;; A lightweight Spacemacs-like configuration.

;;; Code:
(require 'core "~/.emacs.d/core.el")

(memacs/make-prefix-map 'memacs-prefix-map memacs-prefix-key evil-motion-state-map)

(memacs/bind-prefix-map 'memacs-prefix-map memacs-prefix-bindings)

;; Magit
(memacs/make-prefix-map 'memacs-magit-prefix-map
			memacs-magit-prefix-key
			'memacs-prefix-map "Magit")

(memacs/bind-prefix-map 'memacs-magit-prefix-map memacs-magit-prefix-bindings)

;; Buffer Management
(memacs/make-prefix-map 'memacs-buffer-prefix-map
			memacs-buffer-prefix-key
			'memacs-prefix-map "Buffers")

(memacs/bind-prefix-map 'memacs-buffer-prefix-map memacs-buffer-prefix-bindings)

;; For some reason, Buffer-menu-mode-map is listed in evil-overriding-maps.
;; This clobbers the SPC prefix, so we need to override it here.
(define-key Buffer-menu-mode-map (kbd "SPC") 'memacs-prefix-map)
 
;; Window Management
(memacs/make-prefix-map 'memacs-window-prefix-map
			memacs-window-prefix-key
			'memacs-prefix-map "Windows")

(memacs/bind-prefix-map 'memacs-window-prefix-map memacs-window-prefix-bindings)

;; File Management
(memacs/make-prefix-map 'memacs-file-prefix-map
			memacs-file-prefix-key
			'memacs-prefix-map "Files")

(memacs/bind-prefix-map 'memacs-file-prefix-map memacs-file-prefix-bindings)

;; Help
(memacs/make-prefix-map 'memacs-help-prefix-map
			memacs-help-prefix-key
			'memacs-prefix-map "Help")

(memacs/bind-prefix-map 'memacs-help-prefix-map memacs-help-prefix-bindings)

;; Terminal Management
(global-set-key (kbd "C-'") 'shell-pop)


;;; bindings.el ends here
