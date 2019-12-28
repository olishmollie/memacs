;;; bindings.el --- Emacs bindings.

;;; Commentary:
;;; A lightweight Spacemacs-like configuration.

;;; Code:
(require 'evil)
(require 'which-key)
(require 'magit)

;; MeMacs prefix
(define-prefix-command 'memacs-prefix-map)
(define-key evil-motion-state-map (kbd "SPC") 'memacs-prefix-map)

;; This enables which-key to process extra arguments of `define-key'.
(setq which-key-enable-extended-define-key t)

(defun register-evil-prefix (prefix-map keys bindings prefix-name)
  "Bind KEYS and BINDINGS to PREFIX-MAP and set its which-key name to PREFIX-NAME."
  (define-prefix-command prefix-map)
  (define-key evil-motion-state-map (kbd keys) prefix-map)
  (which-key-add-key-based-replacements keys prefix-name)
  (dolist (binding bindings)
    (define-key prefix-map (car binding) (cdr binding))))

;; Magit
(let ((bindings '(("s" . magit-status))))
  (register-evil-prefix 'magit-prefix-map "SPC g" bindings "Magit"))

;; Buffer Management
(let ((bindings '(("b" . list-buffers)
		  ("p" . previous-buffer)
		  ("n" . next-buffer)
		  ("d" . kill-this-buffer))))
  (register-evil-prefix 'buffer-prefix-map "SPC b" bindings "Buffers"))

;; For some reason, Buffer-menu-mode-map is listed in evil-overriding-maps.
;; This clobbers the SPC prefix, so we need to override it here.
(define-key Buffer-menu-mode-map (kbd "SPC") 'memacs-prefix-map)
 
;; Window Management
(let ((bindings '(("/" . ("vertical-split" . evil-window-vsplit))
		  ("-" . ("horizontal-split" . evil-window-split))
		  ("h" . ("window-left" . evil-window-left))
		  ("l" . ("window-right" . evil-window-right))
		  ("k" . ("window-up" . evil-window-up))
		  ("j" . ("window-down" . evil-window-down))
		  ("d" . ("window-delete" . evil-window-delete))
		  ("o" . delete-other-windows))))
  (register-evil-prefix 'window-prefix-map "SPC w" bindings "Windows"))

;; File Management
(let ((bindings '(("f" . find-file))))
  (register-evil-prefix 'file-prefix-map "SPC f" bindings "Files"))
  
;; Terminal Management
(global-set-key (kbd "C-'") 'shell-pop)

;;; bindings.el ends here
