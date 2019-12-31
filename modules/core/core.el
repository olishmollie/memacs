;;; core.el --- MeMacs functions.

;;; Commentary:

;;; Code:
(defvar memacs-modules-directory (concat user-emacs-directory "modules/"))
(defvar memacs-backup-directory (concat user-emacs-directory "backups"))
(defvar memacs-custom-file (concat memacs-modules-directory "custom.el"))

(defvar memacs-modules nil)

(setq inhibit-startup-screen t)
(setq ring-bell-function 'ignore)
(setq backup-by-copying t)
(setq confirm-kill-processes nil)
(setq custom-file memacs-custom-file)

;; Appearance/Basic Behavior
(tool-bar-mode -1)
(scroll-bar-mode -1)
(menu-bar-mode -1)
(add-to-list 'default-frame-alist '(fullscreen . maximized))
(add-to-list 'default-frame-alist '(font . "Source Code Pro-14"))
(add-to-list 'default-frame-alist '(cursor-color . "#F8F8F2"))
(global-linum-mode t)
(global-hl-line-mode)
(electric-pair-mode t)
(show-paren-mode t)
(global-auto-revert-mode t)

;; Backups
(if (not (file-exists-p memacs-backup-directory))
    (make-directory memacs-backup-directory t))
(add-to-list 'backup-directory-alist `("." . ,memacs-backup-directory))

;; Delete trailing whitespace
(add-hook 'before-save-hook 'delete-trailing-whitespace)

(defun memacs/load-directory (dir)
  "Load all '.el' files in DIR."
  (let ((load-it (lambda (f)
		   (load-file (concat (file-name-as-directory dir) f)))
		 ))
    (mapc load-it (directory-files dir nil "\\.el$"))))

(defun memacs/load-module (module)
  "Load MODULE into MeMacs."
  (load-file (concat memacs-modules-directory (symbol-name module) ".el")))

(defun memacs/load-core-modules ()
  "Load MeMacs core modules.  Order is important."
  (load-file (concat memacs-modules-directory "core/packages.el"))
  (load-file (concat memacs-modules-directory "core/bindings.el")))

(memacs/load-core-modules)

(provide 'core)

;;; core ends here