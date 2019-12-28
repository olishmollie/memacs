;;; core.el --- Basic emacs configurations.

;;; Commentary:

;;; Code:

;; Appearance
(setq inhibit-startup-screen t)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(menu-bar-mode -1)
(add-to-list 'default-frame-alist '(fullscreen . maximized))
(add-to-list 'default-frame-alist '(font . "Source Code Pro-14"))

;; Backups
(setq backup-by-copying t)
(defvar --backup-directory (concat user-emacs-directory "/backups"))
(if (not (file-exists-p --backup-directory))
        (make-directory --backup-directory t))
(add-to-list 'backup-directory-alist `("." . ,--backup-directory))

;; Custom
(defvar --custom-file (concat user-emacs-directory "custom.el"))
(setq custom-file --custom-file)

;; Global Modes
(global-auto-revert-mode t)
(global-linum-mode t)

;; Turn Off Bell
(setq ring-bell-function 'ignore)

;;; core ends here
