;;; basic.el --- Basic configuration.

;;; Commentary:

;;; Code:

(when (eq 'darwin system-type)
  (require 'server)
  (load "server")
  (unless (server-running-p)
    (server-start)))

(defvar memacs-modules-directory (concat user-emacs-directory "modules/"))
(defvar memacs-backup-directory (concat user-emacs-directory "backups"))
(defvar memacs-custom-file (concat memacs-modules-directory "custom.el"))

(defvar memacs-modules nil)

;; Appearance/Basic Behavior
(setq-default indent-tabs-mode nil)
(setq inhibit-startup-screen t)
(setq ring-bell-function 'ignore)
(setq backup-by-copying t)
(setq confirm-kill-processes nil)
(setq custom-file memacs-custom-file)
(setq kill-buffer-query-functions
      (remq 'process-kill-buffer-query-function
            kill-buffer-query-functions))

(tool-bar-mode -1)
(scroll-bar-mode -1)
(add-to-list 'default-frame-alist '(fullscreen . maximized))
(show-paren-mode t)
(column-number-mode t)
(global-auto-revert-mode t)
(add-hook 'prog-mode-hook #'hl-line-mode)
(add-hook 'prog-mode-hook #'linum-mode)

;; ido
(require 'ido)
(setq ido-auto-merge-work-directories-length -1)
(setq ido-enable-flex-matching t)
(setq ido-everywhere t)
(ido-mode 1)

;; GDB
(require 'gdb-mi)
(setq gdb-many-windows t)
(setq gdb-show-main t)

;; Backups
(if (not (file-exists-p memacs-backup-directory))
    (make-directory memacs-backup-directory t))
(add-to-list 'backup-directory-alist `("." . ,memacs-backup-directory))

;; Delete trailing whitespace
(add-hook 'before-save-hook #'delete-trailing-whitespace)

;; Use tabs in shell-script mode
(add-hook 'sh-mode-hook
          (lambda ()
            (setq tab-width 4)
            (setq indent-tabs-mode t)))

;; Kill completion buffer after closing the minibuffer.
(add-hook 'minibuffer-exit-hook
          '(lambda ()
             (let ((buffer "*Completions*"))
               (and (get-buffer buffer)
                    (kill-buffer buffer)))))

;; TODO -- figure out how this and other adising works.
(defadvice quit-window (before quit-window-always-kill)
  "When running `quit-window', e.g. pressing 'q' in help buffers, always kill the buffer."
  (when (called-interactively-p 'interactive) (ad-set-arg 0 t)))
(ad-activate 'quit-window)

(provide 'core)

;;; basic.el ends here
