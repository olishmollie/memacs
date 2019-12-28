;;; init.el --- My Emacs init script.

;;; Commentary:

;;; Code:
(load-file (concat user-emacs-directory "core.el"))

;; Package Configuration
(require 'package)
(setq package-enable-at-startup nil)
(setq package-archives '(("org"   . "http://orgmode.org/elpa/")
                         ("gnu"   . "http://elpa.gnu.org/packages/")
                         ("melpa" . "https://melpa.org/packages/")))
(package-initialize)

;; Bootstrap `use-package`
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(require 'use-package)

;; Ensure All Packages Are Installed
(require 'use-package-ensure)
(setq use-package-always-ensure t)

;; Copy Shell Env to GUI Emacs
(use-package exec-path-from-shell
  :init (when (memq window-system '(mac ns x))
	  (exec-path-from-shell-initialize)))

;; Monokai Theme
(use-package monokai-theme
  :init (load-theme 'monokai t))

;; Vim Emulator
(use-package evil
  :commands evil-set-initial-state
  :config
  (evil-set-initial-state 'vterm-mode 'emacs)
  :init (evil-mode t))

;; Popup Key Binding Hints
(use-package which-key
  :config
  (setq which-key-separator " ")
  (setq which-key-prefix-prefix "+")
  :init (which-key-mode))

;; Incremental Completion Framework
(use-package ivy
  :init (ivy-mode t))

;; Project Management
(use-package projectile
  :init (projectile-mode t))

;; Code Completion Framework
(use-package company
  :init (global-company-mode))

;; Syntax Checking
(use-package flycheck
  :init (global-flycheck-mode))

;; Language Server Protocol
(use-package lsp-mode
  :commands lsp
  :hook ((lsp-mode . lsp-ui-mode)
	 (c-mode . lsp)
	 (c++-mode . lsp)
	 (js-mode . lsp))
  :config (setq lsp-prefer-flymake nil))
(use-package lsp-ui
  :requires lsp)
(use-package company-lsp
  :requires lsp)
(use-package lsp-ivy
  :requires lsp)

;; Terminal Management
(use-package vterm
  :hook (vterm-mode . (lambda () (linum-mode -1))))
(use-package shell-pop
  :init
  (setq shell-pop-shell-type
	'("vterm" "*vterm*" (lambda nil (vterm))))
  (setq shell-pop-universal-key "C-'")
  (setq shell-pop-full-span t))

;; Version Control
(use-package magit
  :init (global-set-key (kbd "C-x g") 'magit-status))
(use-package git-gutter-fringe
  :config (setq git-gutter-fr:side 'right-fringe)
  :init (global-git-gutter-mode t))

;; Auto Update Packages
(use-package auto-package-update
  :config (setq auto-package-update-delete-old-versions t
		auto-package-update-hide-results t)
  :init (auto-package-update-maybe))

;;; init.el ends here
