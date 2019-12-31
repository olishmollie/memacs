;;; packages.el --- Global package installation and configuration.

;;; Commentary:

;;; Code:
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

;; Auto Update Packages
(use-package auto-package-update
  :init (setq auto-package-update-delete-old-versions t
		auto-package-update-hide-results t)
  :config (auto-package-update-maybe))

;; Copy Shell Env to GUI Emacs
(use-package exec-path-from-shell
  :init (when (memq window-system '(mac ns x))
	  (exec-path-from-shell-initialize)))

;; Monokai Theme
(use-package monokai-theme
  :config (load-theme 'monokai t))

;; Vim Emulation
(use-package evil
  :init (setq evil-want-C-u-scroll t)
  :config (evil-mode t))
(use-package evil-commentary
  :after evil
  :config (evil-commentary-mode t))
(use-package evil-surround
  :after evil
  :config (global-evil-surround-mode t))

;; Popup Key Binding Hints
(use-package which-key
  :defer t
  :config (which-key-mode))

;; Incremental Completion Framework
(use-package counsel
  :config (ivy-mode t))

;; Project Management
(use-package projectile
  :defer t
  :config (projectile-mode t))

;; Code Completion Framework
(use-package company
  :defer t
  :config (global-company-mode))

;; Syntax Checking
(use-package flycheck
  :defer t
  :config (global-flycheck-mode))

;; Language Server Protocol
(use-package lsp-mode
  :commands lsp
  :hook ((c-mode . lsp)
	 (c++-mode . lsp)
	 (js-mode . lsp)
	 (lsp-mode . lsp-ui-mode))
  :init (setq lsp-prefer-flymake nil))
(use-package lsp-ui
  :after lsp
  :init
  (setq lsp-ui-flycheck-enable t
	lsp-ui-doc-position 'top)
  :config
  (set-face-background 'lsp-ui-doc-background
		       (face-attribute 'default :background)))
(use-package company-lsp)
(use-package lsp-ivy)

;; Terminal Management
(use-package vterm
  :hook (vterm-mode . (lambda () (linum-mode -1))))
(use-package shell-pop
  :after vterm
  :init (setq shell-pop-full-span t
	      shell-pop-shell-type '("vterm" "*vterm*" (lambda nil (vterm)))))

;; Version Control
(use-package magit
  :defer t)
(use-package evil-magit
  :after magit)
(use-package diff-hl
  :defer t
  :hook (magit-post-refresh . diff-hl-magit-post-refresh)
  :init (setq diff-hl-side 'right)
  :config
  (global-diff-hl-mode t)
  (diff-hl-margin-mode t))

;;; packages.el ends here
