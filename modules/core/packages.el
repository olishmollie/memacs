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

;; Key Binding Management
(use-package general)

;; Auto Update Packages
(use-package auto-package-update
  :init (setq auto-package-update-delete-old-versions t
              auto-package-update-hide-results t)
  :config (auto-package-update-maybe))

;; Copy Shell Env to GUI Emacs
(use-package exec-path-from-shell
  :init (when (memq window-system '(mac ns x))
	  (exec-path-from-shell-initialize)))

;; Themes
(use-package doom-themes
  :config
  (load-theme 'doom-one t))

;; Vim Emulation
(use-package evil
  :init
  (setq evil-want-C-u-scroll t)
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

;; Project Management
(use-package projectile
  :init (setq projectile-project-search-path memacs-project-directory)
  :config
  (projectile-mode t))

;; Syntax Checking
(use-package flycheck
  :config (global-flycheck-mode))

;; Formatting
(use-package clang-format)

;; Language Server Protocol
(use-package lsp-mode
  :commands lsp
  :init (setq lsp-prefer-flymake nil
              lsp-enable-snippet nil))
;; (use-package lsp-ui
;;   :after lsp
;;   :init
;;   (setq lsp-ui-flycheck-enable t
;; 	lsp-ui-doc-position 'top)
;;   :config
;;   (set-face-background 'lsp-ui-doc-background
;; 		       (face-attribute 'default :background)))
(use-package company-lsp)
;; (use-package lsp-ivy)


;;; packages.el ends here
