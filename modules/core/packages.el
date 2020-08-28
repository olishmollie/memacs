;;; packages.el --- Global package installation and configuration.

;;; Commentary:

;;; Code:

(require 'package)
(setq package-enable-at-startup nil)
(setq package-archives '(("org"   . "http://orgmode.org/elpa/")
                         ("gnu"   . "http://elpa.gnu.org/packages/")
                         ("melpa" . "https://melpa.org/packages/")))
(package-initialize)
(package-refresh-contents t)

;; Bootstrap `use-package`
(unless (package-installed-p 'use-package)
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

;; Vim Emulation
;; TODO -- Bind something to (suspend-frame) for pausing an emacs instance
;; in the terminal.
(use-package evil
  :init (setq evil-want-C-u-scroll t)
  :config (evil-mode t))
(use-package evil-commentary
  :after evil
  :config (evil-commentary-mode t))
(use-package evil-surround
  :after evil
  :config (global-evil-surround-mode t))

;; Popup Key Binding
(use-package which-key
   :defer t
   :config (which-key-mode))

;; Project
(use-package projectile
   :config (projectile-mode t))

;;; packages.el ends here
