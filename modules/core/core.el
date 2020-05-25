;;; core.el --- MeMacs core variables and functions.

;;; Commentary:

;;; Code:

(require 'ido)

(defvar memacs-modules-directory (concat user-emacs-directory "modules/"))
(defvar memacs-backup-directory (concat user-emacs-directory "backups"))
(defvar memacs-custom-file (concat memacs-modules-directory "custom.el"))

(defvar memacs-modules
  "A list of modules that are loaded on startup. Defined in 'memacs.el'"
  nil)

(defvar memacs-veemacs-modes
  '(Buffer-menu-mode
    compilation-mode
    completion-list-mode
    custom-mode
    dired-mode
    flycheck-error-list-mode
    help-mode
    package-menu-mode
    special-mode))

(defun memacs/load-module (module)
  "Load MODULE into MeMacs."
  (load-file (concat memacs-modules-directory (symbol-name module) ".el")))

(defun memacs/load-core ()
  "Load MeMacs core modules.  Order is important."
  (load-file (concat memacs-modules-directory "core/packages.el"))
  (load-file (concat memacs-modules-directory "core/bindings.el")))

(defun memacs/add-ido-keybindings ()
  "Add ido keybindings."
  (define-key ido-completion-map (kbd "C-n") #'ido-next-match)
  (define-key ido-completion-map (kbd "C-p") #'ido-prev-match)
  (define-key ido-completion-map (kbd "TAB") #'ido-exit-minibuffer))

(defmacro memacs/veemacs-state (mode &rest bindings)
  "Force Emacs state in MODE while providing BINDINGS.

  Many modes are more useful in Emacs state.  This macro
  adds some essential vim keybindings to these modes while
  maintaining the useful Emacs bindings."
  `(progn (evil-set-initial-state (quote ,mode) 'emacs)
          (evil-add-hjkl-bindings ,(intern (concat (symbol-name mode) "-map")) 'emacs ,@bindings)))

(defun memacs/major-mode-hook (mode)
  "Return the hook associated with MODE."
  (intern (concat (symbol-name mode) "-hook")))

(defun memacs/create-major-mode-prefix ()
  "Create a global major mode prefix based on MODE."
  (which-key-add-major-mode-key-based-replacements
    major-mode
    "SPC m" mode-name)
    (general-create-definer memacs/major-mode-prefix
      :keymaps 'local
      :states '(normal insert emacs visual visual-line)
      :prefix "SPC m"
      :non-normal-prefix "M-SPC m"))

(provide 'core)

;;; core.el ends here
