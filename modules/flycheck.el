;;; flycheck.el --- Support and configuration for Flycheck.

;;; Commentary:

;;; Code:

;; Syntax Checking
(use-package flycheck
  :config (global-flycheck-mode))

(defun memacs/create-flycheck-prefix ()
  "Keybindings for modes that use flycheck."
  (general-create-definer memacs/prog-mode-errors-prefix
    :states '(normal insert emacs visual visual-line)
    :keymaps 'local
    :prefix "SPC m e"
    :non-normal-prefix "M-SPC m e")

  (memacs/prog-mode-errors-prefix
   "l" '(flycheck-list-errors :which-key "List Errors")
   "n" '(flycheck-next-error :which-key "Next Error")
   "p" '(flycheck-previous-error :which-key "Previous Error")))

(add-to-list 'memacs-veemacs-modes 'flycheck-error-list-mode)

(add-hook 'prog-mode-hook #'memacs/create-flycheck-prefix)

;;; flycheck.el ends here
