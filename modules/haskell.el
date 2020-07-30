;;; haskell.el --- Haskell language support.

;;; Commentary:

;;; Code:

(use-package haskell-mode)

(defvar memacs-enable-haskell-format-on-save t
  "If non-nil, automatically format a Haskell buffer on save.")

(defun memacs-haskell-format-on-save ()
  "If `memacs-enable-haskell-format-on-save' is non-nil, format the current Java buffer."
  (when memacs-enable-haskell-format-on-save
    (haskell-mode-stylish-buffer)))

(add-hook 'haskell-mode-hook
          (lambda ()
            (add-hook 'before-save-hook #'memacs-haskell-format-on-save nil t)))

;;; haskell.el ends here
