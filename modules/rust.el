;;; rust.el --- Rust language support.

;;; Commentary:

;;; Code:

(use-package rust-mode)

(require 'rust-mode)

(defvar memacs-enable-rust-format-on-save t
  "If non-nil, automatically format a Rust buffer on save.")

(defun memacs-rust-format-on-save ()
  "If `memacs-enable-js-format-on-save' is non-nil, format the current Rust buffer."
  (when memacs-enable-rust-format-on-save
    (rust-format-buffer)))

(add-hook 'rust-mode-hook
          (lambda ()
            (when (fboundp #'flycheck-select-checker)
                (flycheck-select-checker 'rust-clippy))
            (add-hook 'before-save-hook #'memacs-rust-format-on-save nil t)))

;;; rust ends here
