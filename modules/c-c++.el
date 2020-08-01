;;; c-c++.el --- C/C++ language support.

;;; Commentary:

;;; Code:

(require 'cc-mode)

(use-package clang-format)

(defvar memacs-enable-clang-format-on-save t
  "If non-nil, automatically format a C/C++ buffer on save.
Try setting it as a dir-local.")

(defun memacs-c-c++-format-on-save ()
  "If `memacs-enable-clang-format-on-save' is non-nil, format the current buffer with clang-format."
  (when memacs-enable-clang-format-on-save
    (clang-format-buffer)))

(defun memacs-init-c-c++-mode ()
  "Initialize c-c++ mode."
  (setq c-basic-offset 4)
  (add-hook 'before-save-hook #'memacs-c-c++-format-on-save nil t))

(add-hook 'c-mode-hook #'memacs-init-c-c++-mode)
(add-hook 'c++-mode-hook #'memacs-init-c-c++-mode)

;;; c-c++.el ends here
