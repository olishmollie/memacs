;;; c-c++.el --- C/C++ language support.

;;; Commentary:

;;; Code:

(defvar memacs-enable-clang-format-on-save t
  "If non-nil, automatically format a C/C++ buffer on save.")

(defun memacs/clang-format-on-save ()
  "If `memacs-enable-clang-format-on-save' is non-nil, format the current buffer with clang-format."
  (when memacs-enable-clang-format-on-save
    (lsp-format-buffer)))

(add-hook 'c-mode-common-hook
          (lambda () (add-hook 'before-save-hook #'memacs/clang-format-on-save nil t)))

;;; c-c++.el ends here
