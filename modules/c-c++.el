;;; c-c++.el --- C/C++ language support.

;;; Commentary:

;;; Code:

(use-package clang-format)

(defun memacs/clang-format-on-save ()
  "Reformat buffer if .clang-format exists in the projectile project root."
  (when (f-exists? (expand-file-name ".clang-format" (projectile-project-root)))
    (clang-format-buffer)))

(add-hook 'c-mode-common-hook (lambda () (add-hook 'before-save-hook #'memacs/clang-format-on-save nil t)))

;;; c-c++ ends here.
