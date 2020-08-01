;;; java.el --- Java language support.

;;; Commentary:

;;; Code:

(defvar memacs-enable-java-format-on-save t
  "If non-nil, automatically format a Java buffer on save.")

(defun memacs-java-format-on-save ()
  "If `memacs-enable-java-format-on-save' is non-nil, format the current Java buffer."
  (when memacs-enable-java-format-on-save
    (clang-format-buffer)))

(add-hook 'java-mode-hook
          (lambda ()
            (add-hook 'before-save-hook #'memacs-java-format-on-save nil t)))

;;; java.el ends here
