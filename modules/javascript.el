;;; javascript.el --- JavaScript/Typescript language support.

;;; Commentary:

;;; Code:

(use-package prettier-js)

(use-package typescript-mode
  :config
  (add-to-list 'auto-mode-alist '("\\.tsx?\\'" . typescript-mode)))

(require 'prettier-js)

(defvar memacs-enable-js-format-on-save t
  "If non-nil, automatically format a JavaScript buffer on save.")

(defun memacs/js-format-on-save ()
  "If `memacs-enable-js-format-on-save' is non-nil, format the current JavaScript buffer."
  (when memacs-enable-js-format-on-save
    (prettier-js)))

(defun memacs/init-js-modes ()
  "Init js and ts modes."
  (lsp)
  (add-hook 'before-save-hook #'memacs/js-format-on-save nil t))

(add-hook 'js-mode-hook #'memacs/init-js-modes)
(add-hook 'typescript-mode-hook #'memacs/init-js-modes)

;;; javascript.el ends here
