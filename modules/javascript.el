;;; javascript.el --- JavaScript/Typescript language support.

;;; Commentary:

;;; Code:

(use-package prettier-js)

(use-package typescript-mode
  :config
  (add-to-list 'auto-mode-alist '("\\.ts\\'" . typescript-mode)))

(use-package tide
  :config
  (tide-hl-identifier-mode +1))

(require 'prettier-js)

(defvar memacs-enable-js-format-on-save t
  "If non-nil, automatically format a JavaScript buffer on save.")

(defun memacs-js-format-on-save ()
  "If `memacs-enable-js-format-on-save' is non-nil, format the current JavaScript buffer."
  (when memacs-enable-js-format-on-save
    (if (string-equal "json" (file-name-extension buffer-file-name))
        (let ((prettier-js-args '("--parser=json")))
          (prettier-js))
      (prettier-js))))

(defun memacs-init-ts-mode ()
  "Configure typsecript mode."
  (tide-setup)
  (tide-hl-identifier-mode +1))


(defun memacs-init-js-mode ()
  "Init JavaScript mode."
  (add-hook 'before-save-hook #'memacs-js-format-on-save nil t))

(add-hook 'js-mode-hook #'memacs-init-js-mode)
(add-hook 'typescript-mode-hook #'memacs-init-ts-mode)

;;; javascript.el ends here
