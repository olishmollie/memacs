;;; dlang.el --- D language support.

;;; Commentary:

;;; Code:

(use-package d-mode)
(use-package company-dcd)

(add-hook 'd-mode-hook 'company-dcd-mode)

(add-hook 'd-mode-hook (lambda ()
                         (setq c-basic-offset 4)
                         (company-dcd-mode)))


;;; dlang.el ends here
