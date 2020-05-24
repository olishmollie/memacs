;;; go.el --- Go language support.

;;; Commentary:

;;; Code:

(use-package go-mode
  :config
  (setq gofmt-command "goimports"))

(add-hook 'go-mode-hook
          (lambda ()
            (lsp)
            (setq tab-width 4)
            (add-hook 'before-save-hook #'gofmt-before-save)))

;;; go.el ends here
