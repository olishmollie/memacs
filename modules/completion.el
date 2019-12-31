;;; completion.el --- Auto complete installation and configuration.

;;; Commentary:

;;; Code:

(use-package company
  :defer t
  :config (global-company-mode))

(defun memacs/add-company-keybindings ()
  "Add keybindings to company-mode."
  (define-key evil-insert-state-map (kbd "C-n") #'company-complete)
  (define-key evil-insert-state-map (kbd "C-p") #'company-complete)
  (define-key company-search-map (kbd "C-n") #'company-select-next)
  (define-key company-search-map (kbd "C-p") #'company-select-previous))

(add-hook 'company-mode-hook #'memacs/add-company-keybindings)

;;; completion ends here
