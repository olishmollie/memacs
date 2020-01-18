;;; completion.el --- Auto complete installation and configuration.

;;; Commentary:

;;; Code:

(defun memacs/company-init ()
  "Initialize company mode."
  (add-hook 'prog-mode-hook
            (lambda ()
              (if (not (file-remote-p default-directory))
                  (company-mode)))))

(use-package company
  :defer t
  :init
  (setq company-minimum-prefix-length 1
	company-selection-wrap-around t)
  :config
  (add-hook 'company-mode-hook #'memacs/add-company-keybindings)
  (memacs/company-init))

(defun memacs/add-company-keybindings ()
  "Add keybindings to company-mode."
  (define-key evil-insert-state-map (kbd "C-n") #'company-complete)
  (define-key evil-insert-state-map (kbd "C-p") #'company-complete)
  (define-key company-active-map (kbd "C-n") #'company-select-next)
  (define-key company-active-map (kbd "C-p") #'company-select-previous)
  (define-key company-search-map (kbd "C-n") #'company-select-next)
  (define-key company-search-map (kbd "C-p") #'company-select-previous))


;;; completion ends here
