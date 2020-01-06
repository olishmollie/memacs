;;; filetree.el --- Neotree configuration.

;;; Commentary:

;;; Code:

;; File Tree Sidebar
(use-package all-the-icons)
(use-package neotree
  :defer t
  :requires all-the-icons
  :config
  (setq neo-theme 'icons
	neo-smart-open t
	neo-auto-indent-point t
	neo-autorefresh t
	neo-mode-line-type 'default
	neo-banner-message "MeMacs")
  (when (eq 'darwin system-type)
    (setq neo-default-system-application "open")))

(memacs/bind-prefix-map 'memacs-file-prefix-map
			'(("t" . ("tree" . neotree-toggle))))

(when (fboundp #'doom-themes-neotree-config)
  (doom-themes-neotree-config))

(defun memacs/add-neotree-keybindings ()
  "Add keybindings to neotree-mode."
  (define-key evil-normal-state-local-map (kbd "TAB") #'neotree-enter)
  (define-key evil-normal-state-local-map (kbd "RET") #'neotree-change-root)
  (define-key evil-normal-state-local-map (kbd "<backtab>") #'neotree-select-up-node)
  (define-key evil-normal-state-local-map (kbd "x") #'neotree-collapse-all)
  (define-key evil-normal-state-local-map (kbd "p") #'neotree-quick-look)
  (define-key evil-normal-state-local-map (kbd "q") #'neotree-hide)
  (define-key evil-normal-state-local-map (kbd "g") #'neotree-refresh)
  (define-key evil-normal-state-local-map (kbd "A") #'neotree-stretch-toggle)
  (define-key evil-normal-state-local-map (kbd "n") #'neotree-create-node)
  (define-key evil-normal-state-local-map (kbd "d") #'neotree-delete-node)
  (define-key evil-normal-state-local-map (kbd "r") #'neotree-rename-node)
  (define-key evil-normal-state-local-map (kbd "H") #'neotree-hidden-file-toggle))

(add-hook 'neotree-mode-hook #'memacs/add-neotree-keybindings)

;;; filetree ends here
