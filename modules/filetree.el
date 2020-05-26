;;; filetree.el --- Neotree configuration.

;;; Commentary:

;;; Code:

;; File Tree Sidebar
(use-package all-the-icons)
(use-package neotree
  :defer t
  :requires all-the-icons
  :init
  (setq neo-theme 'icons
	neo-smart-open t
	neo-auto-indent-point t
	neo-autorefresh t
	neo-mode-line-type 'default
	neo-banner-message "MeMacs")
  (when (eq 'darwin system-type)
    (setq neo-default-system-application "open"))
  :config
  (when (fboundp #'doom-themes-neotree-config)
    (doom-themes-neotree-config)))

(memacs-file-prefix
  "t" '(neotree-toggle :which-key "Tree"))

(memacs-veemacs-state neotree-mode
  (kbd "SPC") #'memacs-global-prefix-map
  (kbd "/")   #'evil-search-forward
  (kbd "w")   #'evil-forward-word-begin
  (kbd "b")   #'evil-backward-word-begin
  (kbd "gg")  #'beginning-of-buffer
  (kbd "G")   #'end-of-buffer
  (kbd "C-d") #'evil-scroll-down
  (kbd "C-u") #'evil-scroll-up
  (kbd "}")   #'evil-forward-paragraph
  (kbd "{")   #'evil-backward-paragraph
  (kbd "x")   #'neotree-collapse-all
  (kbd "C-n") #'neotree-create-node
  (kbd "C-k") #'neotree-delete-node
  (kbd "C-c") #'neotree-change-root
  (kbd "C-r") #'neotree-rename-node)

;;; filetree ends here
