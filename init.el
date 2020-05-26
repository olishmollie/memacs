;;; init.el --- MeMacs init script.

;;; Commentary:

;;; Code:

(require 'core (concat user-emacs-directory "modules/core/core.el"))

(memacs-load-core)

(load-file (concat user-emacs-directory "memacs.el"))

(dolist (module memacs-modules)
  (memacs-load-module module))

(dolist (face memacs-faces)
  (memacs-load-module face))

;; Allow escape to exit the minibuffer.
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

;; Create major-mode-prefix for prog-mode derived modes.
(add-hook 'prog-mode-hook
        (lambda ()
            (add-hook (memacs-major-mode-hook major-mode)
                    #'memacs-create-major-mode-prefix nil t)))

;; Allow certain modes to retain emacs flavor, with some extra vim-like keybindings.
(dolist (mode memacs-veemacs-modes)
(eval `(memacs-veemacs-state ,mode
                                (kbd "SPC")     #'memacs-global-prefix-map
                                (kbd "/")       #'evil-search-forward
                                (kbd "w")       #'evil-forward-word-begin
                                (kbd "b")       #'evil-backward-word-begin
                                (kbd "n")       #'evil-search-next
                                (kbd "N")       #'evil-search-previous
                                (kbd "gg")      #'beginning-of-buffer
                                (kbd "G")       #'end-of-buffer
                                (kbd "C-d")     #'evil-scroll-down
                                (kbd "C-u")     #'evil-scroll-up
                                (kbd "}")       #'evil-forward-paragraph
                                (kbd "{")       #'evil-backward-paragraph)))

;; Use ido whenever possible.
(add-hook 'ido-setup-hook #'memacs-add-ido-keybindings)

;;; init.el ends here
