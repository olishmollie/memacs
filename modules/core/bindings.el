;;; bindings.el --- Emacs bindings.

;;; Commentary:
;;; A lightweight Spacemacs-like configuration.

;;; Code:

(require 'which-key)

(general-create-definer memacs/global-prefix
  :states '(normal insert emacs)
  :prefix "SPC"
  :non-normal-prefix "M-SPC"
  :prefix-command 'memacs-global-prefix-map)

(which-key-declare-prefixes
  "SPC b" "Buffers"
  "SPC f" "Files"
  "SPC h" "Help"
  "SPC p" "Projects"
  "SPC w" "Windows")

(general-create-definer memacs/buffer-prefix
  :states '(normal insert emacs)
  :prefix "SPC b"
  :non-normal-prefix "M-SPC b")

(general-create-definer memacs/file-prefix
  :states '(normal insert emacs)
  :prefix "SPC f"
  :non-normal-prefix "M-SPC f")

(general-create-definer memacs/help-prefix
  :states '(normal insert emacs)
  :prefix "SPC h"
  :non-normal-prefix "M-SPC h")

(general-create-definer memacs/project-prefix
  :states '(normal insert emacs)
  :prefix "SPC p"
  :non-normal-prefix "M-SPC p")

(general-create-definer memacs/window-prefix
  :states '(normal insert emacs)
  :prefix "SPC w"
  :non-normal-prefix "M-SPC w")

(memacs/global-prefix
  "!"   '(shell-command :which-key "Shell Command")
  ":"   '(eval-expression :which-key "Eval")
  "SPC" '(execute-extended-command :which-key "M-x"))

(memacs/buffer-prefix
 "b"   '(ivy-switch-buffer :which-key "Switch Buffer")
 "d"   '(kill-this-buffer :which-key "Kill Buffer")
 "l"   '(list-buffers :which-key "List Buffers")
 "n"   '(next-buffer :which-key "Next Buffer")
 "m"   '((lambda () (interactive) (switch-to-buffer "*Messages*")) :which-key "*Messages*")
 "p"   '(previous-buffer :which-key "Previous Buffer")
 "s"   '((lambda () (interactive) (switch-to-buffer "*scratch*")) :which-key "*scratch*")
 "TAB" '((lambda () (interactive) (switch-to-buffer nil) :which-key "Other Buffer")))

(memacs/file-prefix
 "f" '(counsel-find-file :which-key "Find File"))

(memacs/help-prefix
 "?" '(help-for-help :which-key "Help")
 "." '(display-local-help :which-key "Local Help")
 "a" '(counsel-apropos :which-key "Search")
 "f" '(counsel-describe-function :which-key "Describe Function")
 "k" '(describe-key :which-key "Describe Key")
 "m" '(describe-mode :which-key "Describe Mode")
 "p" '(describe-package :which-key "Describe Package")
 "s" '(describe-syntax :which-key "Describe Syntax")
 "v" '(counsel-describe-variable :which-key "Describe Variable"))

(memacs/project-prefix
 "f" '(projectile-find-file :which-key "Find File")
 "g" '(projectile-grep :which-key "Grep")
 "l" '(projectile-edit-dir-locals :which-key "Edit Dir-Locals")
 "p" '(projectile-switch-project :which-key "Switch Project")
 "r" '(projectile-replace :which-key "Replace String"))

(memacs/window-prefix
 "/" '(evil-window-vsplit :which-key "Vertical Split")
 "-" '(evil-window-split :which-key "Horizontal Split")
 "d" '(evil-window-delete :which-key "Delete Window")
 "h" '(evil-window-left :which-key "Move Left")
 "j" '(evil-window-down :which-key "Move Down")
 "k" '(evil-window-up :which-key "Move Up")
 "l" '(evil-window-right :which-key "Move Right")
 "o" '(delete-other-windows :which-key "Delete Others"))

(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

;; Many modes are more useful in emacs state. This section
;; adds some essential vim keybindings to these modes while
;; maintaining the usefule emacs bindings.
(evil-set-initial-state 'help-mode 'emacs)
(evil-add-hjkl-bindings help-mode-map 'emacs
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
  (kbd "{")       #'evil-backward-paragraph)

(evil-set-initial-state 'Buffer-menu-mode 'emacs)
(evil-add-hjkl-bindings Buffer-menu-mode-map 'emacs
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
  (kbd "{")       #'evil-backward-paragraph)

(evil-set-initial-state 'dired-mode 'emacs)
(evil-add-hjkl-bindings dired-mode-map 'emacs
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
  (kbd "{")       #'evil-backward-paragraph)

(evil-set-initial-state 'package-menu-mode 'emacs)
(evil-add-hjkl-bindings package-menu-mode-map 'emacs
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
  (kbd "{")       #'evil-backward-paragraph)

(evil-set-initial-state 'compilation-mode 'emacs)
(evil-add-hjkl-bindings compilation-mode-map 'emacs
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
  (kbd "{")       #'evil-backward-paragraph)

(evil-set-initial-state 'custom-mode 'emacs)
(evil-add-hjkl-bindings custom-mode-map 'emacs
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
  (kbd "{")       #'evil-backward-paragraph)

(evil-set-initial-state 'completion-list-mode 'emacs)
(evil-add-hjkl-bindings completion-list-mode-map 'emacs
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
  (kbd "{")       #'evil-backward-paragraph)

;;; bindings.el ends here
