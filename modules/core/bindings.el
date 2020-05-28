;;; bindings.el --- MeMacs core bindings.

;;; Commentary:
;;; A lightweight Spacemacs-like configuration.

;;; Code:

(require 'ido)
(require 'which-key)

(general-create-definer memacs-global-prefix
  :states '(normal insert emacs visual visual-line)
  :prefix "SPC"
  :non-normal-prefix "M-SPC"
  :prefix-command 'memacs-global-prefix-map)

(memacs-global-prefix
  "!"   '(shell-command :which-key "Shell Command")
  ":"   '(eval-expression :which-key "Eval")
  "SPC" '(execute-extended-command :which-key "M-x"))

(which-key-add-key-based-replacements
  "SPC b" "Buffers"
  "SPC f" "Files"
  "SPC h" "Help"
  "SPC p" "Projects"
  "SPC w" "Windows")

(general-create-definer memacs-buffer-prefix
  :states '(normal insert emacs visual visual-line)
  :prefix "SPC b"
  :non-normal-prefix "M-SPC b")

(memacs-buffer-prefix
 "b"   '(switch-to-buffer :which-key "Switch Buffer")
 "d"   '(evil-delete-buffer :which-key "Delete Buffer")
 "k"   '(kill-current-buffer :which-key "Kill Buffer")
 "l"   '(list-buffers :which-key "List Buffers")
 "n"   '(next-buffer :which-key "Next Buffer")
 "m"   '((lambda () (interactive) (switch-to-buffer "*Messages*")) :which-key "*Messages*")
 "p"   '(previous-buffer :which-key "Previous Buffer")
 "s"   '((lambda () (interactive) (switch-to-buffer "*scratch*")) :which-key "*scratch*")
 "TAB" '((lambda () (interactive) (switch-to-buffer nil)) :which-key "Other Buffer"))

(general-create-definer memacs-file-prefix
  :states '(normal insert emacs visual visual-line)
  :prefix "SPC f"
  :non-normal-prefix "M-SPC f")

(memacs-file-prefix
  "f" '(find-file :which-key "Find File")
  "w" '(evil-write :which-key "Write File"))

(general-create-definer memacs-help-prefix
  :states '(normal insert emacs visual visual-line)
  :prefix "SPC h"
  :non-normal-prefix "M-SPC h")

(memacs-help-prefix
 "?" '(help-for-help :which-key "Help")
 "." '(display-local-help :which-key "Local Help")
 "a" '(apropos :which-key "Search")
 "f" '(describe-function :which-key "Describe Function")
 "k" '(describe-key :which-key "Describe Key")
 "m" '(describe-mode :which-key "Describe Mode")
 "p" '(describe-package :which-key "Describe Package")
 "s" '(describe-syntax :which-key "Describe Syntax")
 "v" '(describe-variable :which-key "Describe Variable"))

(general-create-definer memacs-project-prefix
  :states '(normal insert emacs visual visual-line)
  :prefix "SPC p"
  :non-normal-prefix "M-SPC p")

(memacs-project-prefix
 "f" '(projectile-find-file :which-key "Find File")
 "g" '(projectile-grep :which-key "Grep")
 "l" '(projectile-edit-dir-locals :which-key "Edit Dir-Locals")
 "p" '(projectile-switch-project :which-key "Switch Project")
 "r" '(projectile-replace :which-key "Replace String"))

(general-create-definer memacs-window-prefix
  :states '(normal insert emacs visual visual-line)
  :prefix "SPC w"
  :non-normal-prefix "M-SPC w")

(memacs-window-prefix
 "/" '(evil-window-vsplit :which-key "Vertical Split")
 "-" '(evil-window-split :which-key "Horizontal Split")
 "d" '(evil-window-delete :which-key "Delete Window")
 "h" '(evil-window-left :which-key "Move Left")
 "j" '(evil-window-down :which-key "Move Down")
 "k" '(evil-window-up :which-key "Move Up")
 "l" '(evil-window-right :which-key "Move Right")
 "o" '(delete-other-windows :which-key "Delete Others"))

;;; bindings.el ends here
