;;; bindings.el --- MeMacs bindings.

;;; Commentary:
;;; A lightweight Spacemacs-like configuration.

;;; Code:

(require 'which-key)

(general-create-definer memacs/global-prefix
  :states '(normal insert emacs visual visual-line)
  :prefix "SPC"
  :non-normal-prefix "M-SPC"
  :prefix-command 'memacs-global-prefix-map)

(memacs/global-prefix
  "!"   '(shell-command :which-key "Shell Command")
  ":"   '(eval-expression :which-key "Eval")
  "SPC" '(execute-extended-command :which-key "M-x"))

(which-key-add-key-based-replacements
  "SPC b" "Buffers"
  "SPC f" "Files"
  "SPC h" "Help"
  "SPC p" "Projects"
  "SPC w" "Windows")

(general-create-definer memacs/buffer-prefix
  :states '(normal insert emacs visual visual-line)
  :prefix "SPC b"
  :non-normal-prefix "M-SPC b")

(memacs/buffer-prefix
 "b"   '(switch-to-buffer :which-key "Switch Buffer")
 "d"   '(evil-delete-buffer :which-key "Kill Buffer")
 "l"   '(list-buffers :which-key "List Buffers")
 "n"   '(next-buffer :which-key "Next Buffer")
 "m"   '((lambda () (interactive) (switch-to-buffer "*Messages*")) :which-key "*Messages*")
 "p"   '(previous-buffer :which-key "Previous Buffer")
 "s"   '((lambda () (interactive) (switch-to-buffer "*scratch*")) :which-key "*scratch*")
 "TAB" '((lambda () (interactive) (switch-to-buffer nil)) :which-key "Other Buffer"))

(general-create-definer memacs/file-prefix
  :states '(normal insert emacs visual visual-line)
  :prefix "SPC f"
  :non-normal-prefix "M-SPC f")

(memacs/file-prefix
  "f" '(find-file :which-key "Find File")
  "w" '(evil-write :which-key "Write File"))

(general-create-definer memacs/help-prefix
  :states '(normal insert emacs visual visual-line)
  :prefix "SPC h"
  :non-normal-prefix "M-SPC h")

(memacs/help-prefix
 "?" '(help-for-help :which-key "Help")
 "." '(display-local-help :which-key "Local Help")
 "a" '(apropos :which-key "Search")
 "f" '(describe-function :which-key "Describe Function")
 "k" '(describe-key :which-key "Describe Key")
 "m" '(describe-mode :which-key "Describe Mode")
 "p" '(describe-package :which-key "Describe Package")
 "s" '(describe-syntax :which-key "Describe Syntax")
 "v" '(describe-variable :which-key "Describe Variable"))

(general-create-definer memacs/project-prefix
  :states '(normal insert emacs visual visual-line)
  :prefix "SPC p"
  :non-normal-prefix "M-SPC p")

(memacs/project-prefix
 "f" '(projectile-find-file :which-key "Find File")
 "g" '(projectile-grep :which-key "Grep")
 "l" '(projectile-edit-dir-locals :which-key "Edit Dir-Locals")
 "p" '(projectile-switch-project :which-key "Switch Project")
 "r" '(projectile-replace :which-key "Replace String"))

(general-create-definer memacs/window-prefix
  :states '(normal insert emacs visual visual-line)
  :prefix "SPC w"
  :non-normal-prefix "M-SPC w")

(memacs/window-prefix
 "/" '(evil-window-vsplit :which-key "Vertical Split")
 "-" '(evil-window-split :which-key "Horizontal Split")
 "d" '(evil-window-delete :which-key "Delete Window")
 "h" '(evil-window-left :which-key "Move Left")
 "j" '(evil-window-down :which-key "Move Down")
 "k" '(evil-window-up :which-key "Move Up")
 "l" '(evil-window-right :which-key "Move Right")
 "o" '(delete-other-windows :which-key "Delete Others"))

(defun memacs/create-prog-mode-prefix ()
  "Create lsp keybindings based on current major mode."
  (which-key-add-key-based-replacements
    "SPC m" mode-name
    "SPC m j" "Jump To"
    "SPC m e" "Errors")

  (general-create-definer memacs/prog-mode-prefix
    :states '(normal insert emacs visual visual-line)
    :prefix "SPC m"
    :non-normal-prefix "M-SPC m")

  (memacs/prog-mode-prefix
   "d" '(lsp-disconnect :which-key "Disconnect LSP")
   "f" '(lsp-format-buffer :which-key "Format Buffer")
   "r" '(lsp-rename :which-key "Rename")
   "x" '(lsp-restart-workspace :which-key "Restart LSP"))


  (general-create-definer memacs/prog-mode-jump-prefix
    :states '(normal insert emacs visual visual-line)
    :keymaps 'local
    :prefix "SPC m j"
    :non-normal-prefix "M-SPC m j")

  (memacs/prog-mode-jump-prefix
   "d" '(lsp-find-definition :which-key "Definition")
   "i" '(lsp-goto-implementation :which-key "Implementation")
   "r" '(lsp-find-references :which-key "References")
   "t" '(lsp-goto-type-definition :which-key "Type Definition"))

  (general-create-definer memacs/prog-mode-errors-prefix
    :states '(normal insert emacs visual visual-line)
    :keymaps 'local
    :prefix "SPC m e"
    :non-normal-prefix "M-SPC m e")

  (memacs/prog-mode-errors-prefix
   "l" '(flycheck-list-errors :which-key "List Errors")
   "n" '(flycheck-next-error :which-key "Next Error")
   "p" '(flycheck-previous-error :which-key "Previous Error")))

(add-hook 'prog-mode-hook #'memacs/create-prog-mode-prefix)

(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

(defmacro memacs/veemacs-state (mode &rest bindings)
  "Force Emacs state in MODE while providing BINDINGS.

  Many modes are more useful in Emacs state.  This macro
  adds some essential vim keybindings to these modes while
  maintaining the useful Emacs bindings."
  `(progn (evil-set-initial-state (quote ,mode) 'emacs)
          (evil-add-hjkl-bindings ,(intern (concat (symbol-name mode) "-map")) 'emacs ,@bindings)))

(dolist (mode '(Buffer-menu-mode
                compilation-mode
                completion-list-mode
                custom-mode
                dired-mode
                flycheck-error-list-mode
                help-mode
                package-menu-mode))
  (eval `(memacs/veemacs-state ,mode
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

;; Ido bindings
(require 'ido)
(defun memacs/add-ido-keybindings ()
  "Add ido keybindings."
  (define-key ido-completion-map (kbd "C-n") #'ido-next-match)
  (define-key ido-completion-map (kbd "C-p") #'ido-prev-match)
  (define-key ido-completion-map (kbd "TAB") #'ido-exit-minibuffer))

(add-hook 'ido-setup-hook #'memacs/add-ido-keybindings)

;;; bindings.el ends here
