;;; javascript.el --- JavaScript language support.

;;; Commentary:

;;; Code:

(use-package prettier-js)

(defvar memacs-enable-js-format-on-save t
  "If non-nil, automatically format a JavaScript buffer on save.")

(defun memacs/js-format-on-save ()
  "If `memacs-enable-js-format-on-save' is non-nil, format the current JavaScript buffer."
  (when memacs-enable-js-format-on-save
    (prettier-js)))

(defun memacs/add-js-lsp-keybindings()
  "Add C/C++ keybindings."
  (which-key-add-key-based-replacements
    "SPC m" "JavaScript"
    "SPC m j" "Jump To")

  (general-create-definer memacs/js-prefix
    :states '(normal insert emacs visual visual-line)
    :keymaps 'local
    :prefix "SPC m"
    :non-normal-prefix "M-SPC m")
  (memacs/js-prefix
    "d" '(lsp-disconnect :which-key "Disconnect LSP")
    "f" '(prettier-js :which-key "Format Buffer")
    "r" '(lsp-rename :which-key "Rename")
    "x" '(lsp-restart-workspace :which-key "Restart LSP"))

  (general-create-definer memacs/js-jump-prefix
    :states '(normal insert emacs visual visual-line)
    :keymaps 'local
    :prefix "SPC m j"
    :non-normal-prefix "M-SPC m j")
  (memacs/js-jump-prefix
    "d" '(lsp-find-definition :which-key "Definition")
    "i" '(lsp-goto-implementation :which-key "Implementation")
    "r" '(lsp-find-references :which-key "References")
    "t" '(lsp-goto-type-definition :which-key "Type Definition")))

(add-hook 'js-mode-hook
          (lambda ()
            (add-hook 'before-save-hook #'memacs/js-format-on-save nil t)
            (memacs/add-js-lsp-keybindings)))

;;; javascript.el ends here
