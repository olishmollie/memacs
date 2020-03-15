;;; rust.el --- Rust language support.

;;; Commentary:

;;; Code:

(use-package rust-mode)

(defvar memacs-enable-rust-format-on-save t
  "If non-nil, automatically format a Rust buffer on save.")

(defun memacs/rust-format-on-save ()
  "If `memacs-enable-js-format-on-save' is non-nil, format the current Rust buffer."
  (when memacs-enable-rust-format-on-save
    (rust-format-buffer)))

(defun memacs/add-rust-lsp-keybindings()
  "Add Rust keybindings."
  (which-key-add-key-based-replacements
    "SPC m" "JavaScript"
    "SPC m j" "Jump To")

  (general-create-definer memacs/rust-prefix
    :states '(normal insert emacs visual visual-line)
    :keymaps 'local
    :prefix "SPC m"
    :non-normal-prefix "M-SPC m")
  (memacs/rust-prefix
    "d" '(lsp-disconnect :which-key "Disconnect LSP")
    "f" '(prettier-rust :which-key "Format Buffer")
    "r" '(lsp-rename :which-key "Rename")
    "x" '(lsp-restart-workspace :which-key "Restart LSP"))

  (general-create-definer memacs/rust-jump-prefix
    :states '(normal insert emacs visual visual-line)
    :keymaps 'local
    :prefix "SPC m j"
    :non-normal-prefix "M-SPC m j")
  (memacs/rust-jump-prefix
    "d" '(lsp-find-definition :which-key "Definition")
    "i" '(lsp-goto-implementation :which-key "Implementation")
    "r" '(lsp-find-references :which-key "References")
    "t" '(lsp-goto-type-definition :which-key "Type Definition")))

(add-hook 'rust-mode-hook
          (lambda ()
            (lsp)
            (add-hook 'before-save-hook #'memacs/rust-format-on-save)
            (memacs/add-rust-lsp-keybindings)))

;;; rust ends here
