;;; c-c++.el --- C/C++ language support.

;;; Commentary:

;;; Code:

(defvar memacs-enable-clang-format-on-save t
  "If non-nil, automatically format a C/C++ buffer on save.
Try setting it as a dir-local.")

(defun memacs/c-c++-format-on-save ()
  "If `memacs-enable-clang-format-on-save' is non-nil, format the current buffer with clang-format."
  (when memacs-enable-clang-format-on-save
    (clang-format-buffer)))

(defun memacs/add-c-c++-lsp-keybindings ()
  "Add C/C++ keybindings."
  (which-key-add-key-based-replacements
    "SPC m" "C/C++"
    "SPC m j" "Jump To"
    "SPC m e" "Errors")

  (general-create-definer memacs/c-c++-prefix
    :states '(normal insert emacs visual visual-line)
    :keymaps 'local
    :prefix "SPC m"
    :non-normal-prefix "M-SPC m")
  (memacs/c-c++-prefix
   "d" '(lsp-disconnect :which-key "Disconnect LSP")
   "f" '(lsp-format-buffer :which-key "Format Buffer")
   "r" '(lsp-rename :which-key "Rename")
   "x" '(lsp-restart-workspace :which-key "Restart LSP"))

  (general-create-definer memacs/c-c++-jump-prefix
    :states '(normal insert emacs visual visual-line)
    :keymaps 'local
    :prefix "SPC m j"
    :non-normal-prefix "M-SPC m j")
  (memacs/c-c++-jump-prefix
   "d" '(lsp-find-definition :which-key "Definition")
   "i" '(lsp-goto-implementation :which-key "Implementation")
   "r" '(lsp-find-references :which-key "References")
   "t" '(lsp-goto-type-definition :which-key "Type Definition"))

   (general-create-definer memacs/c-c++-errors-prefix
     :states '(normal insert emacs visual visual-line)
     :keymaps 'local
     :prefix "SPC m e"
     :non-normal-prefix "M-SPC m e")
   (memacs/c-c++-errors-prefix
    "l" '(flycheck-list-errors :which-key "List Errors")
    "n" '(flycheck-next-error :which-key "Next Error")
    "p" '(flycheck-previous-error :which-key "Previous Error")))

(defun memacs/init-c-c++-mode ()
  "Initialize c-c++ mode."
  (lsp)
  (setq c-basic-offset 4)
  (memacs/add-c-c++-lsp-keybindings)
  (add-hook 'before-save-hook #'memacs/c-c++-format-on-save nil t))

(add-hook 'c-mode-common-hook #'memacs/init-c-c++-mode)

;;; c-c++.el ends here
