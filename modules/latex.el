;;; latex.el --- Latex packages and configuration.

;;; Commentary:

;;; Code:

(use-package tex
  :defer t
  :ensure auctex
  :config
  (add-hook 'LaTeX-mode-hook #'memacs/add-latex-bindings))

(defun memacs/add-latex-bindings ()
  "Add latex keybindings."
  (which-key-add-key-based-replacements
    "SPC m" "Latex"
    "SPC m e" "Environments"
    "SPC m n" "Narrowing")
  (general-create-definer memacs/latex-prefix
    :states '(normal insert emacs visual visual-line)
    :keymaps 'local
    :prefix "SPC m"
    :non-normal-prefix "M-SPC m"
    "`" '(TeX-next-error :whick-key "Display Error")
    "a" '(TeX-command-run-all :which-key "Compile")
    "c" '(TeX-clean :which-key "Clean")
    "v" '(TeX-view :which-key "View"))
  (general-create-definer memacs/latex-environments-prefix
    :states '(normal insert emacs visual visual-line)
    :keymaps 'local
    :prefix "SPC m e"
    :non-normal-prefix "M-SPC m e"
    "i" '(LaTeX-environment :which-key "Insert Environment")
    "c" '(LaTeX-close-environment :which-key "Close Environment")
    "b" '(LaTeX-find-matching-begin :which-key "Find Begin")
    "e" '(LaTeX-find-matching-end :which-key "Find End"))
  (general-create-definer memacs/latex-narrowing-prefix
    :states '(normal insert emacs visual visual-line)
    :keymaps 'local
    :prefix "SPC m n"
    :non-normal-prefix "M-SPC m n"
    "g" '(TeX-narrow-to-group :which-key "Narrow to Group")
    "e" '(LaTeX-narrow-to-environment) :which-key "Narrow to Environment"))

;;; latex.el ends here
