;;; latex.el --- Latex packages and configuration.

;;; Commentary:

;;; Code:

(use-package tex
  :defer t
  :ensure auctex
  :init
  (setq TeX-clean-confirm nil)
  :config
  ;; (add-hook 'LaTeX-mode-hook #'memacs/add-latex-bindings)
  (add-hook 'LaTeX-mode-hook (lambda ()
                               (memacs/add-latex-bindings)
                               (setq evil-shift-width LaTeX-indent-level))))
(use-package latex-preview-pane
  :commands (latex-preview-pane-mode))

(defun memacs/latex-compile()
  "Compile latex file, clean intermediate files, and delete extra buffers."
  (interactive)
  (TeX-command-run-all nil)
  (TeX-clean nil))

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
    :non-normal-prefix "M-SPC m")
  (memacs/latex-prefix
    "`" '(TeX-next-error :whick-key "Display Error")
    "a" '(memacs/latex-compile :which-key "Compile")
    "c" '(TeX-clean :which-key "Clean")
    "p" '(latex-preview-pane-mode :which-key "Preview")
    "v" '(TeX-view :which-key "View"))

  (general-create-definer memacs/latex-environments-prefix
    :states '(normal insert emacs visual visual-line)
    :keymaps 'local
    :prefix "SPC m e"
    :non-normal-prefix "M-SPC m e")
  (memacs/latex-environments-prefix
    "i" '(LaTeX-environment :which-key "Insert Environment")
    "c" '(LaTeX-close-environment :which-key "Close Environment")
    "b" '(LaTeX-find-matching-begin :which-key "Find Begin")
    "e" '(LaTeX-find-matching-end :which-key "Find End"))

  (general-create-definer memacs/latex-narrowing-prefix
    :states '(normal insert emacs visual visual-line)
    :keymaps 'local
    :prefix "SPC m n"
    :non-normal-prefix "M-SPC m n")
  (memacs/latex-narrowing-prefix
    "g" '(TeX-narrow-to-group :which-key "Narrow to Group")
    "e" '(LaTeX-narrow-to-environment :which-key "Narrow to Environment")
    "w" '(widen :which-key "Widen")))

;;; latex.el ends here
