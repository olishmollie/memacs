;;; lsp.el --- LSP configuration and support.

;;; Commentary:

;;; Code:

;; Language Server Protocol
(use-package lsp-mode)
;; (use-package lsp-ui
;;   :after lsp
;;   :init
;;   (setq lsp-ui-flycheck-enable t
;; 	lsp-ui-doc-position 'top)
;;   :config
;;   (set-face-background 'lsp-ui-doc-background
;; 		       (face-attribute 'default :background)))
(use-package company-lsp)
;; (use-package lsp-ivy)

(defvar memacs-lsp-modes
  '(c-mode
    c++-mode
    go-mode
    java-mode
    js-mode
    rust-mode))

(defun memacs/lsp-mode-prefix ()
  "Helpful keybindings for modes that use lsp mode."
  (which-key-add-key-based-replacements
    "SPC m j" "Jump To"
    "SPC m e" "Errors")

  (memacs/major-mode-prefix
   "d" '(lsp-disconnect :which-key "Disconnect LSP")
   "f" '(lsp-format-buffer :which-key "Format Buffer")
   "r" '(lsp-rename :which-key "Rename")
   "x" '(lsp-restart-workspace :which-key "Restart LSP"))

  (general-create-definer memacs/major-mode-jump-prefix
    :states '(normal insert emacs visual visual-line)
    :keymaps 'local
    :prefix "SPC m j"
    :non-normal-prefix "M-SPC m j")

  (memacs/major-mode-jump-prefix
   "d" '(lsp-find-definition :which-key "Definition")
   "i" '(lsp-goto-implementation :which-key "Implementation")
   "r" '(lsp-find-references :which-key "References")
   "t" '(lsp-goto-type-definition :which-key "Type Definition")))

(defun memacs/add-lsp-mode-prefix ()
  "Add `lsp-mode-prefix' to modes that use lsp-mode."
  (if (seq-contains memacs-lsp-modes major-mode)
      (add-hook (memacs/major-mode-hook major-mode) #'memacs/lsp-mode-prefix nil t)))

;; Add lsp-mode to languages' hooks.
(dolist (mode memacs-lsp-modes)
  (let ((hook (memacs/major-mode-hook mode)))
    (add-hook hook #'lsp)))

(add-hook 'prog-mode-hook #'memacs/add-lsp-mode-prefix)

;;; lsp.el ends here
