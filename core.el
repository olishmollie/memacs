;;; core.el --- Core functions of MeMacs.

;;; Commentary:

;;; Code:
(load-file "~/.emacs.d/packages.el")

(require 'evil)
(require 'which-key)

;; Defaults
(defvar memacs-prefix-key "SPC"
  "MeMacs prefix key.  Defaults to SPC.")
(defvar memacs-magit-prefix-key "g"
  "Magit prefix key.  Defaults to 'g'.")
(defvar memacs-buffer-prefix-key "b"
  "Buffer management prefix key.  Defaults to 'b'.")
(defvar memacs-window-prefix-key "w"
  "Window management prefix key.  Defaults to 'w'.")
(defvar memacs-file-prefix-key "f"
  "File management prefix key.  Defaults to 'f'.")
(defvar memacs-help-prefix-key "h"
  "Help access prefix key.  Defaults to 'h'.")

(defvar memacs-prefix-bindings
  '(("SPC" . ("command" . execute-extended-command))
    (":" . ("eval" . eval-expression)))
  "Default top level MeMacs prefix bindings.")

(defvar memacs-magit-prefix-bindings '(("s" . magit-status))
  "Default magit prefix bindings.")

(defvar memacs-buffer-prefix-bindings
  '(("b" . list-buffers)
    ("p" . previous-buffer)
    ("n" . next-buffer)
    ("d" . kill-this-buffer))
  "Default buffer prefix bindings.")

(defvar memacs-window-prefix-bindings
  '(("/" . ("vertical-split" . evil-window-vsplit))
    ("-" . ("horizontal-split" . evil-window-split))
    ("h" . ("window-left" . evil-window-left))
    ("l" . ("window-right" . evil-window-right))
    ("k" . ("window-up" . evil-window-up))
    ("j" . ("window-down" . evil-window-down))
    ("d" . ("window-delete" . evil-window-delete))
    ("o" . delete-other-windows))
  "Default window prefix bindings.")

(defvar memacs-file-prefix-bindings
  '(("f" . find-file))
  "Default file prefix bindings.")

(defvar memacs-help-prefix-bindings
 '(("v" . describe-variable)
   ("f" . describe-function)
   ("m" . describe-mode)
   ("k" . describe-key)
   ("?" . help-for-help)
   ("." . display-local-help)
   ("P" . describe-package)
   ("a" . apropos-command)
   ("s" . describe-syntax))
 "Default help prefix bindings.")
 
(defun memacs/make-prefix-map (symbol key parent &optional name)
  "Bind SYMBOL to a new prefix given by KEY with parent keymap PARENT.

  Optionally pass NAME for help screen text."
  (define-prefix-command symbol)
  (define-key parent (kbd key) symbol)
  (if name (which-key-add-key-based-replacements
	     (concat memacs-prefix-key " " key) name)))

(defun memacs/bind-prefix-map (map bindings)
  "Bind BINDINGS to MAP."
  (dolist (binding bindings)
    (let ((key (car binding))
	  (def (cdr binding)))
      (define-key map (kbd key) def))))

(provide 'core)
;;; core ends here

