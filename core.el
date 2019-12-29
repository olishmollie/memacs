;;; core.el --- Core functions of MeMacs.

;;; Commentary:

;;; Code:
(load-file "~/.emacs.d/packages.el")
(require 'evil)
(require 'which-key)

;; This enables which-key to process cons arguments of `define-key'.
(setq which-key-enable-extended-define-key t)

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
    (define-key map (car binding) (cdr binding))))

(provide 'core)
;;; core ends here

