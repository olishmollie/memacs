;;; bindings.el --- Emacs bindings.

;;; Commentary:
;;; A lightweight Spacemacs-like configuration.

;;; Code:

(require 'company)
(require 'dired)
(require 'evil)
(require 'help-mode)
(require 'ivy)
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
  "Help management prefix key.  Defaults to 'h'.")

(defvar memacs-prefix-bindings
  '(("SPC" . ("command" . execute-extended-command))
    (":" . ("eval" . eval-expression))
    ("'" . ("shell" . shell-pop)))
  "Default top level MeMacs prefix bindings.")

(defvar memacs-magit-prefix-bindings '(("s" . magit-status))
  "Default magit prefix bindings.")

(defvar memacs-buffer-prefix-bindings
  '(("b" . ivy-switch-buffer)
    ("d" . kill-this-buffer)
    ("l" . list-buffers)
    ("n" . next-buffer)
    ("p" . previous-buffer))
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
  '(("f" . counsel-find-file))
  "Default file prefix bindings.")

(defvar memacs-help-prefix-bindings
 '(("v" . counsel-describe-variable)
   ("f" . counsel-describe-function)
   ("m" . describe-mode)
   ("k" . describe-key)
   ("?" . help-for-help)
   ("." . display-local-help)
   ("P" . describe-package)
   ("a" . counsel-apropos)
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

(memacs/make-prefix-map 'memacs-prefix-map memacs-prefix-key evil-motion-state-map)
(memacs/bind-prefix-map 'memacs-prefix-map memacs-prefix-bindings)

;; Magit
(memacs/make-prefix-map 'memacs-magit-prefix-map
			memacs-magit-prefix-key
			'memacs-prefix-map "Magit")

(memacs/bind-prefix-map 'memacs-magit-prefix-map memacs-magit-prefix-bindings)

;; Buffer Management
(memacs/make-prefix-map 'memacs-buffer-prefix-map
			memacs-buffer-prefix-key
			'memacs-prefix-map "Buffers")

(memacs/bind-prefix-map 'memacs-buffer-prefix-map memacs-buffer-prefix-bindings)

;; Window Management
(memacs/make-prefix-map 'memacs-window-prefix-map
			memacs-window-prefix-key
			'memacs-prefix-map "Windows")

(memacs/bind-prefix-map 'memacs-window-prefix-map memacs-window-prefix-bindings)

;; File Management
(memacs/make-prefix-map 'memacs-file-prefix-map
			memacs-file-prefix-key
			'memacs-prefix-map "Files")

(memacs/bind-prefix-map 'memacs-file-prefix-map memacs-file-prefix-bindings)

;; Help
(memacs/make-prefix-map 'memacs-help-prefix-map
			memacs-help-prefix-key
			'memacs-prefix-map "Help")

(memacs/bind-prefix-map 'memacs-help-prefix-map memacs-help-prefix-bindings)

;; Company
(define-key evil-insert-state-map (kbd "C-n") 'company-complete)
(define-key evil-insert-state-map (kbd "C-p") 'company-complete)
(define-key company-active-map (kbd "C-n") 'company-select-next)
(define-key company-active-map (kbd "C-p") 'company-select-previous)
(define-key company-search-map (kbd "C-n") 'company-select-next)
(define-key company-search-map (kbd "C-p") 'company-select-previous)

;; Overrides
(define-key Buffer-menu-mode-map (kbd "SPC") 'memacs-prefix-map)
(define-key dired-mode-map (kbd "SPC") 'memacs-prefix-map)

(evil-set-initial-state 'help-mode 'emacs)
(evil-add-hjkl-bindings help-mode-map 'emacs
  (kbd "/")       'evil-search-forward
  (kbd "w")       'evil-forward-word-begin
  (kbd "b")       'evil-backward-word-begin
  (kbd "n")       'evil-search-next
  (kbd "N")       'evil-search-previous
  (kbd "gg")      'beginning-of-buffer
  (kbd "G")       'end-of-buffer
  (kbd "C-d")     'evil-scroll-down
  (kbd "C-u")     'evil-scroll-up
  (kbd "}")       'evil-forward-paragraph
  (kbd "{")       'evil-backward-paragraph)

(define-key package-menu-mode-map (kbd "SPC") 'memacs-prefix-map)
(evil-add-hjkl-bindings package-menu-mode-map 'emacs
  (kbd "/")       'evil-search-forward
  (kbd "w")       'evil-forward-word-begin
  (kbd "b")       'evil-backward-word-begin
  (kbd "n")       'evil-search-next
  (kbd "N")       'evil-search-previous
  (kbd "gg")      'beginning-of-buffer
  (kbd "G")       'end-of-buffer
  (kbd "C-d")     'evil-scroll-down
  (kbd "C-u")     'evil-scroll-up
  (kbd "}")       'evil-forward-paragraph
  (kbd "{")       'evil-backward-paragraph)

;;; bindings.el ends here
