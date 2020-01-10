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

;; FIXME: The regex might match unwanted strings because of the first optional memacs-prefix-key.
;; It would be better to give it exactly the right keybinding. No problems yet,
;; but if they show up it'll probably be this function to blame.
(defun memacs/add-to-which-key (prefix-key key name)
  "Add PREFIX-KEY and KEY to which-key under NAME."
  (message "prefix-key = %s, key = %s, name = %s" prefix-key key name)
  (let ((regex (concat "\\(?:" memacs-prefix-key "\\)?" (regexp-quote (concat prefix-key " " key)))))
    (add-to-list 'which-key-replacement-alist `((,regex . nil) . (nil . ,name)))))

(defun memacs/prefix-map-key (map)
  "Given a prefix-map MAP, get its corresponding prefix-key."
  (symbol-value (intern (replace-regexp-in-string "map" "key" (symbol-name map)))))

(defun memacs/make-prefix-map (symbol parent &optional name)
  "Bind SYMBOL to a new prefix given by KEY with parent keymap PARENT.

  Optionally pass NAME for which-key text."
  (define-prefix-command symbol)
  (let ((key (memacs/prefix-map-key symbol)))
    (define-key parent (kbd key) symbol)
    (if name (memacs/add-to-which-key (memacs/prefix-map-key parent) key name))))


(defun memacs/bind-prefix-map (map bindings)
  "Bind BINDINGS to MAP and configure which-key with their names."
  (dolist (binding bindings)
    (let* ((prefix-key (memacs/prefix-map-key map))
	   (key (car binding))
	   (def (cdr binding))
	   (name (car def))
	   (command (cdr def)))
      (memacs/add-to-which-key prefix-key key name)
      (define-key map (kbd key) command))))

(defvar memacs-prefix-key "SPC"
  "MeMacs prefix-key.  Defaults to SPC.")
(defvar memacs-buffer-prefix-key "b"
  "Buffer management prefix-key.  Defaults to 'b'.")
(defvar memacs-window-prefix-key "w"
  "Window management prefix-key.  Defaults to 'w'.")
(defvar memacs-file-prefix-key "f"
  "File management prefix-key.  Defaults to 'f'.")
(defvar memacs-help-prefix-key "h"
  "Help management prefix-key.  Defaults to 'h'.")
(defvar memacs-project-prefix-key "p"
  "Project management prefix-key.  Defaults to 'p'.")
(defvar memacs-lsp-prefix-key "l"
  "LSP prefix-key.  Defaults to 'l'.")
(defvar memacs-lsp-jump-prefix-key "j"
  "LSP jump prefix-key.  Defaults to 'j'.")

(defvar memacs-prefix-bindings
  '(("SPC" . ("M-x" . execute-extended-command))
    (":" . ("eval" . eval-expression)))
  "Default top level MeMacs prefix bindings.")

(defvar memacs-buffer-prefix-bindings
  `(("b" . ("switch-buffer" . ivy-switch-buffer))
    ("d" . ("kill-buffer" . kill-this-buffer))
    ("l" . ("list-buffers" . list-buffers))
    ("n" . ("next-buffer" . next-buffer))
    ("p" . ("previous-buffer" . previous-buffer)))
  "Default buffer prefix bindings.")

(defvar memacs-window-prefix-bindings
  '(("/" . ("vertical-split" . evil-window-vsplit))
    ("-" . ("horizontal-split" . evil-window-split))
    ("h" . ("window-left" . evil-window-left))
    ("l" . ("window-right" . evil-window-right))
    ("k" . ("window-up" . evil-window-up))
    ("j" . ("window-down" . evil-window-down))
    ("d" . ("window-delete" . evil-window-delete))
    ("o" . ("delete-other-windows" . delete-other-windows)))
  "Default window prefix bindings.")

(defvar memacs-file-prefix-bindings
  '(("f" . ("find-file" . counsel-find-file)))
  "Default file prefix bindings.")

(defvar memacs-help-prefix-bindings
 '(("v" . ("describe-variable" . counsel-describe-variable))
   ("f" . ("describe-function" . counsel-describe-function))
   ("m" . ("describe-mode" . describe-mode))
   ("k" . ("describe-key" . describe-key))
   ("?" . ("help-for-help" . help-for-help))
   ("." . ("display-local-help" . display-local-help))
   ("P" . ("describe-package" . describe-package))
   ("a" . ("apropos" . counsel-apropos))
   ("s" . ("describe-syntax" . describe-syntax)))
 "Default help prefix bindings.")

(defvar memacs-project-prefix-bindings
  '(("f" . ("find-file" . projectile-find-file))
    ("p" . ("switch-project" . projectile-switch-project))
    ("g" . ("grep" . projectile-grep))
    ("l" . ("edit-dir-locals" . projectile-edit-dir-locals))
    ("r" . ("replace-string" . projectile-replace)))
  "Default project prefix bindings.")

(defvar memacs-lsp-prefix-bindings
  '(("j" . ("Jump" . 'memacs-lsp-jump-prefix-key))))

(defvar memacs-lsp-jump-prefix-bindings
  '(("d" . ("jump-to-definition" . #'lsp-find-definition))
    ("i" . ("jump-to-implementation" . #'lsp-goto-implementation))
    ("t" . ("jump-to-type-definition" . #'lsp-goto-type-definition))
    ("r" . ("jump-to-references" . #'lsp-find-references))))

(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

(memacs/make-prefix-map 'memacs-prefix-map evil-motion-state-map)
(memacs/bind-prefix-map 'memacs-prefix-map memacs-prefix-bindings)

;; Buffer Management
(memacs/make-prefix-map 'memacs-buffer-prefix-map 'memacs-prefix-map "Buffers")
(memacs/bind-prefix-map 'memacs-buffer-prefix-map memacs-buffer-prefix-bindings)

;; Window Management
(memacs/make-prefix-map 'memacs-window-prefix-map 'memacs-prefix-map "Windows")
(memacs/bind-prefix-map 'memacs-window-prefix-map memacs-window-prefix-bindings)

;; File Management
(memacs/make-prefix-map 'memacs-file-prefix-map 'memacs-prefix-map "Files")
(memacs/bind-prefix-map 'memacs-file-prefix-map memacs-file-prefix-bindings)

;; Help
(memacs/make-prefix-map 'memacs-help-prefix-map 'memacs-prefix-map "Help")
(memacs/bind-prefix-map 'memacs-help-prefix-map memacs-help-prefix-bindings)

;; Project Management
(memacs/make-prefix-map 'memacs-project-prefix-map 'memacs-prefix-map "Projects")
(memacs/bind-prefix-map 'memacs-project-prefix-map memacs-project-prefix-bindings)

;; LSP
(memacs/make-prefix-map 'memacs-lsp-prefix-map 'memacs-prefix-map "LSP")
(memacs/make-prefix-map 'memacs-lsp-jump-prefix-map 'memacs-lsp-prefix-map "Jump")
(memacs/bind-prefix-map 'memacs-lsp-jump-prefix-map memacs-lsp-jump-prefix-bindings)

;; Many modes are more useful in emacs state. This section
;; adds some essential vim keybindings to these modes while
;; maintaining the usefule emacs bindings.
(defvar memacs-core-vim-bindings
  '((kbd "SPC")     #'memacs-prefix-map
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
    (kbd "{")       #'evil-backward-paragraph)
  "A list of core VIM bindings.  Used in useful Emacs modes.")

(evil-set-initial-state 'help-mode 'emacs)
(evil-add-hjkl-bindings help-mode-map 'emacs
  (kbd "SPC")     #'memacs-prefix-map
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
  (kbd "{")       #'evil-backward-paragraph)

(evil-set-initial-state 'Buffer-menu-mode 'emacs)
(evil-add-hjkl-bindings Buffer-menu-mode-map 'emacs
  (kbd "SPC")     #'memacs-prefix-map
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
  (kbd "{")       #'evil-backward-paragraph)

(evil-set-initial-state 'dired-mode 'emacs)
(evil-add-hjkl-bindings dired-mode-map 'emacs
  (kbd "SPC")     #'memacs-prefix-map
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
  (kbd "{")       #'evil-backward-paragraph)

(evil-set-initial-state 'package-menu-mode 'emacs)
(evil-add-hjkl-bindings package-menu-mode-map 'emacs
  (kbd "SPC")     #'memacs-prefix-map
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
  (kbd "{")       #'evil-backward-paragraph)

(evil-set-initial-state 'compilation-mode 'emacs)
(evil-add-hjkl-bindings compilation-mode-map 'emacs
  (kbd "SPC")     #'memacs-prefix-map
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
  (kbd "{")       #'evil-backward-paragraph)

(evil-set-initial-state 'custom-mode 'emacs)
(evil-add-hjkl-bindings custom-mode-map 'emacs
  (kbd "SPC")     #'memacs-prefix-map
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
  (kbd "{")       #'evil-backward-paragraph)

(evil-set-initial-state 'completion-list-mode 'emacs)
(evil-add-hjkl-bindings completion-list-mode-map 'emacs
  (kbd "SPC")     #'memacs-prefix-map
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
  (kbd "{")       #'evil-backward-paragraph)
;;; bindings.el ends here
