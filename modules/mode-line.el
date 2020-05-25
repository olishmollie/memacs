;;; mode-line.el --- Mode line packages and configuration.

;;; Commentary:

;;; Code:

;; (use-package doom-modeline
;;   :init (doom-modeline-mode 1)
;;   :config (setq doom-modeline-icon nil))

(require 'all-the-icons)
(require 'evil)
(require 'flycheck)

(setq-default mode-line-format
              '((:eval (memacs/render-mode-line
                        (memacs/mode-line-left)
                        (memacs/mode-line-right)))))

(defun memacs/render-mode-line (left right)
  "Render the Memacs mode line, writing LEFT normally and padding RIGHT with the correct spacing."
  (let* ((available-width (- (window-width) (length left) 2)))
    (format (format " %%s %%%ds " available-width) left right)))

(defun memacs/mode-line-left ()
  "Render left aligned segment of the Memacs mode-line."
  (concat (memacs/mode-line-evil-state)
          "  "
          (memacs/mode-line-buffer-status)
          "  "
          (memacs/mode-line-file-name)
          "  "
          (format-mode-line "%l:%c")))

(defun memacs/mode-line-right ()
  "Render right aligned segment of the Memacs mode-line."
  (concat (format-mode-line "%Z")
          "  "
          (propertize mode-name 'face 'memacs-mode-line-blue)
          "  "
          (memacs/mode-line-vc-branch)
          "  "
          (memacs/mode-line-flycheck-status)))

(defun memacs/mode-line-evil-state ()
  "Mode line segment displaying current evil state."
  (cond ((string= evil-state "insert")
         (propertize "<I>" 'face 'memacs-mode-line-blue))
        ((string= evil-state "visual")
         (propertize "<V>" 'face 'memacs-mode-line-yellow))
        ((string= evil-state "emacs")
         (propertize "<E>" 'face 'memacs-mode-line-purple))
        (t (propertize "<N>" 'face 'memacs-mode-line-green))))

(defun memacs/mode-line-buffer-status ()
  "Mode line segment describing current buffer status."
  (cond ((derived-mode-p 'special-mode)
         (propertize "Q" 'face 'memacs-mode-line-yellow))
        (buffer-read-only
         (propertize "%%" 'face 'memacs-mode-line-yellow))
        ((buffer-modified-p)
         (propertize "*" 'face 'memacs-mode-line-red))
        (t (propertize "-" 'face 'memacs-mode-line-default))))

(defun memacs/mode-line-short-file-name (file-name)
  "Shorten FILE-NAME to a reasonable length."
  (let ((user (user-login-name)))
    (replace-regexp-in-string (concat "/Users/" user) "~" file-name)))

(defun memacs/mode-line-file-name ()
  "Mode line segment describing current file name or buffer name."
  (let ((file-name (buffer-file-name)))
    (if file-name
        (propertize (memacs/mode-line-short-file-name file-name))
      (buffer-name))))

(defun memacs/mode-line-vc-branch ()
  "Memacs mode-line vc-branch segment."
  (unless (null vc-mode)
    (propertize (concat "*" (substring vc-mode 5)) 'face 'memacs-mode-line-green)))

(defun memacs/mode-line-flycheck-status ()
  "Memacs mode-line flycheck status."
    (let-alist (flycheck-count-errors flycheck-current-errors)
      (cond
       (.error (propertize (format "!%s" .error) 'face 'memacs-mode-line-red))
       (.warning (propertize (format "!%s" .warning) 'face 'memacs-mode-line-yellow))
       (t (propertize "-" 'face 'memacs-mode-line-green)))))

;; Add mode-line padding
(set-face-attribute 'mode-line nil
                    :box `(:line-width 12 :color ,(face-attribute 'mode-line :background)))
(set-face-attribute 'mode-line-inactive nil
                    :box `(:line-width 12 :color ,(face-attribute 'mode-line-inactive :background)))

;; Custom faces
(make-face 'memacs-mode-line-yellow)
(set-face-attribute 'memacs-mode-line-yellow nil
                    :foreground "yellow")

(make-face 'memacs-mode-line-red)
(set-face-attribute 'memacs-mode-line-red nil
                    :foreground "red")

(make-face 'memacs-mode-line-green)
(set-face-attribute 'memacs-mode-line-green nil
                    :foreground "green")

(make-face 'memacs-mode-line-blue)
(set-face-attribute 'memacs-mode-line-blue nil
                    :inherit 'font-lock-keyword-face)

(make-face 'memacs-mode-line-purple)
(set-face-attribute 'memacs-mode-line-purple nil
                    :foreground "purple")

(make-face 'memacs-mode-line-default)
(set-face-attribute 'memacs-mode-line-default nil
                    :inherit 'mode-line-face)

;; ;;; mode-line ends here
