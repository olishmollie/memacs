;;; mode-line.el --- Mode line packages and configuration.

;;; Commentary:

;;; Code:

(use-package doom-modeline
  :init
  (setq doom-modeline-height 35
        doom-modeline-project-detection t
        doom-modeline-buffer-file-name-style 'relative-to-project)

  :config (doom-modeline-mode))
;;; mode-line ends here
