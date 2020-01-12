;;; init.el --- My Emacs init script.

;;; Commentary:

;;; Code:
(require 'core (concat user-emacs-directory "modules/core/core.el"))

(setq memacs-project-directory "~/Dev/")

(setq memacs-modules '(filetree
		       version-control
		       completion
		       terminal
		       mode-line
                       c-c++
                       latex))

(dolist (module memacs-modules)
  (memacs/load-module module))
;;; init.el ends here
