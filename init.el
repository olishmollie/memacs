;;; init.el --- My Emacs init script.

;;; Commentary:

;;; Code:
(require 'core (concat user-emacs-directory "modules/core/core.el"))

(setq memacs-project-directory "~/Dev/")

(setq memacs-modules '(c-c++
                       custom
		       completion
                       filetree
                       go
                       java
                       javascript
                       latex
		       mode-line
                       rust
		       terminal
		       version-control))

(dolist (module memacs-modules)
  (memacs/load-module module))
;;; init.el ends here
