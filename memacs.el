;;; memacs.el -- MeMacs load configuration.

;;; Commentary:

;;; Code:

(require 'core (concat user-emacs-directory "modules/core/core.el"))

(setq memacs-modules '(basic
                       c-c++
                       custom
		       completion
                       dlang
                       filetree
                       flycheck
                       go
                       java
                       javascript
                       latex
                       lsp
		       mode-line
                       rust
		       terminal
                       verilog
		       version-control))

(dolist (module memacs-modules)
  (memacs/load-module module))

;;; memacs.el ends here
