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
                       rust
		       terminal
                       verilog
		       version-control))

(setq memacs-faces '(mode-line
                     themes))

;;; memacs.el ends here
