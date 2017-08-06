
(require 'google-this)
(google-this-mode 1)

(defun jl/google-this-region-noconfirm (prefix)
  "Google the current line without confirmationl.
PREFIX determines quoting."
  (interactive "P")
  (google-this-region prefix 'noconfirm))

(global-set-key "\C-cgs" 'jl/google-this-region-noconfirm)
