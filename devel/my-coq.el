
(load "~/.emacs.d/lisp/PG/generic/proof-site")

(require 'my-coq-ssr)

(defun my-configure-coq-mode ()
  (interactive)
  (company-coq-mode)
  (setq-local company-minimum-prefix-length 1)
  (setq-local coq-double-hit-enable t)
  (auto-complete-mode -1))

(add-hook 'coq-mode-hook #'my-configure-coq-mode)

(setq coq-prog-name "coqtop")


(provide 'my-coq)
