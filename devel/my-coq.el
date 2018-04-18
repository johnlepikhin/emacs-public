
(load "~/.emacs.d/lisp/PG/generic/proof-site")

(defun my-configure-coq-mode ()
  (interactive)
  (company-coq-mode)
  (setq-local company-minimum-prefix-length 2)
  (coq-double-hit-enable)
  (auto-complete-mode -1))

(add-hook 'coq-mode-hook #'my-configure-coq-mode)

(setq coq-prog-name "coqtop")


(provide 'my-coq)
