
(defun my-yaml-setup ()
  (interactive)
  (setq yaml-indent-offset 4))

(add-hook 'yaml-mode-hook 'my-yaml-setup)


(provide 'my-yaml)
