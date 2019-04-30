
(defun my-puppet-setup ()
  (interactive)
  (setq puppet-indent-level 2
        puppet-include-indent 2))

(add-hook 'puppet-mode-hook 'my-puppet-setup)

(provide 'my-puppet)
