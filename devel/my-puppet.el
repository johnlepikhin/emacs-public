
(defun my-puppet-setup ()
  (interactive)
  (setq puppet-indent-level 4
        puppet-include-indent 4))

(add-hook 'puppet-mode-hook 'my-puppet-setup)

(provide 'my-puppet)
