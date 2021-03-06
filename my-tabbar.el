
(require 'tabbar)

(global-set-key [s-left] 'tabbar-backward-tab)
(global-set-key [s-right] 'tabbar-forward-tab)

(defun my-modification-state-change ()
  (tabbar-set-template tabbar-current-tabset nil)
  (tabbar-display-update))

(add-hook 'after-save-hook 'my-modification-state-change)
(add-hook 'first-change-hook 'my-modification-state-change)

(defadvice undo (after undo-after activate) (my-modification-state-change))
(defadvice undo-tree-undo (after undo-after activate) (my-modification-state-change))

(defun my-tabbar-buffer-groups () ;; customize to show all normal files in one group
   "Returns the name of the tab group names the current buffer belongs to.
 There are two groups: Emacs buffers (those whose name starts with '*', plus
 dired buffers), and the rest.  This works at least with Emacs v24.2 using
 tabbar.el v1.7."
   (list (cond ((string-match "Org Agenda" (buffer-name)) "Org-Agenda")
                ((and (string-match "-TODO[.]org$" (buffer-name)) (string-match "/work/" (buffer-file-name))) "Org-TODO-work")
                ((string-match "-TODO[.]org$" (buffer-name)) "Org-TODO")
                ((string-match "[.]s?org$" (buffer-name)) "Org")
                ((string-match "[.]s?org<" (buffer-name)) "Org")
                ((string-equal "*Group*" (buffer-name)) "Gnus")
                ((string-match "[*]Summary " (buffer-name)) "Gnus")
                ((string-match "[*]Article " (buffer-name)) "Gnus")
                ((string-match "[*]unsent " (buffer-name)) "Gnus")
                ((string-equal "*" (substring (buffer-name) 0 1)) "emacs")
                ((eq major-mode 'dired-mode) "emacs")
                ((string-equal "TAGS" (buffer-name)) "emacs")
                ((string-equal "bbdb" (buffer-name)) "emacs")
                ((string-match "org_archive$" (buffer-name)) "org-archives")
                ((string-match "[.]el$" (buffer-name)) "elisp")
                ((string-match "^magit[0-9a-z-]*:" (buffer-name)) "magit")
                (t "user"))))

(setq tabbar-buffer-groups-function 'my-tabbar-buffer-groups)

(global-set-key [s-up] 'tabbar-forward-group)
(global-set-key [s-down] 'tabbar-backward-group)

(tabbar-mode)

(provide 'my-tabber)
