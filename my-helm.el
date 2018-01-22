
(require 'recentf)

(recentf-mode 1)
(setq recentf-max-menu-items 25)

(defun my-helm-search-all ()
  (interactive)
  (helm :sources '(helm-source-buffers-list
                   helm-source-recentf
                   helm-source-info-pages
                   helm-source-notmuch
                   helm-source-info-bbdb
                   helm-source-google-suggest)
        :buffer "*helm my command*"))

(global-set-key "\M-s\ \M-s" 'my-helm-search-all)

(provide 'my-helm)
