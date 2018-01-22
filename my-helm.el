
(require 'helm-for-files)
(require 'helm-locate)
(require 'helm-bbdb)
(require 'helm-notmuch)
(require 'recentf)

(recentf-mode 1)
(setq recentf-max-menu-items 25)

(defun my-helm-search-all ()
  (interactive)
  (helm :sources '(helm-source-buffers-list
                   helm-source-info-pages
                   helm-source-notmuch
                   helm-source-bbdb
                   helm-source-google-suggest
                   helm-source-locate)
        :buffer "*helm completions*"))

(global-set-key "\M-s\ \M-s" 'my-helm-search-all)

(provide 'my-helm)
