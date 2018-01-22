
(require 'helm-for-files)
(require 'helm-locate)
(require 'helm-bbdb)
(require 'helm-info)
(require 'helm-notmuch)
(require 'recentf)
(require 'helm-net)

(recentf-mode 1)
(setq recentf-max-menu-items 25)

(when (executable-find "curl")
  (setq helm-google-suggest-use-curl-p t)
  (setq helm-net-prefer-curl t))

(setq helm-wikipedia-suggest-url "https://ru.wikipedia.org/w/api.php?action=opensearch&search=%s"
      helm-wikipedia-summary-url "https://ru.wikipedia.org/w/api.php?action=parse&format=json&prop=text&section=0&page=%s"
      helm-search-suggest-action-wikipedia-url "https://en.wikipedia.org/wiki/Special:Search?search=%s")

(defun my-helm-search-all ()
  (interactive)
  (helm :sources '(helm-source-buffers-list
                   helm-source-recentf
                   helm-source-info-pages
                   helm-source-notmuch
                   helm-source-bbdb
                   helm-source-wikipedia-suggest
                   helm-source-google-suggest)
        :buffer "*helm completions*"))

(global-set-key "\M-s\ \M-s" 'my-helm-search-all)

(provide 'my-helm)
