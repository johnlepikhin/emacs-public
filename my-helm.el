
(require 'helm-for-files)
(require 'helm-locate)
(require 'helm-bbdb)
(require 'helm-info)
(require 'helm-notmuch)
(require 'recentf)
(require 'helm-buffers)
(require 'helm-net)
(require 'helm-org-rifle)
(require 'helm-elisp)

(recentf-mode 1)
(setq recentf-max-menu-items 25)

(when (executable-find "curl")
  (setq helm-google-suggest-use-curl-p t)
  (setq helm-net-prefer-curl t))

(setq helm-wikipedia-suggest-url "https://en.wikipedia.org/w/api.php?action=opensearch&search=%s"
      helm-wikipedia-summary-url "https://en.wikipedia.org/w/api.php?action=parse&format=json&prop=text&section=0&page=%s"
      helm-search-suggest-action-wikipedia-url "https://en.wikipedia.org/wiki/Special:Search?search=%s")

(setq helm-input-idle-delay 0.1)

(setq my-helm-sources
            '(helm-source-buffers-list
                   helm-source-recentf
                   helm-source-info-pages
                   helm-source-notmuch
                   helm-source-complex-command-history
                   helm-source-bbdb
                   helm-source-etags-select
                   ; helm-source-wikipedia-suggest
                   ; helm-source-google-suggest
                   ))

(defun my-helm-search-all ()
  (interactive)
  (require 'helm-x-files)
  (let ((sources (append (helm-org-rifle-get-sources-for-open-buffers) my-helm-sources)))
    (unless helm-source-buffers-list
      (setq helm-source-buffers-list
            (helm-make-source "Buffers" 'helm-source-buffers)))
    (helm :sources sources
          :buffer "*helm completions*")))

(global-set-key "\M-s\ \M-s" 'my-helm-search-all)

(provide 'my-helm)
