;;; my-yasnippet --- My YASnippet configuration

(require 'yasnippet)

;;; Commentary:

;;; Code:

(setq yas-snippet-dirs '("/home/evgenii/.emacs.d/public/yasnippets"))

(yas-global-mode 1)

(define-key yas-minor-mode-map (kbd "C-c y") #'yas-expand)

(provide 'my-yasnippet)
;;; my-yasnippet ends here
