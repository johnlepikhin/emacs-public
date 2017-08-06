
;(require 'bbdb-autoloads)
(require 'bbdb)
(load "bbdb-com" t)
(bbdb-initialize 'gnus 'message 'reportmail 'w3)
;(bbdb-insinuate-reportmail)
(bbdb-insinuate-message)

(add-to-list 'load-path "~/.emacs.d/lisp/")
(autoload 'gmail2bbdb-import-file "gmail2bbdb" "" t)
(setq gmail2bbdb-bbdb-file "~/.emacs.d/bbdb")



;(add-hook 'message-mode-hook
;          '(lambda ()
;;             (unless *no-memory*
;;               (flyspell-mode 1))
;;             (bbdb-initialize 'message)
;;             (bbdb-initialize 'gnus)
;             (local-set-key "<TAB>" 'bbdb-complete-mail)))

(provide 'init-bbdb)
