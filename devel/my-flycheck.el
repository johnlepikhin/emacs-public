;;; my-flycheck --- My flycheck configuration

;;; Commentary:

;;; Code:

(require 'flycheck)

(add-hook 'after-init-hook #'global-flycheck-mode)

(setq flycheck-display-errors-function nil)

(provide 'my-flycheck)

;;; my-flycheck ends here
