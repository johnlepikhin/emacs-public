;;; my-flycheck --- My flycheck configuration

;;; Commentary:

;;; Code:

(require 'flycheck)
(require 'flycheck-inline)

(add-hook 'after-init-hook #'global-flycheck-mode)

(with-eval-after-load 'flycheck
  (add-hook 'flycheck-mode-hook #'flycheck-inline-mode))

(setq flycheck-display-errors-function nil
      flycheck-display-errors-delay 0.1)

(provide 'my-flycheck)

;;; my-flycheck ends here
