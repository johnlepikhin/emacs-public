
(require 'undo-tree)

;; (global-undo-tree-mode 1)

(global-set-key (kbd "C-x u") 'undo-tree-visualize)

; (defalias 'undo 'undo-tree-undo)
; (global-set-key (kbd "C-z") 'undo)

; (defalias 'redo 'undo-tree-redo)
; (global-set-key (kbd "C-S-z") 'redo)

(provide 'my-undo-tree)
