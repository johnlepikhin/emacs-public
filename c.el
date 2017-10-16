
(require 'xcscope)
(require 'ggtags)

(setq
 c-default-style '((java-mode . "java")
                   (awk-mode . "awk")
                   (other . "gnu"))
 c-basic-offset 4
 tab-width 4)

(add-hook
 'c-mode-common-hook
 (lambda ()
   (when (derived-mode-p 'c-mode 'c++-mode 'java-mode 'asm-mode)
     (c-toggle-auto-newline -1)
     (setq ac-sources (append '(ac-source-semantic) ac-sources))
     (local-set-key (kbd "RET") 'newline-and-indent)
     (linum-mode t)
     (ggtags-mode 1))))

(define-key ggtags-mode-map (kbd "C-c g s") 'ggtags-find-other-symbol)
(define-key ggtags-mode-map (kbd "C-c g h") 'ggtags-view-tag-history)
(define-key ggtags-mode-map (kbd "C-c g r") 'ggtags-find-reference)
(define-key ggtags-mode-map (kbd "C-c g f") 'ggtags-find-file)
(define-key ggtags-mode-map (kbd "C-c g c") 'ggtags-create-tags)
(define-key ggtags-mode-map (kbd "C-c g u") 'ggtags-update-tags)
(define-key ggtags-mode-map (kbd "M-,") 'pop-tag-mark)
