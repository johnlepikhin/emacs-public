
(require 'xcscope)
(require 'ggtags)

(setq
 c-default-style '((java-mode . "java")
                   (awk-mode . "awk")
                   (other . "gnu"))
 c-basic-offset 4
 tab-width 4)

(defun my-man2 (word)
  (interactive)
  (manual-entry (concat word "(2)")))

(defun my-man2-current-word ()
  (interactive)
  (my-man2 (current-word)))

(add-hook
 'c-mode-common-hook
 (lambda ()
   (when (derived-mode-p 'c-mode 'c++-mode 'java-mode 'asm-mode)
     (c-toggle-auto-newline -1)
     (setq ac-sources (append '(ac-source-semantic) ac-sources)
           c-default-style "linux"
           c-basic-offset 4)
     (local-set-key (kbd "RET") 'newline-and-indent)
     (local-set-key [f1] 'my-man2-current-word)
     (linum-mode t)
     (ggtags-mode 1))))

(define-key ggtags-mode-map (kbd "C-c g s") 'ggtags-find-other-symbol)
(define-key ggtags-mode-map (kbd "C-c g h") 'ggtags-view-tag-history)
(define-key ggtags-mode-map (kbd "C-c g r") 'ggtags-find-reference)
(define-key ggtags-mode-map (kbd "C-c g f") 'ggtags-find-file)
(define-key ggtags-mode-map (kbd "C-c g c") 'ggtags-create-tags)
(define-key ggtags-mode-map (kbd "C-c g u") 'ggtags-update-tags)
(define-key ggtags-mode-map (kbd "M-,") 'pop-tag-mark)
