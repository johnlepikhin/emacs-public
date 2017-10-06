
(setq home-directory (getenv "HOME"))

;; update PATH
(setenv "PATH" (concat (getenv "PATH") ":" home-directory "/bin:" home-directory "/perl5/bin"))

;(exec-path-from-shell-initialize)

; (add-to-list 'exec-path `(concat home-directory "/bin"))
; (add-to-list 'exec-path `(concat home-directory "/perl5/bin"))
; (setq exec-path
;       (let ((newdirs '(`(concat home-directory "/bin")
;                        `(concat home-directory "/perl5/bin"))))
;         (append exec-path newdirs)))

;; hide welcome screen
(setq inhibit-startup-screen t)

;; disable toolbar
(tool-bar-mode -1)

;; disable backup files
(setq make-backup-files nil)

;; save minubuffer history
(savehist-mode 1)

;; show matched paren
(global-set-key (kbd "C-`") 'match-paren)

(defun match-paren (arg)
  "Go to the matching paren if on a paren; otherwise insert %."
  (interactive "p")
  (cond ((looking-at "\\s\(") (forward-list 1) (backward-char 1))
        ((looking-at "\\s\)") (forward-char 1) (backward-list 1))
        (t (self-insert-command (or arg 1)))))


(defun comment-dwim-line-or-region ()
  "[Un]comment line or region"
  (interactive)
  (if mark-active
      (comment-dwim t)
    (progn
      (comment-line 1)
      (previous-line))))

(global-set-key (kbd "C-;") 'comment-dwim-line-or-region)


(setq comment-style 'multi-line)

(global-set-key [C-return] (lambda () (interactive) (point-to-register 'r)))
(global-set-key [M-return] (lambda () (interactive) (jump-to-register 'r)))

;; my coordinates

;; Moscow

(setq
 calendar-location-name "Moscow"
 calendar-latitude 55.5
 calendar-longitude 37.4)

;; different things

(setq-default indent-tabs-mode nil)
(setq line-number-mode t)
(setq column-number-mode t)

(ac-config-default)

(setq browse-url-browser-function 'browse-url-chromium)

;; isearch keybindings

(define-key isearch-mode-map (kbd "<up>") 'isearch-ring-retreat )
(define-key isearch-mode-map (kbd "<down>") 'isearch-ring-advance )

(define-key isearch-mode-map (kbd "<left>") 'isearch-repeat-backward)
(define-key isearch-mode-map (kbd "<right>") 'isearch-repeat-forward)

(define-key minibuffer-local-isearch-map (kbd "<left>") 'isearch-reverse-exit-minibuffer)
(define-key minibuffer-local-isearch-map (kbd "<right>") 'isearch-forward-exit-minibuffer)
