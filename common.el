
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

;; save file positions
(save-place-mode 1)

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
      (forward-line -1))))

(global-set-key (kbd "C-;") 'comment-dwim-line-or-region)


(setq comment-style 'multi-line)

(global-set-key [C-return] (lambda () (interactive) (point-to-register 'r)))
(global-set-key [M-return] (lambda () (interactive) (jump-to-register 'r)))

;; me

(setq user-full-name "Evgenii Lepikhin")
(setq user-mail-address "johnlepikhin@gmail.com")

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

(require 'auto-complete)
(require 'auto-complete-config)
(ac-config-default)

(setq browse-url-browser-function 'browse-url-chromium)

;; isearch keybindings

(define-key isearch-mode-map (kbd "<up>") 'isearch-ring-retreat )
(define-key isearch-mode-map (kbd "<down>") 'isearch-ring-advance )

(define-key isearch-mode-map (kbd "<left>") 'isearch-repeat-backward)
(define-key isearch-mode-map (kbd "<right>") 'isearch-repeat-forward)

(define-key minibuffer-local-isearch-map (kbd "<left>") 'isearch-reverse-exit-minibuffer)
(define-key minibuffer-local-isearch-map (kbd "<right>") 'isearch-forward-exit-minibuffer)





;; hydra in rectangle mode
; (require 'hydra)
; (require 'rect)
; (defhydra hydra-rectangle (:body-pre (rectangle-mark-mode 1)
;                            :color pink
;                            :post (deactivate-mark))
;   "
;   ^_k_^     _d_elete    _s_tring
; _h_   _l_   _o_k        _y_ank
;   ^_j_^     _n_ew-copy  _r_eset
; ^^^^        _e_xchange  _u_ndo
; ^^^^        ^ ^         _p_aste
; "
;   ("h" rectangle-backward-char nil)
;   ("l" rectangle-forward-char nil)
;   ("k" rectangle-previous-line nil)
;   ("j" rectangle-next-line nil)
;   ("e" hydra-ex-point-mark nil)
;   ("n" copy-rectangle-as-kill nil)
;   ("d" delete-rectangle nil)
;   ("r" (if (region-active-p)
;            (deactivate-mark)
;          (rectangle-mark-mode 1)) nil)
;   ("y" yank-rectangle nil)
;   ("u" undo nil)
;   ("s" string-rectangle nil)
;   ("p" kill-rectangle nil)
;   ("o" nil nil))

; (global-set-key (kbd "C-x SPC") 'hydra-rectangle/body)


;; hydra in dired
(defhydra hydra-dired (:hint nil :color pink)
  "
_+_ mkdir          _v_iew           _m_ark             _(_ details        _i_nsert-subdir    wdired
_C_opy             _O_ view other   _U_nmark all       _)_ omit-mode      _$_ hide-subdir    C-x C-q : edit
_D_elete           _o_pen other     _u_nmark           _l_ redisplay      _w_ kill-subdir    C-c C-c : commit
_R_ename           _M_ chmod        _t_oggle           _g_ revert buf     _e_ ediff          C-c ESC : abort
_Y_ rel symlink    _G_ chgrp        _E_xtension mark   _s_ort             _=_ pdiff
_S_ymlink          ^ ^              _F_ind marked      _._ toggle hydra   \\ flyspell
_r_sync            ^ ^              ^ ^                ^ ^                _?_ summary
_z_ compress-file  _A_ find regexp
_Z_ compress       _Q_ repl regexp

T - tag prefix
"
  ("\\" dired-do-ispell)
  ("(" dired-hide-details-mode)
  (")" dired-omit-mode)
  ("+" dired-create-directory)
  ("=" diredp-ediff)         ;; smart diff
  ("?" dired-summary)
  ("$" diredp-hide-subdir-nomove)
  ("A" dired-do-find-regexp)
  ("C" dired-do-copy)        ;; Copy all marked files
  ("D" dired-do-delete)
  ("E" dired-mark-extension)
  ("e" dired-ediff-files)
  ("F" dired-do-find-marked-files)
  ("G" dired-do-chgrp)
  ("g" revert-buffer)        ;; read all directories again (refresh)
  ("i" dired-maybe-insert-subdir)
  ("l" dired-do-redisplay)   ;; relist the marked or singel directory
  ("M" dired-do-chmod)
  ("m" dired-mark)
  ("O" dired-display-file)
  ("o" dired-find-file-other-window)
  ("Q" dired-do-find-regexp-and-replace)
  ("R" dired-do-rename)
  ("r" dired-do-rsynch)
  ("S" dired-do-symlink)
  ("s" dired-sort-toggle-or-edit)
  ("t" dired-toggle-marks)
  ("U" dired-unmark-all-marks)
  ("u" dired-unmark)
  ("v" dired-view-file)      ;; q to exit, s to search, = gets line #
  ("w" dired-kill-subdir)
  ("Y" dired-do-relsymlink)
  ("z" diredp-compress-this-file)
  ("Z" dired-do-compress)
  ("q" nil)
  ("." nil :color blue))

(eval-after-load "dired" '(progn (define-key dired-mode-map "." 'hydra-dired/body)))
