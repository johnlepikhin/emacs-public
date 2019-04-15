
;;;; Code:

(setenv "PERL_LOCAL_LIB_ROOT" (concat (getenv "PERL_LOCAL_LIB_ROOT") ":" (getenv "HOME") "/perl5"))
(setenv "PERL5LIB" (concat (getenv "PERL5LIB") ":" (getenv "HOME") "/perl5/lib/perl5"))

(defalias 'perl-mode 'cperl-mode)

(require 'tramp)
(require 'helm-perldoc)

; (add-to-list 'company-backends 'company-plsense)
; (add-hook 'perl-mode-hook 'company-mode)
; (add-hook 'cperl-mode-hook 'company-mode)

(defun perl-insert-json ()
  (interactive)
  (shell-command-on-region (point) (point) "xclip -o | json_to_perl.pl" t))

(defun perl-mode-perltidy ()
  "Perltidy buffer or region if this is perl file."
  (interactive)
  (let ((saved-line (line-number-at-pos)))
    (save-excursion
      (when (eq major-mode 'cperl-mode)
        (if (use-region-p)
            (perltidy-region (region-beginning) (region-end))
          (perltidy-buffer))))
    (goto-line saved-line)))

(defun my-perltidy-subroutine ()
  "Perltidy current subroutine keeping current position in the buffer as close as possible"
  (interactive)
  (let ((saved-line (line-number-at-pos)))
    (perltidy-subroutine)
    (goto-line saved-line)))

(defun my-perl-tab-indent ()
  (interactive)
  (if (use-region-p)
      (perltidy-region (region-beginning) (region-end))
    (cperl-indent-command)))

(require 'dropdown-list)

(defun perlcritic-disable-for-line ()
  (interactive)
  (let ((error-id (flycheck-error-id (car (flycheck-overlay-errors-at (point))))))
    (if error-id
        (progn
          (move-beginning-of-line nil)
          (insert (concat "## no critic (" error-id) ")\n")
          (indent-according-to-mode)
          (move-end-of-line nil)
          (insert "\n## use critic")))
    (message "No error found here")))

(setq my-perlysense-is-loaded 'nil)
(defun my-load-perlysense ()
  (interactive)
  (if (not my-perlysense-is-loaded)
      (progn
        (setq my-perlysense-is-loaded 't)
        (global-unset-key "\C-o")
        (setq ps/key-prefix "\C-o")

        ;; *** PerlySense load (don't touch) ***
        (setq ps/external-dir (shell-command-to-string "perly_sense external_dir"))
        (if (string-match "Devel.PerlySense.external" ps/external-dir)
            (progn
              (message
               "PerlySense elisp files  at (%s) according to perly_sense, loading..."
               ps/external-dir)
              (setq-local load-path (cons
                                     (expand-file-name
                                      (format "%s/%s" ps/external-dir "emacs")
                                      ) load-path))
              (load "perly-sense")
              )
          (message "Could not identify PerlySense install dir.
    Is Devel::PerlySense installed properly?
    Does 'perly_sense external_dir' give you a proper directory? (%s)" ps/external-dir)
          )

        ;; ** Misc Config **

        ;; Run calls to perly_sense as a prepared shell command. Experimental
        ;; optimization, please try it out.
        (setq ps/use-prepare-shell-command t))))

(defun my-perl-goto-vc-project ()
  (interactive)
  ;; (perl-mode-perltidy-buffer)
  (save-buffer)
  (ps/go-to-vc-project))

(defun my-cperl-mode-setup ()
  (prettify-symbols-mode)
  (setq-local cperl-indent-level 4)
  (helm-perldoc:setup)
  (my-load-perlysense)

  (local-set-key (kbd "C-c i d") 'my-perltidy-subroutine)
  (local-set-key (kbd "C-c i b") 'perl-mode-perltidy)
  (local-set-key (kbd "C-c h") 'helm-perldoc-at-point)
  (local-set-key (kbd "TAB") 'my-perl-tab-indent)

  ;; (add-hook 'before-save-hook #'perl-mode-perltidy-buffer t)
  (local-set-key (kbd "M-;") 'hippie-expand)
  (local-set-key [f5] 'perlcritic-disable-for-line)
  (local-set-key (kbd "C-c / p") 'google-cpan-word)
  (local-set-key (kbd "C-c j") 'perl-insert-json)
  (local-set-key (kbd "C-x C-M-d") 'perl-document-current-function)
  (local-set-key (kbd "C-x C-M-s") 'perl-insert-sub-template)
  (local-set-key (kbd "\M-.") 'helm-etags-plus-select)
  (local-set-key (kbd "\M-,") 'helm-etags-plus-history-go-back))

(add-hook 'cperl-mode-hook 'my-cperl-mode-setup)

(setq cperl-highlight-variables-indiscriminately t)

(eval-after-load "cperl-mode"
  '(progn
     (helm-perldoc:setup)))

(provide 'my-perl)
