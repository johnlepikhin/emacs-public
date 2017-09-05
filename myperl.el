

(setenv "PERL_LOCAL_LIB_ROOT" (concat (getenv "PERL_LOCAL_LIB_ROOT") ":" (getenv "HOME") "/perl5"))
(setenv "PERL5LIB" (concat (getenv "PERL5LIB") ":" (getenv "HOME") "/perl5/lib/perl5"))


(defalias 'perl-mode 'cperl-mode)

(setq cperl-indent-level 4)

(global-set-key (kbd "M-;") 'hippie-expand)


(defun perl-mode-perltidy-buffer ()
  "perltidy buffer if this is perl file"
  (interactive)
  (when (eq major-mode 'cperl-mode)
    (perltidy-buffer)))

(add-hook 'before-save-hook #'perl-mode-perltidy-buffer)



(require 'dropdown-list)
(require 'rfringe)
(require 'flymake)

(defun my-copy-flymake-error()
  (interactive)
  (let ((err (get-char-property (point) 'help-echo)))
    (when err
      (progn
        (message "Copied warning to kill-ring")
        (kill-new err)))))

(defun google-pbp-module (module)
  "Google for Perl best practices module"
  (interactive)
  (progn
    (google-this-parse-and-search-string
     (concat "site:search.cpan.org \"Perl::Critic::Policy::" module "\"")
     nil (google-this-lucky-search-url))
    (message (concat "Googling for " module))))

(defun my-search-flymake-error()
  (interactive)
  (let ((err (get-char-property (point) 'help-echo)))
    (when err
      (if (string-match ", \\([A-Z][A-Za-z0-9]+::[A-Z][A-Za-z0-9:]+\\))" err)
          (google-pbp-module (match-string 1 err))
        (google-this-string nil err 'noconfirm)))))

(global-set-key [f3] 'flymake-display-err-menu-for-current-line)
(global-set-key [(control f3)] 'my-search-flymake-error)
(global-set-key [f4] 'flymake-goto-next-error)
(global-set-key [(control f4)] 'my-copy-flymake-error)

(require 'flymake-perlcritic)

(setq ps/load-flymake t)

;; *** PerlySense Config ***

;; ** PerlySense **
;; The PerlySense prefix key (unset only if needed, like for \C-o)
(global-unset-key "\C-o")
(setq ps/key-prefix "\C-o")

(setq ps/load-flymake t)

;; *** PerlySense load (don't touch) ***
(setq ps/external-dir (shell-command-to-string "perly_sense external_dir"))
(if (string-match "Devel.PerlySense.external" ps/external-dir)
    (progn
      (message
       "PerlySense elisp files  at (%s) according to perly_sense, loading..."
       ps/external-dir)
      (setq load-path (cons
                       (expand-file-name
                        (format "%s/%s" ps/external-dir "emacs")
                        ) load-path))
      (load "perly-sense")
      )
  (message "Could not identify PerlySense install dir.
    Is Devel::PerlySense installed properly?
    Does 'perly_sense external_dir' give you a proper directory? (%s)" ps/external-dir)
  )


;; ** Flymake Config **
;; If you only want syntax check whenever you save, not continously
(setq flymake-no-changes-timeout 9999)
(setq flymake-start-syntax-check-on-newline nil)

;; ** Code Coverage Visualization **
;; If you have a Devel::CoverX::Covered database handy and want to
;; display the sub coverage in the source, set this to t
(setq ps/enable-test-coverage-visualization nil)

;; ** Color Config **
;; Emacs named colors: http://www.geocities.com/kensanata/colors.html
;; The following colors work fine with a white X11
;; background. They may not look that great on a console with the
;; default color scheme.
(set-face-background 'flymake-errline "antique white")
(set-face-background 'flymake-warnline "lavender")
(set-face-background 'dropdown-list-face "lightgrey")
(set-face-background 'dropdown-list-selection-face "grey")


;; ** Misc Config **

;; Run calls to perly_sense as a prepared shell command. Experimental
;; optimization, please try it out.
(setq ps/use-prepare-shell-command t)

;; *** PerlySense End ***
