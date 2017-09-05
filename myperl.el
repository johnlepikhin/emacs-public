

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

(defun google-cpan (string)
  "Google CPAN for string"
  (interactive)
  (progn
    (google-this-parse-and-search-string
     (concat "site:search.cpan.org " string)
     nil (google-this-lucky-search-url))
    (message (concat "Search CPAN for " string))))

(defun google-pbp-module (module)
  "Google for Perl best practices module"
  (interactive)
  (google-cpan (concat "\"Perl::Critic::Policy::" module "\"")))

(defun google-cpan-word ()
  "Google CPAN for word at point"
  (interactive)
  (google-cpan (thing-at-point 'word)))

(defun perlcritic-extract-module (string)
  (interactive)
  (if (string-match ", \\([A-Z][A-Za-z0-9]+::[A-Z][A-Za-z0-9:]+\\))" string)
      (match-string 1 string)
    nil))

(defun my-search-flymake-error()
  (interactive)
  (let ((err (get-char-property (point) 'help-echo)))
    (when err
      (let ((pbp-module (perlcritic-extract-module err)))
        (if pbp-module
            (google-pbp-module pbp-module)
          (google-this-string nil err 'noconfirm))))))

(defun perlcritic-disable-for-line ()
  (interactive)
  (let ((err (get-char-property (point) 'help-echo)))
    (if err
      (let ((pbp-module (perlcritic-extract-module err)))
        (if pbp-module
            (progn
              (move-beginning-of-line nil)
              (insert (concat "## no critic (" pbp-module) ")\n")
              (indent-according-to-mode)
              (move-end-of-line nil)
              (insert "\n## use critic"))))
      (message "No error found here"))))

(global-set-key [f3] 'flymake-display-err-menu-for-current-line)
(global-set-key [(control f3)] 'my-search-flymake-error)
(global-set-key [f4] 'flymake-goto-next-error)
(global-set-key [(control f4)] 'my-copy-flymake-error)
(global-set-key [f5] 'perlcritic-disable-for-line)
(global-set-key (kbd "C-c / p") 'google-cpan-word)

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
