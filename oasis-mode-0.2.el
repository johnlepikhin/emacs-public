;;; oasis-mode.el --- major-mode for editing OCaml _oasis files

;; Copyright 2016
;; Keith WACLENA <http://www.lib.uchicago.edu/keith/>

;; Author: Keith WACLENA <http://www.lib.uchicago.edu/keith/>
;; Version: 0.2

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:
;; This `oasis-mode' is designed to edit _oasis files, as used in the
;; OCaml ecosystem.

;;; Code:

(defconst oasis-mode-version "0.0"
  "The version of oasis-mode.")

(defvar oasis-mode-font-lock-keywords
  `(;; var: val
    ("^[[:blank:]]*\\([[:alnum:]]+\\(\\$?:\\|\\+\\)\\).*"
     (1 'font-lock-variable-name-face)
     (2 'font-lock-variable-name-face))
    ;; sections
    ("^\\([[:alpha:]]+\\)[[:blank:]]*\\(.*\\)"
     (1 'font-lock-keyword-face)
     (2 'font-lock-constant-face))
    ;; conditionals
    ("^[[:blank:]]+\\(if\\|else\\)[[:blank:]]*\\(.*\\)[[:blank:]]*$"
     (1 'font-lock-builtin-face)
     (2 'font-lock-function-name-face)))
  "Keywords to highlight in Oasis mode.")

(add-to-list 'auto-mode-alist '("/_oasis$" . oasis-mode))

(declare-function conf-mode-initialize "conf-mode")

;;;###autoload
(define-derived-mode oasis-mode conf-colon-mode "Oasis"
  "Major mode for editing OCaml _oasis files.
Comments start with `#'.
For details see `conf-colon-mode'.  Example:

Executable versions
  Path:       .
  MainIs:     versions.ml"
  (require 'conf-mode)
  (define-key oasis-mode-map [(control c) (control c)] 'compile)
  (conf-mode-initialize "#" 'oasis-mode-font-lock-keywords))

(provide 'oasis-mode)

;;; oasis-mode.el ends here
