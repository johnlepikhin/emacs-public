
(require 'google-translate)
(require 'google-translate-default-ui)
(global-set-key "\C-cgt" 'google-translate-at-point)
(global-set-key "\C-cgT" 'google-translate-query-translate)
(setq google-translate-default-target-language "ru")
(setq google-translate-default-source-language "auto")
