
(require 'go-autocomplete)
(require 'auto-complete-config)
(ac-config-default)

(setq ac-go-gocode-bin "~/work/go/bin/gocode"
      godef-command "~/work/go/bin/godef"
      gofmt-command "~/bin/go/bin/gofmt")

(provide 'my-golang)
