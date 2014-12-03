;; Windows specific settings

;; Make sure that the bash executable can be found
(setq explicit-shell-file-name "c:/apps/cygwin/bin/bash.exe")
(setq shell-file-name explicit-shell-file-name)
(add-to-list 'exec-path "c:/apps/cygwin/bin")
