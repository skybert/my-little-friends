(add-to-list 'load-path "~/src/emacs-eclim")

(require 'eclim)
(global-eclim-mode)

;; Variables
(setq eclim-auto-save t
      eclim-executable "/opt/eclipse/eclim"
      eclimd-executable "/opt/eclipse/eclimd"
      eclimd-wait-for-process nil
      eclimd-default-workspace "~/src/workspace-eclim"
      eclim-use-yasnippet nil
      help-at-pt-display-when-idle t
      help-at-pt-timer-delay 0.1
      ac-delay 0.5
      )

;; Call the help framework with the settings above & activate
;; eclim-mode
(help-at-pt-set-timer)

;; Hook eclim up with auto complete mode
(require 'auto-complete-config)
(ac-config-default)
(require 'ac-emacs-eclim-source)
(ac-emacs-eclim-config)
