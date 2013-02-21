;; Add ELPA packages and emacs-eclim to the load path
(let ((default-directory "~/.emacs.d/elpa"))
  (normal-top-level-add-subdirs-to-load-path))

;; Variables
(setq eclim-auto-save t
      eclim-executable "/opt/eclipse/eclim"
      eclimd-executable "/opt/eclipse/eclimd"
      eclimd-wait-for-process nil
      eclimd-default-workspace "~/src/workspace"
      help-at-pt-display-when-idle t
      help-at-pt-timer-delay 0.1
      )

;; Load both eclim and eclimd (so that we can control eclimd from
;; within Emacs)
(require 'eclim)
(require 'eclimd)

;; Call the help framework with the settings above & activate
;; eclim-mode
(help-at-pt-set-timer)
(global-eclim-mode)

;; Hook eclim up with auto complete mode
(require 'auto-complete-config)
(ac-config-default)
(require 'ac-emacs-eclim-source)
(add-hook 'eclim-mode-hook (lambda ()
                             (add-to-list 'ac-sources 'ac-source-emacs-eclim)
                             (add-to-list 'ac-sources 'ac-source-emacs-eclim-c-dot)))
