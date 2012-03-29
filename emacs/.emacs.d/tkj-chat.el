(require 'erc-log)

(setq erc-autojoin-mode t
      erc-default-server "catbert.escenic.com"
      erc-default-nicks "torstein"
      erc-autojoin-channels-alist
      '(("catbert.escenic.com" "#platform" "#innovation" "#developers")
        ("irc.freenode.net" "#tossug"))
      erc-log-channels-directory "~/.erc/logs/"
      erc-log-write-after-send t
      erc-save-buffer-on-part t
      erc-modules (quote (autojoin
                          button
                          completion
                          fill
                          irccontrols
                          keep-place
                          list
                          log
                          match
                          menu
                          move-to-prompt
                          netsplit
                          networks
                          noncommands
                          readonly
                          ring
                          smiley
                          stamp
                          spelling
                          track))
      )

      
;; save logs before quittign emacs
;;(defadvice save-buffers-kill-emacs (before save-logs (arg) activate)
;;  (save-some-buffers t (lambda ()
;;                       (when (eq major-mode 'erc-mode) t))
;;                         (when (and (eq major-mode 'erc-mode)
;;                                    (not (null buffer-file-name))))
;;                         ))
