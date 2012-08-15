(require 'erc-log)

(setq erc-default-server "localhost"
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

(setq erc-autojoin-channels-alist
      '(
        ("catbert.escenic.com" "#developers" "#platform")
        ("irc.freenode.net" "#emacs")
        ))

(erc :server "localhost" :port 6667 :nick "tkj")
(erc :server "catbert.escenic.com" :port 6667 :nick "torstein")

(defun my-reformat-jabber-backlog ()
  "Fix \"unkown participant\" backlog messages from bitlbee."
  (save-excursion
    (goto-char (point-min))
    (if (looking-at
         "^<root> System message: Message from unknown participant \\([^:]+\\):")
        (replace-match "<\\1>"))))
(add-hook 'erc-insert-modify-hook 'my-reformat-jabber-backlog)

;; save logs before quittign emacs
;;(defadvice save-buffers-kill-emacs (before save-logs (arg) activate)
;;  (save-some-buffers t (lambda ()
;;                       (when (eq major-mode 'erc-mode) t))
;;                         (when (and (eq major-mode 'erc-mode)
;;                                    (not (null buffer-file-name))))
;;                         ))
