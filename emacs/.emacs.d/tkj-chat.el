;; logging
(require 'erc-log)
<<<<<<< HEAD
(setq erc-log-channels-directory "~/.erc/logs/"
=======

(setq erc-autojoin-mode t
      erc-default-server "localhost"
      erc-default-nicks "tkj"
      erc-autojoin-channels-alist
      '(("catbert.escenic.com" "#platform" "#innovation" "#developers")
        ("localhost" "saas@conference.chat.ardendo.se")
        ("irc.freenode.net" "#tossug"))
      erc-log-channels-directory "~/.erc/logs/"
>>>>>>> 81815c5fa2375fa6bb7ceb0e07f2d4464ec585d6
      erc-log-write-after-send t
      erc-save-buffer-on-part t)

<<<<<<< HEAD
;; auto join
(require 'erc-join)
(erc-autojoin-mode 1)
(setq erc-autojoin-channels-alist
      '(
        ("catbert.escenic.com" "#developers" "#platform")
        ))

;; general options
(setq erc-default-server "localhost")

;; connect to these IRC servers
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

=======
      
;; save logs before quittign emacs
;;(defadvice save-buffers-kill-emacs (before save-logs (arg) activate)
;;  (save-some-buffers t (lambda ()
;;                       (when (eq major-mode 'erc-mode) t))
;;                         (when (and (eq major-mode 'erc-mode)
;;                                    (not (null buffer-file-name))))
;;                         ))
>>>>>>> 81815c5fa2375fa6bb7ceb0e07f2d4464ec585d6
