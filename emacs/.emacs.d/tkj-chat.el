;; logging
(require 'erc-log)
(setq erc-log-channels-directory "~/.erc/logs/"
      erc-log-write-after-send t
      erc-save-buffer-on-part t)

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
(defun tkj-load-chat()
  (interactive)
  (erc :server "localhost" :port 6667 :nick "tkj")
  (erc :server "catbert.escenic.com" :port 6667 :nick "torstein"))

(defun my-reformat-jabber-backlog ()
  "Fix \"unkown participant\" backlog messages from bitlbee."
  (save-excursion
    (goto-char (point-min))
    (if (looking-at
         "^<root> System message: Message from unknown participant \\([^:]+\\):")
        (replace-match "<\\1>"))))
(add-hook 'erc-insert-modify-hook 'my-reformat-jabber-backlog)

