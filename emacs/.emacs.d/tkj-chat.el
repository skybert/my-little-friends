;; logging & setting idle
(require 'erc-log)
(require 'erc-autoaway)
(require 'erc-image)
(require 'erc-colorize)

(setq erc-default-server "localhost"
      erc-log-channels-directory "~/.erc/logs"
      erc-log-write-after-send t
      erc-autoaway-idle-seconds 600
      erc-enable-logging t
      erc-save-buffer-on-part t)

(erc-log-mode)

;; general options
(setq erc-default-server "localhost")

;; connect to these IRC servers
(defun tkj-load-chat()
  (interactive)
  (erc :server "localhost" :port 6667 :nick "torstein"))

;; Spell checking
(add-hook 'erc-mode-hook 'flyspell-mode)

;; auto completion
(add-hook 'erc-mode-hook 'auto-complete-mode)
;; From the bitlbee wiki: Since the server sends wrong JIDs for the
;; "from" field (123456_chat_name@conf.hipchat.com/real name here),
;; all you can do is using client scripts to fix this up
(defun my-reformat-jabber-backlog ()
  (save-excursion
    (goto-char (point-min))
    (if (looking-at
         "^<root> Message from unknown participant Your Name:")
        (replace-match "<yournick>"))))
(add-hook 'erc-insert-modify-hook 'my-reformat-jabber-backlog)
