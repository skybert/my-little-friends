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
      erc-save-buffer-on-part t
      erc-hide-list '("JOIN" "PART" "QUIT"))

(erc-log-mode)

;; general options
(setq erc-default-server "localhost")

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
         "^<root> Message from unknown participant \\([^:]+\\):")
        (replace-match "<\\1>"))))
(add-hook 'erc-insert-modify-hook 'my-reformat-jabber-backlog)

;; social
(require 'erc-youtube)
(require 'erc-tweet)
(add-to-list 'erc-modules 'youtube)
(add-to-list 'erc-modules 'tweet)
(erc-update-modules)


(defun tkj-insert-happy-face()
  (interactive)
  (insert "😊"))

(defun tkj-insert-sad-face()
  (interactive)
  (insert "😠"))
