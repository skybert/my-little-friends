;; ERC modules & hipchat integration
(require 'erc-log)
(require 'erc-autoaway)
(require 'erc-image)
(require 'erc-colorize)
(require 'erc-tweet)
(require 'ac-hipchat-nick)

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

;; Adjust text wrapping/filling whenever the window is resized
(add-hook 'window-configuration-change-hook
          '(lambda ()
             (setq erc-fill-column (- (window-width) 2))))

;; Spell checking
(add-hook 'erc-mode-hook 'flyspell-mode)

;; auto completion
(add-hook 'erc-mode-hook 'auto-complete-mode)

;; auto completion of HipChat nicks
(add-hook 'erc-mode-hook 'ac-hipchat-nick-setup)

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

(add-to-list 'erc-modules 'tweet)
(erc-update-modules)

(defun tkj-insert-happy-face()
  (interactive)
  (insert "😊"))

(defun tkj-insert-sad-face()
  (interactive)
  (insert "😠"))

(defun tkj-insert-winking-face()
  (interactive)
  (insert "😉"))

(defun tkj-insert-right-arrow()
  (interactive)
  (insert "→"))

(defun tkj-insert-scream()
  (interactive)
  (insert "😱"))

(defun tkj-insert-kiss()
  (interactive)
  (insert "💏"))

(defun tkj-insert-thumbs-up()
  (interactive)
  (insert "👍"))

(defun tkj-close-all-chats()
  (interactive)
  (condition-case nil (kill-buffer "#platform") (error nil))
  (condition-case nil (kill-buffer "#developers") (error nil))
  (condition-case nil (kill-buffer "#all") (error nil))
  (condition-case nil (kill-buffer "#cue") (error nil))
  (condition-case nil (kill-buffer "#support") (error nil)))
